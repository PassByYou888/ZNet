{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Author:       Angus Robertson, Magenta Systems Ltd
Description:  TIcsFtpMulti is a high level FTP Delphi component that allows uploading
              or downloading of multiple files from or to an FTP server, from a
              single function call.
Creation:     May 2001
Updated:      Aug 2023
Version:      V9.0
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2023 by Angus Robertson, Magenta Systems Ltd,
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


TIcsFtpMulti is a high level FTP Delphi component that allows uploading or
downloading of multiple files from or to an FTP server, from a single
function call.  The component handles listing local and remote files,
including subdirectories, and comparing them to avoid unnecessary
transfers, selection using a file mask, deletion of old files, resuming
failed FTP downloads, unzipping of downloaded files, or zipping before
upload, deletion after uploading or downloading, archive copy after
upload.  A progress event provides various levels of information for
logging or display, depending upon application requirements, and allows
transfers to be cancelled.

TIcsFtpMulti descends from ICS TFtpClient, and publishes all it's logon and proxy
properties and events.  The component is undocumented, except for source code
comments, but end user help is available describing most of the functionality in
Magenta Systems DUN Manager application (from https://www.magsys.co.uk/dunman/),
look under Scheduled Task Properties, FTP General, FTP Common, FTP Upload and FTP
Download. DUN Manager also provides a compiled test application for TIcsFtpMulti (but
DM source code is not available).


Main functions:

FtpDir - build file directory from FTP server
FtpLogon - connect and logon to FTP server
DispFtpDir - logon and return formatted file directory from FTP server
FtpDownload - logon and download files from FTP server
FtpUpload - logon and upload files to FTP server
Cancel - abort FTP xfers



14 May 2001  - baseline
5 June 2001  - added ReplRO to replace read only files on download
8 June 2001  - added Fr to TIcsFileRec elements so names are unique
25 June 2001 - added LogLevelDelimFile and LogLevelDelimTot to return info
                 for each file suitable for further processing
29 June 2001 - fixed problem with fMask not getting all files to check
9 July 2001  - added HostName1 and HostName2 as alternates
1 Aug 2001   - use AbortXfer instead of Abort, cleaner
6 Aug 2001   - added archive directory move after upload
8 Aug 2001   - added zipping of uploads and downloads using VCLZip component from http://vclzip.bizland.com/
2 Sept 2001  - more error handling in delete after ftp upload, fixed case comparison
12 Dec 2001  - added TIcsTaskResult to distinguish results better
22 Dec 2001  - finally made zipping a conditional compile for those that have not bought it
8 Jan 2002   - fixed UNIX dates being last year if greater than today
             - don't change case of directories if mixed specified
21 Apr 2002  - free VCLZIP arhivestream (needed for 2.23)
11 July 2002 - added ResFailed and MinResSize, allow FTP resume of partial downloads
4 Sept 2002  - added IgnoreFileExt - list is tmp;ftp;xxx;etc
08 Oct 2002  - no longer using trailing slashes on directories internally, still seems to work
09 Dec 2002  - slight change in UNIX directory detection
               uploads, added UpImmed to optionally delete/move after each file done
               time comparisons, added UseUTC to optionally with UTC/GMT time
07 Jan 2003  - better error handling when connection lost
               use FEAT command to see what FTP server will do
               try and use MDTM and SIZE command to get and set upload time stamp
03 Feb 2003  - show FTP host being contacted at info log level
24 Apr 2003  - no longer supporting UTC time, upload MDTM now standard time
               download deleting empty directories
               building FTP dirs now uses real temp file to avoid conflicts between multiple DMs
2 May 2003   - changed GetDirList with new params
17 Jun 2003  - added duration (ms) to LogLevelDelimFile events
24 Aug 2003  - ensure unzip directory exists, might help trap unzipping errors
               unzip errors in task log
4 Oct 2003   - when deleting old local files, don't delete FTP/TMP resume files
26 Oct 2003  - support MLSD directory list command - but ws_ftp server returns local date, not UTC
               fixed double slash in FTP download PC name
               added SpecificFiles property with | delimited list passed in SrcFName, up and down
               added unzip error handler - VCLZIP 3 only
               check each file mod date and size before download, more accurate than DIR
               set upload date in UTC
               use streams for DIR listing to avoid temporary files
15 Dec 2003  - check both sockets are closed, otherwise abort, reset fCancelFlag at start
30 Dec 2003  - allow FTP DIR response to be logged using fDispRemList
               fixed a problem with a blank subdir stopping directory listing (introduced with DIR streams)
4 Jan 2003   - using magsubs1 for common stuff instead of magsubs4
             - using TIcsFileCopy for events
11 Jan 2004  - made Delphi 5 compatible, SpecificFiles not supported (no TStringList.Delimiter)
12 Feb 2004  - clear progress event, correct upload none response message
28 Aug 2004  - added FtpLogoff, FtpDownFiles (same as FtpDownload but already logged-on)
               added FtpDownOneFile, FtpUpOneFile and FtpCheckFile
               FtpDir no longer changes directories when listing (except LIST dirname fails if it contains a space on IIS/5)
               added workaround for error sending PORT command when PO lost, causing xfer failure
               fUpImmed forced, by checking each upload file after it's done to avoid directory afterwards
               if remote directory listing fails for uploads, give up to stop all files being uploaded, unless replacing all files
               FCReplNewer also replaces if target file empty and source file not empty
               support MFMT modify file modification time command if available
               support MD5 get has sum for download or upload file check corruption
               repeat failed download up to three times if FailRepeat<>0
               upload to temporary file and rename once checked successfully
               uploads now supporting resume on failure
               fixed divide by zero error calculating total upload duration (fixed download already)
               override WaitUntilReady in ftpcli.pas to avoid slow down problems
12 Oct 2004  - added file sizes when upload fails due to wrong size
               always log single line LIST/MLSD used for checking single files
               messing with updating UTC time stamps
29 Nov 2004  - suppressed state/request logging, more cancel breakouts and logging
13 Jan 2005  - removed sleep from WaitUntilReady
14 Feb 2005  - count failed resumed uploads/downloads, limit retries to five
10 Apr 2005  - fixed FTP MS-DOS LIST date 12:30AM being converted to 12:30 not 00:30
             - trim src and tar directories
18 Apr 2005  - using new IcsGetTickCount functions that support wrapping at 49 days
30 July 2005 - TIcsFindList moved to magclasses.pas
11 Aug 2005  - more error handling to trap lost connection during upload/download and prevent false OK
20 Aug 2005  - corrected false fail with no connection for all uploads
6 Sept 2005  - cleaned up unused variables, wait up to 5 secs for sockets to close after quit
             - 64-bit support, FtpCheckFile has 64-bit size (not backward compatible)
             - prevent too rapid progress messages, which slow down transfers, ProgressSecs = 5
             - FtpCli v2.100 fixes problem uploading files about 10K in size
19 Oct 2005  - fixed bug with short or zero upload or download duration showing as 120 hours
               don't retry for 501 permission error
3 Nov 2005   - latest ICS needs OnProgress64 not OnProgre64
5 Dec 2005   - now supporting ICS v5 SSL dated Nov 2005 or later
             - support Mode Z compression
26 Jan 2006  - support for new icslogger (not here), new SSL version
15 Feb 2006  - report error descriptions as well as numbers
16 Mar 2006  - check for single file use MDTM if MLST fails
             - ignore MLSD cdir and pdir lines with names
24 June 2006 - if xfer cancelled, set abort in progress to stop data being written
               resume download assume end of part file corrupt and start fMinResSize earlier
               increased fMinResSize default from 10K to 65K
               if xfer cancelled, still report download result
6 Aug 2006   - added KeepAliveSecs property in TFtpCli, if not zero sets Winsock keepalive
                  for control connection to stop it being closed by firewalls, typically 30 secs
               supporting XCRC command if MD5 command not available, to check files xfer'd OK
3 Sept 2006  - fixed some logging that broke on files large than 2 gigs
               fixed FtpCheckFile now allowing for MLST mixed case facts (Serv-U)
18 Sep 2006  - unit is now MagentaFtp supporting ICS V6
31 Oct 2006  - use callback during MD5/CRC calculations for progress and processmessages
6 Nov 2006   - new SSL session cache
6 Jan 2007   - allow for base directory with drive (ie c:) when setting root directory
               stopped using LIST path (CD instead) since some servers don't support it
19 July 2007 - ensure resume position reset before directory commands
               report DIR error if FtpCheckFile fails
               more logging for resumed downloads
6 Aug 2007   - MLSD listings don't ignore single character files and directories
03 Mar 2008  - MSLD fix for Serv-U where it only listed directories and no files
               new NoFeatCmd property which stops FEAT command being sent where
                 servers have not implemented features 'correctly' causing FTP to fail
               using ALLO command to check space on server before uploads
               send CLNT client string on logon
               use SITE DMLSD command to list directories including subdirectore
                  and SITE CMLSD for single dirs (ICS FTP server only at present)
               LookupFTPReq and LookupFTPState moved to TFtpCli
               LogLevelDelimFile has actual xfer size after duration, including failures
               using XMD5 if available, log time taken by MD5/CRC commands
               adding File/Delim logging for failed MD5/CRC on download and
                 report MD5/CRC error for upload failed (not FTP response)
               don't use ModeZ for directory listing, except recursive
               added ZlibNoCompExt property which is list of file extensions which
                 should no use Mode Z, defaults to '.zip;.rar;.7z;.cab;.lzh;.gz;.avi;.wmv;.mpg;.mp3;.jpg;.png;'
               added ZlibMaxSize property maximum file size for Mode Z (servers may struggle with large files)
               don't attempt to resume xfer if partial file larger than new file
               don't repeatedly abort as errors reported, which may cause problem with Zlib errors
               added MaxResumeAttempts property, default 10 (was fixed 5)
               added MaxXferAttempts property, default 3
               if download tmp file already open, use tmp2 instead
               check more than fMinResSize downloaded for file size mismatch in case file is smaller on resume
               when downloading, get server file size/date before each repeat attempt in case changed
               if passive connection fails, Abort xfer so server cleans up
               retry on 501 errors
               added MagFtpOps property to disable advanced features:
                      magftpNoFeat, magftpNoZlib, magftpNoMd5Crc, magftpNoTmpFile
               when no new files to upload, still delete old target files
               warn if ignoring delete old target files
11 Jun 2008  - ensure lists set as sorted after sorting
7 Aug 2008   - 2.3 - updated for latest ICS V6 and V7, and for Delphi 2009 compatibility
               Note: FTP does not yet support Unicode commands
22 Sept 2008 - 2.4 - added magftpNoUtf8 property to turn off UTF8
               don't attempt to access files or directories with Unicode substitution ? character
               don't keep upload resume files unless some data actually sent
               don't attempt to upload _xxx.ftp resume files
               uploading always allowed to create base directory, and it now works properly
               WS_FTP MLST with 501 Invalid number of arguments with spaces, so try quoting file name
               SSL send PBSZ before PROT to keep MS FTP7 happy
18 Nov 2008  - 3.2 - renamed to MagentaFtp3
               support UTF8 FTP commands and file listings with ICS V7.02, ICS V6 is not supported
               added magftpNoUtf8 property to turn off UTF8, and magftpIgnoreUtf8 if server will not turn it off
               support full Unicode file listing and xfers with Delphi 2009 and later only
               don't attempt to access files or directories with Unicode substitution ? character
               check file names only have ANSI characters unless UTF8 is enabled
               send HOST command before logon unless magftpNoHost specified (for virtual FTP servers)
               fixed bug if PWD returned blank rather than '/' (Indy)
               ensure Progress event called xfer starts rather than after 2 secs
               support XDMLSD and XCLMSD commands
17 May 2009  - 3.4 - don't ignore failed MD5/XMD5 command but report error, prefer MD5 to XMD5
               added MaskLocDir and MaskRemDir flags to take masked directory from SrcFName and
                 add to local and/or remote directory, typically for dated directories
               Unicode MD5sum and CRC32B, add magftpNoMd5 and magftpNoCrc to allow them to be tested separately
7 Aug 2010   - 3.5 - fix a problem parsing UNIX file listing with strange upper case file attributes
               fixed various string casts for D2009 and later
11 Aug 201   - 3.7 - ICS changes, new throttling, zlib 1.2.5
               support MultiThreaded using TIcsWndControl.MessagePump instead of Application.ProcessMessages
               some checksum Info logging should only have been File logging
               added TIcsFtpMultiThread component which runs TIcsFtpMulti in a thread, tested with 250 threads running together
               added NoProgress property to skip LogLevelProg progress log events
               FTP empty directories if EmptyDirs property set
               now registered in MagentaXferReg
               fixed bug listing sub-directories from root with MLSD command
20 Oct 2011  - 3.8 - log time and speed of each download
               most file sizes now reported in Kbytes. Mbytes, Gbytes instead of bytes
24 Aug 2012 - 4.0 - updated to support ICS V8 with IPv6
24 Jul 2013 - 4.1 - default to allowing IPv4 or IPv6 host names
              added IgnorePaths, ignore files where source has specific partial path, list is
               c:\temp;c:\temp2\;c:\temp3\;etc, or destination for deletion marches partial path
              added Wow64RedirDisable property for Win64 allow all files to be copied correctly from Win32
              added new ProgressEvent which passes TIcsCopyProgress record updated for progress of
                 current file and session including total bytes copied allowing percentage progress display
              when checking copy, use MemoryStream for improved listing performance
              using TIcsStringBuild to build listings
              Fixed bug in WaitUntilReady that meant some sync methods with multiple
               commands randomly terminated prematurely allowing further commands to
               be sent usually resulting in not ready errors, reproduced with resumed uploads.
13 Jul 2015 - 4.2 - better SSL handshake reporting
              added SSL server certificate checking
23 Oct 2015 - 4.3 - better SSL certificate reporting
              failed certificate report error as last FTP error
              Warning, self signed certificates are not trusted
23 Feb 2016 - 4.4 - fixed a bug that always treated upload file names as lower case
7 Dec 2016  - 4.5 - more friendly errors
              removed TX509Ex now using TX509Base
              using OpenSSL certificate verification host checking
              set SSL session caching correctly
              only check and report SSL certificates once per session
6 Mar 2017  - 4.6 - simplified SSL certificate reporting
              set SSL security level low, ideally should be configurable
18 Jun 2018 - 4.7 - Use built-in CA bundle if file missing.
              Added SslCliSecurity property to set security level to TSslCliSecurity
              Changed GetUAgeSizeFile to IcsGetUAgeSizeFile
18 Mar 2019 - V8.60 - Adapted for main ICS packages and FMX support. SSL only.
              Renamed TMagFtp to  TIcsFtpMulti.
              Most Types have Ics added, so: TIcsTaskResult now TIcsTaskResult.
              No longer needs Forms.
7 Aug 2019  - V8.62 - Support NO_DEBUG_LOG properly, Builds USE_SSL.
3 Nov 2019  - V8.63 - Added SslCliSecurity, FtpType and IgnorePaths to TIcsFtpMultiThread.
              Threaded FTP now sends all log events to screen.
04 Sep 2020 - V8.65 - Pending, lots of Windows only APIs need Posix conversion
                      BOOL to Boolean.
9 Dec 2020  - V8.65 - Pending, lots of Windows only APIs need Posix conversion
              BOOL to Boolean.
              If SSL handshake fails due to bad certificate or chain, remove
                 SSL session from cache so an immediate retry does not succeed by
                 skipping the certificate checks.
              Renamed Ics.Posix.Messages.pas to Ics.Posix.PXMessages.pas.
May 24, 2021 - V8.67 - Replaced Stream.Seek with Stream.Position.
Jan 04, 2022 - V8.68 - Remove SSL session cache port so data session can use control session.
Apr 14, 2022 - V8.69 - Previously the FtpSslRevocation property was only effective when
                 checking the windows certificate store, now it also works with bundle
                 files using the TOcspHttp component and OCSP stapling if available.
Oct 20, 2022 - V8.70 - Removed VclZip support, replaced by TZipFile from System.Zip unit for D10
                 and later, if you still need zipping for earlier compilers keep an old version
                 of this unit.
               Don't always exit immediately if control channel closes, so something gets logged,
                 perhaps after a massive upload or download.
Feb 14, 2023 - V8.71 - Ensure inherited destroy called.
               Fixed an exception freeing stringslists after copying completed, Win64 only?
               Using Int64 ticks.
               Don't compress iso, pdf, vhd, vhdx files.
               Disable MD5/CRC32 by default, connections are now reliable and they are slow.
               Improved error handling for downloads that complete without receiving a
                 226 Success response and timeout, so file is treated as complete and
                 saved (but no MD5sum, etc), would seem the control channel has died
                 but KeepAlive does not help, only seen with transfers of 30 minutes
                 or more, many gigs in size.
               Added NoopIntervalMins property that causes NOOP command to be sent every
                 x minutes during long transfers (over 10MB) as a keep-alive to stop the
                 control channel being closed due to inactivity. Beware some older FTP
                 servers may treat NOOP as illegal and fail the transfer (including ICS
                 FTP server V8.70 and earlier).  Not convinced KeepAliveSecs works on modern
                 versions of Windows. Defaults to 10 minutes, set to zero to disable NOOP.
               FTP file progress now shows correct size, percent and speed when resuming
                 partial transfers, but totals are for the complete file size.
Aug 08, 2023 V9.0  Updated version to major release 9.



Unicode Compatibility with various web servers
Note: UTF8 support may not include Unicode characters outside ANSI codeset
OPTS UTF8 or OPTS UTF8 ON command must be sent before most servers support
UTF8 file listings or uploads

ICS V6 - does not support UTF8

ICS V7 - support UTF8, fully Unicode capable when build with Delphi 2009 or later,
defaults to UTF8 OFF and returns ANSI file listings, OPTS UTF8 or OPTS UTF8 ON
enables UTF8 file listings and uploads

Microsoft IIS/5 and IIS/6 no UTF8 support

Microsoft FTP7 IIS/7 for Windows 2008 - fully Unicode capable, defaults to UTF8 OFF and
returns ANSI file listings, OPTS UTF8 or OPTS UTF8 ON enables UTF8 file listings and uploads

FileZilla Server - fully Unicode capable, defaults to UTF8 ON returning UFT8 file listings
and uploads, but can be disabled with OPTS UTF8 OFF command when listings revert to ANSI

WS_FTP Server 6.1.1 and 7.0.0 - fully Unicode capable, defaults to UTF8 OFF and returns
ANSI file listings, OPTS UTF8 or OPTS UTF8 ON enables UTF8 file listings and uploads,
UTF8 can not be set off so don't send command if ANSI needed
MLST fails with 501 if file name includes a space

RhinoSoft Serv-U FTP Server v7.2 - no Unicode support, defaults to UTF8 OFF but returns
UTF8 file listings (with ? for non-supported unicode characters), OPTS UTF8 ON enables
UTF8 uploads.  OPTS UTF8 OFF command reverts listings to ANSI
MLST fails with 550 for file names with any UTF8 escaped characters
MDTM YYYYMMDDHHMMSS fail if timezone used (worked in earlier releases)
v7.3 fixes the MLST and MDTM errors, and adds MFMT, v8 will support Unicode

Gene6 FTP Server v3.10.0 - fully Unicode capable, defaults to UTF8 OFF and returns ANSI
file listings, OPTS UTF8 or OPTS UTF8 ON enables UTF8 file listings and uploads, UTF8
can not be set off so don't send command if ANSI needed.
MLSD fails with a Unicode sub-directory argument (but CWD works OK with Unicode)

Indy 10 FTP Server component built with Delphi 2007 - no UTF8 support (probably,
but may be configured using some hidden option)
MLST fails with 250 end if a file name is passed


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit Z.ICS9.OverbyteIcsFtpMulti;
{$ENDIF}

{$I Include\Z.ICS9.OverbyteIcsDefs.inc}

{$IFDEF COMPILER23_UP}      { TZipFile may work with XE2 and better, but not tested }
    {$DEFINE ZIPPING}       { V8.70 use System.Zip }
{$ENDIF}


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

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    Z.ICS9.OverbyteIcsWinCrypt,
{$ENDIF}
{$IFDEF POSIX}
    Posix.Time,
    Z.ICS9.Ics.Posix.WinTypes,
    Z.ICS9.Ics.Posix.PXMessages,
{$ENDIF}
    {$Ifdef Rtl_Namespaces}System.Classes{$Else}Classes{$Endif},
    {$Ifdef Rtl_Namespaces}System.Sysutils{$Else}Sysutils{$Endif},
    {$Ifdef Rtl_Namespaces}System.Masks{$Else}Masks{$Endif},
{$IFDEF FMX}
    Z.ICS9.Ics.Fmx.OverbyteIcsWndControl,
    Z.ICS9.Ics.Fmx.OverbyteIcsWSocket,
    Z.ICS9.Ics.Fmx.OverbyteIcsFtpCli,
    Z.ICS9.Ics.Fmx.OverbyteIcsBlacklist,
    Z.ICS9.Ics.Fmx.OverbyteIcsFileCopy,
    Z.ICS9.Ics.Fmx.OverbyteIcsSslSessionCache,
    Z.ICS9.Ics.Fmx.OverbyteIcsSslX509Utils,
    Z.ICS9.Ics.Fmx.OverbyteIcsMsSslUtils,
    Z.ICS9.Ics.Fmx.OverbyteIcsSslHttpRest,   { V8.69 }
{$ELSE}
    Z.ICS9.OverbyteIcsWndControl,
    Z.ICS9.OverbyteIcsWSocket,
    Z.ICS9.OverbyteIcsFtpCli,
    Z.ICS9.OverbyteIcsBlacklist,
    Z.ICS9.OverbyteIcsFileCopy,
    Z.ICS9.OverbyteIcsSslSessionCache,
    Z.ICS9.OverbyteIcsSslX509Utils,
    Z.ICS9.OverbyteIcsMsSslUtils,
    Z.ICS9.OverbyteIcsSslHttpRest,          { V8.69 }
{$ENDIF FMX}
  Z.ICS9.OverByteIcsFtpSrvT,
  Z.ICS9.OverbyteIcsMD5,
  Z.ICS9.OverbyteIcsCRC,
  Z.ICS9.OverbyteIcsTypes,
  Z.ICS9.OverbyteIcsLogger,
  Z.ICS9.OverbyteIcsUtils
{$IFDEF Zipping}
    ,System.Zip            { V8.70 VclZip gone }
{$ENDIF}
  , Z.ICS9.OverbyteIcsSSLEAY, Z.ICS9.OverbyteIcsLIBEAY,
  Z.ICS9.OverbyteIcsTicks64;              { V8.71 }

{ NOTE - these components only build with SSL, there is no non-SSL option }
{$IFDEF MSWINDOWS}

{$IFDEF USE_SSL}


const
    FtpMultiCopyRight : String = ' TIcsFtpMulti (c) 2023 V9.0 ';

type
// host type, for directory listing
    THostType = (FTPTYPE_NONE, FTPTYPE_UNIX, FTPTYPE_DOS, FTPTYPE_MVS,
                 FTPTYPE_AS400, FTPTYPE_MLSD) ;
    TXferMode = (XferModeBinary, XferModeAscii) ;
    TBulkMode = (BulkModeNone, BulkModeDownload, BulkModeUpload) ;
    TCaseFile = (FileLowerCase, FileMixedCase) ;
    TFtpType  = (FtpTypeNone,
                 FtpTypeAuthSslCtl, FtpTypeAuthSslData, FtpTypeAuthSslBoth,
                 FtpTypeConnSslCtl, FtpTypeConnSslData, FtpTypeConnSslBoth);
    { AuthSsl    = explicit encryption on port 21 using AUTH command }
    { ConnSsl    = implicit encryption on port 990 forced on connection }
    TIcsFtpMultiOpt  = (magftpNoFeat, magftpNoZlib, magftpNoMd5Crc, magftpNoTmpFile,
                   magftpNoUtf8, magftpIgnoreUtf8, magftpNoHost,
                   magftpNoMd5, magftpNoCrc);   // 15 Apr 2009
    TIcsFtpMultiOpts = set of TIcsFtpMultiOpt;
    TFtpThreadOpt = (ftpthdList, ftpthdDownCheck, ftpthdDownFiles,
                    ftpthdUpCheck, ftpthdUpFiles) ;  // 14 Feb 2011
    TFtpSslVerifyMethod = (ftpSslVerNone, ftpSslVerBundle, ftpSslVerWinStore) ;   // 20 Apr 2015

const
    FtpTypeStrings: array[Low(TFtpType)..High(TFtpType)] of String =
    ('Not Secured',
     'Secured SSL/TLS Auth - Control Only',
     'Secured SSL/TLS Auth - Data Only',
     'Secured SSL/TLS Auth - Control and Data',
     'Secured SSL/TLS Conn - Control Only',
     'Secured SSL/TLS Conn - Data Only',
     'Secured SSL/TLS Conn - Control and Data') ;

    ResInfServer = 0 ; ResInfFName = 1 ; ResInfStamp = 2 ; ResInfSize = 3 ;
    ResInfAttempts = 4 ; ResInfLastBytes = 5 ;

type
  TIcsFtpMulti = class(TSslFtpClient)
  private
    { Private declarations }
        fCancelFlag: boolean ;
        fProgFileSize: Int64 ;
        fLoggedIn: boolean ;
        fFtpErrFlag: boolean ;
        fIcsFileCopy: TIcsFileCopy ;
        fFtpType: TFtpType ;   // 11 Nov 2005, settable even if no SSL
        fFtpSslVerMethod: TFtpSslVerifyMethod;  // 20 Apr 2015
        fFtpSslRootFile: string ;  // 20 Apr 2015
        fFtpSslPort: string ;
        fFtpSslRevocation: boolean;   // 20 Apr 2015
        fFtpSslReportChain: boolean ;  // 20 Apr 2015
        fFtpSslCliSecurity: TSslCliSecurity;   // June 2018
        fSslSessCache: boolean ;
        fSslContext: TSslContext ;
        fExternalSslSessionCache: TSslAvlSessionCache ;
        fMsCertChainEngine: TMsCertChainEngine;   // 20 Apr 2015
        FOcspHttp: TOcspHttp;                 { V8.69 }
        FNoopIntervalMins: Integer;           { V8.71 }
        FNoopTimer: TIcsTimer;                { V8.71 }
  protected
    { Protected declarations }
        fBulkMode: TBulkMode ;
        fHostName1: string ;
        fHostName2: string ;
        fSrcDir: String ;
        fSrcFName: String ;
        fTarDir: String ;
        fCopyType: TIcsFileCopyType ;
        fSubDirs: Boolean ;
        fDelDone: Boolean ;
        fDelOldTar: Boolean ;
        fMask: Boolean ;
        fPrev: Boolean ;
        fRepl: TIcsFileCopyRepl ;
        fReplRO: boolean ;
        fSafe: Boolean ;
        fTimeStamp: boolean ;
        fLocalHost: string ;
        fDispLog: boolean ;
        fDispFiles: boolean ;
        fDispLDir: boolean ;
        fDispRDir: boolean ;
        fHostType: THostType ;
        fXferMode: TXferMode ;
        fCaseFile: TCaseFile ;
        fDiffStampMins: integer ;
        fCopyEvent: TBulkCopyEvent ;
        fReqResponse: string ;
        fServRootDir: string ;
        fServBaseDir: string ;
        fMaxAttempts: integer ;     // logon attempts
        fAttemptDelay: integer ;
        fUpArchDir: String ;
        fUpArchive: Boolean ;
        fResFailed: Boolean ;
        fMinResSize: Int64 ;        // also used for Resume Overlap 24 June 2006
        fIgnoreFileExt: string ;
        fUpImmed: Boolean ;                        // 17 Aug 2004 - now ignored
        fSpecificFiles: boolean ;    // 14 Oct 03
        fDispRemList: boolean ;      // 30 Dec 2003
        fCurRemDir: string ;         // 10 Aug 2004
        fFailRepeat: integer ;       // 20 Aug 2004
        fProgressSecs: integer ;     // 5 Sept 2005
        fUseCompression: boolean ;   // 2 Dec 2005
        fUsingCompression: boolean ; // 5 Dec 2005
        fTimeZoneStr: string ;       // 8 Nov 2007
        fZlibNoCompExt: string ;     // 2 Dec 2007
        fZlibMaxSize: int64 ;        // 9 Dec 2007 - zero means no compression
        fMaxResumeAttempts: integer ; // 31 Dec 2007  resume attempts
        fMagFtpOpts: TIcsFtpMultiOpts;    // 5 Jan 2008
        fMaskLocDir: boolean ;       // 8 Apr 2009
        fMaskRemDir: boolean ;       // 8 Apr 2009
        fNoProgress: boolean ;       // 15 Feb 2011
        fEmptyDirs: boolean ;        // 17 Feb 2011
        fIgnorePaths: String ;       // 22 May 2013
        fCopyProg: TIcsCopyProgress ;   // 22 May 2013 replaces most fTotxx/fProcxx variables
        fWow64RedirDisable: boolean ;// 22 May 2013
        fProgressEvent: TProgressEvent ;  // 22 May 2013
    {$IFDEF Zipping}
        fZipDownDel: Boolean ;
        fZipped: Boolean ;
        fZipExtFmt: TIcsZipExtFmt ;
        fZipPath: TIcsZipPath ;
        fZipDir: String ;
        procedure ZipProgressEvent(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);   { V8.70 }
     {$ENDIF}

        procedure doCopyEvent (const LogLevel: TIcsCopyLogLevel; const Info: string) ;
        procedure onFtpClientProg64(Sender: TObject; Count: Int64;
                                                     var Abort: Boolean);
        procedure onFtpClientDisplay(Sender: TObject; var Msg: String);
        procedure onFtpError(Sender: TObject; var Msg: String);
        procedure OnFtpResponse (Sender: TObject) ;
        procedure OnFtpSessConn (Sender: TObject; Error: word) ;
        procedure OnFtpSessClosed (Sender: TObject; Error: word) ;
        procedure OnFtpRequestDone (Sender: TObject; RqType: TFtpRequest; Error: Word) ;
        procedure OnFtpStateChange (Sender: TObject) ;
        procedure OnFTPSocksConnected (Sender: TObject; Error: word) ;
        procedure onMagCopyEvent (LogLevel: TIcsCopyLogLevel ; Info: string ;
                                                  var Cancel: boolean) ;
        function IntDownOne (const RemDir, RemFile, RemFull, LocFileFull: string;
                               const RFSize: Int64; RFileUDT: TDateTime): integer ;

        function IntUpOne (const LocFileFull, RemDir, RemFile: string;
                                 const RFSize: Int64; RFileUDT: TDateTime): integer ;
        function WaitUntilReady : Boolean; Override ;
        procedure SetSrcDir (S: string) ;
        procedure SetTarDir (S: string) ;
        procedure onZlibProg (Sender: TObject; Count: Int64; var Cancel: Boolean); // 9 Dec 2007
        procedure sysDelayX (aMs: longword);
        procedure OnFTPSslVerifyPeer(Sender: TObject; var Ok: Integer;
          Cert : TX509Base);
        procedure OnFTPSslCliNewSession(Sender: TObject; SslSession: Pointer;
                                        WasReused: Boolean; var IncRefCount : Boolean);
        procedure OnFTPSslCliGetSession(Sender: TObject; var SslSession: Pointer;
                                                                var FreeSession : Boolean);
        procedure OnFTPSslHandshakeDone(Sender: TObject; ErrCode: Word;
                                          PeerCert: TX509Base; var Disconnect: Boolean);
        procedure OnFTPSslCliCertRequest(Sender: TObject; var Cert: TX509Base);
        procedure IcsProgEvent (Sender: TObject; LogOption: TLogOption; const Msg : String) ;   { V8.69 }
        procedure NoopTimerTimer(Sender: TObject);                                              { V8.71 }

  public
    { Public declarations }

        SrcFiles: TIcsFDirRecs  ;
        SrcFileList: TIcsFindList ;
        TotSrcFiles: integer ;
        TarFiles: TIcsFDirRecs  ;
        TarFileList: TIcsFindList ;
        TotTarFiles: integer ;
        LogRcvdCerts: boolean ; // 20 Apr 2015

        constructor Create (Aowner: TComponent) ; override ;
        destructor  Destroy ; override ;
        function DispFtpDir (var dirlisting: string): TIcsTaskResult ;
        function FtpDir (var FtpFiles: TIcsFDirRecs ;
                            var FtpFileList: TIcsFindList; const ListDirs: boolean = false): TIcsTaskResult ;  // 17 Feb 2011
        function FtpLogon: TIcsTaskResult ;
        function FtpDownload (const CheckFiles: boolean): TIcsTaskResult ;
        function FtpUpload (const CheckFiles: boolean): TIcsTaskResult ;
        procedure Cancel ;
        function UnpackFtpFDir (DirStream: TStream; RemDir, BaseDir: string;
                  Level: integer ; var DCodePage: Cardinal ; var HostType: THostType;
                            var TotFiles: integer; var RemFiles: TIcsFDirRecs ): integer ;
        procedure FtpLogoff ;
        function FtpDownFiles (const CheckFiles: boolean): TIcsTaskResult ;
        function FtpDownOneFile (const FdirSrc, Fnamesrc, Fnametar: string ;
                                            Replopt: TIcsFileCopyRepl) : TIcsTaskResult ;
        function FtpUpOneFile (const LocFileFull, RemTarDir, RemTarFile: string;
                                            Replopt: TIcsFileCopyRepl) : TIcsTaskResult ;
        function FtpCheckFile (const RemDir, RemFile: string ;
                                 var FSize: Int64; var FileUDT: TDateTime): boolean;

{ Pass it the string that you get back when you get the
{ Current path for the FTP site. It returns
{   FTPType of   FTP_TYPE_NONE or FTP_TYPE_MVS depending
{ if it looks like the folder string format is that of an VMS system
{ or not. After using this function, you will need to use the FTP_ConvertLines
{ to further determine if the server type is DOS/WINDOWS or UNIX or AS-400}
        Procedure SetupVMS(const BaseFolder: string; var HostType: THostType);

  published
    { Published declarations }

    property BulkMode: TBulkMode       read fBulkMode       write fBulkMode ;
    property HostName1: string         read fHostName1      write fHostName1 ;
    property HostName2: string         read fHostName2      write fHostName2 ;
    property SrcDir: String            read fSrcDir         write SetSrcDir ;
    property SrcFName: String          read fSrcFName       write fSrcFName ;
    property TarDir: String            read fTarDir         write SetTarDir ;
    property CopyType: TIcsFileCopyType   read fCopyType       write fCopyType ;
    property SubDirs: Boolean          read fSubDirs        write fSubDirs ;
    property DelDone: Boolean          read fDelDone        write fDelDone ;
    property DelOldTar: Boolean        read fDelOldTar      write fDelOldTar ;
    property Mask: Boolean             read fMask           write fMask ;
    property Prev: Boolean             read fPrev           write fPrev ;
    property Repl: TIcsFileCopyRepl       read fRepl           write fRepl ;
    property ReplRO: boolean           read fReplRO         write fReplRO ;
    property Safe: Boolean             read fSafe           write fSafe ;
    property TimeStamp: boolean        read fTimeStamp      write fTimeStamp ;
    property LocalHost: string         read fLocalHost      write fLocalHost ;
    property DispLog: boolean          read fDispLog        write fDispLog ;
    property DispFiles: boolean        read fDispFiles      write fDispFiles ;
    property DispLDir: boolean         read fDispLDir       write fDispLDir ;
    property DispRDir: boolean         read fDispRDir       write fDispRDir ;
    property HostType: THostType       read fHostType       write fHostType ;
    property XferMode: TXferMode       read fXferMode       write fXferMode ;
    property CaseFile: TCaseFile       read fCaseFile       write fCaseFile ;
    property DiffStampMins: integer    read fDiffStampMins  write fDiffStampMins ;
    property FailRepeat: integer       read fFailRepeat     write fFailRepeat ;     // 20 Aug 2004
    property ProgressSecs: integer     read fProgressSecs   write fProgressSecs ;   // 5 Sept 2005
    property CopyEvent: TBulkCopyEvent read fCopyEvent      write fCopyEvent ;
    property FtpType: TFtpType         read fFtpType        write fFtpType ;      // 11 Nov 2005
    property UseCompression: boolean   read fUseCompression write fUseCompression ; // 3 Dec 2005
    property SslSessCache: boolean     read fSslSessCache   write fSslSessCache ; // 11 Nov 2005
    property FtpSslPort: string        read fFtpSslPort     write fFtpSslPort ;   // 11 Nov 2005
    property FtpSslVerMethod: TFtpSslVerifyMethod read fFtpSslVerMethod write fFtpSslVerMethod ;  // 20 Apr 2015
    property FtpSslRootFile: string    read fFtpSslRootFile write fFtpSslRootFile ;         // 20 Apr 2015
    property FtpSslRevocation: boolean read fFtpSslRevocation write fFtpSslRevocation ;     // 20 Apr 2015
    property FtpSslReportChain: boolean read fFtpSslReportChain write fFtpSslReportChain;   // 20 Apr 2015
    property FtpSslCliSecurity: TSslCliSecurity read fFtpSslCliSecurity  write fFtpSslCliSecurity;   // June 2018
    property OcspHttp: TOcspHttp       read FOcspHttp         write FOcspHttp;               { V8.69 }
    property NoopIntervalMins: Integer read FNoopIntervalMins write FNoopIntervalMins;       { V8.71 }
    property TotProcFiles: integer     read fCopyProg.TotProcFiles ;
    property ProcOKFiles: integer      read fCopyProg.ProcOKFiles ;
    property DelOKFiles: integer       read fCopyProg.DelOKFiles ;
    property ProcFailFiles: integer    read fCopyProg.ProcFailFiles ;
    property ReqResponse: string       read fReqResponse ;
    property SkippedFiles: integer     read fCopyProg.SkippedFiles ;
    property ServRootDir: string       read fServRootDir ;
    property ServBaseDir: string       read fServBaseDir ;
    property LoggedIn: boolean         read fLoggedIn ;
    property MaxAttempts: integer      read fMaxAttempts    write fMaxAttempts ;   // logon attempts
    property AttemptDelay: integer     read fAttemptDelay   write fAttemptDelay ;
    property PassiveX: Boolean         read FPassive        write FPassive;
    property UpArchDir: String         read fUpArchDir      write fUpArchDir ;
    property UpArchive: Boolean        read fUpArchive      write fUpArchive ;
    property ResFailed: Boolean        read fResFailed      write fResFailed ;
    property MinResSize: Int64         read fMinResSize     write fMinResSize ;
    property IgnoreFileExt: string     read fIgnoreFileExt  write fIgnoreFileExt ;
    property UpImmed: Boolean          read fUpImmed        write fUpImmed ;
    property SpecificFiles: Boolean    read fSpecificFiles  write fSpecificFiles ;
    property DispRemList: boolean      read fDispRemList    write fDispRemList ;
    property ZlibNoCompExt: string     read fZlibNoCompExt  write fZlibNoCompExt ; // 2 Dec 2007
    property ZlibMaxSize: Int64        read fZlibMaxSize    write fZlibMaxSize ;   // 9 Dec 2007
    property MaxResumeAttempts: integer  read fMaxResumeAttempts write fMaxResumeAttempts ; // 31 Dec 2007
    property MagFtpOpts: TIcsFtpMultiOpts   read fMagFtpOpts     write fMagFtpOpts ;    // 5 Jan 2008
    property MaskLocDir: boolean       read fMaskLocDir     write fMaskLocDir ;    // 8 Apr 2009
    property MaskRemDir: boolean       read fMaskRemDir     write fMaskRemDir ;    // 8 Apr 2009
    property NoProgress: boolean       read fNoProgress     write fNoProgress ;    // 15 Feb 2011
    property EmptyDirs: Boolean        read fEmptyDirs      write fEmptyDirs ;     // 17 Feb 2011
    property IgnorePaths: String       read fIgnorePaths    write fIgnorePaths ;   // 22 May 2013
    property CopyProg: TIcsCopyProgress   read fCopyProg ;                            // 22 May 2013
    property Wow64RedirDisable: boolean read fWow64RedirDisable write fWow64RedirDisable ; // 22 May 2013
    property ProgressEvent: TProgressEvent read fProgressEvent write fProgressEvent; // 22 May 2013
  {$IFDEF Zipping}
    property ZipDownDel: Boolean       read fZipDownDel     write fZipDownDel ;
    property Zipped: Boolean           read fZipped         write fZipped ;
    property ZipExtFmt: TIcsZipExtFmt     read fZipExtFmt      write fZipExtFmt ;
    property ZipPath: TIcsZipPath         read fZipPath        write fZipPath ;
    property ZipDir: String            read fZipDir         write fZipDir ;
  {$ENDIF}

  end;

// 23 Sept 2010 threaded version of TIcsFtpMulti

  TThreadEvent = Procedure (LogLevel: TIcsCopyLogLevel ; const Id, Info: string ;
                                              var Cancel: boolean) of object ;

  TIcsFtpMultiThread = class(TThread)
  private
        FFtpThreadOpt: TFtpThreadOpt ;
        FTaskRes: TIcsTaskResult ;
        FDirListing: String ;
        FLogLevel: TIcsCopyLogLevel ;
        FInfo: string ;
        FId: string ;
        FAbort: boolean ;
        FLogmaskName: string ;
        FBuffLogStream: TIcsBuffLogStream ;
{$IFNDEF NO_DEBUG_LOG}
        FIcsLog: TIcsLogger;
{$ENDIF}
//  protected
        // from TCustomWSocket
        FLocalAddr          : String;     { IP address for local interface to use }
        // from TIcsWndControl
        FMultiThreaded      : Boolean;
        // from TCustomFtpCli
        FTimeout            : Integer;                 { Given in seconds }
        FHostName           : String;
        FPort               : String;
        FSocketFamily       : TSocketFamily;     // March 2013
        FSocketErrs         : TSocketErrs;       // Nov 2016
        FCodePage           : LongWord;
//      FSystemCodepage     : LongWord;
        FDataPortRangeStart : DWORD;
        FDataPortRangeEnd   : DWORD;
//      FLastDataPort       : DWORD;
        FDSocketSndBufSize  : Integer;{AG V7.26}
        FDSocketRcvBufSize  : Integer;{AG V7.26}
//      FLocalAddr          : String;
        FUserName           : String;
        FPassWord           : String;
        FAccount            : String;
//      FLocalFileName      : String;
//      FHostFileName       : String;
//      FHostDirName        : String;
//      FDnsResult          : String;
//      FType               : Char;
//      FShareMode          : Word;
        FConnectionType     : TFTPConnectionType;
        FProxyServer        : String;
        FProxyPort          : String;
        FOptions            : TFtpOptions;
        FPassive            : Boolean;
        FNewOpts            : string;        { V2.102 arguments for OPTS command }
        FTransferMode       : TFtpTransMode; { V2.102 new tranfer mode }
//    FSupportedExtensions : TFtpExtensions; { V2.94  which features server supports }
        FKeepAliveSecs      : integer;       { V2.107 zero means window default }
        FClientIdStr        : String;        { V2.113 string sent for CLNT command }
        FSocksPassword      : String;        { V7.00 }
        FSocksPort          : String;        { V7.00 }
        FSocksServer        : String;        { V7.00 }
        FSocksUserCode      : String;        { V7.00 }
        FLanguage           : String;        { V7.01 language argment for LANG command }
        FLangSupport        : String;        { V7.01 list of languages server supports }
//      FZlibWorkDir        : String;        { V2.113 zlib work directory }
{$IF DEFINED(UseBandwidthControl) or DEFINED(BUILTIN_THROTTLE)}
        FBandwidthLimit     : Integer;  // Bytes per second
        FBandwidthSampling  : Integer;  // mS sampling interval
{$IFEND}
        // from TIcsFtpMulti
//        fLoggedIn: boolean ;
        fFtpType: TFtpType ;
        fFtpSslVerMethod: TFtpSslVerifyMethod; // 20 Apr 2015
        fFtpSslPort: String;
        fFtpSslRevocation: boolean;       // 20 Apr 2015
        fFtpSslReportChain: boolean ;     // 20 Apr 2015
        fFtpSslRootFile: string ;  // 20 Apr 2015
        fFtpSslCliSecurity: TSslCliSecurity;   // V8.63
        fSslSessCache: boolean ;
        fBulkMode: TBulkMode ;
        fHostName1: String ;
        fHostName2: String ;
        fSrcDir: String ;
        fSrcFName: String ;
        fTarDir: String ;
        fCopyType: TIcsFileCopyType ;
        fSubDirs: Boolean ;
        fDelDone: Boolean ;
        fDelOldTar: Boolean ;
        fMask: Boolean ;
        fPrev: Boolean ;
        fRepl: TIcsFileCopyRepl ;
        fReplRO: boolean ;
        fSafe: Boolean ;
        fTimeStamp: boolean ;
        fLocalHost: String ;
        fDispLog: boolean ;
        fDispFiles: boolean ;
        fDispLDir: boolean ;
        fDispRDir: boolean ;
        fHostType: THostType ;
        fXferMode: TXferMode ;
        fCaseFile: TCaseFile ;
        fDiffStampMins: integer ;
        fTotProcFiles: integer ;
        fProcOKFiles: integer ;
        fDelOKFiles: integer ;
        fProcFailFiles: integer ;
        fSkippedFiles: integer ;
        fReqResponse: String ;
        fMaxAttempts: integer ;
        fAttemptDelay: integer ;
        fUpArchDir: String ;
        fUpArchive: Boolean ;
        fResFailed: Boolean ;
        fMinResSize: Int64 ;
        fIgnoreFileExt: string ;
        fSpecificFiles: boolean ;
        fDispRemList: boolean ;
        fFailRepeat: integer ;
        fProgressSecs: integer ;
        fUseCompression: boolean ;
        fZlibNoCompExt: String ;
        fZlibMaxSize: int64 ;
        fMaxResumeAttempts: integer ;
        fMagFtpOpts: TIcsFtpMultiOpts;
        fMaskLocDir: boolean ;
        fMaskRemDir: boolean ;
        fNoProgress: boolean ;
        fEmptyDirs: boolean ;
        fIgnorePaths: String ; // V8.63
        FNoopIntervalMins: Integer;           { V8.71 }
  public
    IcsFTPMultiCli: TIcsFtpMulti ;
    FThreadEvent: TThreadEvent ;
    constructor CreateThread;
    procedure LogEvent (LogLevel: TIcsCopyLogLevel ; Info: String ; var Cancel: boolean) ;
    procedure IcsLogEvent (Sender: TObject; LogOption: TLogOption; const Msg : String) ;
    procedure CallThreadEvent ;
    procedure Execute; override;
    property FtpThreadOpt         : TFtpThreadOpt        read FFtpThreadOpt
                                                         write FFtpThreadOpt;
    property TaskRes              : TIcsTaskResult          read FTaskRes;
    property DirListing           : string               read FDirListing;
    property LogmaskName          : string               read FLogmaskName
                                                         write FLogmaskName;
    property ID                   : string               read FID
                                                         write FID ;
    property Timeout              : Integer              read FTimeout
                                                         write FTimeout;
    property MultiThreaded        : Boolean              read FMultiThreaded
                                                         write FMultiThreaded;
    property CodePage             : LongWord             read  FCodePage
                                                         write FCodePage;
    property KeepAliveSecs        : Integer              read  FKeepAliveSecs
                                                         write FKeepAliveSecs;
    property Options              : TFtpOptions          read  FOptions
                                                         write FOptions;
    property ClientIdStr          : String               read  FClientIdStr
                                                         write FClientIdStr;
    property BandwidthLimit       : Integer              read  FBandwidthLimit
                                                         write FBandwidthLimit;
    property BandwidthSampling    : Integer              read  FBandwidthSampling
                                                         write FBandwidthSampling;
    property TransferMode         : TFtpTransMode        read  FTransferMode
                                                         write FTransferMode;
    property NewOpts              : String               read  FNewOpts
                                                         write FNewOpts;
    property HostName             : String               read  FHostName
                                                         write FHostName;
    property Port                 : String               read  FPort
                                                         write FPort;
    property DataPortRangeStart   : DWORD                read  FDataPortRangeStart
                                                         write FDataPortRangeStart;
    property DataPortRangeEnd     : DWORD                read  FDataPortRangeEnd
                                                         write FDataPortRangeEnd;
    property DataSocketSndBufSize : Integer              read  FDSocketSndBufSize   {AG V7.26}
                                                         write FDSocketSndBufSize;
    property DataSocketRcvBufSize : Integer              read  FDSocketRcvBufSize   {AG V7.26}
                                                         write FDSocketRcvBufSize;
    property LocalAddr            : String               read  FLocalAddr
                                                         write FLocalAddr;
    property UserName             : String               read  FUserName
                                                         write FUserName;
    property PassWord             : String               read  FPassWord
                                                         write FPassWord;
    property ConnectionType       : TFtpConnectionType   read  FConnectionType
                                                         write FConnectionType;
    property ProxyServer          : String               read  FProxyServer
                                                         write FProxyServer;
    property ProxyPort            : String               read  FProxyPort
                                                         write FProxyPort;
    property SocksPassword        : String               read  FSocksPassword
                                                         write FSocksPassword;
    property SocksPort            : String               read  FSocksPort
                                                         write FSocksPort;
    property SocksServer          : String               read  FSocksServer
                                                         write FSocksServer;
    property SocksUserCode        : String               read  FSocksUserCode
                                                         write FSocksUserCode;
    property Account              : String               read  FAccount
                                                         write FAccount;
    property Language             : String               read  FLanguage
                                                         write FLanguage;
    property LangSupport          : String               read  FLangSupport;
    property BulkMode: TBulkMode       read fBulkMode       write fBulkMode ;
    property HostName1: string         read fHostName1      write fHostName1 ;
    property HostName2: string         read fHostName2      write fHostName2 ;
    property SrcDir: String            read fSrcDir         write fSrcDir ;
    property SrcFName: String          read fSrcFName       write fSrcFName ;
    property TarDir: String            read fTarDir         write fTarDir ;
    property CopyType: TIcsFileCopyType   read fCopyType       write fCopyType ;
    property SubDirs: Boolean          read fSubDirs        write fSubDirs ;
    property DelDone: Boolean          read fDelDone        write fDelDone ;
    property DelOldTar: Boolean        read fDelOldTar      write fDelOldTar ;
    property Mask: Boolean             read fMask           write fMask ;
    property Prev: Boolean             read fPrev           write fPrev ;
    property Repl: TIcsFileCopyRepl       read fRepl           write fRepl ;
    property ReplRO: boolean           read fReplRO         write fReplRO ;
    property Safe: Boolean             read fSafe           write fSafe ;
    property TimeStamp: boolean        read fTimeStamp      write fTimeStamp ;
    property LocalHost: string         read fLocalHost      write fLocalHost ;
    property DispLog: boolean          read fDispLog        write fDispLog ;
    property DispFiles: boolean        read fDispFiles      write fDispFiles ;
    property DispLDir: boolean         read fDispLDir       write fDispLDir ;
    property DispRDir: boolean         read fDispRDir       write fDispRDir ;
    property HostType: THostType       read fHostType       write fHostType ;
    property XferMode: TXferMode       read fXferMode       write fXferMode ;
    property CaseFile: TCaseFile       read fCaseFile       write fCaseFile ;
    property DiffStampMins: integer    read fDiffStampMins  write fDiffStampMins ;
    property FailRepeat: integer       read fFailRepeat     write fFailRepeat ;
    property ProgressSecs: integer     read fProgressSecs   write fProgressSecs ;
    property FtpType: TFtpType         read fFtpType        write fFtpType ;
    property UseCompression: boolean   read fUseCompression write fUseCompression ;
    property SocketFamily: TSocketFamily read FSocketFamily write FSocketFamily ;
    property SocketErrs: TSocketErrs   read FSocketErrs     write FSocketErrs;      { Nov 2016}
    property SslSessCache: boolean     read fSslSessCache   write fSslSessCache ;
    property FtpSslPort: string        read fFtpSslPort     write fFtpSslPort ;
    property FtpSslVerMethod: TFtpSslVerifyMethod read fFtpSslVerMethod write fFtpSslVerMethod ;  // 20 Apr 2015
    property FtpSslRootFile: string    read fFtpSslRootFile write fFtpSslRootFile ;         // 20 Apr 2015
    property FtpSslRevocation: boolean read fFtpSslRevocation write fFtpSslRevocation ;     // 20 Apr 2015
    property FtpSslReportChain: boolean read fFtpSslReportChain write fFtpSslReportChain;   // 20 Apr 2015
    property FtpSslCliSecurity: TSslCliSecurity read fFtpSslCliSecurity  write fFtpSslCliSecurity;   // V8.63
    property NoopIntervalMins: Integer read FNoopIntervalMins write FNoopIntervalMins;       { V8.71 }
    property TotProcFiles: integer     read fTotProcFiles ;
    property ProcOKFiles: integer      read fProcOKFiles ;
    property DelOKFiles: integer       read fDelOKFiles ;
    property ProcFailFiles: integer    read fProcFailFiles ;
    property ReqResponse: String       read fReqResponse ;
    property SkippedFiles: integer     read fSkippedFiles ;

    property MaxAttempts: integer      read fMaxAttempts    write fMaxAttempts ;
    property AttemptDelay: integer     read fAttemptDelay   write fAttemptDelay ;
    property PassiveX: Boolean         read FPassive        write FPassive;
    property UpArchDir: String         read fUpArchDir      write fUpArchDir ;
    property UpArchive: Boolean        read fUpArchive      write fUpArchive ;
    property ResFailed: Boolean        read fResFailed      write fResFailed ;
    property MinResSize: Int64         read fMinResSize     write fMinResSize ;
    property IgnoreFileExt: string     read fIgnoreFileExt  write fIgnoreFileExt ;
    property SpecificFiles: Boolean    read fSpecificFiles  write fSpecificFiles ;
    property DispRemList: boolean      read fDispRemList    write fDispRemList ;
    property ZlibNoCompExt: String     read fZlibNoCompExt  write fZlibNoCompExt ;
    property ZlibMaxSize: Int64        read fZlibMaxSize    write fZlibMaxSize ;
    property MaxResumeAttempts: integer  read fMaxResumeAttempts write fMaxResumeAttempts ;
    property MagFtpOpts: TIcsFtpMultiOpts   read fMagFtpOpts     write fMagFtpOpts ;
    property MaskLocDir: boolean       read fMaskLocDir     write fMaskLocDir ;
    property MaskRemDir: boolean       read fMaskRemDir     write fMaskRemDir ;
    property NoProgress: boolean       read fNoProgress     write fNoProgress ;
    property EmptyDirs: Boolean        read fEmptyDirs      write fEmptyDirs ;
    property IgnorePaths: String       read fIgnorePaths    write fIgnorePaths ;    // V8.63
  end ;

const
    AppTicksPerFtp = 50 ;   // 22 May 2013 millisecs to open file when calculation session duration

{$ENDIF USE_SSL}
{$ENDIF MSWINDOWS}

implementation

{$IFDEF MSWINDOWS}
{$IFDEF USE_SSL}

function ConvUSADate (info: string): TDateTime ;
// mm/dd/yyyy
var
    yy, mm, dd: word ;
begin
    result := 0 ;
    info := trim (info) ;
    if length (info) <> 10 then exit ;
    yy := StrToIntDef (copy (info, 7, 4), 0);
    mm := StrToIntDef (copy (info, 1, 2), 0);
    dd := StrToIntDef (copy (info, 4, 2), 0);
    if NOT TryEncodeDate (yy, mm, dd, result) then result := 0 ;
end ;

{function GetWinsockErr (error: integer): string ;
begin
    result := WSocketErrorDesc (error) + ' (' + IntToStr (error) + ')' ;
end ;

function atoi64(value : String) : Int64;
var
    i : Integer;
begin
    Result := 0;
    i := 1;
    while (i <= Length(Value)) and (Value[i] = ' ') do
        i := i + 1;
    while (i <= Length(Value)) and (Value[i] >= '0') and (Value[i] <= '9')do begin
        Result := Result * 10 + ord(Value[i]) - ord('0');
        i := i + 1;
    end;
end;

function CalcSpeed (DurTicks, FSize: int64): int64 ;  // 12 Oct 2011
begin
    if DurTicks <= 0 then DurTicks := 10 ;  // stop division by zero
     result := (1000 * FSize) div DurTicks;
end ;}

constructor TIcsFtpMulti.Create(Aowner:TComponent);
begin
    inherited create(AOwner);
 // winsock bug fix for fast connections
    FControlSocket.ComponentOptions := [wsoNoReceiveLoop] ;
    FDataSocket.ComponentOptions := [wsoNoReceiveLoop] ;
    fIcsFileCopy := TIcsFileCopy.Create (self) ;
    fIcsFileCopy.CopyEvent := onMagCopyEvent ;
    fIcsFileCopy.MultiThreaded := FMultiThreaded ; // 16 Sept 2010
    SrcFileList := TIcsFindList.Create ;
    TarFileList := TIcsFindList.Create ;
    SetLength (SrcFiles, 0) ;
    SetLength (TarFiles, 0) ;
    TotSrcFiles := 0 ;
    TotTarFiles := 0 ;
    fCancelFlag := false ;
    fLoggedIn := false ;
    fMaxAttempts := 3 ;
    fAttemptDelay := 5 ;   // seconds
    fProgressSecs := 2 ;   // update progress every two seconds default
    fUseCompression := false ;
    fResFailed := false ;
    fMinResSize := 65535 ;   // also used for resume overlap
    fFailRepeat := 3 ;       // 31 Dec 2007 was 0
    fSpecificFiles := false ;
    fDispRemList := false ;
    fCurRemDir := 'xXx' ;
    onDisplay := onFTPClientDisplay ;
    OnProgress64 := onFTPClientProg64 ;
    onError := onFtpError ;  // this event stops FtpClient raising exceptions
    OnResponse := onFTPResponse ;
    OnSessionConnected := onFTPSessConn ;
    OnSessionClosed := onFTPSessClosed ;
    OnRequestDone := onFTPRequestDone ;
    onStateChange := onFTPStateChange ;
    FControlSocket.OnSocksConnected := OnFTPSocksConnected ;
    fFtpType := FtpTypeNone ;  // 11 Nov 2005, even for no SSL
    fFtpSslVerMethod := ftpSslVerNone ;  // 20 Apr 2015
    fFtpSslRootFile := 'RootCaCertsBundle.pem' ; // 20 Apr 2015
    fFtpSslPort := '990' ;
    fZlibNoCompExt := '.zip;.rar;.7z;.cab;.lzh;.gz;.avi;.wmv;.mpg;.mp3;.jpg;.png;.iso;.pdf;.;vhd;.vhdx;'; // 2 Dec 2007 V8.71 more large files
    fZlibMaxSize := 500000000 ;  // 9 Dec 2007 - 500 meg
    OnZlibProgress := onZlibProg ; // 9 Dec 2007 not working yet...
    fMaxResumeAttempts := 10 ;     // 31 Dec 2007
    fMagFtpOpts := [magftpNoMd5Crc] ;  // V8.71 disable MD5/CRC32 by default, connections are now reliable and they are slow
    FSocketFamily := sfAny ;  // March 2013 allow IPv4 or IPv6
    IcsCopyProgClearAll (fCopyProg) ;  // 22 May 2013
    fSslSessCache := false ; // true ;
    fExternalSslSessionCache := nil ;
    OnSslVerifyPeer := onFTPSslVerifyPeer ;
    OnSslCliGetSession := onFTPSslCliGetSession ;
    OnSslCliNewSession := onFTPSslCliNewSession ;
    OnSslHandshakeDone := onFTPSslHandshakeDone ;
    OnSslCliCertRequest := onFTPSslCliCertRequest ;
    fSslContext := TSslContext.Create (self) ;
    SslContext := fSslContext ;
    fSslContext.SslVerifyPeer := false ;
    fFtpSslCliSecurity := sslCliSecIgnore;  // June 2018
    FOcspHttp := TOcspHttp.Create(Self);   { V8.69 }
    FOcspHttp.OnOcspProg := IcsProgEvent;   { V8.69 }
    FOcspHttp.CacheFName := 'ocsftpcache.recs';   { V8.69 }
    fDiffStampMins := 2;                          { V8.70 }
    FNoopIntervalMins := 10;                      { V8.71 }
    FNoopTimer := TIcsTimer.Create(Self);         { V8.71 }
    FNoopTimer.Enabled := False;                  { V8.71 }
    FNoopTimer.OnTimer := NoopTimerTimer;         { V8.71 }
end ;

destructor TIcsFtpMulti.Destroy;
begin
{ V8.71 Ensure inherited destroy called }
    try
        FNoopTimer.Enabled := False;   { V8.71 }
        FreeAndNil (FNoopTimer);       { V8.71 }
        FreeAndNil (FOcspHttp);        { V8.69 }
        FreeAndNil (fIcsFileCopy);
        FreeAndNil (SrcFileList);
        FreeAndNil (TarFileList);
        FreeAndNil (FMsCertChainEngine) ;
        FreeAndNil (fExternalSslSessionCache) ;
        FreeAndNil (fSslContext) ;
    finally
        inherited Destroy;
    end;
end;

procedure TIcsFtpMulti.OnFTPSslVerifyPeer(Sender: TObject; var Ok: Integer; Cert : TX509Base);
var
    info: string ;
begin
    OK := 1; // don't check certificate until handshaking over
    if LogRcvdCerts then   // 20 Apr 2015
    begin
         info := 'Received Certificate, Depth ' + IntToStr (Cert.VerifyDepth) + #13#10 +
                 'Verify Result: ' + Cert.VerifyErrMsg + #13#10 +
                 Cert.CertInfo (true) + #13#10 ;  // Mar 2017 simplify
        doCopyEvent (LogLevelDiag, info);
    end;
end ;

procedure TIcsFtpMulti.OnFTPSslCliNewSession(Sender: TObject; SslSession: Pointer;
                                            WasReused: Boolean; var IncRefCount : Boolean);
var
    FtpCli: TSslFtpClient ;
begin
    { SslCliNewSession/SslCliGetSession allow external, client-side session }
    { caching.                                                              }
    doCopyEvent (LogLevelDiag, 'Starting SSL Session');
    if not fSslSessCache then Exit;
    if (not WasReused) then
    begin
        FtpCli := Sender as TSslFtpClient ;
        fExternalSslSessionCache.CacheCliSession (SslSession,
                        FtpCli.ControlSocket.PeerAddr{+ FtpCli.ControlSocket.PeerPort}, IncRefCount);
                                                  { V8.68 no port so data session can use control session }
        doCopyEvent (LogLevelDiag, 'Cache SSL Session: New');
    end
    else
        doCopyEvent (LogLevelDiag, 'Cache SSL Session: Reuse');
end ;

procedure TIcsFtpMulti.OnFTPSslCliGetSession(Sender: TObject; var SslSession: Pointer;
                                                        var FreeSession : Boolean);
var
    FtpCli: TSslFtpClient ;
begin
    { SslCliNewSession/SslCliGetSession allow external, client-side session }
    { caching.                                                              }
    if not fSslSessCache then Exit;
    doCopyEvent (LogLevelDiag, 'Check for Old SSL Session');
    FtpCli := Sender as TSslFtpClient ;
    SslSession := fExternalSslSessionCache.GetCliSession(
                     FtpCli.ControlSocket.PeerAddr {+ FtpCli.ControlSocket.PeerPort}, FreeSession);
                                               { V8.68 no port so data session can use control session }
    if Assigned (SslSession) then   // Dec 2016
        doCopyEvent (LogLevelDiag, 'Old SSL Session Found Cached')
    else
        doCopyEvent (LogLevelDiag, 'No Old SSL Session Cached');
end ;

procedure TIcsFtpMulti.OnFTPSslHandshakeDone(Sender: TObject; ErrCode: Word;
                                  PeerCert: TX509Base; var Disconnect: Boolean);
var
    CertChain: TX509List;
    ChainVerifyResult: LongWord;
    info, VerifyInfo: String;
    Safe: Boolean;
    FtpCtl: TWSocket ;      // Dec 2016
begin
    FtpCtl := (Sender as TSslFtpClient).ControlSocket ;  // Dec 2016

  // nothing much to do if SSL failed or event said disconnect
    if (ErrCode <> 0) or Disconnect then
    begin
        doCopyEvent (LogLevelInfo, FtpCtl.SslServerName + ' SSL Handshake Failed - ' + FtpCtl.SslHandshakeRespMsg) ;  // Dec 2014
        Disconnect := TRUE;
        exit;
    end  ;

    doCopyEvent (LogLevelInfo, FtpCtl.SslServerName + ' ' + FtpCtl.SslHandshakeRespMsg) ;     // Dec 2014
    if (SslAcceptableHosts.IndexOf (FtpCtl.SslServerName + PeerCert.Sha1Hex) >= 0) or  // Dec 2016 done it already
          FtpCtl.SslSessionReused OR (fFtpSslVerMethod = ftpSslVerNone) then
    begin
        exit; // nothing to do, go ahead
    end ;

 // Property SslCertChain contains all certificates in current verify chain
    CertChain := FtpCtl.SslCertChain;

 // see if validating against Windows certificate store
    if fFtpSslVerMethod = ftpSslVerWinStore then
    begin
        // start engine
        if not Assigned (FMsCertChainEngine) then
            FMsCertChainEngine := TMsCertChainEngine.Create;

      // see if checking revoocation, CRL checks and OCSP checks in Vista+, very slow!!!!
        if fFtpSslRevocation then
            FMsCertChainEngine.VerifyOptions := [mvoRevocationCheckChainExcludeRoot]
        else
            FMsCertChainEngine.VerifyOptions := [];

        // This option doesn't seem to work, at least when a DNS lookup fails
        FMsCertChainEngine.UrlRetrievalTimeoutMsec := 10000;

        { Pass the certificate and the chain certificates to the engine      }
        FMsCertChainEngine.VerifyCert (PeerCert, CertChain, ChainVerifyResult, True);

        Safe := (ChainVerifyResult = 0) or
                { We ignore the case if a revocation status is unknown.      }
                (ChainVerifyResult = CERT_TRUST_REVOCATION_STATUS_UNKNOWN) or
                (ChainVerifyResult = CERT_TRUST_IS_OFFLINE_REVOCATION) or
                (ChainVerifyResult = CERT_TRUST_REVOCATION_STATUS_UNKNOWN or
                                     CERT_TRUST_IS_OFFLINE_REVOCATION);

       { The MsChainVerifyErrorToStr function works on chain error codes     }
        VerifyInfo := MsChainVerifyErrorToStr (ChainVerifyResult); // Nov 2016

    // MSChain ignores host name, so see if it failed using OpenSSL
        if PeerCert.VerifyResult = X509_V_ERR_HOSTNAME_MISMATCH then
        begin  // Nov 2016
            Safe := False;
            VerifyInfo := PeerCert.FirstVerifyErrMsg;
        end;
    end
    else if fFtpSslVerMethod = ftpSslVerBundle then
    begin
        VerifyInfo := PeerCert.FirstVerifyErrMsg;   // Nov 2016
        Safe := (PeerCert.VerifyResult = X509_V_OK);   { check whether SSL chain verify result was OK }

      { V8.69 check OCSP to see if revoked, if we got a chain of certificates }
      { note this is a soft check, if we don't have a stapled OCSP response from the TLS handshake, we get it from an
        OCSP HTTP server and cache it but don't wait for the response. So next attempt comes from cache.  }
        if (Safe and FFtpSslRevocation and PeerCert.IsCertLoaded and (CertChain.Count > 0)) then begin
            FOcspHttp.ClearOcsp;
            FOcspHttp.DebugLevel := DebugConn;
            FOcspHttp.OcspCert := PeerCert;
            FOcspHttp.OcspInters := CertChain;
            if (Length(FtpCtl.OcspStapleRaw) > 50) and
                 (FtpCtl.OcspStapleStatus = OCSP_RESPONSE_STATUS_SUCCESSFUL) then
                                        FOcspHttp.OcspRespRaw := FtpCtl.OcspStapleRaw;
            if FOcspHttp.CheckOcspRevoked(fSslContext.GetX509Store, 0) then
                Safe := False;
            VerifyInfo := FOcspHttp.OcspLastResp;
            FOcspHttp.OcspInters := Nil;
            doCopyEvent (LogLevelInfo, FtpCtl.SslServerName + ' ' + VerifyInfo)
        end;
    end
    else
    begin
        exit ;  // unknown method
    end ;

  // tell user verification failed
    if NOT Safe then
    begin
        FErrorMessage := 'SSL Chain Verification Failed: ' + VerifyInfo + ', Domain: ';
        if PeerCert.SubAltNameDNS = '' then
            FErrorMessage := FErrorMessage + IcsUnwrapNames (PeerCert.SubjectCName)
        else
            FErrorMessage := FErrorMessage + IcsUnwrapNames (PeerCert.SubAltNameDNS) ;    // Nov 2016
        FErrorMessage := FErrorMessage + ', Expected: ' + FtpCtl.SslServerName ;  // Nov 2016
        doCopyEvent (LogLevelInfo, FErrorMessage);
    end
    else
    begin
        doCopyEvent (LogLevelInfo, FtpCtl.SslServerName + ' SSL Chain Verification Succeeded') ;
        SslAcceptableHosts.Add (FtpCtl.SslServerName + PeerCert.Sha1Hex) ;  // Dec 2016 save it
    end ;

// if certificate checking failed, see if the host is specifically listed as being allowed anyway
    if (NOT Safe) and (SslAcceptableHosts.IndexOf (FtpCtl.SslServerName) > -1) then  // 19 Oct 2015
    begin
        Safe := true ;
        doCopyEvent (LogLevelInfo, FtpCtl.SslServerName + ' SSL Succeeded with Acceptable Host Name') ;
    end ;

  // tell user about all the certificates we found
    if fFtpSslReportChain and (CertChain.Count > 0) then
    begin
        info := FtpCtl.SslServerName + ' ' + IntToStr (CertChain.Count) +
             ' SSL Certificates in the verify chain:' + #13#10 +
                CertChain.AllCertInfo (true, true) + #13#10 ; // Mar 2017 report all certs, backwards
        doCopyEvent (LogLevelInfo, info);
    end;

  // all failed, V8.65 need to remove cached SSL session so it's not reused!!!
    if NOT Safe then
    begin
        Disconnect := TRUE;
        if fSslSessCache then begin
            if fExternalSslSessionCache.RemoveSession(FtpCtl.PeerAddr {+ FtpCtl.PeerPort}) then
                                              { V8.68 no port so data session can use control session }
                doCopyEvent (LogLevelDiag, 'SSL Session Uncached After Failure')
            else
                doCopyEvent (LogLevelDiag, 'SSL Session Not Found in Cache');
        end;
    end;
end ;


procedure TIcsFtpMulti.IcsProgEvent (Sender: TObject; LogOption: TLogOption; const Msg : String) ;   { V8.69 }
begin
    doCopyEvent (LogLevelInfo, Msg) ;
end ;


procedure TIcsFtpMulti.OnFTPSslCliCertRequest(Sender: TObject; var Cert: TX509Base);
begin
    doCopyEvent (LogLevelDiag, 'Certificate Request Ignored') ;
end;


procedure TIcsFtpMulti.SetSrcDir (S: string) ;
begin
    fSrcDir := Trim (S) ;
end ;


procedure TIcsFtpMulti.SetTarDir (S: string) ;
begin
    fTarDir := Trim (S) ;
end ;


function TIcsFtpMulti.WaitUntilReady : Boolean;
var
    DummyHandle     : THandle;
begin
{$IFNDEF WIN64}
  {$IFNDEF COMPILER24_UP}
    Result    := TRUE;           { Make dcc32 happy }
  {$ENDIF}
{$ENDIF}
    FTimeStop := IcsGetTrgSecs64(FTimeout);    { V8.71 Int64 }
    while TRUE do begin
        // 24 July 2013 InternalReady happens between multiple commands, ignore it
        if FState in [ftpReady {, ftpInternalReady}] then begin
            { Back to ready state, the command is finished }
            Result := (FRequestResult = 0);
            break;
        end;

        if FMultiThreaded then  // 21 Sept 2010 stop threads using all CPU
        begin
            if ftpWaitUsingSleep in FOptions then
                Sleep(0)
            else begin
                DummyHandle := INVALID_HANDLE_VALUE;
                MsgWaitForMultipleObjects(0, {PChar(0)^}DummyHandle, FALSE, 1000, QS_ALLINPUT {or QS_ALLPOSTMESSAGE});
            end;
        end;

        if FTerminated or ((FTimeout > 0) and IcsTestTrgTick64(FTimeStop)) then begin    { V8.71 Int64 }
            { Timeout occured }
{$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loProtSpecInfo) then
                DebugLog(loProtSpecInfo, '! Aborting connection on sync request timeout, secs ' + IntToStr(FTimeout));     { V8.71 }
{$ENDIF}
            AbortAsync;
            FErrorMessage := '426 Timeout, after ' + IntToStr(IcsElapsedSecs64(FTimeStop)) + ' secs';  { V8.71 }
            FStatusCode   := 426;
            Result        := FALSE; { Command failed }
            break;
        end;
        MessagePump;
    end;
end;

procedure TIcsFtpMulti.sysDelayX (aMs: longword);
var
    Trg: Int64;      { V8.71 }
begin
    Trg := IcsGetTrgMsecs64 (aMs) ;
    while True do
    begin
        MessagePump ; // 15 Sept 2010 needed to support MultiThreaded
        if FTerminated then break ;
        if IcsTestTrgTick64 (Trg) then break ;
    end ;
end;

(*
APPLE UNIX TYPE
drwx--x--x   2 64        512 Feb  5 06:30 mpo
-rw-r--r--   1 root        0 Nov 11  1999 mswin.qif
drwxr-xr-x   2 57        512 Nov  5  1999 neural
d-wx-wx--x   3 root     1536 Feb  4 23:30 outgoing
drwxr-xr-x   3 208       512 Nov  5  1999 papers
drwxr-xr-x   2 134       512 Nov  5  1999 patch
lrwxr-xr-x   1 root       12 Jan 26 19:00 pii -> winsock2/pii
drwxr-xr-x   2 78        512 Nov  5  1999 pld_fpga
drwxr-xr-x   2 770      1024 Jan 18 00:32 reqadm
drwxr-xr-x   3 70        512 Nov  5  1999 rmx


UNIX TYPE

drwx--x--x   2 64       root          512 Feb  5 06:30 mpo
-rw-r--r--   1 root     root            0 Nov 11  1999 mswin.qif
drwxr-xr-x   2 57       root          512 Nov  5  1999 neural
d-wx-wx--x   3 root     50           1536 Feb  4 23:30 outgoing
drwxr-xr-x   3 208      root          512 Nov  5  1999 papers
drwxr-xr-x   2 134      root          512 Nov  5  1999 patch
lrwxr-xr-x   1 root     root           12 Jan 26 19:00 pii -> winsock2/pii
drwxr-xr-x   2 78       root          512 Nov  5  1999 pld_fpga
drwxr-xr-x   2 770      root         1024 Jan 18 00:32 reqadm
drwxr-xr-x   3 70       root          512 Nov  5  1999 rmx
drwxr-xr-x   2 root     root          512 Nov  5  1999 rz1000
drwxr-xr-x   5 676      root          512 Nov  5  1999 sal
drwxr-xr-x   2 root     root          512 Nov  5  1999 sbios
lrwxr-xr-x   7 109      root          512 Nov  5  1999 support -> ../win95/support
lrwxr-x--x 116 730      50           2048 Feb  5 02:21 swdev -> support
drwxr-xr-x   2 67       root          512 Nov  5  1999 tis
-rw-r--r--   1 root     root            0 Nov 11  1999 unix.qif
drwxr-xr-x   2 91       root          512 Nov  5  1999 winsock2

-rw-r--r--   1 root     root            0 May 21  1996 .hide
lrwxrwxrwx   1 root     root           13 Apr 30  1998 ld-linux.so -> ld-linux.so.1
lrwxrwxrwx   1 root     root           18 Apr 30  1998 ld-linux.so.1 -> ld-linux.so.1.8.10
-rwxr-xr-x   1 root     root        82412 Mar 16  1997 ld-linux.so.1.8.10
-rwxr-xr-x   1 root     root        17412 May 21  1996 ld.so
lrwxrwxrwx   1 root     root           14 Mar 21  1998 libc.so.4 -> libc.so.4.6.27
-rwxr-xr-x   1 root     root       634880 May 21  1996 libc.so.4.6.27
lrwxrwxrwx   1 root     root           14 Apr 30  1998 libc.so.5 -> libc.so.5.4.23
-rwxr-xr-x   1 root     root       602076 Apr 30  1998 libc.so.5.4.23
lrwxrwxrwx   1 root     root           14 Mar 21  1998 libm.so.4 -> libm.so.4.6.27
-rwxr-xr-x   1 root     root       110592 May 21  1996 libm.so.4.6.27

list at 7 Jan
drwxrwxr-x   2 cixip    cixip        1024 Jun 13  2001 .
drwxrwxr-x   6 cixip    cixip        1536 Dec 23 13:03 ..
-rw-rw-r--   1 cixip    cixip         268 Dec 23 13:09 .htaccess
-rw-r--r--   1 cixip    cixip     1260608 Jan  2 15:57 codelook.zip
-rw-r--r--   1 cixip    cixip       32759 Dec 15 17:33 dialacc.htm
-rw-r--r--   1 cixip    cixip      402646 Dec 15 17:33 dialimob.htm
-rw-r--r--   1 cixip    cixip      322159 Dec 15 17:34 dialintl.htm
-rw-r--r--   1 cixip    cixip        9585 Dec 18 00:28 index.shtml
-rw-rw-r--   1 cixip    cixip        3640 Oct 11  1998 maglogo.gif
-rw-rw-r--   1 cixip    cixip        2763 Oct 11  1998 maglogo2.gif
-rw-rw-r--   1 cixip    cixip       67350 Dec 15 17:35 tarifcod.htm
-rw-rw-r--   1 cixip    cixip      867377 Dec 15 17:36 tariffr.zip
-rw-rw-r--   1 cixip    cixip      722470 Dec 15 17:37 tariffrz.zip
-rw-rw-r--   1 cixip    cixip      152289 Jul  6  2001 tarifmob1.htm
-rw-rw-r--   1 cixip    cixip       99824 Jul  6  2001 tarifmob2.htm
-rw-rw-r--   1 cixip    cixip        1126 Oct 11  1998 tcback.gif

magsys web merula
drw-rw-rw-   1 user     group           0 Dec  2 00:01 .
drw-rw-rw-   1 user     group           0 Dec  2 00:01 ..
-rw-rw-rw-   1 user     group      380560 Sep  2 00:00 ex020901.log
-rw-rw-rw-   1 user     group      825953 Sep  3 00:00 ex020902.log
-rw-rw-rw-   1 user     group      926278 Sep  4 00:00 ex020903.log
-rw-rw-rw-   1 user     group      906454 Sep  5 00:00 ex020904.log
-rw-rw-rw-   1 user     group      930487 Sep  6 00:00 ex020905.log


DOS/WINDOWS TYPE

02-04-00  12:16AM       <DIR>          TurboView
01-15-00  11:52PM       <DIR>          twain
06-09-98  06:07PM                 1197 default.htm
06-13-98  09:08PM                37376 initcaps.dll
06-13-98  12:08AM                 1107 initcaps.htm
06-13-98  12:38PM                  562 initcaps.inf
04-17-99  05:14PM                 2183 register.html
06-09-98  06:03PM              3720660 savage2.mp3
06-13-98  05:10PM                 3244 tcltest.htm

VMS - MULTINET TYPE

WORK:[POOR]
WORK:[POOR.WWW]
WORK:[POOR.WWW.TEST]

A1MAIL$.DIR;1              27  27-JUL-1997 08:46 [POOR] (RWE,RWE,,)
A1MAIL$USER_PROFILE.DAT;1
                            4  10-APR-1995 14:48 [POOR] (RWD,RW,,)
CECILIA.DIS;1               1  18-AUG-1994 16:48 [POOR] (RWED,RWED,,)
FTP_SERVER.LOG;16           2   6-FEB-2000 14:42 [POOR] (RWED,RWED,RE,)
GOPHERRC.;1                 0   5-OCT-1993 14:30 [POOR] (RWD,RWD,,)
LOGIN.COM;6                 1  12-OCT-1994 10:24 [POOR] (RWED,RWED,,)
MAIL$107FE97600050098.MAI;1
                           11   6-JUL-1994 17:50 [POOR] (RW,RW,,)
MAIL$1170B62600050098.MAI;1
                           23   7-JUL-1994 22:33 [POOR] (RW,RW,,)
MAIL$1283D3CF00050096.MAI;1
                            5  25-SEP-1992 17:53 [POOR] (RW,RW,,)
MAIL$26813FB800050098.MAI;1
                            7   3-AUG-1994 17:55 [POOR] (RW,RW,,)
MAIL$2A72F73700050098.MAI;1
                           13   8-AUG-1994 18:22 [POOR] (RW,RW,,)
MAIL$41A9489100050098.MAI;1
                           15   7-SEP-1994 07:19 [POOR] (RW,RW,,)
MAIL$437DF4FC00050098.MAI;1
                            4   9-SEP-1994 15:14 [POOR] (RW,RW,,)
MAIL$43B5E1B800050098.MAI;1
                            4   9-SEP-1994 21:54 [POOR] (RW,RW,,)
MAIL$4739E47E00050098.MAI;1
                            5  14-SEP-1994 09:17 [POOR] (RW,RW,,)
MAIL$4B27FF2500050098.MAI;1
                           15  19-SEP-1994 09:19 [POOR] (RW,RW,,)
MAIL$4BE3236800050098.MAI;1
                            8  20-SEP-1994 07:38 [POOR] (RW,RW,,)
MAIL$674BF17F00050097.MAI;1
                           22   3-DEC-1993 10:02 [POOR] (RW,RW,,)
MAIL$70F3003400050097.MAI;1
                            5  15-DEC-1993 16:50 [POOR] (RW,RW,,)
MAIL$74C790AA00050097.MAI;1
                           10  20-DEC-1993 13:49 [POOR] (RW,RW,,)
MAIL$BD5D07D400050097.MAI;1
                            8  22-MAR-1994 22:41 [POOR] (RW,RW,,)
MAIL$D08322A500050097.MAI;1
                            8  16-APR-1994 07:31 [POOR] (RW,RW,,)
MAIL$DFC3537400050097.MAI;1
                           75   5-MAY-1994 17:19 [POOR] (RW,RW,,)
MAIL$FBB7778400050097.MAI;1
                            5  10-JUN-1994 07:04 [POOR] (RW,RW,,)
MAIL$FE93373E00050097.MAI;1
                            4  13-JUN-1994 22:22 [POOR] (RW,RW,,)
MAIL.MAI;1                165   5-JUL-1994 08:15 [POOR] (RW,RW,,)
TEST.EXE;1                  4  27-OCT-1992 12:08 [POOR] (RWED,RWED,RE,)
TEST.PAS;2                  1   6-FEB-2000 14:33 [POOR] (RWED,RWED,RE,)
WWW.DIR;1                   1  27-JUL-1997 08:47 [POOR] (RWE,RWE,E,E)


Total of 453 blocks in 29 files.


AS-400 TYPE


QPGMR            8192 05/30/98 08:08:03 *FILE      QSRVSRC

QPGMR            8192 05/30/98 08:08:03 *FILE      QTBLSRC

QPGMR           12288 05/30/98 08:41:04 *FILE      QTXTSRC

QSECOFR         40960 10/08/99 10:25:23 *FILE      REPLACE

QSECOFR                                 *MEM       REPLACE.REPLACE
QUSER           69632 02/05/99 16:19:28 *DIR       HTML/
QUSER            1575 02/05/99 16:06:37 *STMF      index.htm.$$$  <---- EDITORS WAY OF .BAK
GUEST            1733 02/02/00 18:32:43 *STMF      index.htm
QUSER          151552 07/16/99 12:33:16 *DIR       DPISALES/
QUSER           14398 10/21/98 12:49:52 *STMF      readme.htm
QSECOFR         69632 10/03/98 15:22:35 *DIR       java_applets/
QSECOFR         53248 02/02/00 18:21:57 *DIR       PersonalWebPages/   <---- TURBOVIEW COOL PIC'S
QUSER           69632 12/04/98 15:17:54 *DIR       Art/
QUSER             949 10/21/98 15:48:59 *STMF      Manage_More_With_Less.htm
GUEST            1733 02/02/00 18:30:28 *STMF      index.htm.bak
QPGMR               0 02/10/00 15:59:33 *STMF      index.html
GUEST           45056 02/19/00 10:42:35 *DIR       Turbo/      <----- HERE'S MY DEFAULT DIRECTORY FOR USER TURBO
250 List completed.
770 bytes received in 0.00 seconds (770000.00 Kbytes/sec)  <---I TYPED CD PERSONALWEBPAGES
ftp> 250 "/home/personalwebpages" is current directory.    <--- FOR SOME REASON WHAT I TYPED DOESN'T SHOW UP
ftp> 200 PORT subcommand request successful.
125 List started.
QSECOFR         65536 02/02/00 18:21:57 *DIR       PhilWeb/
QSECOFR         61440 02/02/00 18:22:01 *DIR       KathyWeb/
250 List completed.                     ^^^^  NOTICE THIS IS A DIRECTORY
123 bytes received in 0.00 seconds (123000.00 Kbytes/sec)
ftp> 250 "/home/personalwebpages/philweb" is current directory.
ftp> 200 PORT subcommand request successful.
125 List started.
QSECOFR        187061 02/02/00 18:21:53 *STMF      wurz98.jpg
QSECOFR         44431 02/02/00 18:21:53 *STMF      wurz_can98.jpg
QSECOFR         98696 02/02/00 18:21:53 *STMF      stewart_elf.jpg
QSECOFR        102959 02/02/00 18:21:53 *STMF      schumacher_96.jpg
QSECOFR        123243 02/02/00 18:21:53 *STMF      schum_hun98.jpg
QSECOFR         33009 02/02/00 18:21:53 *STMF      schu_mon_97.jpg
QSECOFR        170500 02/02/00 18:21:54 *STMF      lauda_75.jpg
QSECOFR         71069 02/02/00 18:21:54 *STMF      hill_aus_98.jpg
QSECOFR         71935 02/02/00 18:21:54 *STMF      hakinn_br-gp98.jpg
QSECOFR         55924 02/02/00 18:21:54 *STMF      gilles.jpg
QSECOFR         35333 02/02/00 18:21:54 *STMF      emmo_73_sp.jpg
QSECOFR         80092 02/02/00 18:21:54 *STMF      diniz_fire.jpg
QSECOFR         76466 02/02/00 18:21:54 *STMF      brundle_aus_96.jpg
QSECOFR        137132 02/02/00 18:21:55 *STMF      brands_72.jpg
QSECOFR        233319 02/02/00 18:21:55 *STMF      andretti_zandvoort_78.jpg
QSECOFR         44398 02/02/00 18:21:55 *STMF      79brapiquet.jpg
QSECOFR          6657 02/02/00 18:21:55 *STMF      WP_THUMB_0.JPG
QSECOFR          7753 02/02/00 18:21:55 *STMF      WP_THUMB_1.JPG
QSECOFR          3792 02/02/00 18:21:55 *STMF      WP_THUMB_2.JPG
QSECOFR          3312 02/02/00 18:21:55 *STMF      WP_THUMB_3.JPG
QSECOFR          4550 02/02/00 18:21:55 *STMF      WP_THUMB_4.JPG
QSECOFR          3381 02/02/00 18:21:55 *STMF      WP_THUMB_5.JPG
QSECOFR          4336 02/02/00 18:21:56 *STMF      WP_THUMB_6.JPG
QSECOFR          4904 02/02/00 18:21:56 *STMF      WP_THUMB_7.JPG
QSECOFR          3673 02/02/00 18:21:56 *STMF      WP_THUMB_8.JPG
QSECOFR          4959 02/02/00 18:21:56 *STMF      WP_THUMB_9.JPG
QSECOFR          3416 02/02/00 18:21:56 *STMF      WP_THUMB_10.JPG
QSECOFR          4408 02/02/00 18:21:56 *STMF      WP_THUMB_11.JPG
QSECOFR          4686 02/02/00 18:21:56 *STMF      WP_THUMB_12.JPG
QSECOFR          5350 02/02/00 18:21:56 *STMF      WP_THUMB_13.JPG
QSECOFR          2096 02/02/00 18:21:56 *STMF      WP_THUMB_14.JPG
QSECOFR          4597 02/02/00 18:21:57 *STMF      WP_THUMB_15.JPG
QSECOFR          2867 02/02/00 18:21:57 *STMF      index.html
QSECOFR           532 02/02/00 18:21:57 *STMF      WebInfo.INI
250 List completed.                     ^^^^^  THIS IS A  STMF ====  Stream file
2293 bytes received in 0.27 seconds (8.49 Kbytes/sec)
ftp> ?Invalid command
ftp> 221 QUIT subcommand received.

MORE TYPES OF SHIT ON THE AS/400 FILE SYSTEM

 Object types that are commonly used or that you are likely to see on
 this display include the following:
AUTL        Authorization list
BLKSF       Block special file
CFGL        Configuration list
CLS         Class
CMD         Command
CTLD        Controller description
DDIR        Distributed directory
DEVD        Device description
DIR         Directory
DOC         Document
DSTMF       Distributed stream file
FILE        Database file or device file
FLR         Folder
JOBD        Job description
JOBQ        Job queue
LIB         Library
LIND        Line description
MSGQ        Message queue
OUTQ        Output queue
PGM         Program
SBSD        Subsystem description
SOMOBJ      System Object Model object
STMF        Stream file
SYMLNK      Symbolic link
USRPRF      User profile
*)
(*
Procedure LoadFakeInOutList(Var InOutListN:TStringList);
Begin
InOutListN.Clear;
InOutListN.Add('');
InOutListN.Add('QPGMR            8192 05/30/98 08:08:03 *FILE      QSRVSRC');
InOutListN.Add('');
InOutListN.Add('QPGMR            8192 05/30/98 08:08:03 *FILE      QTBLSRC');
InOutListN.Add('');
InOutListN.Add('QPGMR           12288 05/30/98 08:41:04 *FILE      QTXTSRC');
InOutListN.Add('');
InOutListN.Add('QSECOFR         40960 10/08/99 10:25:23 *FILE      REPLACE');
InOutListN.Add('');
InOutListN.Add('QSECOFR                                 *MEM       REPLACE.REPLACE');
InOutListN.Add('QUSER           69632 02/05/99 16:19:28 *DIR       HTML/');
InOutListN.Add('QUSER            1575 02/05/99 16:06:37 *STMF      index.htm.$$$  <---- EDITORS WAY OF .BAK');
InOutListN.Add('GUEST            1733 02/02/00 18:32:43 *STMF      index.htm');
InOutListN.Add('QUSER          151552 07/16/99 12:33:16 *DIR       DPISALES/');
InOutListN.Add('QUSER           14398 10/21/98 12:49:52 *STMF      readme.htm');
InOutListN.Add('QSECOFR         69632 10/03/98 15:22:35 *DIR       java_applets/');
InOutListN.Add('QSECOFR         53248 02/02/00 18:21:57 *DIR       PersonalWebPages/');
InOutListN.Add('QUSER           69632 12/04/98 15:17:54 *DIR       Art/');
InOutListN.Add('QUSER             949 10/21/98 15:48:59 *STMF      Manage_More_With_Less.htm');
InOutListN.Add('GUEST            1733 02/02/00 18:30:28 *STMF      index.htm.bak');
InOutListN.Add('QPGMR               0 02/10/00 15:59:33 *STMF      index.html');
InOutListN.Add('GUEST           45056 02/19/00 10:42:35 *DIR       Turbo/');
InOutListN.Add('QSECOFR         65536 02/02/00 18:21:57 *DIR       PhilWeb/');
InOutListN.Add('QSECOFR         61440 02/02/00 18:22:01 *DIR       KathyWeb/');
InOutListN.Add('123 bytes received in 0.00 seconds (123000.00 Kbytes/sec)');
InOutListN.Add('QSECOFR        187061 02/02/00 18:21:53 *STMF      wurz98.jpg');
InOutListN.Add('QSECOFR         44431 02/02/00 18:21:53 *STMF      wurz_can98.jpg');
InOutListN.Add('QSECOFR         98696 02/02/00 18:21:53 *STMF      stewart_elf.jpg');
InOutListN.Add('QSECOFR        102959 02/02/00 18:21:53 *STMF      schumacher_96.jpg');
InOutListN.Add('QSECOFR        123243 02/02/00 18:21:53 *STMF      schum_hun98.jpg');
InOutListN.Add('QSECOFR         33009 02/02/00 18:21:53 *STMF      schu_mon_97.jpg');
InOutListN.Add('QSECOFR        170500 02/02/00 18:21:54 *STMF      lauda_75.jpg');
InOutListN.Add('QSECOFR         71069 02/02/00 18:21:54 *STMF      hill_aus_98.jpg');
InOutListN.Add('QSECOFR         71935 02/02/00 18:21:54 *STMF      hakinn_br-gp98.jpg');
InOutListN.Add('QSECOFR         55924 02/02/00 18:21:54 *STMF      gilles.jpg');
InOutListN.Add('QSECOFR         35333 02/02/00 18:21:54 *STMF      emmo_73_sp.jpg');
InOutListN.Add('QSECOFR         80092 02/02/00 18:21:54 *STMF      diniz_fire.jpg');
InOutListN.Add('QSECOFR         76466 02/02/00 18:21:54 *STMF      brundle_aus_96.jpg');
InOutListN.Add('QSECOFR        137132 02/02/00 18:21:55 *STMF      brands_72.jpg');
InOutListN.Add('QSECOFR        233319 02/02/00 18:21:55 *STMF      andretti_zandvoort_78.jpg');
InOutListN.Add('QSECOFR         44398 02/02/00 18:21:55 *STMF      79brapiquet.jpg');
InOutListN.Add('QSECOFR          6657 02/02/00 18:21:55 *STMF      WP_THUMB_0.JPG');
InOutListN.Add('QSECOFR          7753 02/02/00 18:21:55 *STMF      WP_THUMB_1.JPG');
InOutListN.Add('QSECOFR          3792 02/02/00 18:21:55 *STMF      WP_THUMB_2.JPG');
InOutListN.Add('QSECOFR          3312 02/02/00 18:21:55 *STMF      WP_THUMB_3.JPG');
InOutListN.Add('QSECOFR          4550 02/02/00 18:21:55 *STMF      WP_THUMB_4.JPG');
InOutListN.Add('QSECOFR          3381 02/02/00 18:21:55 *STMF      WP_THUMB_5.JPG');
InOutListN.Add('QSECOFR          4336 02/02/00 18:21:56 *STMF      WP_THUMB_6.JPG');
InOutListN.Add('QSECOFR          4904 02/02/00 18:21:56 *STMF      WP_THUMB_7.JPG');
InOutListN.Add('QSECOFR          3673 02/02/00 18:21:56 *STMF      WP_THUMB_8.JPG');
InOutListN.Add('QSECOFR          4959 02/02/00 18:21:56 *STMF      WP_THUMB_9.JPG');
InOutListN.Add('QSECOFR          3416 02/02/00 18:21:56 *STMF      WP_THUMB_10.JPG');
InOutListN.Add('QSECOFR          4408 02/02/00 18:21:56 *STMF      WP_THUMB_11.JPG');
InOutListN.Add('QSECOFR          4686 02/02/00 18:21:56 *STMF      WP_THUMB_12.JPG');
InOutListN.Add('QSECOFR          5350 02/02/00 18:21:56 *STMF      WP_THUMB_13.JPG');
InOutListN.Add('QSECOFR          2096 02/02/00 18:21:56 *STMF      WP_THUMB_14.JPG');
InOutListN.Add('QSECOFR          4597 02/02/00 18:21:57 *STMF      WP_THUMB_15.JPG');
InOutListN.Add('QSECOFR          2867 02/02/00 18:21:57 *STMF      index.html');
InOutListN.Add('QSECOFR           532 02/02/00 18:21:57 *STMF      WebInfo.INI');
End;

FTP MLSD command, same format for MSLT for a single file
size=0;type=cdir;perm=fdelcmp;create=20020616151738;modify=20031002125810; .
size=0;type=pdir;perm=fdelcmp;create=20020616151738;modify=20031002125810; ..
size=17199;type=file;perm=fdrwa;create=20030616152030;modify=20031001190100; 00master.zip
size=182928;type=file;perm=fdrwa;create=20030922195127;modify=20030922190600; 12=page-004394.zip
size=134503;type=file;perm=fdrwa;create=20030923181732;modify=20030923170800; 12=page-004399.zip
size=225460;type=file;perm=fdrwa;create=20030923193147;modify=20030923185600; 12=page-004400.zip
size=205011;type=file;perm=fdrwa;create=20030923120836;modify=20030922225700; 12=page-004405.zip
size=191721;type=file;perm=fdrwa;create=20030905141821;modify=20030904181100; 20=page-004320.zip
size=183977;type=file;perm=fdrwa;create=20030905142247;modify=20030904181100; 20=page-004321.zip
size=0;type=dir;perm=fdelcmp;create=20030219123018;modify=20030305153855; errors
size=0;type=dir;perm=fdelcmp;create=20021217151845;modify=20030903193625; new software
size=0;type=dir;perm=fdelcmp;create=20020805160304;modify=20031002133003; sql logs
size=70806;type=file;perm=fdrwa;create=20030718113340;modify=20031001185600; vehinfiles.zip
size=0;type=dir;perm=fdelcmp;create=20020801100314;modify=20031004124403; zip logs


FTP SITE DMLSD -R and SITE CMLSD -R includes recursive sub-directories,
currently only ICS FTP server 1.54 and later
size=87284;type=file;perm=fdrwa;create=20071119100609;modify=20061129190135; /vmp-20061129.log
size=37256;type=file;perm=fdrwa;create=20071119100609;modify=20061130195708; /vmp-20061130.log
size=18785;type=file;perm=fdrwa;create=20070716105754;modify=20070716105754; /archmon/PC19==20070716-115754.zip
size=67489;type=file;perm=fdrwa;create=20070718115746;modify=20070718115746; /archmon/PC19==20070718-125746.zip
size=40207;type=file;perm=fdrwa;create=20070810085724;modify=20070810085724; /archmon/PC19==20070810-095724.zip
size=1011267;type=file;perm=fdrwa;create=20070303023249;modify=20050622130524; /downinst/99=inst-all-900179.zip
size=8531099;type=file;perm=fdrwa;create=20070810085721;modify=20070809184912; /download/30=page-023447.zip
size=1850041;type=file;perm=fdrwa;create=20070716153422;modify=20070716114356; /download/30=page-023817.zip
size=2870709;type=file;perm=fdrwa;create=20070716153423;modify=20070716124420; /download/30=page-023819.zip

Enterprise UNIX 2.4.02
-CR-------FTP A orclb2b       609      253 Jun 03 05:19 Schneider_2010Jun03_05_19_01_227.dat
-CR-------FTP A orclb2b     29571      253 Jun 03 15:16 Schneider_2010Jun03_15_16_18_334.dat
-CR-------FTP A orclb2b      8262      253 Jun 03 15:16 Schneider_2010Jun03_15_16_22_323.dat
-CR-------FTP A orclb2b     10752      253 Jun 03 15:16 Schneider_2010Jun03_15_16_26_378.dat
-CR-------FTP A orclb2b      9316      253 Jun 03 15:16 Schneider_2010Jun03_15_16_30_323.dat
-CR-------FTP A orclb2b      9396      253 Jun 03 15:16 Schneider_2010Jun03_15_16_34_776.dat
-CR-------FTP A orclb2b     10225      406 Jun 04 06:24 Schneider_2010Jun04_06_24_28_749.dat
-CR-------FTP A orclb2b      8185      292 Jun 04 09:36 Schneider_2010Jun04_09_36_29_663.dat
-CR-------FTP A orclb2b     10231      253 Jun 04 09:36 Schneider_2010Jun04_09_36_33_750.dat
-CR-------FTP A orclb2b     10274      253 Jun 04 09:36 Schneider_2010Jun04_09_36_37_858.dat
-CR-------FTP A orclb2b     10295      253 Jun 04 09:37 Schneider_2010Jun04_09_37_15_014.dat
-CRT------FTP A orclb2b      7608      253 Jun 04 12:07 Schneider_2010Jun04_12_07_18_065.dat

*)



Procedure TIcsFtpMulti.SetupVMS (const BaseFolder: string; var HostType: THostType);
Begin
    HostType := FTPTYPE_NONE ;
    If Pos(':[',BaseFolder) <> 0 Then HostType := FTPTYPE_MVS ;
End;

function IncludeTrailingUnixDelimiter(const S : String): String;
begin
    if (Length(S) > 0) and (S[Length(S)] <> '/') then
        Result := S + '/'
    else if (Length(S) = 0) then    // 22 Oct 2008
        Result := '/'
    else
        Result := S;
end;

Function Strunc(S1:String):String;
Var
    Len:Integer;
Begin
    Len:=Length(S1);
    While (Len>0) And (S1[1]=' ') Do
    Begin
        Delete(S1,1,1);
        Dec(Len)
    End;
    Len:=Length(S1);
    While (Len>0) And (S1[Len]=' ') Do
    Begin
        Delete(S1,Len,1);
        Dec(Len);
    End;
    Strunc:=S1;
End;

// unpack FTP directory in downloaded file into dynamic array
// returns number of actual files in directory (it may include other dirs)

function TIcsFtpMulti.UnpackFtpFDir (DirStream: TStream; RemDir, BaseDir: string; Level: integer ;
                               var DCodePage: Cardinal ; var HostType: THostType;
                                    var TotFiles: integer; var RemFiles: TIcsFDirRecs ): integer ;
var
    RawLines, RawCurLine: RawByteString ;
    initdlen, RawTotLen, RawLineStart, RawLineEnd: integer ;
Type
    SArray=Array[0..20] Of String;
Const
    Months:Array[1..12] Of String =     // v3.5
     ('JANUARY','FEBRUARY','MARCH','APRIL',
     'MAY','JUNE','JULY','AUGUST',
     'SEPTEMBER','OCTOBER','NOVEMBER','DECEMBER');
Var
    CurYear,Month,Day:Word;
    PosX,X,MaxX,I:Integer;
    MyGroup,MyLink,S2,MySize,MyAttr,MyDate,MyTime,MyName,MyDir,S1:String;
    MyDateTime:TDateTime;
    Second,Fifth:String;
    JunkArray:SArray;
    ACodePage: Cardinal;
    CurLine: String;

Function IntToStr2(const W1:Integer):String;
Var
    S1:String;
Begin
    S1:=IntToStr(W1);
    If Length(S1)<2 Then S1:='0'+S1;
    IntToStr2:=S1;
End;

Function Value(const S1:String):LongInt;
Var
    X,Y:LongInt;
Begin
    Val(S1,X,Y);
    Value:=X;
End;


Function GetFirstParam(Var S1:String):String;
Var
    X:Integer;
Begin
    If S1='' Then
    Begin
        Result:='';
        Exit;
    End;
    If S1[Length(S1)]<>' ' Then S1:=S1+' ';
    X:=Pos(' ',S1);
    Result:=Copy(S1,1,X-1);
    S1:=Copy(S1,X,Length((S1)));
    S1:=Strunc(S1);
End;





    Function MonthMatch(P2:String):Integer;
    Var
        X:Integer;
    Begin
        Result:=0;
        X:=1;
        Repeat
            If Pos(P2,Months[X])<>0 Then Result:=X;
            Inc(X);
        Until (Result<>0) Or (X>12);
    End;

    Function SuckParmsB(Var S1:String):String;
    Var
        Y:Integer;
    Begin
        If S1='' Then
        Begin
            Result:='';
            Exit;
        End;
        If S1[Length(S1)]<>' ' Then S1:=S1+' ';
        Y:=Pos(']',S1);
        Result:=Copy(S1,1,Y);
        S1:=Copy(S1,Y+1,Length((S1)));
        S1:=Strunc(S1);
    End;

    Function GetFifth(S1:String):String;
    Begin
        Result:=GetFirstParam(S1);
        Result:=GetFirstParam(S1);
        Result:=GetFirstParam(S1);
        Result:=GetFirstParam(S1);
        Result:=GetFirstParam(S1);
        If Result='' Then Result:='-';
    End;

    Function GetSecond(S1:String):String;
    Begin
        Result:=GetFirstParam(S1);
        Result:=GetFirstParam(S1);
        If Result='' Then Result:='-';
    End;

    Function Filter(S1:String):String;
    Var
        X:Integer;
    Begin
        For X:=1 To Length(S1) Do
        Begin
           If AnsiChar (S1[X]) In [#0..#27] Then S1[X]:=' ';   // v3.5
        End;
        Result:=S1;
    End;

    Function FormatDate2(S1:String):String;
    Var
        Year:Integer;
    Begin
        While Pos('-',S1)<>0 Do S1[Pos('-',S1)]:='/';
        Year:=Value(Copy(S1,7,2));
        If Year<30 Then
            Year:=2000+Year
        Else
            Year:=1900+Year;
        Result:=Copy(S1,1,6)+IntToStr(Year);
    End;

    Function FormatDate1(S1:String):String;
    Var
        P1,P2,P3:String;
        X:Integer;
        Found:Integer;
    Begin
        Result:=S1;
        X:=Pos('-',S1);
        If X=0 Then X:=Pos('/',S1);
        If X<>0 Then
        Begin
            P1:=IntToStr2(Value(Copy(S1,1,X-1)));
            S1:=Copy(S1,X+1,Length(S1));
            X:=Pos('-',S1);
            If X=0 Then X:=Pos('/',S1);
            If X<>0 Then
            Begin
                P2:=UpperCase(Copy(S1,1,X-1));
                P3:=Copy(S1,X+1,Length(S1));
                If NOT(AnsiChar (P2[1]) In ['0'..'9']) Then
                Begin
                    Found:=MonthMatch(P2);
                    If Found>0 Then P2:=IntToStr2(Found);
                    Result:=P2+'/'+P1+'/'+P3;
                End
                Else
                    Result:=P1+'/'+P2+'/'+P3;
            End;
        End;
    End;

    Function FormatTime1(S1:String):String;
    Var
        X:Integer;
    Begin
        S1:=UpperCase(S1);
        If Length(S1)>5 Then
        Begin
            If Pos('PM',S1)<>0 Then
            Begin
                X:=Value(Copy(S1,1,2));
                If X<12 Then Inc(X,12);
                Result:=IntToStr2(X)+Copy(S1,3,3);
            End
            Else If Pos('AM',S1)<>0 Then  // angus, 9 Apr 2005
            Begin
                X:=Value(Copy(S1,1,2));
                If X=12 Then X:=0 ; //  allow for 12:00AM midnight being hour 00:00
                Result:=IntToStr2(X)+Copy(S1,3,3);
            End
            Else
                Result:=Copy(S1,1,5);
            Result := Result + ':00' ;  // angus fake seconds
        End
        Else
            Result:=S1;
    End;

    Function AdjustS1(Junk1Array:SArray):String;
    Var
        Found:Boolean;
        Y,X,TopEnd:Integer;
    Begin
        TopEnd:=High(Junk1Array);
        Found:=False;
        Result:='BAD INPUT LINE!';
        X:=0;
        Repeat
            If MonthMatch(UpperCase(Junk1Array[X]))>0 Then
            Begin
                Result:='';
                For Y:=X-1 To TopEnd-1 Do
                Begin
                    Result:=Result+Junk1Array[Y]+' ';
                End;
               Found:=True;
            End;
            Inc(X);
        Until (Found=True) Or (X>=TopEnd);
    End;

    function FindMlsFact (const response, fact: string): string ;
    var
        I: integer ;
        S: string ;
    begin
        result := '' ;
        I := Pos (fact, response) ;   // ie type=, size=, modify=, perm=
        if I <= 0 then exit ;
        I := I + Length (fact) ;
        if I > Length (response) then exit ;
        S := Copy (response, I, 999) ;   // ie size=183977;type=fil
        I := Pos (';', S) ;  // fact terminator
        if I <= 0 then exit ;
        result := Copy (S, 1, Pred (I)) ;
    end ;

    function CheckUnixAttributes (const AttrStr: string): boolean ;   // 7 June 2010
    var
        len, I: integer ;
    begin
        result := true ;
        len := Length (AttrStr) ;
        if len > 10 then len := 10 ;
        for I := 1 to Len do
        begin
            if Pos (AttrStr [1], '-cldwrxstCLDWRXST') = 0 then  // only these characters allowed as UNIX attributes
            begin                                               // some idiot servers return capital letters
                result := false ;
                exit ;
            end;
        end;
    end;

// mainline start
Begin
    result := 0 ;
    RemDir := IncludeTrailingUnixDelimiter (RemDir) ;
    initdlen := Length (BaseDir) ;
    if NOT Assigned (DirStream) then exit ;
    RawTotLen := DirStream.Size ;
    if RawTotLen <= 1 then exit ;
    DirStream.Position := 0; { V8.67 Seek (0, soFromBeginning) ; }
    try  // finally

// load FTP file directory, make sure it's not empty, check if any UTF8 escaped characters
    SetLength (RawLines, RawTotLen);
    DirStream.ReadBuffer (RawLines [1], RawTotLen);
    S2 := 'ANSI' ;
    ACodePage := CP_ACP ;
    if NOT IsUsAscii (RawLines) then
    begin
        if (DCodepage = CP_UTF8) or IsUtf8Valid (RawLines) then // 16 Sept 2008 check for UTF8
        begin
            S2 := 'UTF8' ;
            ACodePage := CP_UTF8 ;
            if DCodePage <> CP_UTF8 then
            begin
                doCopyEvent (LogLevelInfo, 'Setting UTF8 ON in Response to UTF8 Directory') ;
                DCodePage := CP_UTF8 ;  // pass back to caller
                fNewOpts := 'UTF8 ON' ;
                Opts ;
                doCopyEvent (LogLevelInfo, 'UTF8 Opts: ' + LastResponse) ;
            end ;
        end ;
    end;

// count lines in listing, and check for MVS header
    MaxX := 1 ;
    for RawLineEnd := 1 to RawTotLen - 1 do  // skip last IcsLF
    begin
        if RawLines [RawLineEnd] = IcsLF then inc (MaxX) ;
        if (MaxX < 10) and (HostType <> FTPTYPE_MVS) then  // look for header: WORK:[POOR]
        begin
            if (RawLines [RawLineEnd] = ':') then
            begin
                if (RawLines [RawLineEnd + 1] = '[') then
                begin
                    I := RawLineEnd + 1 ;
                    while (RawLines [I] <> IcsLF) do
                    begin
                        if RawLines [I] = ']' then
                        begin
                            HostType := FTPTYPE_MVS;
                            break ;
                        end;
                        inc (I) ;
                        if (I > RawTotLen) then break ; // sanity test
                    end ;
                end ;
            end ;
        end ;
    end ;
    doCopyEvent (LogLevelDiag, 'Server Returned ' + IcsIntToCStr (MaxX) + ' Line(s), Bytes ' +
                IcsIntToCStr (DirStream.Size) + ' for Directory: ' + RemDir + ', Format: ' + S2) ;
    SetLength (RemFiles, TotFiles + MaxX + 2) ;   // allocate space for filerecs

// keep year, for UNIX listing without it
    MyDateTime:=Date;
    DecodeDate(MyDateTime,CurYear,Month,Day);

// read each line in the FTP filelist
    RawLineStart := 1 ;
    RawLineEnd := 1 ;
    For X:=1 To MaxX Do
    Begin
        if (RawLineEnd > RawTotLen) then break ; // sanity test
        while (RawLineEnd <= RawTotLen) and (RawLines [RawLineEnd] <> IcsLF) do inc (RawLineEnd) ; // find next line end
        I := RawLineEnd - RawLineStart ;
        if (RawLineEnd > 1) and (RawLines [RawLineEnd - 1] < IcsSpace) then dec (I) ; // remove CR
        RawCurLine := Copy (RawLines, RawLineStart, I) ;  // get next line
        inc (RawLineEnd) ;
        RawLineStart := RawLineEnd ;

      // see if logging raw list, do it for single file anyway
        if fDispRemList or (MaxX = 1) then doCopyEvent (LogLevelDiag, String (RawCurLine)) ;   // v3.5

       // 16 Sept 2008 convert UTF8 rawstring to UTF16 widestring or ANSI
    {$IFDEF COMPILER12_UP}
        CurLine := AnsiToUnicode (RawCurLine, ACodePage) ;
    {$ELSE}
        CurLine := ConvertCodepage (RawCurLine, ACodePage, CP_ACP);
    {$ENDIF}
        if Length (CurLine) = 0 then continue ;
        MyDate:='';
        MyTime:='';
        MyName:='';
        MyDir:='';
        MyAttr:='';
        MySize:='';
        MyLink:='';
        MyGroup:='';
        S2:='';
        if HostType = FTPTYPE_MLSD then
            S1 := CurLine       // get full line for processing
        else
        begin
            S1:=Strunc(Filter(CurLine));    // get filtered full line for processing
            Fifth:=GetFifth(S1);
            Second:=GetSecond(S1);
        end ;
        If S1<>'' Then
        Begin

        // IBM MVS line
            If HostType = FTPTYPE_MVS Then
            Begin
                MyName:=GetFirstParam(S1);
                If S1='' Then   // get next line, listing split over two lines
                Begin
                    while (RawLines [RawLineEnd] <> IcsLF) and  (RawLineEnd <= RawTotLen) do inc (RawLineEnd) ; // find next line end
                    I := RawLineEnd - RawLineStart ;
                    if (RawLineEnd > 1) and (RawLines [RawLineEnd - 1] < IcsSpace) then dec (I) ; // remove CR
                    RawCurLine := Copy (RawLines, RawLineStart, I) ;  // get next line
                    inc (RawLineEnd) ;
                    RawLineStart := RawLineEnd ;
                    if fDispRemList then doCopyEvent (LogLevelDiag, String (RawCurLine)) ;   // v3.5
                    S1 := Strunc (AnsiToUnicode (RawCurLine, ACodePage)) ;
                End;
                MySize:=IntToStr(Value(GetFirstParam(S1))*512);
                MyDate:=GetFirstParam(S1);
                MyTime:=FormatTime1(GetFirstParam(S1));
                MyDate:=FormatDate1(MyDate);
                MyGroup:=SuckParmsB(S1);
                MyAttr:=GetFirstParam(S1);
                If Pos('.DIR',UpperCase(MyName))<>0 Then
                Begin
                    MyDir:=sDirLit;
                    MyName:=IcsExtractNameOnly(Copy(MyName,1,Pos(';',MyName)-1));
                End
                Else
                Begin
                    MyName:=Copy(MyName,1,Pos(';',MyName)-1);
                    MyDir:='';
                End;
            End

         // FTP MSLD command
            else if HostType = FTPTYPE_MLSD then
            begin
                I := Pos (#32, S1) ;  /// file name follows first space in line
                if (I > 1) and (Length (S1) >= Succ (I)) then  // 6 Aug 2007 allow for single char directory
                                     MyName := Copy (S1, Succ (I), 999) ;
                S1 := AnsiLowerCase (S1) ;
                S2 := FindMlsFact (S1, 'type=') ;
                if (S2 = 'dir') {or (S2 = 'cdir') or (S2 = 'pdir')} then  // 16 Mar 2006
                    MyDir := sDirLit
                else if S2 <> 'file' then MyName := '' ;
              // could look for os.unix=slink, etc
                if MyName <> '' then
                begin
                    MySize := FindMlsFact (S1, 'size=') ;
                    MyDate := FindMlsFact (S1, 'modify=') ;
                    MyAttr := FindMlsFact (S1, 'perm=') ;
                end ;
            end

        // UNIX line
            Else if CheckUnixAttributes (S1) then   // 7 June 2010 simplified checking UNIX attributes
            Begin
                HostType := FTPTYPE_UNIX;
                MyAttr:=GetFirstParam(S1);
                If MyAttr[1]='d' Then MyDir:=sDirLit;
                GetFirstParam(S1);
                GetFirstParam(S1);   // DLR Fix to make sure it works with APPLE.COM
                JunkArray[0]:=GetFirstParam(S1);
                JunkArray[1]:=GetFirstParam(S1);
                JunkArray[2]:=GetFirstParam(S1);
                JunkArray[3]:=GetFirstParam(S1);
                JunkArray[4]:=GetFirstParam(S1);
                JunkArray[5]:=GetFirstParam(S1);
                JunkArray[6]:=S1;
                S1:=AdjustS1(JunkArray);

                MySize:=GetFirstParam(S1);
                MyDate:=GetFirstParam(S1);
                MyDate:=GetFirstParam(S1)+'/'+MyDate;
                MyTime:=FormatTime1(GetFirstParam(S1));
                If Pos(':',MyTime)=0 Then       // no time available, fake it
                Begin
                    MyDate:=FormatDate1(MyDate+'/'+MyTime);
                    MyTime:='00:00';
                End
                Else
                Begin
             // no year, use today - but correct to last year later if less than 9 months old
                    MyDate:=FormatDate1(MyDate+'/'+IntToStr(CurYear));
                End;
                MyDate:=FormatDate1(MyDate);
                MyName:=S1;
                If Pos('->',MyName)<>0 Then
                Begin
                    PosX:=Pos('->',MyName);
                    MyName:=Strunc(Copy(S1,1,PosX-1));
                    MyLink:=Strunc(Copy(S1,PosX+2,Length(S1)));
                End;
            End

        // Windows NT, maybe DOS
            Else If ((S1[3]='-') And (S1[6]='-')) Or ((S1[3]='/') And (S1[6]='/')) Then
            Begin
                HostType := FTPTYPE_DOS;
                MyDate:=FormatDate2(GetFirstParam(S1));
                MyTime:=FormatTime1(GetFirstParam(S1));
                MyDir:=GetFirstParam(S1);
                If (MyDir<>'') And (MyDir[1]<>'<') Then
                Begin
                    MySize:=MyDir;
                    MyDir:='';
                End;
                MyName:=Strunc(S1);
            End

        // IBM AS400
            Else If (Fifth[1]='*') Or (Second[1]='*') Then
            Begin
                HostType := FTPTYPE_AS400;
                GetFirstParam(S1);
                MySize:=GetFirstParam(S1);
                If MySize='*MEM' Then
                Begin
                    MySize:='N/A';
                    MyDate:='N/A';
                    MyTime:='N/A';
                    MyDir:='*MEM';
                    MyName:=S1;
                End
                Else
                Begin
                    MyDate:=GetFirstParam(S1);
                    MyTime:=GetFirstParam(S1);
                    MyDir:=GetFirstParam(S1);
                    MyName:=S1;
                End;
                If NOT((MyDir='*DDIR') Or (MyDir='*DIR') Or (MyDir='*LIB')  Or
                    (MyDir='*MEM') Or (MyDir='*FLR') Or (MyDir='*BLKSF')Or
                    (MyDir='*DOC') Or (MyDir='*DSTMF')Or (MyDir='*FILE') Or
                    (MyDir='*PGM') Or (MyDir='*STMF')) Then MyName:='';
                If (MyDir='*DIR') Or (MyDir='*DDIR') Or (MyDir='*LIB')  Or
                    (MyDir='*FLR') Then MyDir:=sDirLit
                Else
                    MyDir:='';
            End;

        // found a file, add to list
            If MyName<>'' Then
            Begin
                inc (TotFiles) ;
                inc (result) ;
                with RemFiles [pred (TotFiles)] do
                begin
                    if MyName [1] = '/' then // 22 Nov 2007 name includes directories
                    begin
                        FrFullName := MyName ;
                        I := Length (MyName) ;
                        while (I > 1) and (MyName [I] <> '/') do dec (I) ;
                        FrFileName := Copy (MyName, I + 1, 255) ;
                        FrSubDirs :=  Copy (MyName, 1, I) ;
                    end
                    else
                    begin
                        FrFileName := MyName ;
                        FrSubDirs := Copy (RemDir, initdlen, 99) ;
                        FrFullName := RemDir + FrFileName ;
                        FrDirLevel := Level ;
                    end ;
                    FrFileBytes := atoi64 (MySize) ;  // 4 Sept 2005
                    if HostType = FTPTYPE_MLSD then
                    begin
                         FrFileUDT := MDTM2Date (myDate) ;     // Warning - ws_ftp returns local time not UTC for mlsd
                         FrFileDT := UTCToLocalDT (FrFileUDT) ;
                    end
                    else
                    begin
                        if Length (MyDate) <> 0 then
                            FrFileDT := ConvUSADate (MyDate) + StrToTime (MyTime)
                        else
                            FrFileDT := 0 ;
                    // see if date is more than three months beyond today, assume last year
                        if FrFileDT > (MyDateTime + 90) then
                        begin
                            FrFileDT := IncMonth (FrFileDT, -12) ;
                        end ;
                        FrFileUDT := 0 ;  // accurate UTC time may be found using MDTM command later
                    end ;
                    if MyDir = '' then
                        FrExtra := Copy (MyAttr, 1, 4)
                    else
                        FrExtra := MyDir ;
                    FrLinks := MyLink ;
                    if ((MyName = '.') or (MyName = '..')) then
                        FrFileCopy := FCStateIgnore
                    else if (MyDir = sDirLit) then
                    begin
                     //   FrFileCopy := FCStateDir ;   21 Feb 2011 confusing
                        FrFileCopy := FCStateNone ;
                        FrFileAttr := faDirectory ;  // 21 Feb 2011
                    end
                    else if (MyLink <> '') then
                       FrFileCopy := FCStateIgnore
                    else
                    begin
                        FrFileCopy := FCStateNone ;
                        inc (result) ;
                    end ;
                end ;
            End;
        End;
    End;
    finally
        SetLength (RemFiles, TotFiles) ;
    end ;
End;

// connect and logon to FTP server
// also setups most common FTP parameters and event handlers
// keep server root directory and creates base directory

function TIcsFtpMulti.FtpLogon: TIcsTaskResult ;
var
    remdir, fname: string ;
    attemptnr: integer ;
    ret: boolean ;
begin
    result := TaskResAbort ;
    IcsCopyProgClearAll (fCopyProg) ;  // 22 May 2013
    fCancelFlag := false ;
    fServRootDir := '' ;
    fServBaseDir := '' ;
    if fBulkMode = BulkModeDownload then
        remdir := fSrcDir
    else
        remdir := fTarDir ;
    fUsingCompression := false ;
    if CaseFile = FileLowerCase then fServBaseDir := AnsiLowerCase (fServBaseDir) ;
    if Timeout < 15 then Timeout := 15 ;    // sync timeout
    if Port = '0' then Port := 'ftp' ;
    ResumeAt := 0 ; // 4 July 2007 ensure resume position reset
    if fMaxAttempts = 0 then fMaxAttempts := 3 ;
    if fAttemptDelay = 0 then fAttemptDelay := 5 ;
    fProgFileSize := 0 ;    // no progress counter
    if fHostName1 <> '' then fHostName := fHostName1 ;
// UserName 'anonymous', Password 'ftp@' for anonymous FTP access to public servers
    fReqResponse := '' ;
    if (fBulkMode = BulkModeNone) then
        fReqResponse := 'Must Specify FTP Mode'
    else if (fUserName = '') then
        fReqResponse := 'Must Specify FTP Logon Name'
    else if (fPassword = '') then
        fReqResponse := 'Must Specify FTP Logon Password'
    else if (fHostName = '') then
        fReqResponse := 'Must Specify FTP Host Name'
    else if (remdir = '') then
        fReqResponse := 'Must Specify FTP Remote Directory' ;
    if fReqResponse <> '' then
    begin
        doCopyEvent (LogLevelInfo, fReqResponse) ;
        result := TaskResFail ;
        exit ;
    end ;
    fPBSZSize := 0 ;
    fSslType := sslTypeNone ;
    if fFtpType in [FtpTypeAuthSslCtl, FtpTypeAuthSslData, FtpTypeAuthSslBoth] then
    begin
        fSslType := sslTypeAuthTls ; // sslTypeAuthSsl ;
    end
    else if fFtpType in [FtpTypeConnSslCtl, FtpTypeConnSslData, FtpTypeConnSslBoth] then
    begin
        fPort := fFtpSslPort ;
        fSslType := sslTypeImplicit ;
    end ;
    if (fSslType > sslTypeNone) and (NOT Assigned (fExternalSslSessionCache)) then
    begin
        fExternalSslSessionCache := TSslAvlSessionCache.Create (self) ;
    end;
 //   FSslContext.SslSecLevel := sslSecLevel80bits ;  // March 2017
    FSslContext.SslCliSecurity := fFtpSslCliSecurity;  // June 2018

  // 20 Apr 2015 see if verifying server SSL certificate
    if (fSslType > sslTypeNone) then   // June 2018 even if not checking certs
    begin
        FSslContext.SslECDHMethod := sslECDHAuto ;  // 11 May 2015
        if (FFtpSslVerMethod > ftpSslVerNone) then   // June 2018
        begin
            FSslContext.SslVerifyPeer := true ;
            FSslContext.SslVerifyPeerModes := [SslVerifyMode_PEER] ;
            FSslContext.SslOcspStatus := true;     { V8.69 use OCSP stapling to get revoked status }
        end;
        FSslContext.SslSessionCacheModes := [sslSESS_CACHE_CLIENT] ;  // Dec 2016
        if fSslSessCache then   // Dec 2016
        begin
            FSslContext.SslSessionCacheModes := [sslSESS_CACHE_CLIENT,
                sslSESS_CACHE_NO_INTERNAL_LOOKUP, sslSESS_CACHE_NO_INTERNAL_STORE] ;
        end;
        if (FFtpSslVerMethod >= ftpSslVerBundle) then  // June 2018 also win
        begin
            fname := fFtpSslRootFile ;
            if (Pos (':', fname) = 0) then fname := ExtractFileDir (ParamStr (0)) + '\' + fname ;
            if NOT FileExists (fname) then
            begin
                fSslContext.SslCALines.Text := sslRootCACertsBundle;  // June 2018 built-in
            end
            else
                fSslContext.SslCAFile := fname;
        end;
    end ;
    if (fSslType > sslTypeNone) then  // 20 Apr 2015 get any SSL context errors now
    begin
        try
            fSslContext.InitContext;
        except
            fReqResponse := 'Error Starting SSL - ' + IcsGetExceptMess (ExceptObject) ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
            result := TaskResFail ;
            exit ;
        end;
    end;

    fReqResponse := 'No Errors' ;
    for attemptnr := 1 to fMaxAttempts do
    begin
    // toggle between two FTP servers
        if fHostName1 <> '' then
        begin
            if (NOT Odd (attemptnr)) and (fHostName2 <> '') then
                fHostName := fHostName2
            else
                fHostName := fHostName1 ;
        end ;
        fHostName := IcsStripIpv6Addr (fHostName) ; // v4.0 strip [] off ipv6 address

  // alternate TLS and SSL
        if (NOT Odd (attemptnr)) and (fSslType <> sslTypeNone) then
        begin
            if fSslType = sslTypeAuthTls then
               fSslType := sslTypeAuthSsl
            else if fSslType = sslTypeAuthSsl then
               fSslType := sslTypeAuthTls ;
        end ;
        doCopyEvent (LogLevelInfo, 'Connect/Logon to FTP Server: ' +
                                    IcsFmtIpv6AddrPort (fHostName, fPort)) ; // v4.0
        fFtpErrFlag := false ;
        HostFileName := '' ;
        LocalFileName := '' ;
        DisplayFileFlag := false ;
        if fCancelFlag then exit ;
        if (magftpNoHost in fMagFtpOpts) then
            ret := Connect           // connect, user name and password
        else
            ret := ConnectHost ;     // connect, host, user name and password 18 Sept 2008 also sends HOST command
        if fCancelFlag then ret := false ;
        if fFtpErrFlag then ret := false ;   // set in error event
        if NOT CheckReady then
        begin
            doCopyEvent (LogLevelInfo, 'Warning - FTP Not Ready') ;
            ret := false ;  // 17 Sept 2010 stop further commands
        end ;
        if ret then
        begin
            doCopyEvent (LogLevelInfo, Trim (LastMultiResponse)) ;
            ret := Pwd ;  // get initial working directory to DirResult
            if fCancelFlag then ret := false ;
            if NOT ret then
            begin
                doCopyEvent (LogLevelInfo, 'Logon Appeared to Fail, Retrying') ;
                ret := Connect ;             // connect, user name and password
                if fCancelFlag then ret := false ;
                if ret then ret := Pwd ;  // get initial working directory
            end ;
            if ret then
            begin
                if Pos (':', remdir) = 2 then  // 6 Jan 2006 if drive change now
                begin
                    HostDirName := remdir ;
                    ret := Cwd ;     // change to another drive
                    if ret then
                    begin
                        ret := Pwd ;  // get initial working directory on new drive
                    end ;
                end ;

            // keep PWD response as root
                fServRootDir := IcsPathDosToUnix(DirResult) ; // 6 Jan 2006 change to Unix, 11 Sept 2008 was not changing directory!!
                fServRootDir := IncludeTrailingUnixDelimiter (fServRootDir) ;
                doCopyEvent (LogLevelInfo, 'Server Start-up Directory: ' + fServRootDir) ;

        // set base directory, combining root and remote dir - warning, might not exist
                if Pos (':', remdir) = 2 then
                    fServBaseDir := fServRootDir   // 6 Jan 2006 no drive
                else if remdir [1] = '/' then
                       fServBaseDir := fServRootDir + Copy (remdir, 2, 999)
                else
                    fServBaseDir := fServRootDir + remdir ;
                fServBaseDir := IncludeTrailingUnixDelimiter (fServBaseDir) ;
                fCurRemDir := fServBaseDir ;
                doCopyEvent (LogLevelInfo, 'Xfer Base Directory: ' + fCurRemDir) ;
            end ;
         // check special server features
            if ret and (NOT (magftpNoFeat in fMagFtpOpts)) then  // 7 Nov 2007 made it optional
            begin
                Feat ;
                doCopyEvent (LogLevelInfo, Trim (LastMultiResponse)) ;
                if (magftpNoMd5Crc in fMagFtpOpts) then FSupportedExtensions :=
                                     FSupportedExtensions - [ftpFeatMD5, ftpFeatXMD5, ftpFeatXCRC] ;
                if (magftpNoMd5 in fMagFtpOpts) then FSupportedExtensions :=
                                                 FSupportedExtensions - [ftpFeatMD5, ftpFeatXMD5] ;   // 15 Apr 2009
                if (magftpNoCrc in fMagFtpOpts) then FSupportedExtensions :=
                                                 FSupportedExtensions - [ftpFeatXCRC] ;   // 15 Apr 2009
                if (magftpNoZlib in fMagFtpOpts) then FSupportedExtensions :=
                                                 FSupportedExtensions - [ftpFeatModeZ] ;

             // 8 Nov 2007 tell server who we are - before UTF8 to keep Gene6 happy
                if (ftpFeatClnt in FSupportedExtensions) and (Length (ClientIdStr) > 4) then
                begin
                    Clnt ;
                end ;

            // 15 Sept 2008 Unicode/UTF8 - note some servers ignore ON/OFF and always turn on
                if (ftpFeatUtf8 in FSupportedExtensions) and (NOT (magftpIgnoreUtf8 in fMagFtpOpts)) then
                begin
                    if (magftpNoUtf8 in fMagFtpOpts) then
                    begin
                        fCodePage := CP_ACP;
                        fNewOpts := 'UTF8 OFF' ;
                    end
                    else
                    begin
                        fCodePage := CP_UTF8 ;
                        fNewOpts := 'UTF8 ON' ;
                    end;
                    Opts ;
                    doCopyEvent (LogLevelInfo, 'UTF8 Opts: ' + LastResponse) ;
                end ;
            end ;

         // 8 Nov 2007 find time zone unless using MLSD
            if (ftpFeatSiteZone in FSupportedExtensions) and (NOT (ftpFeatMLST in FSupportedExtensions)) then
            begin
                if SiteZone then fTimeZoneStr := Trim (LastResponse) ;
            end ;
        end ;
        if ret then
        begin
            result := TaskResOKNew ;
            fLoggedIn := true ;
            doCopyEvent (LogLevelInfo, 'Succesfully logged onto FTP Server: ' + fUserName) ;
         // set transfer mode
            if fXferMode = XferModeBinary then TypeBinary ;  // ftp command
            if fXferMode = XferModeAscii then TypeAscii ;    // ftp command

         // setup Mode Z, if available
            if fUseCompression and (ftpFeatModeZ in FSupportedExtensions) then
            begin
                TransferMode := ftpTransModeStream ;  // 23 Nov 2007 start no compression
                if fZlibMaxSize > 10000 then  // 9 Dec 2007 skip mode z unless more than 10K
                begin
                    if ModeZ then
                    begin
                        fUsingCompression := true ;
                        fNewOpts := 'MODE Z LEVEL 8';    // 15 Sept 2008 ensure correct command sent
                        Opts ;
                    end ;
                end ;
            end ;

            if fSslType <> sslTypeNone then
            begin
                Pbsz ; // ftp command, protection buffer size, always zero 18 Sept 2008 before PROT to keep MS FTP7 happy
                if fFtpType in [FtpTypeConnSslBoth, FtpTypeAuthSslBoth, FtpTypeAuthSslData, FtpTypeConnSslData] then
                begin
                    fProtLevel := 'P' ; // protect data with SSL
                    fPassive := true ;  // must be after connection type
                end
                else
                    fProtLevel := 'C' ; // clear data
                if NOT Prot then  // ftp command, set data channel protection Private or Clear
                begin
                    if fProtLevel = 'P' then
                        fProtLevel := 'C'
                    else
                    begin
                        fProtLevel := 'P' ; // protect data with SSL
                        fPassive := true ;  // must be after connection type
                    end ;
                    Prot ;   // second attempt
                end ;
                if fFtpType in [FtpTypeAuthSslData, FtpTypeConnSslData] then
                    CCC ;  // ftp command, clear control channel
            end ;
            exit ;
        end ;
        fReqResponse := 'Request Failed: ' + ErrorMessage ;
        doCopyEvent (LogLevelInfo, 'Failed to Connect to FTP Server: ' + ErrorMessage) ;
        if Connected then Abort ;
        fLoggedIn := false ;
     // few seconds wait for next attempt
        if fCancelFlag then exit ;
        if attemptnr < fMaxAttempts then
        begin
            doCopyEvent (LogLevelInfo, 'Waiting for ' + IntToStr (fAttemptDelay) + ' secs, then Retrying') ;
            sysDelayX (DWORD (fAttemptDelay) * 1000) ;
        end ;
    end ;
    result := TaskResFail ;
end;

// build file list for remote file using FTP, optionally including sub directories

function TIcsFtpMulti.FtpDir (var FtpFiles: TIcsFDirRecs ; var FtpFileList: TIcsFindList;
                                        const ListDirs: boolean = false): TIcsTaskResult ;  // 17 Feb 2011
var
    fullpath, fname: string ;
    ret, dirsflag, hostnameflag, chkdirflag: boolean ;
    lastdirrec, level, I, totfiles: integer ;
    DirStream: TStream ;
    fstarttick: Int64 ;   { V8.71 }
begin
    fCancelFlag := false ;
    result := TaskResAbort ;
    if NOT fLoggedIn then
    begin
        result := TaskResFail ;
        fReqResponse := 'Not Logged-In Yet' ;
        exit ;
    end ;
    IcsCopyProgClearCur (fCopyProg) ;  // 22 May 2013 clear current
    fullpath := fServBaseDir ;  // base directory
    SetLength (FtpFiles, 0) ;
    totfiles := 0 ;
    FtpFileList.Clear ;
    HostFileName := '' ;  // no argument after LIST/MLST
    hostnameflag := ftpFeatMLST in FSupportedExtensions ;  // try and use HostFileName
    chkdirflag := hostnameflag ;  // one off check
    DirStream := TMemoryStream.Create ;
    LocalFileName := '' ;
    LocalStream := DirStream ; // 26 Oct 2003 - get dirs into stream instead of file
    if ftpFeatMLST in Self.FSupportedExtensions then fHostType := FTPTYPE_MLSD ;
    ResumeAt := 0 ; // 4 July 2007 ensure resume position reset
    fProgFileSize := -1 ;   // so it shows up as a directory without a total size
    fCopyProg.ProgMessBase := 'Getting File Directories from FTP Server, chars ' ;
    if fUsingCompression then
    begin
        if ((ftpFeatSiteDmlsd in FSupportedExtensions) or   // 22 Nov 2007
                            (ftpFeatXDmlsd in FSupportedExtensions)) and fSubDirs then    // 10 Nov 2008
            TransferMode := ftpTransModeZDeflate
        else
            TransferMode := ftpTransModeStream ; // 22 Nov 2007 don't compress single dirs
        if (FCurrTransMode <> TransferMode) then
            ModeZ ;
    end ;
    HostDirName := fullpath ;
    ret := Cwd ;     // change to working directory in HostDirName
    if ret then
        ret := Pwd ;  // print working directory to DirResult
    if ret then
        fCurRemDir := HostDirName ;
    if fCancelFlag then
        ret := false ;
    if NOT Connected then
        ret := false ;
    if NOT ret then
        fCurRemDir := 'xXx' ;  // 15 Sept 2008 invalidate current remote directory if not found so it's created
    dirsflag := ret ;
    level := 0 ;
    lastdirrec := -1 ;  // 21 Feb 2011 not found one yet
    fstarttick := IcsGetTickCount64 ;
    while dirsflag do
    begin
        TMemoryStream (DirStream).Clear ;
        MessagePump ; // 15 Sept 2010 needed to support MultiThreaded
        if fCancelFlag then
        begin
            doCopyEvent (LogLevelDiag, 'Directory Listing Cancelled Before Next Dir') ;
            ret := false
        end
        else
        begin
            sysDelayX (200) ;  // 11 Aug 2004 - short delay to try and fix corrupted PORT command
         // get FTP directory from remote server with HostFileName argument
            if (ftpFeatXDmlsd in FSupportedExtensions) and fSubDirs then        // 10 Nov 2008
            begin
                HostFileName := '-R'; // recursive sub-directories
                ret := XDMlsd ;
            end
            else if (ftpFeatSiteDmlsd in FSupportedExtensions) and fSubDirs then        // 22 Nov 2007
            begin
                HostFileName := '-R'; // recursive sub-directories
                ret := SiteDMlsd ;
            end
            else if (ftpFeatXCmlsd in FSupportedExtensions) and (NOT fSubDirs) then  // 10 Nov 2008
            begin
                ret := XCMlsd ;
            end
            else if (ftpFeatSiteCmlsd in FSupportedExtensions) and (NOT fSubDirs) then  // 25 Nov 2007
            begin
                ret := SiteCMlsd ;
            end
            else if ftpFeatMLST in FSupportedExtensions then    // 5 Oct 2003
                ret := Mlsd
            else
                ret := Dir ;

         // 11 Aug 2004 - special handling for PORT command being truncated, repeat it
         // 500 'RT': command not understood.
            if (NOT ret) and (StatusCode = 500) and (Pos ('RT', ErrorMessage) > 1) then
            begin
                sysDelayX (500) ;
                doCopyEvent (LogLevelDiag, 'Repeating Listing Command, Corrupted PORT Bug') ;
                if ftpFeatMLST in FSupportedExtensions then
                    ret := Mlsd
                else
                    ret := Dir ;
            end ;
        end ;
        if fCancelFlag then
        begin
            doCopyEvent (LogLevelDiag, 'Directory Listing Cancelled During Dir') ;
            ret := false
        end ;
        if NOT Connected then
        begin
            doCopyEvent (LogLevelDiag, 'Directory Listing Abandoned, Not Connected') ;
            ret := false
        end ;
        dirsflag := ret ;
        if dirsflag then
        begin
            dirsflag := false ;  // assume no more subdirectories
            UnpackFtpFDir (LocalStream, fullpath, fServBaseDir, level, fCodePage, fHostType, totfiles, FtpFiles) ;

        // search for a subdirectory, add it's files (and subdirs) to list
            if (totfiles > lastdirrec) and fSubDirs and (NOT ((ftpFeatSiteDmlsd in FSupportedExtensions) or   // 22 Nov 2007
                                                                 (ftpFeatXDmlsd in FSupportedExtensions))) then    // 10 Nov 2008
            begin
                for I := (lastdirrec + 1) to Pred (totfiles) do  // 21 Feb 2011 don't process last directory again
                begin
                    with FtpFiles [I] do
                    begin
                      //  if (FrExtra = sDirLit) and (FrFileCopy = FCStateDir) then  // 21 Feb 2011
                        if ((FrFileAttr and faDirectory) = faDirectory) then  // 21 Feb 2011
                        begin
                         // 7 Nov 2007 check that MLSD HostFileName has not returned it's own directory
                         // Serv-U 4.6.4 bug, also ICS if / missing
                            if chkdirflag and (Length (HostFileName) > 0) then
                            begin
                                fname := FtpFiles [Pred (totfiles)].FrFileName ;
                                if Pos (fname, HostFileName) > 0 then // error, directory returned as filename
                                begin
                                    doCopyEvent (LogLevelDiag, '!!! TEMP, Last HFN ' + HostFileName + ', FName ' + fname) ;
                                    HostFileName := FtpFiles [Pred (totfiles)].FrFullName + '/' ; // see if a real duplicate directory
                                    ret := MLST ;  // FTP command - file facts for HostFileName
                                    if NOT ret then
                                    begin
                                        doCopyEvent (LogLevelDiag, 'Warning - MLSD has Not Correctly Listed Directory, Reverting to Current Mode') ;
                                        hostnameflag := false ;
                                        chkdirflag := false ;  // only once
                                        totfiles := totfiles - 1 ;  // remove directory from listings
                                        HostDirName := HostFileName ;
                                        HostFileName := '' ;
                                        ret := Cwd ;   // change to new directory in HostDirName
                                        if ret then ret := Pwd ;
                                        if ret then fCurRemDir := HostDirName ;
                                        if ret then dirsflag := true ; // more to do, 22 May 2009 unless error
                                        break ;
                                    end ;
                                end
                                else
                                    chkdirflag := false ;  // only once
                            end ;

                        // 14 Sept 2008 skip Unicode names with substitution characters
                            if Pos ('?', FrFullName) > 0 then
                            begin
                                doCopyEvent (LogLevelDiag, 'Skipped Inaccessible Unicode Name: ' + FrFullName) ;
                                continue ;
                            end ;

                        // find next directory to process
                            fullpath := FrFullName + '/' ;   // this is passed to parser
                            lastdirrec := I ;
                            level := succ (FrDirLevel) ;
                            if NOT ListDirs then  // 21 Feb 2011 keep dirs in list
                                          FrFileCopy := FCStateIgnore ;  // stop it being processed again

                         // using HostFileName to avoid changing dirs
                            if hostnameflag then
                            begin
                                HostDirName := '' ;
                                if fCurRemDir <> fServBaseDir then
                                begin
                                    HostDirName := fServBaseDir ;
                                    ret := Cwd ;   // back to base directory in HostDirName
                                    if ret then ret := Pwd ;
                                    if ret then fCurRemDir := HostDirName ;
                                    if ret then dirsflag := true ; // more to do, 22 May 2009 unless error
                                end
                                else
                                    dirsflag := true ; // more to do, 11 Aug 2011
                              // 10 Aug 2004, path passed to LIST/MLST
                                HostFileName := fullpath ;
                            end ;
                            if NOT hostnameflag then  // 7 Nov 2007 not using host names
                            begin
                                HostFileName := '' ;
                                HostDirName := fullpath ;
                                ret := Cwd ;   // change to new directory in HostDirName
                                if ret then ret := Pwd ;
                                if ret then fCurRemDir := HostDirName ;
                                if ret then dirsflag := true ; // more to do, 22 May 2009 unless error
                            end ;
                            break ;
                        end ;
                    end ;
                end ;
            end ;
        end
        else
        begin
            fReqResponse := 'Request Failed: No Directory Listing' ;
            doCopyEvent (LogLevelDiag, 'Directory Listing Abandoned, Failed Response') ;
            dirsflag := false ;   // assume no more subdirectories
            result := TaskResFail ;
        end ;
    end ;
    DirStream.Free ;
    LocalStream := nil;
    fProgFileSize := 0 ;    // no progress counter
    if NOT ret then
    begin
        if fCancelFlag then exit ;
        if NOT Connected then exit ;
        result := TaskResFail ;
        fReqResponse := 'Request Failed: ' + ErrorMessage ;
    // 4 Jan 2008 'Unable to establish data connection - Connection refused' - abort to clean up
        if (StatusCode = 550) and (Pos ('#10061', LastResponse) > 1) then
        begin
            if Connected then AbortXfer ;
        end ;
        exit ;
    end ;
    doCopyEvent (LogLevelInfo, 'Files listed OK, total found ' + IntToStr (totfiles) +
                                        ' took ' + IntToStr (IcsElapsedSecs64 (fstarttick)) + ' secs') ;

// build list from array, omitting directories and rubbish, then sort
    if (totfiles <> 0) then
    begin
        result := TaskResOKNew ;
        FtpFileList.Capacity := totfiles ;
        for I := 0 to Pred (totfiles) do
        begin
            if FtpFiles [I].FrFileCopy <> FCStateIgnore then
                FtpFileList.Add (@FtpFiles [I]) ;
        end ;
        FtpFileList.Sort (IcsCompareFNext) ;
        FtpFileList.Sorted := true ; // 11 June 2008 not sure if really needed
    end
    else
        result := TaskResOKNone ;
end;

function TIcsFtpMulti.DispFtpDir (var dirlisting: string): TIcsTaskResult ;
var
    DirFiles: TIcsFDirRecs  ;
    DirFileList: TIcsFindList ;
begin
    dirlisting := '' ;
    DirFileList := TIcsFindList.Create ;
    try
        result := FtpLogon ;
        if result <> TaskResOKNew then exit ;
        if NOT Connected then exit;  { V8.65 }
        if fCancelFlag then exit ;
        result := FtpDir (DirFiles, DirFileList, fEmptyDirs) ;  // 17  Feb 2011
        if fCancelFlag then exit ;
        if result in [TaskResOKNew, TaskResOKNone] then
                        dirlisting := 'Remote Directory: ' + fSrcDir + IcsCRLF + IcsFmtFileDirList (DirFileList, false) ;
    finally
        if fLoggedIn then Quit ;   // clean log off
        fLoggedIn := false ;
        if Connected then Abort ;
        if (ControlSocket.State <> wsClosed) or (DataSocket.State <> wsClosed) then Abort ;
        DirFileList.Free ;
    end ;
end;

procedure TIcsFtpMulti.doCopyEvent (const LogLevel: TIcsCopyLogLevel; const Info: string) ;
var
    oldflag: boolean ;
begin
    if (LogLevel = LogLevelProg) and fNoProgress then exit ; // 15 Feb 2011
    if Assigned (fCopyEvent) then
    begin
        oldflag := fCancelFlag ;
        fCopyEvent (LogLevel, Info, fCancelFlag) ;
        if oldflag <> fCancelFlag then fCopyEvent (LogLevelDiag, 'FTP Cancelled from Copy Event', oldflag) ;
    end ;
    if Assigned (fProgressEvent) then   // 22 May 2013
    begin
        fCopyProg.LogLevel := LogLevel ;
        fCopyProg.Info := Info ;
        oldflag := fCancelFlag ;
        fProgressEvent (Self, fCopyProg, fCancelFlag) ;
        if oldflag <> fCancelFlag then
        begin
            fCopyProg.LogLevel := LogLevelDiag ;
            fCopyProg.Info := 'FTP Cancelled from Copy Event' ;
            fProgressEvent (Self, fCopyProg, oldflag) ;
        end;
    end;
end ;

procedure MD5Progress (Obj: TObject; Count: Int64 ; var Cancel: Boolean); // callback
begin
    TIcsFtpMulti (Obj).MessagePump ; // 15 Sept 2010 needed to support MultiThreaded
    TIcsFtpMulti (Obj).onFtpClientProg64 (Nil, Count, Cancel) ;
end ;

procedure TIcsFtpMulti.onZlibProg (Sender: TObject; Count: Int64; var Cancel: Boolean); // 9 Dec 2007
begin
    MessagePump ; // 15 Sept 2010 needed to support MultiThreaded
    Cancel := fCancelFlag ;
end ;

procedure TIcsFtpMulti.onMagCopyEvent (LogLevel: TIcsCopyLogLevel ; Info: string ; var Cancel: boolean) ;
begin
    doCopyEvent (LogLevel, Info) ;
    Cancel := fCancelFlag ;
end ;

// called during real downloads and uploads, also directories, MD5sums and CRC32s

procedure TIcsFtpMulti.onFtpClientProg64(Sender: TObject; Count: Int64; var Abort: Boolean);
var
    temp: String ;
begin
    Abort := fCancelFlag ;  // 23 June 2006
    if fNoProgress then
        exit ; // 15 Feb 2011
    if fProgFileSize = 0 then
        exit ;   // noprobably downloading directories
    fCopyProg.CurOKDone := Count ;
    if Sender <> Nil then  // only count real FTP xfers
    begin
        fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + Count ;
    end;
    if fCancelFlag then exit ;
    if (IcsElapsedSecs64 (fCopyProg.LastProgTick) > fProgressSecs) or            { V8.71 }
                    (fCopyProg.LastProgTick = 0) or ((Count > 0) and (Count > fProgFileSize)) then
    begin
        if fProgFileSize < 0 then
            temp := fCopyProg.ProgMessBase + IntToKByte (Count)  // downloading directories
        else
        begin
            if fProgFileSize = 0 then exit ;  // nothing to display yet
            temp := fCopyProg.ProgMessBase + ', ' + IntToKByte (Count) + ' of ' +  IntToKByte (fProgFileSize) ;
        end;
        fCopyProg.LastProgTick := IcsGetTickCount64 ;       { V8.71 }
        if Assigned (fProgressEvent) then   // 20 May 2013
            IcsCopyProgDuration (fCopyProg, AppTicksPerFtp) ;
        doCopyEvent (LogLevelProg, temp) ;
    end;
end;

procedure TIcsFtpMulti.onFtpClientDisplay(Sender: TObject; var Msg: String);
var
    temp: string ;
begin
    temp := Msg ;
    if Length (temp) > 1 then
    begin
        if (Pos ('> PASS', temp) > 0) then
            temp := '> PASS ****'
        else if (temp [1] = '<') then exit ;   // ignore response
    end ;
    doCopyEvent (LogLevelDiag, temp) ;
end;

procedure TIcsFtpMulti.onFtpError(Sender: TObject; var Msg: String);
begin
    fFtpErrFlag := true ;
    doCopyEvent (LogLevelInfo, '!! FTP Error: ' + Msg) ;
    if FState <> ftpAbort then Abort ;  // 28 Dec 2007 don't repeatedly abort
end;

procedure TIcsFtpMulti.OnFtpResponse (Sender: TObject) ;
begin
// 25 Nov 2007 ignore continuation response for FEAT and SITE INDEX/CMLSD
    if Pos ('200-', FLastResponse) = 1 then exit ;
    if Pos ('250-', FLastResponse) = 1 then exit ;
    doCopyEvent (LogLevelDiag, '< ' + FLastResponse) ;
end;

procedure TIcsFtpMulti.OnFtpSessConn (Sender: TObject; Error: word) ;
var
    temp: string ;
begin
    if FState = ftpConnected then
        temp := 'FTP Control Session Connected OK to: '        { V8.71 }
    else
        temp := 'FTP Control SessionConnection failed to: ';
    temp := temp + IcsFmtIpv6AddrPort(AddrResolvedStr, FPort);  { V8.60 }
    if Error <> 0 then temp := temp + ' - ' + GetWinsockErr (Error) ;
    doCopyEvent (LogLevelDiag, temp) ;
end;

procedure TIcsFtpMulti.OnFTPSocksConnected (Sender: TObject; Error: word) ;
var
    temp: string ;
begin
    temp := 'FTP Socks Session Connected' ;
    if Error <> 0 then temp := temp + ' - ' + GetWinsockErr (Error) ;
    doCopyEvent (LogLevelDiag, temp) ;
end;

procedure TIcsFtpMulti.OnFtpSessClosed (Sender: TObject; Error: word) ;
var
    temp: string ;
begin
    temp := 'FTP Control Session Closed' ;         { V8.71 }
    if Error <> 0 then temp := temp + ' - ' + GetWinsockErr (Error) ;
    doCopyEvent (LogLevelDiag, temp) ;
end;

procedure TIcsFtpMulti.OnFtpRequestDone (Sender: TObject; RqType: TFtpRequest; Error: Word) ;
begin
//  error is FTP response, not a real error
end;

procedure TIcsFtpMulti.OnFtpStateChange (Sender: TObject) ;
begin
//
end;


function TIcsFtpMulti.FtpCheckFile (const RemDir, RemFile: string ; var FSize: Int64; var FileUDT: TDateTime): boolean;
var
    DirStream: TStream ;
    FtpFiles: TIcsFDirRecs ;
    ret: boolean ;
    RFname, RFType, RFAttr, facts: String ;
    totfiles, I, laststatus: Integer;
begin
    result := false ;
    try
        FSize := -1 ;
        FileUDT := 0 ;
        HostFileName := '' ;
        LocalFileName := '' ;
        HostDirName := RemDir ;

     // 14 Sept 2008 skip Unicode names with substitution characters
        if (Pos ('?', RemDir) > 0) or (Pos ('?', RemFile) > 0) then
        begin
            doCopyEvent (LogLevelDiag, 'Skipped Inaccessible Unicode Name: ' + RemDir + RemFile) ;
            exit ;
        end ;

        if fCurRemDir <> HostDirName then
        begin
            ret := Cwd ;  // change working directory
            if fCancelFlag then exit ;
            if ret then ret := Pwd ;
            if NOT ret then
            begin
                doCopyEvent (LogLevelInfo, 'Can Not Change to Source Directory: ' + HostDirName) ;
                exit ;
            end ;
            fCurRemDir := HostDirName ;
        end ;
        if NOT Connected then exit ;
        if fCancelFlag then exit ;

     // get latest file size and time stamp
        ResumeAt := 0 ; // 4 July 2007 ensure resume position reset
        laststatus := 550 ;
        if ftpFeatMLST in FSupportedExtensions then
        begin
            HostFileName := RemFile ;
            MLST ;  // FTP command - file facts for HostFileName
            if NOT Connected then exit ;
            if fCancelFlag then exit ;
            laststatus := StatusCode ;
         // 17 Sept 2008 WS_FTP MLST with 501 Invalid number of arguments with spaces, so try quoting file name
            if (laststatus = 501) and (Pos (IcsSpace, RemFile) > 0) then
            begin
                HostFileName := '"' + RemFile + '"' ;
                MLST ;  // FTP command - file facts for HostFileName
                if NOT Connected then exit ;
                if fCancelFlag then exit ;
                laststatus := StatusCode ;
            end ;
            facts := AnsiLowerCase (RemFacts) ; // 3 Sept 2006, Serv-U is mixed case
         //  15 March 2006, check we got some facts - parse may have failed
            if Pos ('size', facts) <= 0 then laststatus := 550 ;
            if laststatus = 250 then
            begin
                DecodeMlsResp64 (facts, RFname, RFType, RFAttr, FSize, FileUDT) ;  // note file name ignored
            end
            else
                doCopyEvent (LogLevelDiag, 'MLST Failed with Status ' + IntToStr (StatusCode) + ' - ' + LastResponse) ;
        end ;

    // 15 March 2006, use MDTM if MLST fails
        if (ftpFeatMDTM in FSupportedExtensions) and (laststatus <> 250) then
        begin
            HostFileName := RemFile ;
            MDTM ;  // FTP command - returns UTC time
            if NOT Connected then exit ;
            if fCancelFlag then exit ;
            if StatusCode = 213 then
            begin
                FileUDT := RemFileDT ;
                Size ;   // FTP command
                if fCancelFlag then exit ;
                if StatusCode = 213 then
                begin
                    FSize := SizeResult ;
                    laststatus := 250 ;  // OK
                end ;
            end
            else
                doCopyEvent (LogLevelDiag, 'MDTM Failed with Status ' + IntToStr (StatusCode) + ' - ' + LastResponse) ;
        end ;

    // 15 March 2006, use LIST if MDTM fails
        if (laststatus <> 250) then
        begin
          // IIS/5 W2K can not LIST directories or files with spaces so need to get whole directory
            HostFileName := RemFile ;
            if Pos (IcsSpace, RemFile) <> 0 then HostFileName := '' ;
            DirStream := TMemoryStream.Create ;
            LocalFileName := '' ;
            LocalStream := DirStream ;
            SetLength (FtpFiles, 0) ;
            try
                totfiles := 0 ;
                sysDelayX (200) ;
                ret := Dir ;    // lists specific file in HostFileName
                if NOT Connected then exit ;
                if fCancelFlag then exit ;
             // 500 'RT': command not understood.
                if (NOT ret) and (StatusCode = 500) and (Pos ('RT', ErrorMessage) > 1) then
                begin
                    sysDelayX (500) ;
                    doCopyEvent (LogLevelDiag, 'Repeating Listing Command, Corrupted PORT Bug') ;
                    ret := Dir ;
                end ;
                if NOT Connected then exit ;
                if fCancelFlag then exit ;
                if ret then
                begin
                    UnpackFtpFDir (LocalStream, HostDirName, fServBaseDir, 0, FCodePage, fHostType, totfiles, FtpFiles) ;
                    if (totfiles >= 1) then  // should only be single file, but may have listed more
                    begin
                        RFname := AnsiLowerCase (RemFile) ;
                        for I := 0 to Pred (totfiles) do
                        begin
                            if AnsiLowerCase (FtpFiles [I].FrFileName) = RFname then
                            begin
                                FSize := FtpFiles [I].FrFileBytes ;
                                FileUDT := DateTimeToUTC (FtpFiles [I].FrFileDT) ;
                                break ;
                            end ;
                        end ;
                    end ;
                end
                else
                begin
                    doCopyEvent (LogLevelDiag, 'DIR Failed with Status ' + IntToStr (StatusCode) + ' - ' + LastResponse) ;
            // 4 Jan 2008 'Unable to establish data connection - Connection refused' - abort to clean up
                    if (StatusCode = 550) and (Pos ('#10061', LastResponse) > 1) then
                    begin
                        if Connected then AbortXfer ;
                    end ;
                end ;
            finally
                SetLength (FtpFiles, 0) ;
                if Assigned (DirStream) then DirStream.Free ;
                LocalStream := nil;
            end ;
        end ;
        if FSize > 0 then result := true ;
    finally
        if fCancelFlag then
        begin
            result := false ;
            fReqResponse := 'Cancelled Checking File' ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
        end ;
    end ;
end ;

procedure TIcsFtpMulti.NoopTimerTimer(Sender: TObject);       { V8.71 }
begin
    FNoopTimer.Enabled := False;
    if fCancelFlag then exit ;
    if NOT fLoggedIn then exit ;
    if NOT Connected then exit ;
    doCopyEvent (LogLevelDiag, 'Sending NOOP Keep-Alive') ;
    NoopAsync;                      { effectively keep-alive }
    FNoopTimer.Enabled := True;
end;

// actual FTP download a file, resuming if necessary

function TIcsFtpMulti.IntDownOne (const RemDir, RemFile, RemFull, LocFileFull: string; const RFSize: Int64; RFileUDT: TDateTime): integer ;
var
    fnametmp, newtardir, fnameftp, info, fileext: string ;
    Ret, resflag: boolean ;
    duration: longword ;
    newsize, partfsize, lastbytes, actualbytes: Int64 ;
    retval, attempts: Integer;
    ResInfRecs: TStringList ;
begin
    Result := 1 ;  // fail
    if fCancelFlag then exit ;
    if NOT fLoggedIn then exit ;
    if NOT Connected then exit ;
    IcsCopyProgClearCur (fCopyProg) ;  // 22 May 2013 clear current about to start a file
    FNoopTimer.Enabled := False;                  { V8.71 }

// 14 Sept 2008 skip Unicode names with substitution characters
    if (Pos ('?', LocFileFull) > 0) then
    begin
        doCopyEvent (LogLevelDiag, 'Skipped Inaccessible Unicode Name: ' + LocFileFull) ;
        doCopyEvent (LogLevelDelimFile, RemFile + '|' + LocFileFull + '|0|0|1|Inaccessible Unicode File|0|0') ;
        inc (fCopyProg.ProcFailFiles) ;
        fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + RFSize ;
        result := 6 ;
        exit ;
    end ;
    if (Pos ('?', RemFull) > 0) then
    begin
        doCopyEvent (LogLevelDiag, 'Skipped Inaccessible Unicode Name: ' + RemFull) ;
        doCopyEvent (LogLevelDelimFile, RemFile + '|' + LocFileFull + '|0|0|1|Inaccessible Unicode File|0|0') ;
        inc (fCopyProg.ProcFailFiles) ;
        fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + RFSize ;
        result := 6 ;
        exit ;
    end ;

// create target directory
    newtardir := ExtractFileDir (LocFileFull) ;
    if NOT IcsForceDirsEx (newtardir) then
    begin
        doCopyEvent (LogLevelInfo, 'Can Not Create Directory: ' + newtardir) ;
        inc (fCopyProg.ProcFailFiles) ;
        fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + RFSize ;
        exit ;
    end ;
    ResInfRecs := TStringList.Create ;
    try // finally

    // try and delete existing file before copying, removing read only if necessary
        retval := IcsDeleteFile (LocFileFull, fReplRO) ;
        if retval > 0 then
        begin
            if retval = 1 then
            begin
                doCopyEvent (LogLevelInfo, 'Can Not Replace Read Only Target File: ' + LocFileFull) ;
                doCopyEvent (LogLevelDelimFile, RemFile + '|' + LocFileFull + '|0|0|1|Can Not Replace Read Only Target File|0|0') ;
            end
            else
            begin
                doCopyEvent (LogLevelInfo, 'Delete Target File Failed: ' + LocFileFull + ' - ' + SysErrorMessage (GetLastError)) ;
                doCopyEvent (LogLevelDelimFile, RemFile + '|' + LocFileFull + '|0|0|1|Delete Target File Failed|0|0') ;
            end ;
            inc (fCopyProg.ProcFailFiles) ;
            fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + RFSize ;
            exit ;
       end ;

    // 22 May 2013 prepare current file progress info
        fCopyProg.CurSrcName := RemFull ;
        fCopyProg.CurTarName := LocFileFull ;
        fCopyProg.CurFileBytes := RFSize ;
        fCopyProg.CurFileStartPos := 0;   { V8.71 }
        fCopyProg.ProcBytesLast := fCopyProg.ProcBytesDone ;

    // tell user what we are doing
        doCopyEvent (LogLevelFile, 'Downloading File: ' + RemFull + ' to ' + LocFileFull  + ', size ' + IntToKByte (RFSize, true)) ;
        fCopyProg.ProgMessBase := fCopyProg.ProgMessBase + IcsCRLF + RemFull ;
        fProgFileSize := RFSize ;   // keep name and size for event handler

    // set compression mode, don't compress certain files
        if fUsingCompression then
        begin
            fileext := ExtractFileExt(AnsiLowercase (LocFileFull));  // 2 Dec 2007 longer list
            if (Pos (fileext, fZlibNoCompExt) > 0) OR (fZlibMaxSize < RFSize) then  // 9 Dec 2007 max size to compress
                TransferMode := ftpTransModeStream
            else
                TransferMode := ftpTransModeZDeflate ;
            if (FCurrTransMode <> TransferMode) then ModeZ ;
        end ;

    // set remote directory, should be OK already since just checked remote file
        HostFileName := '' ;
        LocalFileName := '' ;
        HostDirName := RemDir ;
        ResumeAt := 0 ; // 4 July 2007 ensure resume position reset
        if fCurRemDir <> HostDirName then
        begin
            ret := Cwd ;  // change working directory
            if ret then ret := Pwd ;
            if NOT ret then
            begin
                doCopyEvent (LogLevelInfo, 'Can Not Change to Source Directory: ' + HostDirName) ;
                inc (fCopyProg.ProcFailFiles) ;
                fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + RFSize ;
                exit ;
            end ;
            fCurRemDir := HostDirName ;
        end ;
        if NOT Connected then exit ;
        if fCancelFlag then exit ;

    // V8.71 see if using NOOP keep-alive to try and stop control channel being closed as idle on long transfers
        if (FNoopIntervalMins > 0) and (RFSize > 10*1000*1000) then begin  // min 10 megs
            FNoopTimer.Interval := FNoopIntervalMins * Ticks64PerMinute;
            FNoopTimer.Enabled := True;
        end;

    // see if need to resume previous download
        HostFileName := RemFile ;  // FTP component remote file to download
        if (magftpNoTmpFile in fMagFtpOpts) then  // 6 Jan 2008 - download with correct name
        begin
            fnametmp := LocFileFull ;
            fResFailed := false ;
        end
        else
        begin
            fnametmp := IcsTransChar(AnsiLowerCase (RemFile), '.', '_') ; // don't mess with directories
            fnametmp := IncludeTrailingPathDelimiter (AnsiLowerCase (newtardir)) + fnametmp ;
            fnameftp := fnametmp + '.ftp' ;    // control file
            fnametmp := fnametmp + '.tmp' ;    // destination file name for FTP
        end ;
        if fResFailed then
        begin
            resflag := false ;
            partfsize := 0 ;
            attempts := 0 ;

       // see if resume info file exists, check file on server has same info
            if FileExists (fnameftp) then
            begin
                try
                    ResInfRecs.LoadFromFile (fnameftp) ;  // resume info text file
                    partfsize := IcsGetFileSize (fnametmp) ;
                    if partfsize > 0 then
                    begin
                        if IcsFileInUse (fnametmp) then  // 31 Dec 2007 ensure file not open
                        begin
                            IcsDeleteFile (fnameftp, true) ;
                            partfsize := 0 ;
                            doCopyEvent (LogLevelFile, 'Error Temp File in Use - ' + fnametmp) ;
                            fnametmp := fnametmp + '2' ; // new name
                        end ;
                    end ;
                    if (partfsize >= (fMinResSize + 100)) and (ResInfRecs.Count > ResInfLastBytes) then
                    begin
                        attempts := atoi (ResInfRecs [ResInfAttempts]) ;
                        lastbytes := atoi64 (ResInfRecs [ResInfLastBytes]) ;
                        if (attempts >= fMaxResumeAttempts) then        // 31 Dec 2007
                            doCopyEvent (LogLevelFile, 'Skipped Resume, Too Many Attempts (' + IntToStr (fMaxResumeAttempts) + ')')
                        else if (lastbytes = partfsize) then
                            doCopyEvent (LogLevelFile, 'Skipped Resume, Same Size as Last Attempt')
                        else if (RFSize < partfsize) then
                            doCopyEvent (LogLevelFile, 'Skipped Resume, Old File too Large') // 9 Dec 2007
                        else
                        begin
                            if (ResInfRecs [ResInfServer] = fHostName1) and (ResInfRecs [ResInfFName] = RemFull) and
                               (ResInfRecs [ResInfStamp] = FloatToStr (RFileUDT)) and (ResInfRecs [ResInfSize] = IntToStr (RFSize)) then
                            begin
                            // reduce file size in case content is corrupted near end
                                newsize := IcsTruncateFile (fnametmp, partfsize - fMinResSize) ;  // 24 June 2006
                                partfsize := IcsGetFileSize (fnametmp) ;
                                if newsize <> partfsize then
                                    doCopyEvent (LogLevelFile, 'Failed to Reduce Resume File Size for Overlap')
                                else
                                begin
                                    resflag := true ;
                                    doCopyEvent (LogLevelFile, 'Resuming Partial File Download from: ' +
                                                           IcsInt64ToCStr (partfsize) + ', with Overlap ' + IcsIntToCStr (fMinResSize)) ;
                                end ;
                            end
                            else
                                doCopyEvent (LogLevelFile, 'Unable to Resume, Download File Has Changed') ;
                        end ;
                    end
                    else
                        doCopyEvent (LogLevelFile, 'Skipped Resume, Old File Too Small ' + // 19 July 2007
                                                             IcsInt64ToCStr (partfsize) + ', or Invalid Resume Info') ;
                except
                end ;
            end ;
            ShareMode := ftpShareExclusive ;
            LocalFileName := fnametmp ;        // FTP component destination file
            if NOT Connected then exit ;
            if fCancelFlag then exit ;

    // see if handling resumed transfer
            if resflag then
            begin
                inc (attempts) ;
                ResInfRecs [ResInfAttempts] := IntToStr (attempts) ;
                ResInfRecs [ResInfLastBytes] := IntToStr (partfsize) ;
                ResInfRecs.SaveToFile (fnameftp) ;
                doCopyEvent (LogLevelFile, 'Saved File Resume Info ' + fnameftp) ;
                fCopyProg.CurFileStartPos := partfsize;                           { V8.71 }
                fCopyProg.TotProcBytes := fCopyProg.TotProcBytes - partfsize;     { V8.71 less to transfer }
                fCopyProg.CurStartTick := IcsGetTickCount64 ;                     { V8.71 }
                ret := RestGet ;   // resume download into LocalFileName
                if (NOT (ret OR fCancelFlag)) and (StatusCode = 500) and (Pos ('RT', ErrorMessage) > 1) then
                begin
                    doCopyEvent (LogLevelDiag, 'Repeating Command, Corruption: ' + ErrorMessage) ;
                    sysDelayX (500) ;
                    Ret := RestGet ;
                end ;
                duration := IcsElapsedTicks64 (fCopyProg.CurStartTick) ;   { V8.71 }
            end
            else
            begin
             // write current file into into .ftp file so we can resume if necessary
                IcsDeleteFile (fnametmp, true) ;
                IcsDeleteFile (fnameftp, true) ;
                ResInfRecs.Clear ;
                ResInfRecs.Add (fHostName1) ;               // ResInfServer = 0
                ResInfRecs.Add (String (StringToUtf8 (RemFull))) ;   // ResInfFName = 1
                ResInfRecs.Add (FloatToStr (RFileUDT)) ;    // ResInfStamp = 2
                ResInfRecs.Add (IntToStr (RFSize)) ;        // ResInfSize = 3
                ResInfRecs.Add ('1') ;                      // ResInfAttempts = 4
                ResInfRecs.Add ('0') ;                      // ResInfLastBytes = 5
                ResInfRecs.SaveToFile (fnameftp) ;
                doCopyEvent (LogLevelFile, 'Saved File Resume Info ' + fnameftp) ;
                fCopyProg.CurStartTick := IcsGetTickCount64 ;       { V8.71 }
                Ret := Get ;  // download it
                if (NOT (Ret OR fCancelFlag)) and (FStatusCode = 500) and (Pos ('RT', ErrorMessage) > 1) then
                begin
                    doCopyEvent (LogLevelDiag, 'Repeating Command, Corruption: ' + ErrorMessage) ;
                    sysDelayX (500) ;
                    ret := Get ;
                end ;
                duration := IcsElapsedTicks64 (fCopyProg.CurStartTick) ;   { V8.71 }
            end ;
        end
        else
        begin
            LocalFileName := fnametmp ;
            ShareMode := ftpShareExclusive ;
            IcsDeleteFile (fnametmp, true) ;
            IcsDeleteFile (fnameftp, true) ;
            if fCancelFlag then exit ;
            fCopyProg.CurStartTick := IcsGetTickCount64 ;     { V8.71 }
            Ret := Get ;  // download it
            if (NOT (Ret OR fCancelFlag)) and (StatusCode = 500) and (Pos ('RT', ErrorMessage) > 1) then
            begin
                doCopyEvent (LogLevelDiag, 'Repeating Get Command, Corrupted PORT Bug') ;
                sysDelayX (500) ;
                fCopyProg.CurStartTick := IcsGetTickCount64 ;    { V8.71 }
                Ret := Get ;
            end ;
            duration := IcsElapsedTicks64 (fCopyProg.CurStartTick) ;    { V8.71 }
        end ;
        if duration = 0 then duration := 10 ;  // special case of copy OK but duration too short to measure
        result := 2 ;  // failed after GET
        actualbytes := ByteCount ;  // 2 Jan 2007 keep FTP count before corrupted by LIST
        IcsCopyProgClearCur (fCopyProg) ;  // 22 May 2013 clear current done file
        IcsCopyProgDuration (fCopyProg, AppTicksPerFtp) ;

    // V8.71 ignore error if new file correct size and timeout, control session probably dead so no 226 response recvd
    // but timeout will have aborted control channel so no more files
        FNoopTimer.Enabled := False;   { V8.71 }
        newsize := IcsGetFileSize (fnametmp) ;
        info := 'Downloaded File ' + fnametmp + ', size ' + IntToKByte (newsize, true) ;
        if FErrorMessage <> '' then
            info := info + ' - ' + FErrorMessage ;  { V8.71 aded error }
        doCopyEvent (LogLevelDiag, info) ;
        if (NOT Ret) and (FStatusCode = 426) and (newsize = RFsize) then
        begin
            Ret := True;
            doCopyEvent (LogLevelDiag, '!! Ignored Download Timeout Error, File Appears Correct Size') ;
        end;

    // FTP download failed, give up
        if NOT Ret then
        begin
            if fResFailed then
            begin
                if (newsize <= fMinResSize) then  // failed, kill restart if too small
                begin
                    IcsDeleteFile (fnameftp, true) ;  // kill restart info
                    IcsDeleteFile (fnametmp, true) ;
                    doCopyEvent (LogLevelFile, 'Request Failed: Partial File Deleted, ' +   // 19 July 2007
                                                         'Too Small to Resume ' + IntToKByte (newsize, true)) ;
                end
                else if (newsize >= RFsize) then  // failed, kill restart if too big or correct size
                begin
                    IcsDeleteFile (fnameftp, true) ;  // kill restart info
                    IcsDeleteFile (fnametmp, true) ;
                    doCopyEvent (LogLevelFile, 'Request Failed: File Deleted, ' +   // 9 Dec 2007
                           'Too Large to Resume, Expected File Size ' + IntToKByte (RFSize) + ', Actual Size ' + IntToKByte (newsize))
                end
                else
                    doCopyEvent (LogLevelFile, 'Request Failed: Partial File Downloaded, ' +
                             'Resume Allowed, Expected File Size ' + IntToKByte (RFSize) + ', Actual Size ' + IntToKByte (newsize)) ;
            end
            else
                IcsDeleteFile (fnametmp, true) ;
            doCopyEvent (LogLevelFile, 'Download Failed: ' + LastResponse) ;
            doCopyEvent (LogLevelDelimFile, RemFull + '|' + LocFileFull +
                         '|0|0|1|Download Failed: ' + LastResponse + '|' + IntToStr (duration) + '|' + IntToStr (actualbytes)) ;
        // 4 Jan 2008 'Unable to establish data connection - Connection refused' - abort to clean up
            if (StatusCode = 550) and (Pos ('#10061', LastResponse) > 1) then
            begin
                if Connected then AbortXfer ;
            end ;
            inc (fCopyProg.ProcFailFiles) ;
            fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + RFSize ;
            exit ;
        end ;
    //  if NOT Connected then exit ;         { V8.70 log something, might have worked }
    //  if fCancelFlag then exit ;

    // check size of downloaded file matches server size, will repeat if allowed
        if newsize <> RFSize then
        begin
            if newsize >= 0 then
            begin
                if NOT fResFailed then
                begin
                    IcsDeleteFile (fnametmp, true) ;
                    IcsDeleteFile (fnametmp, true) ;
                    doCopyEvent (LogLevelFile, 'Request Failed: Partial File Deleted, ' +  // 19 July 2007
                                           'Expected File Size ' + IntToKByte (RFSize) + ', Actual Size ' + IntToKByte (newsize)) ;
                end
                else if (newsize >= RFsize) then  // failed, kill restart if too big or correct size
                begin
                    IcsDeleteFile (fnameftp, true) ;  // kill restart info
                    IcsDeleteFile (fnametmp, true) ;
                    doCopyEvent (LogLevelFile, 'Request Failed: File Deleted, ' +   // 9 Dec 2007
                       'Too Large to Resume, Expected File Size ' + IntToKByte (RFSize) + ', Actual Size ' + IntToKByte (newsize)) ;
                end
                else if (actualbytes <= (fMinResSize + 16)) then  // 31 Dec 2007 failed, not enough downloaded on resume
                begin
                    IcsDeleteFile (fnameftp, true) ;  // kill restart info
                    IcsDeleteFile (fnametmp, true) ;
                    doCopyEvent (LogLevelFile, 'Request Failed: File Deleted, ' +   // 9 Dec 2007
                       'Too Little Downloaded to Resume, Expected File Size ' + IntToKByte (RFSize) + ', Actual Size ' + IntToKByte (newsize)) ;
                end
                else
                    doCopyEvent (LogLevelFile, 'Request Failed: Partial File Downloaded, ' +
                           'Resume Allowed, Expected File Size ' + IntToKByte (RFSize) + ', Actual Size ' + IntToKByte (newsize)) ;
            end
            else
            begin
                IcsDeleteFile (fnameftp, true) ;  // kill restart info
                IcsDeleteFile (fnametmp, true) ;
                doCopyEvent (LogLevelFile, 'Request Failed: No File Downloaded') ;
            end ;
            doCopyEvent (LogLevelDelimFile, RemFull + '|' + LocFileFull +
                                   '|0|0|1|Download Failed|' + IntToStr (duration) + '|' + IntToStr (actualbytes)) ;
            inc (fCopyProg.ProcFailFiles) ;
            fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + RFSize ;
            exit ;
        end ;
        IcsDeleteFile (fnameftp, true) ;  // kill restart info, got file OK

     // check MD5 or CRC if possible and repeat if allowed
        if ((ftpFeatMD5 in FSupportedExtensions) OR (ftpFeatXMD5 in FSupportedExtensions)) then
        begin
            doCopyEvent (LogLevelProg, 'Getting Server MD5SUM ' + fnametmp) ;
            fCopyProg.CurStartTick := IcsGetTickCount64 ;        { V8.71 }
            PosStart := 0 ;  // force XMD5 to do entire file, in case size changed
            PosEnd := 0 ;
            if (ftpFeatMD5 in FSupportedExtensions) then  // 22 Nov 2007 support XMD5, 6 Apr 2009 prefer MD5
                ret := MD5
            else
                ret := XMD5 ;  // get MD5SUM
       //   if NOT Connected then exit ;        { V8.70 log something, might have worked }
       //     if fCancelFlag then exit ;
            if ret and (Length (fMD5Result) = 32) then
            begin
                fCopyProg.ProgMessBase := 'Checking Local MD5SUM ' + fnametmp ;
                doCopyEvent (LogLevelProg, fCopyProg.ProgMessBase) ;
                fProgFileSize := newsize ;   // keep name and size for event handler
                info := FtpFileMD5 (fnametmp, Self, MD5Progress) ;  // 8 Apr 2009 widestring version
                if (fMD5Result <> info) { and (Length (info) = 32) } then // 6 Apr 2009 don't assume blank MD5sum is OK
                begin
                    doCopyEvent (LogLevelInfo, 'MD5SUM Compare Failed: ' + fnametmp + ';Rem='+ fMD5Result + ';Loc=' + info) ;
                    doCopyEvent (LogLevelFile, 'Download Failed: MD5SUM Compare Failed') ;
                    doCopyEvent (LogLevelDelimFile, RemFull + '|' + LocFileFull +
                                 '|0|0|1|Download Failed: MD5SUM Compare Failed|' + IntToStr (duration) + '|' + IntToStr (actualbytes)) ;
                    result := 3 ;
                    inc (fCopyProg.ProcFailFiles) ;
                    fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + RFSize ;
                    exit ;
                end ;
                doCopyEvent (LogLevelFile, 'MD5SUM Check OK: ' + fnametmp + ' took ' +    // 20 Sept 2010 was LevelInfo
                                        IntToStr (IcsElapsedSecs64 (fCopyProg.CurStartTick)) + ' secs; Result ' + fMD5Result) ;   { V8.71 }
            end
            else
                doCopyEvent (LogLevelInfo, 'MD5SUM Response Failed: ' + fnametmp + ';Rem='+ fMD5Result) ;   // 6 Apr 2009
        end
        else if (ftpFeatXCrc in FSupportedExtensions) then  // added 10 July 2006
        begin
            doCopyEvent (LogLevelProg, 'Getting Server CRC32 ' + fnametmp) ;
            fCopyProg.CurStartTick := IcsGetTickCount64 ;        { V8.71 }
            PosStart := 0 ;
            PosEnd := 0 ;
            ret := XCRC ;  // get CRC32B
       //   if NOT Connected then exit ;
       //     if fCancelFlag then exit ;
            if ret and (Length (fCrcResult) >= 5) then
            begin
                fCopyProg.ProgMessBase := 'Checking Local CRC32 ' + fnametmp ;
                doCopyEvent (LogLevelProg, fCopyProg.ProgMessBase) ;
                fProgFileSize := newsize ;   // keep name and size for event handler
                info := FtpFileCRC32B (fnametmp, Self, MD5Progress) ;  // 15 Apr 2009
                if (Length (info) = 8) and (Pos (fCrcResult, info) = 0) then
                begin
                    doCopyEvent (LogLevelInfo, 'CRC32 Compare Failed: ' + fnametmp + ';Rem='+ fCrcResult + ';Loc=' + info) ;
                    doCopyEvent (LogLevelFile, 'Download Failed: CRC32 Compare Failed') ;
                    doCopyEvent (LogLevelDelimFile, RemFull + '|' + LocFileFull +
                                 '|0|0|1|Download Failed: CRC32 Compare Failed|' + IntToStr (duration) + '|' + IntToStr (actualbytes)) ;
                    result := 3 ;
                    inc (fCopyProg.ProcFailFiles) ;
                    fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + RFSize ;
                    exit ;
                end ;
                doCopyEvent (LogLevelFile, 'CRC32 Check OK: ' + fnametmp + ' took ' +       // 20 Sept 2010 was LevelInfo
                                    IntToStr (IcsElapsedSecs64 (fCopyProg.CurStartTick)) + ' secs; Result ' + fCrcResult) ;    { V8.71 }
            end ;
        end ;

    // replace old file, removing read only if necessary
        if NOT (magftpNoTmpFile in fMagFtpOpts) then  // 6 Jan 2008 - download with correct name
        begin
            retval := IcsRenameFile (fnametmp, LocFileFull, true, fReplRO) ;
            if retval <> 0 then
            begin
                if (retval = 1) then
                begin
                    doCopyEvent (LogLevelInfo, 'Can Not Replace Read Only File: ' + LocFileFull) ;
                    doCopyEvent (LogLevelDelimFile, RemFull + '|' + LocFileFull + '|0|0|1|Can Not Replace Read Only File|0|0') ;
                end
                else
                begin
                    doCopyEvent (LogLevelInfo, 'Final File Rename Failed: ' + LocFileFull + ' - ' +  SysErrorMessage (retval)) ;
                    doCopyEvent (LogLevelDelimFile, RemFull + '|' + LocFileFull +
                                          '|0|0|1|Final File Rename Failed: ' + SysErrorMessage (retval)+ '|0|0') ;
                end ;
                doCopyEvent (LogLevelInfo, 'File Copied as: ' + LocalFileName) ;
                result := 4 ;
                inc (fCopyProg.ProcFailFiles) ;
                fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + RFSize ;
                exit ;
            end ;
        end ;
    //  if NOT Connected then exit ;     { V8.71 ignore for now }
    //  if fCancelFlag then exit ;       { probably okay so log it all }
        result := 0 ;  // got here so successful

    // set file time stamp,
        if RFileUDT > 10 then
        begin
            doCopyEvent (LogLevelDiag, 'Updating Time Stamp: ' + LocFileFull + ' to (UTC)=' + RFC3339_DateToStr (RFileUDT)) ;
            if NOT UpdateUFileAge (LocFileFull, RFileUDT) then
                doCopyEvent (LogLevelInfo, 'Failed to Update Time Stamp: ' + LocFileFull) ;
        end ;

    // tell user we did it OK
        fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + newsize ;
        doCopyEvent (LogLevelFile, 'Download OK: ' + LocFileFull + ', size: ' + IntToKByte (newsize, true) + ', duration ' +
                                         IcsSecsToStr (duration div 1000) + ', average speed ' +
                                                          IntToKByte (IcsCalcSpeed (duration, actualbytes)) + '/sec') ;
                                                                             // 10 Oct 2011 added duration and speed
        doCopyEvent (LogLevelDelimFile, RemFull + '|' + LocFileFull +
                     '|' + IntToStr (newsize) + '|1|0|OK|' + IntToStr (duration) + '|' + IntToStr (actualbytes)) ;

    finally
        FNoopTimer.Enabled := False;   { V8.71 }
        ResInfRecs.Free ;
        if Assigned (LocalStream) then  // 30 Dec 2007 ensure file never left open
        begin
            LocalStream.Free;
            LocalStream := nil;
        end;
    end ;
end ;

function TIcsFtpMulti.FtpDownOneFile (const FdirSrc, Fnamesrc, Fnametar: string ; Replopt: TIcsFileCopyRepl) : TIcsTaskResult ;
var
    code, fullsrcname, remdir, locfilefull: string ;
    flag: boolean ;
    RFSize, TarFSize: Int64 ;
    TarFileDT: TDateTime;
    retval, loop: Integer;
    RFileUDT: TDateTime ;
    OldWow64: Boolean ;        // 22 May 2013
begin
    result := TaskResFail ;
    fReqResponse := '' ;
    fCancelFlag := false ;
    IcsCopyProgClearAll (fCopyProg) ;  // 22 May 2013 clear all progress stuff
    if NOT fLoggedIn then
    begin
        result := TaskResFail ;
        fReqResponse := 'Must Login to Server' ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
        exit ;
    end ;
    if NOT Connected then
    begin
        result := TaskResFail ;
        fReqResponse := 'No Connection to Server, Cancelled Downloading Files' ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
        exit ;
    end ;

    OldWow64 := false ; // 22 May 2013
    if fWow64RedirDisable then DisableWow64Redir (OldWow64) ; // 22 May 2013
    try  // finally

     // make sure trailing slash for directory
        remdir := IncludeTrailingUnixDelimiter (FdirSrc) ;
        fullsrcname := remdir + fnamesrc ;

    // 14 Sept 2008 skip Unicode names with substitution characters
        if (Pos ('?', fullsrcname) > 0) then
        begin
            doCopyEvent (LogLevelDiag, 'Skipped Inaccessible Unicode Name: ' + fullsrcname) ;
            doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Inaccessible Unicode File|0|0') ;
            exit ;
        end ;
        if (Pos ('?', Fnametar) > 0) then
        begin
            doCopyEvent (LogLevelDiag, 'Skipped Inaccessible Unicode Name: ' + Fnametar) ;
            doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Inaccessible Unicode File|0|0') ;
            exit ;
        end ;

     // add base directory to front of file directory, change to it, if needed
        doCopyEvent (LogLevelFile, 'Check Exists ' + fullsrcname) ;
        if remdir [1] = '/' then
            remdir := fServRootDir + Copy (remdir, 2, 999)
        else
            remdir := fServRootDir + remdir ;

    // FTP download file, multiple attempts
        for loop := 1 to (fFailRepeat + 1) do        // 31 Dec 2007, check file each attempt
        begin
    // check if remote file available, get size and UTC time stamp
            if NOT FtpCheckFile (remdir, fnamesrc, RFSize, RFileUDT) then
            begin
                result := TaskResFail ;
                fReqResponse := 'Can Not Find File: ' + fnamesrc ;
                doCopyEvent (LogLevelInfo, fReqResponse) ;
                exit ;
            end ;
            if NOT Connected then
            begin
                result := TaskResFail ;
                fReqResponse := 'No Connection to Server, Cancelled Downloading Files' ;
                doCopyEvent (LogLevelInfo, fReqResponse) ;
                exit ;
            end ;
            if fCancelFlag then
            begin
                result := TaskResAbort ;
                fReqResponse := 'Cancelled Downloading Files' ;
                doCopyEvent (LogLevelInfo, fReqResponse) ;
                exit ;
            end ;

        // see if replacing existing file
            flag := IcsGetUAgeSizeFile (Fnametar, TarFileDT, TarFSize) ;
            if (NOT flag) and (replopt <> FCReplAlways) then
                              doCopyEvent (LogLevelDiag, 'Download Not Skipped: Target Not Found ' + Fnametar) ;
            if flag and (replopt <> FCReplAlways) then
            begin
                flag := IcsCheckReplace (replopt, true, OneSecondDT * 2, RFSize, TarFSize, RFileUDT, TarFileDT) ;
                if fDispFiles then code := {code + }'; Src=' + DateTimeToStr (RFileUDT) + '; Tar=' + DateTimeToStr (TarFileDT) ;
                if NOT flag then
                begin
                    result := TaskResOKNone ;
                    fReqResponse := 'Download Skipped: ' + fnamesrc ;
                    doCopyEvent (LogLevelInfo, fReqResponse + code) ;
                    doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Download Skipped|0|0') ;
                    exit ;
                end ;
                doCopyEvent (LogLevelDiag, 'Download Not Skipped: ' + fnamesrc + code) ;
            end ;

        // see if making lower case for PC
            if CaseFile = FileLowerCase then
                locfilefull := AnsiLowerCase (Fnametar)
            else
                locfilefull := Fnametar ;

        // 22 May 2013 set session total for single file
            fCopyProg.SessStartTick := IcsGetTickCount64 ;   { V8.71 }
            fCopyProg.TotProcBytes := RFSize ;
            fCopyProg.TotDoneNr := 1 ;
            fCopyProg.TotProcFiles := 1 ;
            fCopyProg.ProcBytesDone := 0 ;

        // now get file
            fCopyProg.ProgMessBase := 'Downloading File' ;
            retval := IntDownOne (remdir, Fnamesrc, fullsrcname, locfilefull, RFSize, RFileUDT) ;
            doCopyEvent (LogLevelProg, '') ;
            if retval = 0 then
            begin
                result := TaskResOKNew ;
                exit ;
            end ;
            if NOT Connected then
            begin
                result := TaskResFail ;
                fReqResponse := 'No Connection to Server, Cancelled Downloading Files' ;
                doCopyEvent (LogLevelInfo, fReqResponse) ;
                exit ;
            end ;
            if fCancelFlag then
            begin
                result := TaskResAbort ;
                fReqResponse := 'Cancelled Downloading Files' ;
                doCopyEvent (LogLevelInfo, fReqResponse) ;
                exit ;
            end ;
            if NOT (retval in [2, 3]) then exit ;  // fail download or fail MD5
         //   if (StatusCode = 501) then exit ;  // 19 Oct 2005 permissions or start failed, 5 Jan 2008 ignore
            if (loop < (fFailRepeat + 1)) then   // 31 Dec 2007
                    doCopyEvent (LogLevelInfo, 'Repeating Download: ' + fnamesrc) ;
        end ;
    finally
        if fWow64RedirDisable then RevertWow64Redir (OldWow64) ; // 22 May 2013
    end;
end ;

// FTP download multiple local files
// returns false if error, with fReqResponse completed

function TIcsFtpMulti.FtpDownload (const CheckFiles: boolean): TIcsTaskResult ;
begin
    fCancelFlag := false ;
// logon to FTP server
    result := FtpLogon ;
    if result <> TaskResOKNew then exit ;

// download multiple files
    try
        result := FtpDownFiles (CheckFiles) ;
    finally
        FtpLogoff ;
        if fCancelFlag then
        begin
            result := TaskResAbort ;
            fReqResponse := 'Cancelled Downloading Files' ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
        end ;
        doCopyEvent (LogLevelProg, '') ;
        fCancelFlag := false ;
    end ;
end ;

// FTP log off and close down - check file not left open

procedure TIcsFtpMulti.FtpLogoff ;
var
    endtick: Int64;      { V8.71 }
begin
    try
        FNoopTimer.Enabled := False;                  { V8.71 }
        if fLoggedIn and Connected then
        begin
            doCopyEvent (LogLevelInfo, 'Logging Off from FTP Server') ;
            Quit ;   // clean log off
        end ;
        fLoggedIn := false ;
        endtick := IcsGetTrgSecs64(5) ;
        while (ControlSocket.State <> wsClosed) or (DataSocket.State <> wsClosed) do
        begin
            if IcsTestTrgTick64(endtick) then
            begin
                doCopyEvent (LogLevelInfo, 'Socket Still Open, Aborting FTP') ;
                Abort ;
                break ;
            end ;
            sysDelayX (50) ;
        end ;
        if Assigned (fExternalSslSessionCache) then fExternalSslSessionCache.Flush;
    except
        doCopyEvent (LogLevelInfo, 'Exception Logging-Off FTP') ;
    end ;
end ;

{$IFDEF Zipping}
procedure TIcsFtpMulti.ZipProgressEvent(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);   { V8.70 }
begin
{    if Assigned (fProgressEvent) then
    begin
        fCopyProg.LogLevel := LogLevelProg ;
        fCopyProg.Info := 'zipping...' ;                    // pending
        fProgressEvent (Self, fCopyProg, fCancelFlag) ;
    end;    }
end;
{$ENDIF}


// FTP download multiple local files
// returns false if error, with fReqResponse completed

function TIcsFtpMulti.FtpDownFiles (const CheckFiles: boolean): TIcsTaskResult ;
var
    newfname, fnametar, cursrcdir, newtardir, fnametmp: string ;
    tempdir, info, newsubdirs, basetardir: string ;
    nodeltot, retval: integer ;
    newsize, totsize, delsize, RFSize: Int64;
    RFileUDT: TDateTime ;
    I, J, loop: integer ;
    duration: longword ;
    SrcFileRec: PTIcsFDirRec ;
    OldWow64: Boolean ;            // 22 May 2013
    listing: TIcsStringBuild ;  // 22 May 2013
{$IFDEF Zipping}
    MyZipFile: TZipFile;         { V8.70 VclZip gone }
    MyZipName, MyDestDir, MyNewFullFile: String;   { V8.70 }
    MyRecreateDirs: Boolean;     { V8.70 }
    MyDateDT: TDateTime;         { V8.70 }
    MyDoneNr: Integer;
{$ENDIF}
    DelDirList: TStringList ;     { V8.70 looking for corruption }
    CopyOnlyList: TStringList ;
begin
    fCancelFlag := false ;
    IcsCopyProgClearAll (fCopyProg) ;  // 22 May 2013 clear all progress stuff
    if NOT fLoggedIn then
    begin
        result := TaskResFail ;
        fReqResponse := 'Must Login to Server' ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
        exit ;
    end ;
    if NOT Connected then
    begin
        result := TaskResFail ;
        fReqResponse := 'No Connection to Server, Cancelled Downloading Files' ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
        exit ;
    end ;
{$IFDEF Zipping}
    MyZipFile := Nil;
    if fZipped then
    begin
        MyZipFile := TZipFile.Create;
        MyZipFile.OnProgress := ZipProgressEvent;
    end ;
{$ENDIF}
    DelDirList := Nil;                               { V8.71 }
    if fDelOldTar then                               { V8.70 }
    begin
        DelDirList := TStringList.Create ;
        DelDirList.Sorted := true ;
        DelDirList.CaseSensitive := false ;
        DelDirList.Duplicates := dupIgnore ;
    end;
    CopyOnlyList := Nil;                             { V8.71 }
    if fSpecificFiles then                           { V8.70 }
    begin
        CopyOnlyList := TStringList.Create ;
        CopyOnlyList.CaseSensitive := false ;
        CopyOnlyList.Duplicates := dupIgnore ;
    end;
    OldWow64 := false ; // 22 May 2013
    if fWow64RedirDisable then DisableWow64Redir (OldWow64) ; // 22 May 2013
    listing := TIcsStringBuild.Create ; // 22 May 2013 make it faster

    try   // finally

        // 8 Apr 2009 - fSrcFName may include directories and masks - yyyy-mm"/D"dd"/*.zip"
        newsubdirs := '' ;
        newfname := fSrcFName ;
        if (NOT fSpecificFiles) and fMask then
        begin
            newfname := IcsPathUnixToDos (newfname) ;
            newfname := IcsGetMaskedName (newfname, fPrev, fLocalHost) ;
            newsubdirs := ExtractFilePath (newfname) ;  // DOS delims
            if Length (newsubdirs) > 0 then
            begin
                if newsubdirs [1] = '\' then newsubdirs := Copy (newsubdirs, 2, 999) ;
            end;
            newfname := ExtractFileName (newfname) ;
        end ;
        if fSpecificFiles then newfname := '*.*' ;

        // set base directory, combining root and remote dir
        if fSrcDir = '' then fSrcDir := '/' ;
        if fSrcDir [1] = '/' then
            fServBaseDir := fServRootDir + Copy (fSrcDir, 2, 999)
        else
            fServBaseDir := fServRootDir + fSrcDir ;
        fServBaseDir := IncludeTrailingUnixDelimiter (fServBaseDir) ;
        if fMaskRemDir and (newsubdirs <> '') then  // 8 Apr 2009 add sub-directories
        begin
            fServBaseDir := IcsPathDosToUnix (fServBaseDir + newsubdirs) ;
            fServBaseDir := IncludeTrailingUnixDelimiter (fServBaseDir) ;
        end;

        // don't delete target files unless processing full directories
        if fDelOldTar then
        begin
            if NOT (fCopyType in [FCTypeArchDir, FCTypeAllDir]) then
            begin
                doCopyEvent (LogLevelInfo, 'Delete Old Target Files only available when using Copy Type All Directory') ;
                fDelOldTar := false ;
            end ;
        end ;

        // build list of source files on FTP server
        doCopyEvent (LogLevelInfo, 'Locating files to Download') ;
        if fCancelFlag then
        begin
            result := TaskResAbort ;
            exit ;
        end ;
        result := FtpDir (SrcFiles, SrcFileList, fEmptyDirs) ;  // 17  Feb 2011, uses fServBaseDir
        if result in [TaskResFail, TaskResAbort] then exit ;
        if fDispRDir then doCopyEvent (LogLevelInfo, 'Source Files on FTP Server:' + IcsCRLF + IcsFmtFileDirList (SrcFileList, false)) ;
        TotSrcFiles := SrcFileList.Count ;
        info := IcsPathDosToUnix(fSrcDir + newsubdirs) ;
        doCopyEvent (LogLevelFile, 'Source Directory: ' + info) ;
        if TotSrcFiles = 0  then
        begin
            result := TaskResOKNone ;
            fReqResponse := 'No Source Files Found' ;
            exit ;
        end ;
        MessagePump ; // 15 Sept 2010 needed to support MultiThreaded
        if fCancelFlag then exit ;

        // build list of target files, so we don't copy unnecessary stuff
        basetardir := ExcludeTrailingPathDelimiter (fTarDir) ;
        if fMaskLocDir and (newsubdirs <> '') then  // 8 Apr 2009 add sub-directories
        begin
            basetardir := IncludeTrailingPathDelimiter (basetardir) + IncludeTrailingPathDelimiter (newsubdirs) ;
        end;
        doCopyEvent (LogLevelFile, 'Target Directory: ' + basetardir) ;
        if NOT IcsForceDirsEx (basetardir) then
        begin
            result := TaskResFail ;
            fReqResponse := 'Can Not Create Target Directory: ' + basetardir ;
            exit ;
        end ;
        if fCancelFlag then exit ;
        fIcsFileCopy.Wow64RedirDisable := fWow64RedirDisable ;  // 22 May 2013
        TotTarFiles := fIcsFileCopy.GetDirList (basetardir, newfname, fCopyType, fSubDirs, 0, 0, TarFiles, TarFileList, fEmptyDirs) ; // 17 Feb 2011) ;
        if fDispLDir then doCopyEvent (LogLevelInfo, 'Target Files on PC:' + IcsCRLF + IcsFmtFileDirList (TarFileList, false)) ;
        MessagePump ; // 15 Sept 2010 needed to support MultiThreaded
        if fCancelFlag then exit ;

        // compare source and target files, see what to download
        fCopyProg.TotProcFiles := fIcsFileCopy.SelectCopyFileList (SrcFileList, TarFileList, newfname, fCopyType,
                         fRepl, fDiffStampMins, true, fIgnoreFileExt, fCopyProg.SkippedFiles, false, fIgnorePaths) ;  // 22 May 2013 added IgnorePaths
        if fCancelFlag then exit ;

        // see if deleting old target files no longer in source directories
        if fDelOldTar and (TotTarFiles <> 0) then
        begin
            fCopyProg.TotDelFiles := fIcsFileCopy.SelectCopyFileList (TarFileList, SrcFileList, '*.*',
                        FCTypeAllDir, FCReplNever, fDiffStampMins, false, fIgnoreFileExt, nodeltot, false, fIgnorePaths) ;  // 22 May 2013 added IgnorePaths   V8.70 diff mins was 0 not fDiffStampMins
        end ;
        if fCopyProg.TotProcFiles = 0  then
        begin
            result := TaskResOKNone ;
            if fCopyProg.SkippedFiles <> 0 then
                fReqResponse := 'All Source Files Skipped Download'
            else
                fReqResponse := 'No Source Files Selected to Download' ;
            exit ;
        end ;

        // see if only copying a list of specific files, deselect any we don't need
        if fSpecificFiles then
        begin
{$IFDEF COMPILER10_UP}   { only supported D2006 and later }
            CopyOnlyList.Delimiter := '|' ;
            CopyOnlyList.StrictDelimiter := True;
            CopyOnlyList.DelimitedText := AnsiLowerCase (fSrcFName) ;
            CopyOnlyList.Sort ;
            CopyOnlyList.Sorted := true ;
{$ENDIF}
            if CopyOnlyList.Count = 0 then
            begin
                result := TaskResOKNone ;
                fReqResponse := 'No Specific Source Files Selected to Download' ;
                exit ;
            end ;
            for I := 0 to Pred (TotSrcFiles) do
            begin
                SrcFileRec := SrcFileList [I] ;
                with SrcFileRec^ do
                begin
                    if FrFileCopy = FCStateSelect then
                    begin
                        if NOT CopyOnlyList.Find (AnsiLowerCase (FrFileName), J) then
                        begin
                             FrFileCopy := FCStateNone ;
                             dec (fCopyProg.TotProcFiles) ;
                        end ;
                    end ;
                end ;
            end ;
            if fCopyProg.TotProcFiles = 0 then
            begin
                result := TaskResOKNone ;
                fReqResponse := 'No Specific Source Files Selected to Download' ;
                exit ;
            end ;
        end ;

        // check size of files that will be deleted, make sure FT resume files not deleted
        delsize := 0 ;
        info := '' ;
        if fDelOldTar and (fCopyProg.TotDelFiles <> 0) then
        begin
            if CheckFiles then
            begin
                listing.Capacity (TotSrcFiles * 50) ;   // no real idea yet
                listing.AppendLine ('Old Files Selected for Deletion are: ') ;
            end;
            for I := 0 to Pred (TotTarFiles) do
            begin
                SrcFileRec := TarFileList [I] ;
                with SrcFileRec^ do
                begin
                    if FrFileCopy = FCStateSelect then
                    begin
                        if Pos ('_', FrFileName) > 0 then  // resume temporary file
                        begin
                            fnametmp := AnsiLowerCase (ExtractFileExt (FrFileName)) ;
                            if (fnametmp = '.tmp') or (fnametmp = '.ftp') then FrFileCopy := FCStateNone ;
                        end ;
                    end ;
                    if FrFileCopy = FCStateSelect then
                    begin
                        inc (delsize, FrFileBytes) ;
                        if CheckFiles then
                        begin
                             if ((FrFileAttr and faDirectory) = faDirectory) then   // 21 Feb 2011 display directory
                                listing.AppendLine (FrFullName + IcsSpace + sDirLit)
                            else
                                listing.AppendLine (FrFullName + ', Size ' + IcsInt64ToCStr (FrFileBytes)) ;
                        end;
                    end ;
                end ;
            end ;
        end ;
        if fCancelFlag then exit ;

        // find size of stuff to copy
        if CheckFiles then
        begin
            listing.Capacity (TotSrcFiles * 50) ;   // no real idea yet
            listing.AppendLine ('Files Selected for Downloading are: ') ;
        end;
        newsize := 0 ;
        for I := 0 to Pred (TotSrcFiles) do
        begin
            SrcFileRec := SrcFileList [I] ;
            with SrcFileRec^ do
            begin
                if FrFileCopy = FCStateSelect then
                begin
                    newsize := newsize + FrFileBytes ;
                    if CheckFiles then
                    begin
                        if ((FrFileAttr and faDirectory) = faDirectory) then   // 21 Feb 2011 display directory
                            listing.AppendLine (FrFullName + IcsSpace + sDirLit)
                        else
                            listing.AppendLine (FrFullName + ', Size ' + IcsInt64ToCStr (FrFileBytes)) ;
                   end;
                end ;
            end ;
        end ;
        if CheckFiles then
        begin
            info := listing.GetString ;
            doCopyEvent (LogLevelInfo, info) ;
            info := '' ;
        end;
        doCopyEvent (LogLevelInfo, 'Source Files Skipped ' + IntToStr (fCopyProg.SkippedFiles)) ;
        info := 'Selected Total Files ' + IntToStr (fCopyProg.TotProcFiles) + ', Total size ' + IntToKByte (newsize, true) ;
        if fCopyProg.TotDelFiles <> 0 then
            info := info + IcsCRLF + 'Old Files to Delete ' + IcsIntToCStr (fCopyProg.TotDelFiles) + ', Total size ' + IntToKByte (delsize, true) ;
        doCopyEvent (LogLevelInfo, info) ;
        fCopyProg.TotProcBytes := newsize ;    // total size of all files
        fCopyProg.TotDelBytes := delsize ;

        // stop now if only checking what will be downoaded - test only
        if CheckFiles then
        begin
            result := TaskResOKNone ;
            exit ;
        end ;
        if fCancelFlag then exit ;

        // see if deleting old target files first, before downloading new files
        if fDelOldTar and (fCopyProg.TotDelFiles <> 0) then
        begin
            doCopyEvent (LogLevelInfo, 'Deleting Old Local Files: ' + basetardir) ;
            for I := 0 to Pred (TotTarFiles) do
            begin
                MessagePump ; // 15ept 2010 needed to support MultiThreaded
                if fCancelFlag then exit ;
                SrcFileRec := TarFileList [I] ;
                with SrcFileRec^ do
                begin
                    if FrFileCopy = FCStateSelect then
                    begin
                // 14 Sept 2008 skip Unicode names with substitution characters
                        if (Pos ('?', FrFullName) > 0) then
                        begin
                            doCopyEvent (LogLevelDiag, 'Skipped Inaccessible Unicode Name: ' + FrFullName) ;
                            doCopyEvent (LogLevelDelimFile, '|' + FrFullName + '|0|0|0|Inaccessible Unicode File|0|0') ;
                            FrFileCopy := FCStateFailed ;
                            inc (fCopyProg.ProcFailFiles) ;
                            continue ;
                        end ;
                        doCopyEvent (LogLevelFile, 'Deleting: ' + FrFullName) ;
                        retval := IcsDeleteFile (FrFullName, fReplRO) ;
                        if retval <= 0 then
                        begin
                            doCopyEvent (LogLevelDelimFile, '|' + FrFullName + '|0|0|0|Old Target File Deleted|0|0') ;
                            inc (fCopyProg.DelOKBytes, FrFileBytes) ;
                            FrFileCopy := FCStateOK ;
                            inc (fCopyProg.DelOKFiles) ;

                         // add directory to list we'll try and delete later, 24 Apr 2003
                            newtardir := Trim (FrSubDirs) ;
                            if NOT DelDirList.Find (newtardir, J) then
                                DelDirList.Add (newtardir) ;
                        end
                        else
                        begin
                            doCopyEvent (LogLevelInfo, 'File Delete Failed: ' + FrFullName+ ' - ' +  SysErrorMessage (retval)) ;
                            FrFileCopy := FCStateFailed ;
                            inc (fCopyProg.ProcFailFiles) ;
                        end ;
                    end ;
                end ;
            end ;
        end ;

        // FTP download
        doCopyEvent (LogLevelInfo, 'Started FTP Download from: ' + IcsFmtIpv6Addr (fHostName)) ;
        MessagePump ; // 15 Sept 2010 needed to support MultiThreaded
        if fCancelFlag then exit ;

        // start real FTP downloading
        cursrcdir := '.,.,.' ;  // illegal
        //    donenr := 0 ;
        totsize := 0 ;
        LocalStream := Nil ;  // download to files, not stream
        fCopyProg.SessStartTick := IcsGetTickCount64 ;          { V8.71 }
        for I := 0 to Pred (TotSrcFiles) do
        begin
            MessagePump ; // 15 Sept 2010 needed to support MultiThreaded
            if fCancelFlag then break ;  // 20 Sept 2010 not exit so totals reported
            if NOT Connected then
            begin
                doCopyEvent (LogLevelInfo, 'Lost FTP Control Connection, Abandoning Downloads') ;
                break ;  // 20 Sept 2010 not exit so totals reported
            end ;
            SrcFileRec := SrcFileList [I] ;
            with SrcFileRec^ do
            begin
                if FrFileCopy <> FCStateSelect then continue ;
                inc (fCopyProg.TotDoneNr) ;
            //    fnamesrc := FrFullName ;
                if (FrSubDirs [1] = '/') then     // remove leading slash
                    tempdir := fServBaseDir + Copy (FrSubDirs, 2, 200)
                else
                    tempdir := fServBaseDir + FrSubDirs ;
                if cursrcdir <> tempdir then
                begin
                    cursrcdir := tempdir ;
                    if (FrSubDirs [1] = '/') then     // check leading slash
                        newtardir := fTarDir + FrSubDirs
                    else
                        newtardir := fTarDir + '/' + FrSubDirs ;
                    newtardir := IcsPathUnixToDos (newtardir) ;
                    if CaseFile = FileLowerCase then newtardir := AnsiLowerCase (newtardir) ;
                end ;

            // see if making lower case for PC
                if CaseFile = FileLowerCase then
                    fnametar := AnsiLowerCase (newtardir + FrFileName)
                else
                    fnametar := newtardir + FrFileName ;

          // 21 Feb 2011 - if a directory, special handling
                if ((FrFileAttr and faDirectory) = faDirectory) then
                begin
                // create target directory
                    doCopyEvent (LogLevelFile, 'Creating Directory: ' + fnametar) ;
                    if NOT IcsForceDirsEx (fnametar) then
                        doCopyEvent (LogLevelInfo, 'Can Not Create Target Directory: ' + fnametar)
                    else
                    begin
                        doCopyEvent (LogLevelFile, 'Created Target Directory OK: ' + fnametar) ;
                        doCopyEvent (LogLevelDelimFile, FrFullName + '|' + fnametar + '|0|1|0|OK|0|0') ;
                        FrFileCopy := FCStateOK ;
                    end ;
                    continue ;
                end ;


             // FTP download file, limit maximum attempts
                retval := 0 ;
                for loop := 1 to (fFailRepeat + 1) do  // 31 Dec 2007
                begin
                    fCopyProg.ProgMessBase := 'Downloading File ' + IntToStr (fCopyProg.TotDoneNr) + ' of ' + IntToStr (fCopyProg.TotProcFiles) ;

                 // try and get latest file size and time stamp, might have changed
                    if FtpCheckFile (cursrcdir, FrFileName, RFSize, RFileUDT) then
                    begin
                        if (RFSize > 0) and (RFileUDT > 10) then
                        begin
                            if RFileUDT > SrcFileRec^.FrFileUDT then FrFileUDT := RFileUDT ;
                            if RFSize <> FrFileBytes then
                            begin
                                doCopyEvent (LogLevelFile, 'File Size Changed: ' + FrFullName  + ' from ' +
                                                    IcsInt64ToCStr (FrFileBytes)  + ' to ' + IcsInt64ToCStr (RFSize)) ;
                                FrFileBytes := RFSize ;
                            end ;
                        end ;
                    end
                    else
                    begin
                        doCopyEvent (LogLevelFile, 'File Not Found On FTP Server: ' + FrFullName) ;
                        // continue ;  currently ignore error in case LIST file did not work
                    end ;
                    if FrFileUDT < 10 then
                        FrFileUDT := DateTimeToUTC (FrFileDT) ;  // 18 Aug 2004

                // FTP get file
                    FrFileCopy := FCStateCopying ;
                    sysDelayX (200) ;  // 11 Aug 2004 - short delay to try and fix corrupted PORT command
                    retval := IntDownOne (cursrcdir, FrFileName, FrFullName, fnametar, FrFileBytes, FrFileUDT) ;
                    if retval = 0 then break ;   // OK, done break loop
                    if fCancelFlag then break ;   // 20 Sept 2010 not exit so totals reported
                    if NOT Connected then break ;
                    if NOT (retval in [2, 3]) then break ;  // fail download or fail MD5
                    if (loop < (fFailRepeat + 1)) then   // 31 Dec 2007
                        doCopyEvent (LogLevelInfo, 'Repeating Download: ' + FrFullName) ;
                end ;
                if retval <> 0 then
                begin
                    inc (fCopyProg.ProcFailFiles) ;
                    FrFileCopy := FCStateFailed ;
                end
                else
                begin
                    newsize := IcsGetFileSize (fnametar) ;
                    inc (fCopyProg.ProcOKFiles) ;
                    inc (totsize, newsize) ;
                    FrFileCopy := FCStateOK ;

              // see if unzipping it, V8.70 now using System.Zip instead of VclZip
        {$IFDEF Zipping}
                    if fZipped and Assigned (MyZipFile) and (AnsiLowerCase (ExtractFileExt (fnametar)) = '.zip') then
                    begin
                        MyZipName := fnametar ;    // set the zip filename
                        try

                       // check it's not corrupted
                            if TZipFile.IsValid(MyZipName) then          { V8.70 VclZip gone }
                            begin
                                MyZipFile.Open(MyZipName, zmRead) ;           // open it and read its information

                       // List filenames in zip file
                                info := '' ;
                                if MyZipFile.Comment <> '' then
                                    info := MyZipFile.Comment + IcsCRLF ;
                                for J := 0 to Pred (MyZipFile.FileCount) do
                                begin
                                    MyDateDT := 0 ;
                                    WinFileDateToDateTime(MyZipFile.FileInfo[J].ModifiedDateTime, MyDateDT);
                                    info := info + Format (sDirLine, [MyZipFile.Filename [J], IntToKByte (MyZipFile.FileInfo[J].UnCompressedSize, true), ' ',
                                                 DateTimeToStr (MyDateDT), ' ']) + TZipCompressionToString(TZipCompression(MyZipFile.FileInfo[J].CompressionMethod)) + IcsCRLF ;
                                 end ;
                                doCopyEvent (LogLevelInfo, 'Unzipping Files:' + IcsCRLF + info) ;

                             // extract all files
                                if (fZipDir = '') and (fZipPath >= PathSpecific) then
                                    fZipPath := PathNew ;
                                MyDestDir := ExtractFileDir (fnametar) ;     // Set destination directory
                                MyRecreateDirs := False ;
                                if fZipPath in [PathOriginal, PathNewOrig, PathSpecOrig] then
                                    MyRecreateDirs := true ;
                                if fZipPath in [PathNew, PathNewOrig] then
                                    MyDestDir := ExtractFileDir (fnametar) + '\' + IcsExtractNameOnly (fnametar) ;
                                if fZipPath >= PathSpecific then
                                    MyDestDir := fZipDir ;
                                if NOT IcsForceDirsEx (MyDestDir) then
                                begin
                                    doCopyEvent (LogLevelFile, 'Failed to Create Unzip Dir: ' + MyDestDir) ;
                                    doCopyEvent (LogLevelDelimFile, fnametar + '|' + MyDestDir + '|0|0|1|Failed to Create Unzip Dir|0|0') ;
                                    continue ;
                                end ;
                                MyDoneNr := 0;
                                for J := 0 to Pred (MyZipFile.FileCount) do
                                begin
                                    try
                                        newsize := -1;
                                        MyNewFullFile := SlashesToBackSlashes(MyZipFile.Filename [J]) ;
                                        if NOT MyRecreateDirs then
                                            MyNewFullFile := ExtractFileName(MyNewFullFile);
                                        MyNewFullFile := IncludeTrailingBackslash (MyDestDir) + MyNewFullFile ;
                                        if MyNewFullFile[Length(MyNewFullFile)] = '\' then  // blank directory in zip, can not unzip it
                                        begin
                                            MyDoneNr := MyDoneNr + 1;
                                            if MyRecreateDirs then
                                            begin
                                                if IcsForceDirsEx(MyNewFullFile) then
                                                    doCopyEvent (LogLevelFile, 'Extracted OK: ' + MyNewFullFile + ', Directory') ;
                                            end;
                                            continue;
                                        end;
                                        MyZipFile.Extract(J, MyDestDir, MyRecreateDirs);    // exception if it died
                                        newsize := IcsGetFileSize (MyNewFullFile) ;
                                    except
                                        doCopyEvent (LogLevelFile, 'Zip Extract Failed ' + MyNewFullFile + ' - ' + IcsGetExceptMess (ExceptObject)) ;
                                    end ;
                                    if newsize >= 0 then
                                    begin
                                        MyDoneNr := MyDoneNr + 1;
                                        doCopyEvent (LogLevelFile, 'Extracted OK: ' + MyNewFullFile + ', size: ' + IntToKByte (newsize, true)) ;
                                        doCopyEvent (LogLevelDelimFile, 'Extracted|' + MyNewFullFile + '|' + IntToStr (newsize) + '|1|0|OK|0|0') ;
                                    end
                                    else
                                    begin
                                        doCopyEvent (LogLevelFile, 'Failed to Extract: ' + MyNewFullFile + ', size: ' + IntToKByte (newsize, true)) ;
                                        doCopyEvent (LogLevelDelimFile, 'Failed Extract|' + MyNewFullFile + '|' + IntToStr (newsize) + '|0|0|Failed to Unzip|0|0') ;
                                    end;
                                end;
                                if MyDoneNr = MyZipFile.FileCount then
                                begin
                                    doCopyEvent (LogLevelFile, 'Unzipped OK: ' + fnametar) ;
                                    if fZipDownDel then
                                    begin
                                        doCopyEvent (LogLevelFile, 'Deleting: ' + fnametar) ;
                                        IcsDeleteFile (fnametar, true) ;
                                        doCopyEvent (LogLevelDelimFile, fnametar + '| |0|0|0|File Deleted After Unzipping|0|0') ;
                                    end ;
                                end
                                else
                                begin
                                    doCopyEvent (LogLevelFile, 'Failed to Unzip: ' + fnametar) ;
                                    doCopyEvent (LogLevelDelimFile, fnametar + '|' + MyDestDir + '|0|0|1|Failed to Unzip File|0|0') ;
                                    continue ;
                                end ;
                            end
                            else
                            begin
                                doCopyEvent (LogLevelInfo, 'Zip File Corrupted:' + fnametar) ;
                                doCopyEvent (LogLevelDelimFile, fnametar + '|' + MyDestDir + '|0|0|1|Zip File Corrupted|0|0') ;
                                continue ;
                            end ;
                        except
                            doCopyEvent (LogLevelFile, 'Zip Decompression Failed ' + fnametar + ' - ' + IcsGetExceptMess (ExceptObject)) ;
                        end;
                        MyZipFile.Close;
                    end ;
        {$ENDIF}
                    if NOT Connected then exit ;

              // see if deleting remote file
                    if fDelDone then
                    begin
                        HostFileName := FrFileName ;
                        doCopyEvent (LogLevelFile, 'Deleting: ' + HostFileName) ;
                        Dele ;   // delete remote file
                        doCopyEvent (LogLevelDelimFile, FrFullName + '| |0|0|0|Source File Deleted After Copy|0|0') ;
                    end ;
                end ;
            end ;
        end ;

        // see if any old empty directories to delete
        if fDelOldTar and (DelDirList.Count <> 0) then     { V8.70 }
        begin
            doCopyEvent (LogLevelInfo, 'Checking for Empty Target Directories: ' + fTarDir) ;
            for I := 0 to Pred (DelDirList.Count) do
            begin
                newtardir := fTarDir + DelDirList [I] ;
                J := Length (newtardir) ;
                while J >= 2 do
                begin
                    if newtardir [J] = '\' then dec (J) ;
                    newtardir := copy (newtardir, 1, J) ;
                    doCopyEvent (LogLevelDiag, 'Checking Directory Empty: ' +  newtardir) ;
                    if NOT IcsCheckDirAny (newtardir) then
                    begin
                     // doCopyEvent (LogLevelDiag, 'Will Delete Dir: ' + curdir) ;
                       if RemoveDir (newtardir) then
                            doCopyEvent (LogLevelFile, 'Removed Directory OK: ' + newtardir)
                        else
                            doCopyEvent (LogLevelFile, 'Failed to Remove Directory: ' + newtardir) ;
                        while J >= 2 do   // search for lower level directory
                        begin
                            dec (J) ;
                            if newtardir [J] = '\' then break ;
                        end ;
                    end
                    else
                        break ;
                end ;
            end ;
        end ;

        // finished, report stuff
        doCopyEvent (LogLevelProg, '') ;
        duration := IcsElapsedTicks64 (fCopyProg.SessStartTick) ;         { V8.71 }
        if fCancelFlag then  // 20 Sept 2010 report something before totals
            result := TaskResAbort
        else
        begin
            result := TaskResOKNone ;
            if fCopyProg.ProcFailFiles <> 0 then
                result := TaskResFail
            else
            begin
                if fCopyProg.ProcOKFiles <> 0 then result := TaskResOKNew ;
            end ;
        end;
        if fCancelFlag then  // 20 Sept 2010 report something before totals
            doCopyEvent (LogLevelInfo, 'Finished, cancelled downloading, done OK: ' + IcsIntToCStr (fCopyProg.ProcOKFiles) +
                 ', failed: ' + IcsIntToCStr (fCopyProg.ProcFailFiles) + ', skipped: ' +  IcsIntToCStr (fCopyProg.SkippedFiles))
        else
            doCopyEvent (LogLevelInfo, 'Finished, files downloaded OK: ' + IcsIntToCStr (fCopyProg.ProcOKFiles) +
                ', failed: ' + IcsIntToCStr (fCopyProg.ProcFailFiles) + ', skipped: ' +  IcsIntToCStr (fCopyProg.SkippedFiles)) ;
        doCopyEvent (LogLevelDelimTot, fSrcDir + '|' + fTarDir + '|' + IntToStr (totsize) + '|' +
                               IntToStr (fCopyProg.ProcOKFiles) + '|' + IntToStr (fCopyProg.ProcFailFiles) + '|Totals|') ;
        doCopyEvent (LogLevelInfo, 'Total size downloaded ' + IntToKByte (totsize, true) + ', duration ' +
              IcsSecsToStr (duration div 1000) + ', average speed ' + IntToKByte (IcsCalcSpeed (duration, totsize)) + '/sec') ;
        if fCopyProg.DelOKFiles <> 0 then doCopyEvent (LogLevelInfo, 'Old target files deleted OK: ' +
                  IcsIntToCStr (fCopyProg.DelOKFiles) + ', Total size deleted ' + IntToKByte (fCopyProg.DelOKBytes, true)) ;
    finally
        if NOT Connected then
        begin
            result := TaskResFail ;
            fReqResponse := 'No Connection to Server, Cancelled Downloading Files' ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
        end
        else if fCancelFlag then
        begin
            result := TaskResAbort ;
            fReqResponse := 'Cancelled Downloading Files' ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
        end ;
{$IFDEF Zipping}
        if fZipped and Assigned (MyZipFile) then
            MyZipFile.Free ;
{$ENDIF}
        if fSpecificFiles then                           { V8.710 }
            FreeAndNil (CopyOnlyList) ;   { V8.70 }
        if fDelOldTar then                               { V8.71 }
            FreeAndNil (DelDirList) ;
        if fWow64RedirDisable then
            RevertWow64Redir (OldWow64) ; // 22 May 2013
        listing.Free ;
    end ;
end;

// FTP single file internal upload

function TIcsFtpMulti.IntUpOne (const LocFileFull, RemDir, RemFile: string; const RFSize: Int64; RFileUDT: TDateTime): integer ;
var
    fnametar, fnametmp, fnameftp, newtardir, info, remfull, fileext: string ;
    remfileUDT: TDateTime ;
    I, J, attempts: integer;
    newsize, uploadsize, partfsize, lastbytes, actualbytes: Int64 ;
    duration: longword ;
    ret, resflag, zipflag: boolean ;
    ResInfRecs: TStringList ;
{$IFDEF Zipping}
     MyZipFile: TZipFile;         { V8.70 VclZip gone }
     MyArchiveStream: TMemoryStream;
{$ENDIF}


    // create new directory on FTP server, one level at a time
    // unless directory already exists
    function CreateFtpDir (XRemDir: string): Boolean;
    var
        I, J: integer ;
        newdir, remaindir: string ;
    begin

    // ignore server root, it must exist so don't try and create it
        remaindir := XRemDir ;
        if fServRootDir <> remaindir then
        begin
            I := Length (fServRootDir) ;
            if fServRootDir = copy (XRemDir, 1, I) then remaindir := copy (XRemDir, succ (I), 999) ;
        end ;

    // remove leading / but not trailing /
        if (length (remaindir) > 1) and (remaindir [1] = '/') then remaindir := copy (remaindir, 2, 999) ;
        J := 0 ;
        while true do
        begin
           // check if directory exists
            HostDirName := XRemDir ;  // full path for Cwd
            result := true ;
            if fCurRemDir = HostDirName then exit ;
            result := Cwd ;
            if fCancelFlag then exit ;
            if result then result := Pwd ;
            if result then fCurRemDir := HostDirName ;
            if result then exit ;
            if Length (remaindir) = 0 then exit ;

        // start in server root, first loop only
            if J = 0 then
            begin
                HostDirName := fServRootDir ;
                result := Cwd ;
                if result then result := Pwd ;
                if NOT result then exit ;
            end ;
            if fCancelFlag then exit ;

       // get next directory to create
            I := Pos ('/', remaindir) ;
            if I <= 1 then exit ;  // dead!!
            newdir  := Copy (remaindir, 1, Pred (I)) ;
            remaindir := Copy (remaindir, succ (I), 999) ;

       // change relatively to directory to see if it exists, else make it
            HostDirName := newdir ;   // relative path for Cwd
            HostFileName := newdir ;  // relative path for Mkd
            result := Cwd ;
            if fCancelFlag then exit ;
            if NOT result then
            begin
                Mkd ;   // make directory
                if fCancelFlag then exit ;

                // make sure it's OK
                if Length (remaindir) > 0 then   // if not reached end of path
                begin
                    result := Cwd ;
                    if result then result := Pwd ;
                    if NOT Result then exit ;
                    fCurRemDir := HostDirName ;
                end ;
            end ;
            inc (J) ;
            if J > 10 then exit ;   // prevent looping
        end ;
    end;

begin
    result := 1 ;  // fail
    if fCancelFlag then exit ;
    if NOT fLoggedIn then exit ;
    if NOT Connected then exit ;
    FNoopTimer.Enabled := False;                  { V8.71 }
{$IFDEF Zipping}
    MyZipFile := Nil;
    MyArchiveStream := Nil;
    if fZipped then
    begin
        MyZipFile := TZipFile.Create;
        MyZipFile.OnProgress := ZipProgressEvent;
    end ;
{$ENDIF}

// 14 Sept 2008 skip Unicode file with non-ANSI characters unlesss UTF8 enabled
    if (Pos ('?', LocFileFull) > 0) then
    begin
        doCopyEvent (LogLevelDiag, 'Skipped Inaccessible Unicode Name: ' + LocFileFull) ;
        doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + RemFile + '|0|0|1|Inaccessible Unicode File|0|0') ;
        inc (fCopyProg.ProcFailFiles) ;
        fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + RFSize ;
        result := 6 ;  // fail
        exit ;
    end ;

// 15 Sept 2008 skip old FTP resume file
    I := Length (LocFileFull) ;
    J := Pos ('.ftp', LocFileFull) ;
    if (J > 4) and ((J + 3) = I) then
    begin
        if LocFileFull [J - 4] = '_' then
        begin
            doCopyEvent (LogLevelDiag, 'Skipped Old FTP Resume File: ' + LocFileFull) ;
            doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + RemFile + '|0|0|1|Old FTP Resume File|0|0') ;
            inc (fCopyProg.ProcFailFiles) ;
            fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + RFSize ;
            result := 6 ;  // fail
            exit ;
        end ;
    end ;

    ResInfRecs := TStringList.Create ;
    try  // finally
        IcsCopyProgClearCur (fCopyProg) ;  // 22 May 2013 clear current about to start a file

    // create archive directory
        if fUpArchive then
        begin
            fDelDone := false ;
            if NOT IcsForceDirsEx (fUpArchDir) then
            begin
                inc (fCopyProg.ProcFailFiles) ;
                fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + RFSize ;
                fReqResponse := 'Can Not Create Archive Directory' ;
                exit ;
            end ;
        end ;

    // 21 Feb 2011 - RFSize=-1 means directory not a file
        if RFSize < 0 then
        begin
            if CaseFile = FileLowerCase then
                newtardir := AnsiLowerCase (RemDir + RemFile + '/')
            else
                newtardir := RemDir + RemFile + '/' ;
            HostDirName := '/' ;
            doCopyEvent (LogLevelDiag, 'Creating Directory: ' + newtardir) ;
            ret := CreateFtpDir (newtardir) ;
            if NOT ret then
            begin
                fReqResponse := 'Can Not Create Target Directory' ;
                doCopyEvent (LogLevelFile, 'Can Not Create Target Directory: '+ newtardir) ;
                result := 6 ;  // fail
            end
            else
            begin
                doCopyEvent (LogLevelFile, 'Created Target Directory OK: ' + newtardir) ;
                doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + newtardir + '|0|1|0|OK|0|0') ;
                result := 0 ; // success
            end;
            exit ;
        end;

    // create remote directories, see if making lower case for FTP server
        if CaseFile = FileLowerCase then
            newtardir := AnsiLowerCase (RemDir)
        else
            newtardir := RemDir ;
        HostDirName := '/' ;
        if fCurRemDir <> newtardir then
        begin
            doCopyEvent (LogLevelDiag, 'Changing to Directory: ' + newtardir) ;
            ret := CreateFtpDir (newtardir) ;
            if NOT ret then
            begin
                fReqResponse := 'Can Not Create Target Directory' ;
                inc (fCopyProg.ProcFailFiles) ;
                fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + RFSize ;
                exit ;
            end ;
        end ;
        if NOT Connected then exit ;
        if fCancelFlag then exit ;

    // see if making lower case for FTP server
        if CaseFile = FileLowerCase then
            fnametar := AnsiLowerCase (RemFile)
        else
            fnametar := RemFile ;
        remfull := newtardir + fnametar ;
        LocalFileName := LocFileFull ;  // FTP source file to upload
        ShareMode := ftpShareDenyWrite ;
        uploadsize := RFSize ;

    // see if zipping file
        zipflag := false ;
    {$IFDEF Zipping}
        if fZipped and Assigned (MyZipFile) and (AnsiLowercase (ExtractFileExt (LocFileFull)) <> '.zip') then
        begin
            doCopyEvent (LogLevelFile, 'Compressing ' + LocFileFull) ;
            MyArchiveStream := TMemoryStream.Create ; // Create archive in a memory stream */
            try
                MyZipFile.Open(MyArchiveStream, zmWrite) ;
                MyZipFile.Add (LocFileFull, ExtractFileName(LocFileFull)) ;
                MyZipFile.Comment := 'Zipped by FTP Task' ;
                MyZipFile.Close ;
                zipflag := true ;
                MyArchiveStream.Position := 0;
                uploadsize := MyArchiveStream.Size ;
                if (uploadsize <= 0) then
                begin
                    doCopyEvent (LogLevelFile, 'Zip Compression Failed ' + LocFileFull) ;
                end
                else
                begin
                    LocalStream := MyArchiveStream ;
                    LocalFileName := '' ;
                    if fZipExtFmt = ExtnReplace then
                        fnametar := ChangeFileExt (fnametar, '.zip')
                    else
                        fnametar := fnametar + '.zip' ;
                    doCopyEvent (LogLevelFile, 'Compressed ' + LocFileFull  + ' from ' +
                                           IntToKByte (RFSize, true)  + ' to ' + IntToKByte (uploadsize, true)) ;
                end ;
            except
                doCopyEvent (LogLevelFile, 'Zip Compression Failed ' + LocFileFull + ' - ' + IcsGetExceptMess (ExceptObject)) ;
            end;
        end ;
    {$ENDIF}

    // start upload, to temporary file, resuming if possible
        if CaseFile = FileLowerCase then     // 23 Feb 2016 was always treated as lower case
            fnametmp := AnsiLowerCase (fnametar)
        else
            fnametmp := fnametar ;
        if (magftpNoTmpFile in fMagFtpOpts) then  // 6 Jan 2008 - download with correct name
        begin
            fResFailed := false ;
        end
        else
        begin
            fnametmp := IcsTransChar (fnametmp, '.', '_') ;// don't mess with directories
            fnametmp := fnametmp + '.tmp' ;    // destination file name for FTP
        end ;

    // 22 May 2013 prepare current file progress info
        fCopyProg.CurSrcName := LocFileFull;
        fCopyProg.CurTarName := RemFull ;
        fCopyProg.CurFileBytes := uploadsize ;
        fCopyProg.CurFileStartPos := 0;   { V8.71 }
        fCopyProg.ProcBytesLast := fCopyProg.ProcBytesDone ;
        doCopyEvent (LogLevelFile, fCopyProg.ProgMessBase + ', ' + LocFileFull  + ' to ' + remfull + ', size ' + IntToKByte (uploadsize, true)) ;
        fCopyProg.ProgMessBase :=  fCopyProg.ProgMessBase + IcsCRLF + RemFile ;
        fProgFileSize := uploadsize ;   // keep name and size for event handler
        if NOT Connected then exit ;
        if fCancelFlag then exit ;

    // set compression mode, don't compress zip files
        if fUsingCompression then
        begin
            fileext := ExtractFileExt(AnsiLowercase (LocFileFull));  // 2 Dec 2007 longer list
            if (Pos (fileext, fZlibNoCompExt) > 0) or zipflag OR (fZlibMaxSize < uploadsize) then  // 9 Dec 2007 max size to compress
                TransferMode := ftpTransModeStream
            else
                TransferMode := ftpTransModeZDeflate ;
            if (FCurrTransMode <> TransferMode) then ModeZ ;
        end ;

    // V8.71 see if using NOOP keep-alive to try and stop control channel being closed as idle on long transfers
        if (FNoopIntervalMins > 0) and (uploadsize > 10*1000*1000) then begin  // min 10 megs
            FNoopTimer.Interval := FNoopIntervalMins * Ticks64PerMinute;
            FNoopTimer.Enabled := True;
        end;

        if (NOT zipflag) and fResFailed then
        begin
            fnameftp := ExtractFileName (LocFileFull) ;
            fnametmp := IcsTransChar (fnameftp, '.', '_') ;// don't mess with directories
            fnameftp := ExtractFilePath (LocFileFull) + fnameftp + '.ftp' ;  // control file in local directory
            resflag := false ;
            attempts := 0 ;
            partfsize := 0 ;

       // see if local resume info file and TMP file on FTP server exist
            if FileExists (fnameftp) then
            begin
                try
                    ResInfRecs.LoadFromFile (fnameftp) ;
                    if NOT FtpCheckFile (newtardir, fnametmp, partfsize, remfileUDT) then
                    begin
                         partfsize := 0 ;
                         doCopyEvent (LogLevelFile, 'Unable to Resume, Partial File Not Found: ' + fnametmp) ;
                    end ;
                    if NOT Connected then exit ;
                    if fCancelFlag then exit ;
                    if (partfsize >= fMinResSize) and (ResInfRecs.Count > ResInfLastBytes) then
                    begin
                        attempts := atoi (ResInfRecs [ResInfAttempts]) ;
                        lastbytes := atoi64 (ResInfRecs [ResInfLastBytes]) ;
                        if (attempts >= fMaxResumeAttempts) then        // 31 Dec 2007
                            doCopyEvent (LogLevelFile, 'Skipped Resume, Too Many Attempts (' + IntToStr (fMaxResumeAttempts) + ')')
                        else if (lastbytes = partfsize) then
                            doCopyEvent (LogLevelFile, 'Skipped Resume, Same Size as Last Attempt')
                        else if (uploadsize < partfsize) then
                            doCopyEvent (LogLevelFile, 'Skipped Resume, Part File too Large') // 9 Dec 2007
                        else
                        begin
                            if (ResInfRecs [ResInfServer] = fHostName1) and (ResInfRecs [ResInfFName] = String (StringToUtf8(remfull))) and
                               (ResInfRecs [ResInfStamp] = FloatToStr (RFileUDT)) and (ResInfRecs [ResInfSize] = IntToStr (uploadsize)) then
                            begin
                                if partfsize = uploadsize then
                                begin
                                    partfsize := 0 ;
                                    doCopyEvent (LogLevelFile, 'Unable to Resume, Upload File Same Size') ;
                                end
                                else
                                begin
                                    resflag := true ;
                                    doCopyEvent (LogLevelFile, 'Resuming Partial File Upload from: ' + IcsInt64ToCStr (partfsize)) ;
                                end ;
                            end
                            else
                                doCopyEvent (LogLevelFile, 'Unable to Resume, Upload File Has Changed') ;
                        end ;
                    end ;
                    sysDelayX (200) ;  // short delay to allows old socket to close
                except
                end ;
            end ;
            fCopyProg.CurStartTick := IcsGetTickCount64 ;        { V8.71 }
            if fCancelFlag then exit ;
            if NOT Connected then exit ;

       // 22 Nov 2007 check if sufficient space ALLOcated for upload
            if (NOT (magftpNoFeat in fMagFtpOpts)) then
            begin
                PosEnd := uploadsize - partfsize ;
                Allo ;
                if StatusCode = 501 then   // 500 command not understood, 200 OK
                begin
                    if (Pos ('insufficient', AnsiLowercase (LastResponse)) > 0) or
                                                (Pos ('not enough', AnsiLowercase (LastResponse)) > 0) then
                    begin
                        doCopyEvent (LogLevelFile, 'Upload Failed: ' + LastResponse) ;
                        doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + fnametmp + '|0|0|1|Upload Failed: ' + LastResponse + '|0|0') ;
                        inc (fCopyProg.ProcFailFiles) ;
                        fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + RFSize ;
                        exit ;
                    end ;
                end ;
            end;
            if fCancelFlag then exit ;
            if NOT Connected then exit ;

         // start resume FTP upload
            HostFileName := fnametmp ;
            LocalFileName := LocFileFull ;  // may have been lost during FtpCheckFile
            ResumeAt := 0 ; // 4 July 2007 ensure resume position reset
            if resflag then
            begin
                inc (attempts) ;
                ResInfRecs [ResInfAttempts] := IntToStr (attempts) ;
                ResInfRecs [ResInfLastBytes] := IntToStr (partfsize) ;
                ResInfRecs.SaveToFile (fnameftp) ;
                doCopyEvent (LogLevelFile, 'Saved File Resume Info ' + fnameftp) ;
                ResumeAt := partfsize ;
                fCopyProg.CurFileStartPos := partfsize;                           { V8.71 }
                fCopyProg.TotProcBytes := fCopyProg.TotProcBytes - partfsize;     { V8.71 less to transfer }
                fCopyProg.CurStartTick := IcsGetTickCount64 ;                     { V8.71 }
                doCopyEvent (LogLevelFile, 'Resuming FTP Upload from Offset ' +
                                         IcsInt64ToCStr (partfsize) + ', Total file size ' + IntToKByte (uploadsize, true)) ;
                ret := RestPut ;   // resume download into LocalFileName
                if fCancelFlag then exit ;
                if NOT Connected then exit ;
                if (NOT (ret OR fCancelFlag)) and (StatusCode = 500) and (Pos ('RT', ErrorMessage) > 1) then
                begin
                    doCopyEvent (LogLevelDiag, 'Repeating Command, Corruption: ' + ErrorMessage) ;
                    sysDelayX (500) ;
                    ret := RestPut ;
                end ;
                if fCancelFlag then exit ;
            //    if NOT Connected then exit ;         { V8.71 log something, might have worked }
            end
            else
            begin
             // write current file into into .ftp file so we can resume if necessary
                IcsDeleteFile (fnameftp, true) ;
                ResInfRecs.Clear ;
                ResInfRecs.Add (fHostName1) ;               // ResInfServer = 0
                ResInfRecs.Add (String (StringToUtf8 (remfull))) ;   // ResInfFName = 1
                ResInfRecs.Add (FloatToStr (RFileUDT)) ;    // ResInfStamp = 2
                ResInfRecs.Add (IntToStr (uploadsize)) ;    // ResInfSize = 3
                ResInfRecs.Add ('1') ;                      // ResInfAttempts = 4
                ResInfRecs.Add ('0') ;                      // ResInfLastBytes = 5
                ResInfRecs.SaveToFile (fnameftp) ;
                doCopyEvent (LogLevelFile, 'Saved File Resume Info ' + fnameftp) ;
                fCopyProg.CurStartTick := IcsGetTickCount64 ;      { V8.71 }
                ret := Put ;  // upload it
                if fCancelFlag then exit ;
        //        if NOT Connected then exit ;         { V8.71 log something, might have worked }
                if (NOT (ret OR fCancelFlag)) and (StatusCode = 500) and (Pos ('RT', ErrorMessage) > 1) then
                begin
                    doCopyEvent (LogLevelDiag, 'Repeating Command, Corruption: ' + ErrorMessage) ;
                    sysDelayX (500) ;
                    ret := Put ;
                end ;
                if fCancelFlag then exit ;
       //         if NOT Connected then exit ;     { V8.71 log something, might have worked }
            end ;
            duration := IcsElapsedTicks64 (fCopyProg.CurStartTick) ;     { V8.71 }
        end
        else
        begin
       // 22 Nov 2007 check if sufficient space ALLOcated for upload
            if (NOT (magftpNoFeat in fMagFtpOpts)) then
            begin
                PosEnd := uploadsize ;
                Allo ;
                if StatusCode = 501 then      // 500 command not understood, 200 OK
                begin
                    if (Pos ('insufficient', AnsiLowercase (LastResponse)) > 0) or
                                            (Pos ('not enough', AnsiLowercase (LastResponse)) > 0) then
                    begin
                        doCopyEvent (LogLevelFile, 'Upload Failed: ' + LastResponse) ;
                        doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + fnametmp + '|0|0|1|Upload Failed: ' + LastResponse + '|0|0') ;
                        inc (fCopyProg.ProcFailFiles) ;
                        fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + RFSize ;
                        exit ;
                    end ;
                end ;
            end ;

        // start FTP upload
            IcsDeleteFile (fnameftp, true) ;
            HostFileName := fnametmp ;
          // don't set LocalFileName, it may have been replaced by a zipped LocalStream
            if fCancelFlag then exit ;
            if NOT Connected then exit ;
            fCopyProg.CurStartTick := IcsGetTickCount64 ;       { V8.71 }
            ret := Put ;  // upload it
            if fCancelFlag then exit ;
         //   if NOT Connected then exit ;     { V8.71 log something, might have worked }
            if (NOT ret) and (StatusCode = 500) and (Pos ('RT', ErrorMessage) > 1) then
            begin
                doCopyEvent (LogLevelDiag, 'Repeating Put Command, Corruption: ' + ErrorMessage) ;
                sysDelayX (500) ;
                ret := Put ;
            end ;
            duration := IcsElapsedTicks64 (fCopyProg.CurStartTick) ;        { V8.71 }
        end ;
        result := 2 ;  // FTP done
        FNoopTimer.Enabled := False;                  { V8.71 }
        actualbytes := ByteCount ;  // 2 Jan 2007 keep FTP count before corrupted by LIST
        if fCancelFlag then exit ;
     // if NOT Connected then exit ;    { V8.70 log something, might have worked }
        if NOT ret then
        begin
            if actualbytes < fMinResSize then
                IcsDeleteFile (fnameftp, true) ;  // 15 Sept 2008 kill resume file if insufficient copied
            doCopyEvent (LogLevelFile, 'Upload Failed: ' + FErrorMessage) ;
            doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + newtardir + fnametar +
                                   '|0|0|1|Upload Failed: ' + LastResponse + '|'+ IntToStr (duration) + '|' + IntToStr (actualbytes)) ;
        // 4 Jan 2008 'Unable to establish data connection - Connection refused' - abort to clean up
            if (StatusCode = 550) and (Pos ('#10061', LastResponse) > 1) then
            begin
                if Connected then AbortXfer ;
            end ;
            inc (fCopyProg.ProcFailFiles) ;
            fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + RFSize ;
            exit ;
        end ;

    // check file arrived OK by getting size
        sysDelayX (200) ;  // short delay to allows old socket to close
        IcsCopyProgClearCur (fCopyProg) ;  // 22 May 2013 clear current done file
        IcsCopyProgDuration (fCopyProg, AppTicksPerFtp) ;
        info := 'Uploaded File ' + fnametar ;
        doCopyEvent (LogLevelProg, info) ;
        doCopyEvent (LogLevelInfo, info) ;  // 24 July 2013
        if FtpCheckFile (newtardir, fnametmp, newsize, remfileUDT) then
        begin
            if fCancelFlag then exit ;
        //    if NOT Connected then exit ;   { V8.71 log something, might have worked }
            if newsize <> uploadsize then
            begin
                doCopyEvent (LogLevelFile, 'File Wrong Size on Server: ' + fnametar +
                                 '; Client ' + IntToKByte (uploadsize, true) + '; Server ' + IntToKByte (newsize, true)) ;
                doCopyEvent (LogLevelFile, 'Upload Failed: ' + LastResponse) ;
                doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + newtardir + fnametar +
                           '|0|0|1|Upload Failed: ' + LastResponse + '|'+ IntToStr (duration) + '|' + IntToStr (actualbytes)) ;
                HostFileName := fnametmp ;
                Dele ;   // delete temp file, ignore error
                inc (fCopyProg.ProcFailFiles) ;
                fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + RFSize ;
                exit ;
            end ;
        end
        else
        begin
            doCopyEvent (LogLevelFile, '!!! File Not Found On FTP Server: ' + HostFileName) ;
        end ;
    //  if NOT Connected then exit ;    { V8.70 log something, might have worked }
        if fCancelFlag then exit ;
        IcsDeleteFile (fnameftp, true) ;  // kill resume file

     // check MD5 or CRC32 if possible (not from zip stream yet) and repeat if allowed
        HostFileName := fnametmp ;
        if (NOT zipflag) and ((ftpFeatMD5 in FSupportedExtensions) or (ftpFeatXMD5 in FSupportedExtensions)) then
        begin
            doCopyEvent (LogLevelProg, 'Getting Remote MD5SUM ' + LocFileFull) ;
            fCopyProg.CurStartTick := IcsGetTickCount64 ;       { V8.71 }
            PosStart := 0 ;  // force XMD5 to do entire file, in case size changed
            PosEnd := 0 ;
            if (ftpFeatMD5 in FSupportedExtensions) then  // 22 Nov 2007 support XMD5, 6 Apr 2009 but prefer MD5
                ret := MD5
            else
                ret := XMD5 ;  // get MD5SUM for remote file, compare with local file
            if fCancelFlag then exit ;
        //    if NOT Connected then exit ;    { V8.71 log something, might have worked }
            if ret and (Length (fMD5Result) = 32) then
            begin
                fCopyProg.ProgMessBase := 'Checking Local MD5SUM ' + LocFileFull ;
                doCopyEvent (LogLevelProg, fCopyProg.ProgMessBase) ;
                fProgFileSize := uploadsize ;   // keep name and size for event handler
                info := FtpFileMD5 (LocFileFull, Self, MD5Progress) ;  // 8 Apr 2009 - widestring version
                if (fMD5Result <> info) then  // 6 Apr 2009 don't assume blank MD5sum is OK
                begin
                    doCopyEvent (LogLevelInfo, 'MD5SUM Compare Failed: ' + LocalFileName + ';Rem='+ fMD5Result + ';Loc=' + info) ;
                    doCopyEvent (LogLevelFile, 'Upload Failed: MD5SUM Compare Failed') ;
                    doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + newtardir + fnametar +
                             '|0|0|1|Upload Failed: MD5SUM Compare Failed|' + IntToStr (duration) + '|' + IntToStr (actualbytes)) ;
                    HostFileName := fnametmp ;
                    Dele ;   // delete temp file, ignore error
                    inc (fCopyProg.ProcFailFiles) ;
                    fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + RFSize ;
                    result := 3 ;
                    exit ;
                end ;
                doCopyEvent (LogLevelFile, 'MD5SUM Check OK: ' + LocFileFull + ' took ' +    // 20 Sept 2010 was LevelInfo
                                            IntToStr (IcsElapsedSecs64 (fCopyProg.CurStartTick)) + ' secs; Result ' + fMD5Result) ;   { V8.71 }
            end
            else
                doCopyEvent (LogLevelInfo, 'MD5SUM Response Failed: ' + fnametmp + ';Rem='+ fMD5Result) ;   // 6 Apr 2009
        end
        else if (NOT zipflag) and (ftpFeatXCrc in FSupportedExtensions) then
        begin
            doCopyEvent (LogLevelProg, 'Getting Remote CRC32 ' + LocFileFull) ;
            fCopyProg.CurStartTick := IcsGetTickCount64 ;       { V8.71 }
            PosStart := 0 ;
            PosEnd := 0 ;
            ret := XCRC ;  // get CRC32 for remote file, compare with local file
            if fCancelFlag then exit ;
        //   if NOT Connected then exit ;      { V8.71 log something, might have worked }
            if ret and (Length (fCrcResult) >= 5) then
            begin
                fCopyProg.ProgMessBase := 'Checking Local CRC32 ' + LocFileFull ;
                doCopyEvent (LogLevelProg, fCopyProg.ProgMessBase) ;
                fProgFileSize := uploadsize ;   // keep name and size for event handler
                info := FtpFileCRC32B (LocFileFull, Self, MD5Progress) ;   // 15 Apr 2009
                if (Length (info) = 8) and (Pos (fCrcResult, info) = 0) then
                begin
                    doCopyEvent (LogLevelInfo, 'CRC32 Compare Failed: ' + LocalFileName + ';Rem='+ fCrcResult + ';Loc=' + info) ;
                    doCopyEvent (LogLevelFile, 'Upload Failed: CRC32 Compare Failed') ;
                    doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + newtardir + fnametar +
                             '|0|0|1|Upload Failed: CRC32 Compare Failed|'+ IntToStr (duration) + '|' + IntToStr (actualbytes)) ;
                    HostFileName := fnametmp ;
                    Dele ;   // delete temp file, ignore error
                    result := 3 ;
                    inc (fCopyProg.ProcFailFiles) ;
                    fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + RFSize ;
                    exit ;
                end ;
                doCopyEvent (LogLevelFile, 'CRC32 Check OK: ' + LocFileFull + ' took ' +    // 20 Sept 2010 was LevelInfo
                                         IntToStr (IcsElapsedSecs64 (fCopyProg.CurStartTick)) + ' secs; Result ' + fCrcResult) ;   { V8.71 }
            end ;
        end ;

    // rename temporary file, deleting original
        if NOT (magftpNoTmpFile in fMagFtpOpts) then  // 6 Jan 2008 - download with correct name
        begin
            HostFileName := fnametar ;
            Dele ;   // delete old target file, ignore error if may not exist
            if fCancelFlag then exit ;
            if NOT Connected then exit ;
            HostFileName := fnametmp ;   // rename from
            LocalFileName := fnametar ;  // rename to
            ret := Ren ;  // rename file (sends RNFR and RNTO commands)
       //     if NOT Connected then exit ;    { V8.71 log something, might have worked }
            if fCancelFlag then exit ;
            if NOT ret then
            begin
                doCopyEvent (LogLevelFile, 'Final Rename Failed from: ' + fnametmp + ' to ' + fnametar) ;
                doCopyEvent (LogLevelFile, 'Upload Failed: ' + LastResponse) ;
                doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + newtardir + fnametar +
                           '|0|0|1|Upload Failed: ' + LastResponse + '|'+ IntToStr (duration) + '|' + IntToStr (actualbytes)) ;
                inc (fCopyProg.ProcFailFiles) ;
                fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + RFSize ;
                exit ;
            end ;
        end ;

    // successful upload
        result := 0 ; // success
        fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + uploadsize ;
        if duration = 0 then duration := 10 ;  // special case of copy OK but duration too short to measure
        doCopyEvent (LogLevelFile, 'Upload OK: ' + LocFileFull + ', size: ' + IntToKByte (uploadsize, true) +
                             ', duration ' + IcsSecsToStr (duration div 1000) + ', average speed ' +
                               IntToKByte (IcsCalcSpeed (duration, actualbytes)) + '/sec') ; // 10 Oct 2011 added duration and speed
        doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + newtardir + fnametar +
                            '|' + IntToStr (uploadsize) + '|1|0|OK|' + IntToStr (duration) + '|' + IntToStr (actualbytes)) ;
        if fCancelFlag then exit ;

    // try and set file time stamp on server, few support command
        HostFileName := fnametar ;
        if ftpFeatMFMT in FSupportedExtensions then
        begin
            RemFileDT := RFileUDT ;       // set UTC time !!
            ret := MFMT ;   // FTP command
            if NOT ret then
                doCopyEvent (LogLevelInfo, 'Failed to Modify File Stamp: ' + LastResponse) ;
        end
        else if ftpFeatMDTMYY in FSupportedExtensions then
        begin
            RemFileDT := RFileUDT ;       // set UTC time !!
            ret := MDTMYY ;  // FTP command
            if NOT ret then
                doCopyEvent (LogLevelInfo, 'Failed to Modify File Stamp: ' + LastResponse) ;
        end ;
    //    if NOT Connected then exit ;   { V8.71 log something, might have worked }
        if fCancelFlag then exit ;

     // immediate delete or move
        if fDelDone then
        begin
            doCopyEvent (LogLevelFile, 'Deleting: ' + LocFileFull) ;
            IcsDeleteFile (LocFileFull, true) ;
            doCopyEvent (LogLevelDelimFile, LocFileFull + '| |0|0|0|Source File Deleted After Copy|0|0') ;
        end ;
        if fUpArchive then
        begin
            doCopyEvent (LogLevelFile, 'Moving to Archive Directory: ' + LocFileFull) ;
            fnametar := IncludeTrailingPathDelimiter (fUpArchDir) + ExtractFileName (LocFileFull) ;
            ret := RenameFile (LocFileFull, fnametar) ;
            if NOT ret then
            begin
                doCopyEvent (LogLevelFile, 'Failed to Move File to Archive Directory, Using Unique Name') ;
                fnametar := IncludeTrailingPathDelimiter (fUpArchDir) + FormatDateTime ('"FTP-at-"yyyymmdd"-"hhnnss-z', Now) + ExtractFileExt (LocFileFull) ;
                ret := RenameFile (LocFileFull, fnametar) ;
            end ;
            if ret then
                doCopyEvent (LogLevelFile, 'Archived as: ' + fnametar) ;
        end ;
        if fTimeStamp then
        begin
            doCopyEvent (LogLevelFile, 'Updating Local Timestamp: ' + LocFileFull) ;
            if RFileUDT > 10 then
                UpdateFileAge (LocFileFull, RFileUDT) ;
        end ;
    finally
        FNoopTimer.Enabled := False;                  { V8.71 }
        ResInfRecs.Free ;
        if Assigned (LocalStream) then  // 30 Dec 2007 ensure file never left open
        begin
            LocalStream.Free;
            LocalStream := nil;
        end;
{$IFDEF Zipping}
        if fZipped then
        begin
            if Assigned (MyArchiveStream) then
                MyArchiveStream.Free ;
            if Assigned (MyZipFile) then
                MyZipFile.Free ;
        end ;
{$ENDIF}
    end ;
end ;

function TIcsFtpMulti.FtpUpOneFile (const LocFileFull, RemTarDir, RemTarFile: string; Replopt: TIcsFileCopyRepl) : TIcsTaskResult ;
var
    code, fulltarname, remdir: string ;
    flag: boolean ;
    SrcFSize, RFSize: Int64 ;
    SrcFileUDT, RFileUDT: TDateTime ;
    retval, loop: integer;
    OldWow64: Boolean ;        // 22 May 2013
begin
    result := TaskResFail ;
    fReqResponse := '' ;
    fCancelFlag := false ;
    IcsCopyProgClearAll (fCopyProg) ;  // 22 May 2013 clear all progress stuff
    if NOT fLoggedIn then
    begin
        result := TaskResFail ;
        fReqResponse := 'Must Login to Server' ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
        exit ;
    end ;
    if NOT Connected then
    begin
        result := TaskResFail ;
        fReqResponse := 'No Connection to Server, Cancelled Uploading Files (1)' ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
        exit ;
    end ;

    OldWow64 := false ; // 22 May 2013
    if fWow64RedirDisable then DisableWow64Redir (OldWow64) ; // 22 May 2013
    try

     // make sure trailing slash for directory
        remdir := RemTarDir ;
        remdir := IncludeTrailingUnixDelimiter (remdir) ;
        fulltarname := remdir + RemTarFile ;

    // 14 Sept 2008 skip Unicode file with non-ANSI characters unlesss UTF8 enabled
        if (Pos ('?', LocFileFull) > 0) then
        begin
            doCopyEvent (LogLevelDiag, 'Skipped Inaccessible Unicode Name: ' + LocFileFull) ;
            doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + fulltarname + '|0|0|1|Inaccessible Unicode File|0|0') ;
            exit ;
        end ;

        doCopyEvent (LogLevelFile, 'Check Exists ' + LocFileFull) ;
        if NOT IcsGetUAgeSizeFile (LocFileFull, SrcFileUDT, SrcFSize) then
        begin
            result := TaskResFail ;
            fReqResponse := 'Can Not Find File: ' + LocFileFull ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
            exit ;
        end ;

     // add base directory to front of file directory, change to it, if needed
        if remdir [1] = '/' then
            remdir := fServRootDir + Copy (remdir, 2, 999)
        else
            remdir := fServRootDir + remdir ;

    // 22 May 2013 set session total for single file
        fCopyProg.SessStartTick := IcsGetTickCount64 ;    { V8.71 }
        fCopyProg.TotProcBytes := SrcFSize ;
        fCopyProg.TotDoneNr := 1 ;
        fCopyProg.TotProcFiles := 1 ;
        fCopyProg.ProcBytesDone := 0 ;

    // check if remote file available, get size and UTC time stamp, see if replacing it
        flag := FtpCheckFile (remdir, RemTarFile, RFSize, RFileUDT) ;
        if flag and (replopt <> FCReplAlways) then
        begin
            flag := IcsCheckReplace (replopt, true, OneSecondDT * 2, SrcFSize, RFSize, SrcFileUDT, RFileUDT) ;
            if fDispFiles then code := '; Src=' + DateTimeToStr (SrcFileUDT) + '; Tar=' + DateTimeToStr (RFileUDT) ;
            if NOT flag then
            begin
                result := TaskResOKNone ;
                fReqResponse := 'Upload Skipped: ' + LocFileFull ;
                doCopyEvent (LogLevelInfo, fReqResponse + code) ;
                doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + fulltarname + '|0|0|1|Upload Skipped|0|0') ;
                exit ;
            end ;
            doCopyEvent (LogLevelDiag, 'Upload Not Skipped: ' + LocFileFull + code) ;
        end ;
        if fCancelFlag then exit ;

    // FTP upload one file, limit maximum attempts
        for loop := 1 to (fFailRepeat + 1) do  // 31 Dec 2007
        begin
            fCopyProg.ProgMessBase := 'Uploading File' ;
            retval := IntUpOne (locfilefull, remdir, RemTarFile, SrcFSize, SrcFileUDT) ;
            doCopyEvent (LogLevelProg, '') ;
            if retval = 0 then
            begin
                result := TaskResOKNew ;
                exit ;
            end ;
            if NOT Connected then  // 11 Aug 2005
            begin
                result := TaskResFail ;
                fReqResponse := 'No Connection to Server, Cancelled Uploading Files (2)' ;
                doCopyEvent (LogLevelInfo, fReqResponse) ;
                exit ;
            end ;
            if fCancelFlag then
            begin
                result := TaskResAbort ;
                fReqResponse := 'Cancelled Uploading Files' ;
                doCopyEvent (LogLevelInfo, fReqResponse) ;
                exit ;
            end ;
            if NOT (retval in [2, 3]) then exit ;  // fail download or fail MD5
            if (loop < (fFailRepeat + 1)) then   // 31 Dec 2007
                doCopyEvent (LogLevelInfo, 'Repeating Upload: ' + locfilefull) ;
        end ;
        fCancelFlag := false ;
    finally
        if fWow64RedirDisable then RevertWow64Redir (OldWow64) ; // 22 May 2013
    end;
end ;

// FTP upload multiple local files
// returns false if error, with fReqResponse completed

function TIcsFtpMulti.FtpUpload (const CheckFiles: boolean): TIcsTaskResult ;
var
    newfname, fnamesrc, cursrcdir, newtardir: string ;
    ret: boolean ;
    tempdir, info, newsubdirs, basesrcdir: String ;
    I, J, nodeltot, loop, retval, duration: integer ;
    newsize, totsize, delsize, SrcFSize: Int64 ;
    SrcFileUDT: TDateTime ;
    SrcFileRec: PTIcsFDirRec ;
    OldWow64: Boolean ;        // 22 May 2013
    listing: TIcsStringBuild ; // 22 May 2013
    CopyOnlyList: TStringList ;       { V8.70 }
begin
    fCancelFlag := false ;
    IcsCopyProgClearAll (fCopyProg) ;  // 22 May 2013 clear all progress stuff
    fSrcDir := ExcludeTrailingBackslash (fSrcDir) ;
    CopyOnlyList := Nil;             { V8.71 }
    if fSpecificFiles then
    begin
        CopyOnlyList := TStringList.Create ;
    end;
    OldWow64 := false ; // 22 May 2013
    if fWow64RedirDisable then DisableWow64Redir (OldWow64) ; // 22 May 2013
    listing := TIcsStringBuild.Create ;

    try  // finally

        // 8 Apr 2009 - fSrcFName may include directories and masks - yyyy-mm"/D"dd"/*.zip"
        newsubdirs := '' ;
        newfname := fSrcFName ;
        if (NOT fSpecificFiles) and fMask then
        begin
            newfname := IcsPathUnixToDos (newfname) ;
          // warning - GetMaskedName currently ANSI only in Delphi 7
            newfname := IcsGetMaskedName (newfname, fPrev, fLocalHost) ;
            newsubdirs := ExtractFilePath (newfname) ;  // DOS delims
            if Length (newsubdirs) > 0 then
            begin
                if newsubdirs [1] = '\' then newsubdirs := Copy (newsubdirs, 2, 999) ;
            end;
            newfname := ExtractFileName (newfname) ;
        end ;
        if fSpecificFiles then newfname := '*.*' ;

        // build list of source files on PC - before accessing server
        basesrcdir := ExcludeTrailingPathDelimiter (fSrcDir) ;
        if fMaskLocDir and (newsubdirs <> '') then  // 8 Apr 2009 add sub-directories
        begin
            basesrcdir := IncludeTrailingPathDelimiter (basesrcdir) +
                                            ExcludeTrailingPathDelimiter (newsubdirs) ;
        end;
        doCopyEvent (LogLevelFile, 'Source Directory: ' + basesrcdir) ;
        fIcsFileCopy.Wow64RedirDisable := fWow64RedirDisable ;  // 22 May 2013
        fIcsFileCopy.GetDirList (basesrcdir, newfname, fCopyType, fSubDirs, 0, 0, SrcFiles, SrcFileList, fEmptyDirs) ; // 17 Feb 2011) ;
        TotSrcFiles := SrcFileList.Count ;
        if TotSrcFiles = 0  then
        begin
            result := TaskResOKNone ;
            fReqResponse := 'No Source Files Found' ;
            exit ;
        end ;

        result := TaskResOKNone ;
        MessagePump ; // 15 Sept 2010 needed to support MultiThreaded
        if fCancelFlag then exit ;

        // don't delete target files unless processing full directories
        if fDelOldTar then
        begin
            if NOT (fCopyType in [FCTypeArchDir, FCTypeAllDir]) then
            begin
                doCopyEvent (LogLevelInfo, 'Delete Old Target Files only available when using Copy Type All Directory') ;
                fDelOldTar := false ;
            end ;
        end ;
        if fDispLDir then doCopyEvent (LogLevelInfo, 'Source Files on PC:' + IcsCRLF + IcsFmtFileDirList (SrcFileList, false)) ;

        // logon to FTP server
        doCopyEvent (LogLevelInfo, 'Connecting to FTP Server: ' + IcsFmtIpv6Addr (fHostName)) ;
        result := FtpLogon ;
        if result <> TaskResOKNew then exit ;
        doCopyEvent (LogLevelInfo, 'Succesfully logged onto FTP Server: ' + fUserName) ;
        Syst ;  // not sure if this is useful
        doCopyEvent (LogLevelInfo, Copy (LastResponse, 5, 999)) ;

        // 8 Apr 2009 base directory already set in FtpLogon, optionally add masked sub directories
        if fMaskRemDir and (newsubdirs <> '') then
        begin
            fServBaseDir := IcsPathDosToUnix (fServBaseDir + newsubdirs) ;
            fServBaseDir := IncludeTrailingUnixDelimiter (fServBaseDir) ;
        end;

        // build list of target files, so we don't copy unnecessary stuff
        // ignore errors, there may be nothing on server, yet
        doCopyEvent (LogLevelInfo, 'Checking files already on FTP Server: ' + fServBaseDir) ;
        result := FtpDir (TarFiles, TarFileList, fEmptyDirs) ;  // 17  Feb 2011
        if result = TaskResAbort then exit ;
        TotTarFiles := TarFileList.Count ;

        // 11 Aug 2004, stop if can not list remote files
        // 15 Sept 2008 don't stop, root may not exist
        if result = TaskResFail then
        begin
            doCopyEvent (LogLevelInfo, fReqResponse) ;
            doCopyEvent (LogLevelInfo, 'Failed to List Files on FTP Server') ;
        end ;
        fReqResponse := '' ;
        if fDispRDir then doCopyEvent (LogLevelInfo, 'Target Files on FTP Server:' + IcsCRLF + IcsFmtFileDirList (TarFileList, false)) ;
        MessagePump ; // 15 Sept 2010 needed to support MultiThreaded
        if fCancelFlag then exit ;

        // compare source and target files, see what to copy
        fCopyProg.TotProcFiles := fIcsFileCopy.SelectCopyFileList (SrcFileList, TarFileList, newfname, fCopyType,
            fRepl, fDiffStampMins, true, fIgnoreFileExt, fCopyProg.SkippedFiles, false, fIgnorePaths) ;  // 22 May 2013 added IgnorePaths
        if fCancelFlag then exit ;

        // see if only copying a list of specific files, deselect any we don't need
        if fSpecificFiles then
        begin
        {$IFDEF COMPILER10_UP}   { only supported D2006 and later }
            CopyOnlyList.Delimiter := '|' ;
            CopyOnlyList.StrictDelimiter := True;
            CopyOnlyList.DelimitedText := AnsiLowerCase (fSrcFName) ;
            CopyOnlyList.Sort ;
            CopyOnlyList.Sorted := true ;
        {$ENDIF}
            if CopyOnlyList.Count = 0 then
            begin
                result := TaskResOKNone ;
                fReqResponse := 'No Specific Source Files Selected to Upload' ;
                exit ;
            end ;
            for I := 0 to Pred (TotSrcFiles) do
            begin
                SrcFileRec := SrcFileList [I] ;
                with SrcFileRec^ do
                begin
                    if FrFileCopy = FCStateSelect then
                    begin
                        if NOT CopyOnlyList.Find (AnsiLowerCase (FrFileName), J) then
                        begin
                             FrFileCopy := FCStateNone ;
                             dec (fCopyProg.TotProcFiles) ;
                        end ;
                    end ;
                end ;
            end ;
        end ;

        // see if deleting old target files no longer in source directories
        if fDelOldTar and (TotTarFiles <> 0) then
        begin
            fCopyProg.TotDelFiles := fIcsFileCopy.SelectCopyFileList (TarFileList, SrcFileList, '*.*',
                FCTypeAllDir, FCReplNever, 0, false, fIgnoreFileExt, nodeltot, false, fIgnorePaths) ;  // 22 May 2013 added IgnorePaths
        end ;
        if (fCopyProg.TotProcFiles = 0) and (fCopyProg.TotDelFiles = 0) then  // 22 Feb 2008 chek if need to delete some files
        begin
            result := TaskResOKNone ;
            if fCopyProg.SkippedFiles <> 0 then
                fReqResponse := 'All Source Files Skipped Upload'
            else
                fReqResponse := 'No Source Files Selected to Upload' ;
            exit ;
        end ;
        MessagePump ; // 15 Sept 2010 needed to support MultiThreaded
        if fCancelFlag then exit ;

        // find size of stuff to copy
        delsize := 0 ;
        info := '' ;
        if fDelOldTar and (fCopyProg.TotDelFiles <> 0) then
        begin
            if CheckFiles then
            begin
                listing.Capacity (TotSrcFiles * 50) ;   // no real idea yet
                listing.AppendLine ('Old Files Selected for Deletion are: ') ;
            end;
            for I := 0 to Pred (TotTarFiles) do
            begin
                SrcFileRec := TarFileList [I] ;
                with SrcFileRec^ do
                begin
                    if FrFileCopy = FCStateSelect then
                    begin
                        inc (delsize, FrFileBytes) ;
                        if CheckFiles then
                        begin
                             if ((FrFileAttr and faDirectory) = faDirectory) then   // 21 Feb 2011 display directory
                                listing.AppendLine (FrFullName + IcsSpace + sDirLit)
                            else
                                listing.AppendLine (FrFullName + ', Size ' + IcsInt64ToCStr (FrFileBytes)) ;
                        end;
                    end ;
                end ;
            end ;
        end ;
        if fCancelFlag then exit ;
        newsize := 0 ;
        if (fCopyProg.TotProcFiles > 0) then
        begin
            if CheckFiles then
            begin
                listing.Capacity (TotSrcFiles * 50) ;   // no real idea yet
                listing.AppendLine ('Files Selected for Uploading are: ') ;
            end;
            for I := 0 to Pred (TotSrcFiles) do
            begin
                SrcFileRec := SrcFileList [I] ;
                with SrcFileRec^ do
                begin
                    if FrFileCopy = FCStateSelect then
                    begin
                        newsize := newsize + FrFileBytes ;
                        if CheckFiles then
                        begin
                            if ((FrFileAttr and faDirectory) = faDirectory) then   // 21 Feb 2011 display directory
                                listing.AppendLine (FrFullName + IcsSpace + sDirLit)
                            else
                                listing.AppendLine (FrFullName + ', Size ' + IcsInt64ToCStr (FrFileBytes)) ;
                        end;
                    end ;
                end ;
            end ;
        end
        else
           if CheckFiles then listing.AppendLine ('No Source Files Selected to Upload') ;
        if CheckFiles then
        begin
            info := listing.GetString ;
            doCopyEvent (LogLevelInfo, info) ;
            info := '' ;
        end;
        doCopyEvent (LogLevelInfo, 'Source Files Skipped ' + IntToStr (fCopyProg.SkippedFiles)) ;
        info := 'Selected Total Files ' + IntToStr (fCopyProg.TotProcFiles) + ', Total size ' + IntToKByte (newsize, true) ;
        if fCopyProg.TotDelFiles <> 0 then
            info := info + IcsCRLF + 'Old Files to Delete ' + IcsIntToCStr (fCopyProg.TotDelFiles) + ', Total size ' + IntToKByte (delsize, true) ;
        fCopyProg.TotProcBytes := newsize ;    // total size of all files
        fCopyProg.TotDelBytes := delsize ;
        doCopyEvent (LogLevelInfo, info) ;

        // stop now if only checking what will be uploaded - test only
        if CheckFiles then
        begin
            result := TaskResOKNone ;
            exit ;
        end ;
        doCopyEvent (LogLevelInfo, 'Started FTP Upload to: ' + IcsFmtIpv6Addr (fHostName)) ;
        MessagePump ; // 15 Sept 2010 needed to support MultiThreaded
        if fCancelFlag then exit ;

        // see if deleting old target files first before uploading files
        if fDelOldTar and (fCopyProg.TotDelFiles <> 0) then
        begin
            doCopyEvent (LogLevelInfo, 'Deleting Old Remote Files: ' + fTarDir) ;
            cursrcdir := '.,.,.' ;  // illegal
            for I := 0 to Pred (TotTarFiles) do
            begin
                MessagePump ; // 15 Sept 2010 needed to support MultiThreaded
                if fCancelFlag then exit ;
                if NOT Connected then exit ;
                SrcFileRec := TarFileList [I] ;
                with SrcFileRec^ do
                begin
                    if FrFileCopy <> FCStateSelect then continue ;

                // 14 Sept 2008 skip Unicode names with substitution characters
                    if (Pos ('?', FrFullName) > 0) then
                    begin
                        doCopyEvent (LogLevelDiag, 'Skipped Inaccessible Unicode Name: ' + FrFullName) ;
                        doCopyEvent (LogLevelDelimFile, '|' + FrFullName + '|0|0|0|Inaccessible Unicode File|0|0') ;
                        FrFileCopy := FCStateFailed ;
                        inc (fCopyProg.ProcFailFiles) ;
                        continue ;
                    end ;
                    if FrSubDirs [1] = '/' then      // remove leading slash
                        tempdir := fServBaseDir + Copy (FrSubDirs, 2, 200)
                    else
                        tempdir := fServBaseDir + FrSubDirs ;
                    if cursrcdir <> tempdir then
                    begin
                        cursrcdir := tempdir ;
                        HostDirName := cursrcdir ;
                        HostFileName := '' ;
                        LocalFileName := '' ;
                        ret := Cwd ;  // change working directory
                        if ret then ret := Pwd ;
                        if NOT ret then
                        begin
                            result := TaskResFail ;
                            fReqResponse := 'Can Not Change to Remote Directory' ;
                            continue ;
                        end ;
                        fCurRemDir := HostDirName ;
                    end ;
                    HostFileName := FrFileName ;
                    doCopyEvent (LogLevelFile, 'Deleting: ' + FrFullName) ;
                    ret := Dele ;   // delete remote file
                    if ret then
                    begin
                        doCopyEvent (LogLevelDelimFile, '|' + FrFullName + '|0|0|0|Old Target File Deleted|0|0') ;
                        inc (fCopyProg.DelOKBytes, FrFileBytes) ;
                        FrFileCopy := FCStateOK ;
                        inc (fCopyProg.DelOKFiles) ;
                    end
                    else
                    begin
                        doCopyEvent (LogLevelInfo, 'FTP Delete Failed: ' + FrFullName + ' - ' +  LastResponse) ;
                        FrFileCopy := FCStateFailed ;
                        inc (fCopyProg.ProcFailFiles) ;
                    end ;
                end ;
            end ;

         // 22 Feb 2008 report now instead of after uploads, stop if no files to upload
            doCopyEvent (LogLevelInfo, 'Old target files deleted OK: ' + IcsIntToCStr (fCopyProg.DelOKFiles) +
                                            ', Total size deleted ' + IntToKByte (fCopyProg.DelOKBytes, true)) ;
            if fCopyProg.TotProcFiles = 0 then
            begin
                if fCopyProg.DelOKFiles > 0 then
                    result := TaskResOKNew
                else
                    result := TaskResFail ;
                fReqResponse := 'Files Deleted, No Source Files Selected to Upload' ;
                exit ;
            end ;
        end ;

        // start real FTP uploading
        cursrcdir := '.,.,.' ;  // illegal
        totsize := 0 ;
        LocalStream := Nil ;  // upload from files, not stream
        fCopyProg.SessStartTick := IcsGetTickCount64 ;      { V8.71 }
        for I := 0 to Pred (TotSrcFiles) do
        begin
            MessagePump ; // 15 Sept 2010 needed to support MultiThreaded
            if fCancelFlag then exit ;
            if NOT Connected then
            begin
                doCopyEvent (LogLevelInfo, 'Lost FTP Control Connection, Abandoning Uploads') ;
                exit ;
            end ;
            SrcFileRec := SrcFileList [I] ;
            with SrcFileRec^ do
            begin
                if FrFileCopy <> FCStateSelect then continue ;
                inc (fCopyProg.TotDoneNr) ;
                fnamesrc := FrFullName ; // warning - conversion from WideString to AnsiString here for Delphi 2007
                tempdir := FrSubDirs ;  // for source file, ditto
                if cursrcdir <> tempdir then
                begin
                    cursrcdir := tempdir ;
                    if tempdir [1] = '\' then      // remove leading slash
                        newtardir := fServBaseDir + Copy (tempdir, 2, 200)
                    else
                        newtardir := fServBaseDir + tempdir ;
                    newtardir := IcsPathDosToUnix (newtardir) ;
                end ;

            // 13 Nov 2008 skip Unicode file with non-ANSI characters
                if (Pos ('?', fnamesrc) > 0) then
                begin
                    doCopyEvent (LogLevelInfo, 'Skipped Inaccessible Unicode Name: ' + fnamesrc) ;
                    doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + FrFileName + '|0|0|1|Inaccessible Unicode File|0|0') ;
                    inc (fCopyProg.ProcFailFiles) ;
                    FrFileCopy := FCStateFailed ;
                    continue ;
                end ;

             // 21 Feb 2011 - if a directory, special handling
                if ((FrFileAttr and faDirectory) = faDirectory) then
                begin
                    SrcFSize := -1 ;
                end
                else

            // 15 Sept 2008 ensure it's not been deleted already, and get current size in case it's changed
                begin
                    doCopyEvent (LogLevelFile, 'Check Exists ' + fnamesrc) ;
                    if NOT IcsGetUAgeSizeFile (fnamesrc, SrcFileUDT, SrcFSize) then
                    begin
                        fReqResponse := 'Can Not Find File: ' + fnamesrc ;
                        doCopyEvent (LogLevelInfo, fReqResponse) ;
                        doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + FrFileName + '|0|0|1|Source File Not Found|0|0') ;
                        inc (fCopyProg.ProcFailFiles) ;
                        FrFileCopy := FCStateFailed ;
                        continue ;
                    end ;
                end ;

          // FTP upload file
                FrFileCopy := FCStateCopying ;
                retval := 0 ;
                for loop := 1 to (fFailRepeat + 1) do  // 31 Dec 2007
                begin
                    fCopyProg.ProgMessBase := 'Uploading File ' + IntToStr (fCopyProg.TotDoneNr) +
                                                                 ' of ' + IntToStr (fCopyProg.TotProcFiles) ;
                    retval := IntUpOne (fnamesrc, newtardir, FrFileName, SrcFSize, SrcFileUDT) ;
                    if retval = 0 then break ;
                    if NOT Connected then break ;
                    if fCancelFlag then exit ;
                    if NOT (retval in [2, 3]) then break ;  // fail download or fail MD5
                    if (StatusCode = 501) then break ;  // 19 Oct 2005 permissions or start failed
                    if (loop < (fFailRepeat + 1)) then   // 31 Dec 2007
                        doCopyEvent (LogLevelInfo, 'Repeating Upload: ' + FrFullName) ;
                end ;
                if retval = 0 then
                begin
                    inc (fCopyProg.ProcOKFiles) ;
                    inc (totsize, FrFileBytes) ;
                    FrFileCopy := FCStateOK ;
                end
                else
                begin
                    inc (fCopyProg.ProcFailFiles) ;
                    FrFileCopy := FCStateFailed ;
                end ;
            end ;
        end ;

        // done
        doCopyEvent (LogLevelProg, '') ;
        duration := IcsElapsedTicks64 (fCopyProg.SessStartTick) ;     { V8.71 }
        result := TaskResOKNone ;
        if fCopyProg.ProcFailFiles <> 0 then
            result := TaskResFail
        else
        begin
            if fCopyProg.ProcOKFiles <> 0 then result := TaskResOKNew ;
        end ;
        doCopyEvent (LogLevelInfo, 'Finished, files uploaded OK: ' + IcsIntToCStr (fCopyProg.ProcOKFiles) + ', failed: ' +
                            IcsIntToCStr (fCopyProg.ProcFailFiles) + ', skipped: ' + IcsIntToCStr (fCopyProg.SkippedFiles)) ;
        doCopyEvent (LogLevelDelimTot, fSrcDir + '|' + fTarDir + '|' + IntToStr (totsize) + '|' +
                            IntToStr (fCopyProg.ProcOKFiles) + '|' + IntToStr (fCopyProg.ProcFailFiles) + '|Totals|') ;
        doCopyEvent (LogLevelInfo, 'Total size uploaded ' + IntToKByte (totsize, true) + ', duration ' +
                                            IcsSecsToStr (duration div 1000) + ', average speed ' +
                                                        IntToKByte (IcsCalcSpeed (duration, totsize)) + '/sec') ;
    finally
        if fSpecificFiles then             { V8.71 }
            FreeAndNil (CopyOnlyList) ;    { V8.70 }
        if fLoggedIn and (NOT Connected) then  // 20 Aug 2005
        begin
            result := TaskResFail ;
            fReqResponse := 'No Connection to Server, Cancelled Uploading Files (3)' ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
        end
        else if fCancelFlag then
        begin
            result := TaskResAbort ;
            fReqResponse := 'Cancelled Uploading Files' ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
        end ;
        FtpLogoff ;

        doCopyEvent (LogLevelProg, '') ;
        fCancelFlag := false ;
        if fWow64RedirDisable then RevertWow64Redir (OldWow64) ; // 22 May 2013
        listing.Free ;
    end ;
end;

procedure TIcsFtpMulti.Cancel ;
begin
    doCopyEvent (LogLevelInfo, 'Cancel FTP Triggered') ;
    FNoopTimer.Enabled := False;                  { V8.71 }
    if Connected then
    begin
        if FState <> ftpAbort then AbortXferAsync ;  // 28 Dec 2007 don't repeatedly abort
        sysDelayX (200) ;  // 9 Dec 2007 - short delay before aborting xfer
    end;
    fCancelFlag := true ;
    fCopyProg.LastProgTick := 0 ; // force progress event  23 June 2006
end ;

// 23 Sept 2010 - threaded version

constructor TIcsFtpMultiThread.CreateThread;
begin
    inherited Create (true) ;  // suspended
    FreeOnTerminate := true ;
    FThreadEvent := Nil ;
    FAbort := false ;
    // warning - don't create components needing windows here, wrong thread
    FPort := 'ftp';
    FTransferMode := FtpTransModeZDeflate ;
    FNewOpts := 'MODE Z LEVEL 8';
    FProxyPort := 'ftp';
    FConnectionType := ftpDirect;
    FOptions := [ftpAcceptLF];
    FLocalAddr := '0.0.0.0';
    FClientIdStr := ftpClientId;
    fMaxAttempts := 3 ;
    fAttemptDelay := 5 ;
    fProgressSecs := 2 ;
    fUseCompression := false ;
    fResFailed := false ;
    fMinResSize := 65535 ;
    fFailRepeat := 3 ;
    fSpecificFiles := false ;
    fDispRemList := false ;
    fFtpType := FtpTypeNone ;
    fFtpSslVerMethod := ftpSslVerNone ;  // Apr 2015
    fFtpSslPort := '990' ;
    fZlibNoCompExt := '.zip;.rar;.7z;.cab;.lzh;.gz;.avi;.wmv;.mpg;.mp3;.jpg;.png;';
    fZlibMaxSize := 500000000 ;
    fMaxResumeAttempts := 10 ;
    fMagFtpOpts := [] ;
end;

procedure TIcsFtpMultiThread.CallThreadEvent ;  // called by Synchronise for main thread
var
    Cancel: boolean ;
begin
    if Assigned (FThreadEvent) then
    begin
        Cancel := false ;
        FThreadEvent (FLogLevel, FId, FInfo, Cancel) ;
        if Cancel then FAbort := true ;
    end;
//    if AbortFlag then FAbort := true ;
end;

procedure TIcsFtpMultiThread.LogEvent (LogLevel: TIcsCopyLogLevel ; Info: String ; var Cancel: boolean) ;
begin
    if FAbort then Cancel := true ;
    if (LogLevel = LogLevelInfo) or (LogLevel = LogLevelFile) or (LogLevel = LogLevelDiag) then
    begin
        if FLogmaskName <> '' then
            FBuffLogStream.WriteLine (FormatDateTime (ISODateLongTimeMask, Now) + IcsSpace + FId + ': ' + Info) ;
    end;
    if Assigned (FThreadEvent) then       { V8.63 }
//  if (LogLevel = LogLevelInfo) or (LogLevel = LogLevelFile) then  { V8.63 }
    begin
        FLogLevel := LogLevel ;
        FInfo := Info ;
        Synchronize (CallThreadEvent) ;
        if FAbort then Cancel := true ;
    end;
end ;

procedure TIcsFtpMultiThread.IcsLogEvent (Sender: TObject; LogOption: TLogOption; const Msg : String) ;
begin
    if FLogmaskName <> '' then
        FBuffLogStream.WriteLine (FormatDateTime (ISODateLongTimeMask, Now) + IcsSpace + FId + ': ' + Msg) ;
end ;

procedure TIcsFtpMultiThread.Execute;
var
    Cancel: boolean ;
begin
    IcsFTPMultiCli := TIcsFtpMulti.Create (Nil) ;
    IcsFTPMultiCli.CopyEvent := LogEvent ;
    if FLogmaskName <> '' then
    begin
        FBuffLogStream := TIcsBuffLogStream.Create (IcsFTPMultiCli, FLogmaskName, '', FileCPUtf8) ;  // Format mask for log file name
{$IFNDEF NO_DEBUG_LOG}
        FIcsLog := TIcsLogger.Create (Nil) ;
        FIcsLog.OnIcsLogEvent := IcsLogEvent ;
        IcsFTPMultiCli.IcsLogger := FIcsLog ;
        FIcsLog.LogOptions := [] ;
    //    FIcsLog.LogOptions := [loDestEvent] + LogAllOptInfo ;
{$ENDIF}
    end ;
    FAbort := false ;
    LogEvent (LogLevelInfo, 'FTP Thread Starting', Cancel) ;
    IcsFTPMultiCli.BulkMode := FBulkMode ;
    IcsFTPMultiCli.Multithreaded := true ;  // must use our own message pump
    IcsFTPMultiCli.LocalHost := FLocalHost ;
    IcsFTPMultiCli.HostName1 := FHostName1 ;
    IcsFTPMultiCli.HostName2 := FHostName2 ;
    IcsFTPMultiCli.UserName := FUserName ;
    IcsFTPMultiCli.Password := FPassword ;
    IcsFTPMultiCli.Port := FPort ;
    IcsFTPMultiCli.SocketFamily := FSocketFamily ; // March 2013
    IcsFTPMultiCli.SocketErrs := FSocketErrs ; // Nov 2016
    IcsFTPMultiCli.AttemptDelay := FAttemptDelay ;
    IcsFTPMultiCli.MaxAttempts := FMaxAttempts ;
    IcsFTPMultiCli.FailRepeat := FFailRepeat ;
    IcsFTPMultiCli.DataSocketSndBufSize := FDSocketSndBufSize ;
    IcsFTPMultiCli.DataSocketRcvBufSize := FDSocketRcvBufSize ;
    IcsFTPMultiCli.ConnectionType := FConnectionType ;
    IcsFTPMultiCli.SocksPort := FSocksPort ;
    IcsFTPMultiCli.SocksServer := FSocksServer ;
    IcsFTPMultiCli.ProxyPort := FProxyPort ;
    IcsFTPMultiCli.ProxyServer := FProxyServer ;
    IcsFTPMultiCli.SocksUsercode := FSocksUsercode ;
    IcsFTPMultiCli.SocksPassword := FSocksPassword ;
    IcsFTPMultiCli.Passive := FPassive ;
    IcsFTPMultiCli.HostType := FHostType ;
    IcsFTPMultiCli.XferMode := FXferMode ;
    IcsFTPMultiCli.CaseFile := FCaseFile ;
    IcsFTPMultiCli.DiffStampMins := FDiffStampMins ;
    IcsFTPMultiCli.Timeout := FTimeout ;
    IcsFTPMultiCli.DispLog := FDispLog ;
    IcsFTPMultiCli.DispFiles := FDispFiles ;
    IcsFTPMultiCli.DispRDir:= FDispRDir ;
    IcsFTPMultiCli.DispLDir:= FDispLDir ;
    IcsFTPMultiCli.UpArchDir := FUpArchDir ;
    IcsFTPMultiCli.UpArchive := FUpArchive ;
    IcsFTPMultiCli.ResFailed := FResFailed ;
    IcsFTPMultiCli.DispRemList := FDispRemList ;
    IcsFTPMultiCli.BulkMode := FBulkMode ;
    IcsFTPMultiCli.SrcDir := FSrcDir ;
    IcsFTPMultiCli.SrcFName := FSrcFName ;
    IcsFTPMultiCli.TarDir := FTarDir ;
    IcsFTPMultiCli.CopyType := FCopyType ;
    IcsFTPMultiCli.DelDone := FDelDone ;
    IcsFTPMultiCli.DelOldTar := FDelOldTar ;
    IcsFTPMultiCli.SubDirs := FSubDirs ;
    IcsFTPMultiCli.Mask := FMask ;
    IcsFTPMultiCli.Prev := FPrev ;
    IcsFTPMultiCli.Repl := FRepl ;
    IcsFTPMultiCli.ReplRO := FReplRO ;
    IcsFTPMultiCli.Safe := FSafe ;
    IcsFTPMultiCli.IgnoreFileExt := FIgnoreFileExt ;
    IcsFTPMultiCli.Options := FOptions ;
    IcsFTPMultiCli.BandwidthLimit := FBandwidthLimit ;
    IcsFTPMultiCli.MagFtpOpts := FMagFtpOpts ;
    IcsFTPMultiCli.NoProgress := FNoProgress ;
    IcsFTPMultiCli.EmptyDirs := fEmptyDirs ;
    IcsFTPMultiCli.SslSessCache := fSslSessCache ;          // 20 Apr 2015
    IcsFTPMultiCli.FtpSslPort := fFtpSslPort ;              // 20 Apr 2015
    IcsFTPMultiCli.FtpSslVerMethod := fFtpSslVerMethod ;    // 20 Apr 2015
    IcsFTPMultiCli.FtpSslRootFile := fFtpSslRootFile ;      // 20 Apr 2015
    IcsFTPMultiCli.FtpSslRevocation := fFtpSslRevocation ; // 20 Apr 2015
    IcsFTPMultiCli.FtpSslReportChain := fFtpSslReportChain ;// 20 Apr 2015
    IcsFTPMultiCli.FtpSslCliSecurity := fFtpSslCliSecurity;   // V8.63
    IcsFTPMultiCli.FtpType := fFtpType;                       // V8.63
    IcsFTPMultiCli.IgnorePaths := fIgnorePaths;               // V8.63
    IcsFTPMultiCli.NoopIntervalMins := FNoopIntervalMins;       { V8.71 }
    FDirListing := '' ;
    case FtpThreadOpt of
        ftpthdList: FTaskRes := IcsFTPMultiCli.DispFtpDir (FDirListing) ;
        ftpthdDownCheck: FTaskRes := IcsFTPMultiCli.FtpDownload (true) ;
        ftpthdDownFiles: FTaskRes := IcsFTPMultiCli.FtpDownload (false) ;
        ftpthdUpCheck: FTaskRes := IcsFTPMultiCli.FtpUpload (true) ;
        ftpthdUpFiles: FTaskRes := IcsFTPMultiCli.FtpUpload (false) ;
    end ;
    FTotProcFiles := IcsFTPMultiCli.TotProcFiles ;
    FProcOKFiles := IcsFTPMultiCli.ProcOKFiles ;
    FDelOKFiles := IcsFTPMultiCli.DelOKFiles ;
    FProcFailFiles := IcsFTPMultiCli.ProcFailFiles ;
    FReqResponse := IcsFTPMultiCli.ReqResponse ;
    FSkippedFiles := IcsFTPMultiCli.SkippedFiles ;
    FInfo := 'FTP Thread Done, Task Result: ' + IcsGetTaskResName (FTaskRes) + ' - ' + IcsFTPMultiCli.ReqResponse + IcsCRLF ;
    LogEvent (FLogLevel, FInfo, Cancel) ;
    if FLogmaskName <> '' then FBuffLogStream.Free ;
{$IFNDEF NO_DEBUG_LOG}
    if Assigned (FIcsLog) then FIcsLog.Free ;
{$ENDIF}
    IcsFTPMultiCli.Free ;
end ;

{$ENDIF USE_SSL}

{$ENDIF MSWINDOWS}

end.
