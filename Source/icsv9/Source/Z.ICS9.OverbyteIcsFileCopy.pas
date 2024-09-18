{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Author:       Angus Robertson, Magenta Systems Ltd
Description:  TIcsFileCopy allows indexing, copying and deleting of multiple
              file directories, using a single function call.
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


TIcsFileCopy allows indexing, copying and deleting of multiple file
directories, using a single function call.  The component handles listing
source and destination files, and comparing them to avoid unnecessary
transfers, selection using a file mask, deletion of old files, delete after
copy, safe copy (to avoid partial files), and zip before copying or unzip
after copy.  A progress event provides various levels of information for
logging or display, depending upon application requirements, and allows
copying to be cancelled. The component is equally useful for copying single
files or entire volumes with hundreds of thousands of files, and provides
'sync files' functionality making it ideal for backups, only copying changed
files and deleting old target files.


Maximum Path Length Limitation
In editions of Windows before Windows 10 version 1607, the maximum length for a
path is MAX_PATH, which is defined as 260 characters.
To specify an extended-length path, use the "\\?\" prefix. For example, "\\?\D:\very long path".

In later versions of Windows, changing a registry key or using the Group Policy tool is
required to remove the limit.
Computer\HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\FileSystem\LongPathsEnabled
(Type: REG_DWORD) must exist and be set to 1.
The application manifest must also include: <ws2:longPathAware>true</ws2:longPathAware>


Main functions:

BuildDirList2 - build file directory, not sorted, date range selection
BuildDirList - build file directory, not sorted
GetDirList - build file directory, sorted
DispLocFiles - build formatted file directory, sorted
SelectCopyFileList - select files to copy by comparing source and target directories
SelCopyFiles - select and copy multiple files, optionally multiple directories
SelectFiles - select multiple files only
CopyFiles - copy multiple files only
DeleteFiles - delete multiple files
DeleteEmptyDirs - delete empty directories
CopyOneFile - copy single file
Cancel - cancel copying


All these functions make use of file directories comprising dynamic arrays
of TIcsFileRec.  These arrays may be used in applications, where individual
access to files is required.

08 May 2001 - baseline
28 May 2001 - fixed single dir never copying anything (recent bug)
6 June 2001 - added ReplyRO to replace read only files, set some errors for single dir
8 June 2001 - added Fr to TIcsFileRec elements so names are unique, using FindFirstFile
11 June 2001 - should compile with D6
25 June 2001 - added LogLevelDelimFile and LogLevelDelimTot to return info
                 for each file suitable for further processing
29 June 2001 - fixed problem with fMask not getting all files to check
24 July 2001 - moved TIcsFindList to magsub4
30 Oct 2001  - only return true if one or more files copied
11 Dec 2001  - added TIcsTaskResult to distinguish results better
26 Dec 2001  - added zipping/unzipping of copies (quite messy, TypeSrcDirs not done yet)
7  Jan 2002  - BuildDirList now returns mixed case directory names as well as file names
21 Feb 2002  - moved FileTimeToDateTime and DateTimeToFileTime to magsubs4
25 Mar 2002  - added MaxFNameLen to improve directory listings (not for zip yet)
06 Aug 2002  - added TaskRunning to TIcsTaskResult
4 Sept 2002  - added IgnoreFileExt - list is tmp;ftp;xxx;etc
8 Oct 2002   - check for file mask in multiple directories in copy type masked
             - no longer using trailing slashes on directories internally, still seems to work
14 Oct 2002  - added FrFileUDT UTC file time stamps to TIcsFileRec record
15 Oct 2002  - fix bug always replacing masked files
9 Dec 2002   - added UseUTC (def true) to use UTC time for directory listings and selections
30 Jan 2003  - ensure source file deleted if it was zipped before copy
24 Apr 2003  - delete empty directories are deleting all files from them
               added CheckDirAny function (true if any files in directory)
29 Apr 2003  - added CopyType FCTypeDates for date range, BuildDirList2 function
2 May 2003   - added DeleteFiles and DeleteEmptyDirs functions
12 May 2003  - added CopyOneFile function, added duration (ms) to LogLevelDelimFile events
30 May 2003  - if duration fast, change 0 to 10 to indicate it did something
11 June 2003 - call copy event every 1000 files during listing and selection,
                   check for cancel (resets DM dead man timer on large dirs)
18 July 2003 - corrected FCReplNewer (did not work if file size different)
24 Aug 2003  - ensure unzip directory exists, might help trap unzipping errors
               unzip errors in task log
22 Oct 2003  - added unzip error handler, VCLZIP 3 only
               added UpdateUFileAge
               single file copy working again
5  Jan 2004  - using magsubs1 for common stuff instead of magsubs4
               directory stuff now part of component to avoid event issues
               fixed copy callback event issue
11 Jan 2004  - made Delphi 5 compatible (no TStringList.CaseSensitive)
29 Jan 2004  - allow to delete RO files after copy, delete empty source directories
               copy progress event now shows counts and file names
               fixed range error on multiple dirs copying totalling over 2 gigs
6 Feb 2004   - added event comment if single file copy skipped
11 Feb 2004  - ensure BuildDirList2 dynamic array is initialised, otherwise do it
21 Feb 2004  - replaced TMask by Match for better masked file selection
19 Mar 2004  - don't delete root source or target directory after copying
25 Aug 2004  - added MagRenameFile and IcsDeleteFile to consolidate delete/rename read only files
               FCReplNewer replaces if target file is empty and source file not empty
               added CheckReplace to commonise date/size replace checking
11 Sept 2004 - GetDirList, SelectFiles and DeleteFiles now use SrcFName mask
                   for FCTypeDates as well as FCTypeMaskDir
30 July 2005 - TIcsFindList moved to magclasses.pas
14 Aug 2005  - added network connection to a remote system with a logon (before copying files)
                  RemoteName is UNC name, LocalName is mapped drive, UserName and Password
                  then Connect and Disconnect.  GetConnection to check for mapped drive
27 Aug 2005  - supporting files larger than 2 gigs, FrFileBytes is now Int64
               sDirLine shows more size, MaxFNameLen from 22 to 32
5 Sept 2005  - MagCheckReplace and CopyOneFile support 64-bits, not backward compatible!
             - prevent too rapid progress messages, which slow down transfers, ProgressSecs = 5
11 Nov 2005  - added TIcsSslCertCheck for FTP and HTTP
3  Nov 2006  - unit now MagentaCopy for ICS V6
03 Mar 2008  - added DelimActualSize which if non-zero is actual size relating to
                   duration, specifically during failed or partial FTP xfers
07 Aug 2008  - 2.3 - ensure lists set as sorted after sorting
               updated file copying to support Unicode/Widestring with Delphi 2009
               updated MaskMatches for unicode and performance (no string copying)
22 Sep 2008  - 3.0 - support Unicode file listing and copying with Delphi 2007 and earlier
               increased size of initial TIcsFDirRecs  array from 1000 to 5000 or 25000
                 if subdirs to avoid too many resizes causing copying of data
               FmtFileDirList builds listing with stream
18 Nov 2008  - 3.1 - fixed FmtFileDirList with D2009
7 Aug 2010   - 3.5 - fixed various string casts for D2009 and later
11 Feb 2011  - 3.7 - list and copy empty directories.  Directory functions have a ListDirs parameter,
               TIcsFileCopyW has a new EmptyDirs property, directory attributes should be copied
               Using FCTypeMaskDir and *.* now selects files without an extension
               added MultiThreaded property to stop Application.ProceessMessages being called
11 Aug 2011  - 3.7 - fix copying into new empty directory instead of next existing directory
               now registered in MagentaXferReg
20 Oct 2011  - 3.8 - most file sizes now reported in Kbytes. Mbytes, Gbytes instead of bytes
               added explict file name length errors (max 255 + drive/path)
24 Aug 2012 - 4.0 - updated to support ICS V8 with IPv6
11 Jun 2013 - 4.1 - added IgnorePaths to SelectCopyFileList and TIcsFileCopy to ignore files where source
                 has specific partial path, list is c:\temp;c:\temp2\;c:\temp3\;etc, or destination for
                 deletion matches partial path
              added Wow64RedirDisable property for Win64 allow all files to be copied correctly from Win32
              added new ProgressEvent which passes TIcsCopyProgress record updated for progress of
                  current file and session including total bytes copied allowing percentage progress display
              using TIcsStringBuild to build listings instead of TMemoryStream
              when checking copy, use TIcsStringBuild for improved listing performance
6 Mar 2017 - 4.6 - changed TULargeInteger to ULARGE_INTEGER to keep modern compilers happy
26 Mar 2019 - V8.60 - Adapted for main ICS packages and FMX support.
              Renamed TMagFileCopy to TIcsFileCopy.
              Most Types have Ics added, so: TTaskResult now TIcsTaskResult.
              No longer needs Forms.
              Using TStringList instead of StringArray
              Before creating directory check not a file of same name, delete it.
              Should build on Posix, not tested, also Delphi 7 again
2 Nov 2019  - V8.63 - IgnorePaths property now works with D7.
20 Feb 2020 - V8.64 - Using TFile.Copy class on MacOS (but no progress display).
              Removed MultiThreaded no longer used, TIcsWndControl version now used.
09 Dec 2020 - V8.65 - Remove hint with D7.
              Wow64 is windows only.
              BOOL to Boolean.
              Pending, lots of Windows only APIs need Posix conversion
              Renamed Ics.Posix.Messages.pas to Ics.Posix.PXMessages.pas.
21 Dec 2021 - V8.68 - DeleteFiles now supports Zipped property so files are zipped with .zip extension
                  before being deleted, useful for cleaning up old logs to save space.
              Moved IcsFileInUse and IcsTruncateFile to Utils.
11 Oct 2022 - V8.70 - Added support for indexing and copying file names longer than 259 characters by
                adding \\?\ to the start of long names passed to Windows APIs, but not logged.
              Removed VclZip support, replaced by TZipFile from System.Zip unit for D10 and later,
              Fixed a possible problem deleting empty directories after copying.
              Changed SelectCopyFileList DiffStamp to DiffStampMins for clarity.
              Fixed a problem selecting files where comparison was 0 instead of -1, too precise.
              Fixed a problem with BuildDirList2 with COMPILER16_UP where SearchRec.TimeStamp
                returns a UTC timestamp instead of local time, unlike SearchRec.Time with returns
                local time, so ignore it.  This caused problems with all file xfer components
                that could not check if old files were unchanged correctly.
              Removed platform warning with FileSetAttr.
22 Jun 2023 - V8.71 - Compiles with D7 again.
              Fixed an exception freeing stringslists after copying completed, Win64 only?
              Updated TIcsCopyProgress tick counters to Int64, added CurFileStartPos to allow
                 duration and speed of resumed transfers to be calculated correctly.
              When calculating file done percentage and speed for resumed transfers, only use
                actual transfer size not total size, so starting from 90% does seem very fast.
Aug 08, 2023 V9.0  Updated version to major release 9.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit Z.ICS9.OverbyteIcsFileCopy;
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
{$WARN SYMBOL_DEPRECATED OFF}                                  { V8.70 stop SearchRec.Time warning }
{$IFDEF BCB}
    {$ObjExportAll On}
{$ENDIF}

{$IFDEF COMPILER23_UP}      { TZipFile may work with XE2 and better, but not tested }
    {$DEFINE ZIPPING}       { V8.70 use System.Zip }
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
{$IFDEF POSIX}
    Posix.Time,
    Z.ICS9.Ics.Posix.WinTypes,
    Z.ICS9.Ics.Posix.PXMessages,
{$ENDIF}
    {$Ifdef Rtl_Namespaces}System.Classes{$Else}Classes{$Endif},
    {$Ifdef Rtl_Namespaces}System.Sysutils{$Else}Sysutils{$Endif},
    {$IFDEF Rtl_Namespaces}System.Masks{$ELSE}Masks{$ENDIF},
{$IFDEF COMPILER16_UP}
    System.IOUtils,
{$ENDIF}
{$IFDEF FMX}
    Z.ICS9.Ics.Fmx.OverbyteIcsWndControl,
    Z.ICS9.Ics.Fmx.OverbyteIcsWSocket,
    Z.ICS9.Ics.Fmx.OverbyteIcsBlacklist,
{$ELSE}
    Z.ICS9.OverbyteIcsWndControl,
    Z.ICS9.OverbyteIcsWSocket,
    Z.ICS9.OverbyteIcsBlacklist,
{$ENDIF FMX}
{$IFDEF MSWINDOWS}
    ShellAPI,
{$ENDIF MSWINDOWS}
    Z.ICS9.OverbyteIcsTypes,
    Z.ICS9.OverbyteIcsUtils,
    Z.ICS9.OverByteIcsFtpSrvT
    {$IFDEF Zipping}
    ,System.Zip            { V8.70 VclZip gone }
    {$ENDIF},
    Z.ICS9.OverbyteIcsTicks64;    { V8.71 }

const
    FileCopyCopyRight : String = ' TIcsFileCopy (c) 2023 V9.0 ';

{$IFDEF MSWINDOWS}        { V8.65 can not build for Posix too many errors }


{ex000616.log.zip      11,123,903  -rwx  23/03/2001 12:57:00    /dir}
{dvdimage.nrg           4,610,785,436    A 26/08/2005 16:01:49  \    }
const
    sDirLine = '%-40s %13s %4s %-20s %s' ;
    sDirLit = '<DIR>' ;
    FileTimeBase = -109205.0;
    FileTimeStep: Extended = 24.0 * 60.0 * 60.0 * 1000.0 * 1000.0 * 10.0; // 100 nSek per Day

    DelimSrcFile = 0 ; DelimTarFile = 1 ; DelimSize = 2 ; DelimSucc = 3 ;
    DelimFail = 4 ; DelimComm = 5 ; DelimDuration = 6 ; DelimActualSize = 7 ;
    DelimTotFields = 8 ;
    sLineEnd: array [0..1] of Char = (IcsCR, IcsLF) ;  // 21 May 2013
type
// file copy selection and replace options
    TIcsFileCopyType = (FCTypeSingle, FCTypeMaskDir, FCTypeArchDir,
                                                   FCTypeAllDir, FCTypeDates) ;

    TIcsFileCopyRepl = (FCReplNever, FCReplAlways, FCReplDiff, FCReplNewer) ;

    TIcsFileCopyState = (FCStateNone, FCStateIgnore,FCStateDir, FCStateSelect,
                                 FCStateCopying, FCStateOK, FCStateFailed) ;

    TIcsCopyLogLevel = (LogLevelInfo, LogLevelFile, LogLevelProg, LogLevelDiag,
                 LogLevelDelimFile, LogLevelDelimTot) ;

    TIcsTaskResult = (TaskResNone, TaskResOKNew, TaskResOKNone, TaskResFail,
                    TaskResAbort, TaskRunning) ;
    TIcsSslCertCheck = (SslCCNone, SslCCWarn, SslCCRequire) ;    // 11 Nov 2005
    {$IFDEF Zipping}
    TIcsZipExtFmt = (ExtnAdd, ExtnReplace) ;
    TIcsZipPath = (PathNone, PathNew, PathOriginal, PathNewOrig,
                PathSpecific, PathSpecOrig) ;
    TIcsZipType = (TypeUnzip, TypeSrcAddX, TypeSrcReplX, TypeSrcDirs) ;
    {$ENDIF}

{$IFDEF MSWINDOWS}
    TIsWow64Process = function (hProcess: THandle; var Wow64Process: Boolean): Boolean; stdcall;  // 14 Dec 2009
    TWow64DisableWow64FsRedirection = function (var Wow64FsEnableRedirection: Boolean): Boolean; stdcall;  // 17 May 2013
    TWow64RevertWow64FsRedirection = function (Wow64FsEnableRedirection: Boolean): Boolean; stdcall;       // 17 May 2013
{$ENDIF MSWINDOWS}

var
    IcsTaskResultNames: array [0..6] of string =
      ('No Result', 'OK New', 'OK None', 'Failed', 'Aborted', 'Running', '') ;
const
    IcsTaskResultStrings: array [Low(TIcsTaskResult)..High(TIcsTaskResult)] of String =         { V8.70 was shortstring }
      ('No Result', 'OK New', 'OK None', 'Failed', 'Aborted', 'Running') ;
    IcsSslCertCheckStrings: array [Low(TIcsSslCertCheck)..High(TIcsSslCertCheck)] of String =
      ('None', 'Warn', 'Require') ;

type
// file listing record - beware OverbyteIcsFtpSrvT has simplified TIcsFileRec
    TIcsFDirRec = record
        FrFileName: string ;    // filename and extension
        FrSubDirs: string ;     // \ for base directory, else located sub directories
        FrFullName: string ;    // basedir, subdirs, filename - complete path
        FrDirLevel: integer ;   // 0 for base directory, or level of sub dirs
        FrDirBaseLen: integer ; // length of basedir within FullName - used for display
        FrFileBytes: int64 ;    // size of file in bytes (FileSize is a function name) 27 Aug 2005 (was int)
        FrFileDT: TDateTime ;   // file time stamp - local time
        FrFileUDT: TDateTime ;  // file time stamp - UTC time
        FrFileAttr: integer ;   // bitmapped Windows file attributes
        FrExtra: string ;       // <DIR> or UNIX file attributes
        FrLinks: string ;       // not blank means this is not a real file
        FrFileCopy: TIcsFileCopyState ;  // file copy state, selected, done, etc,
    end ;
    TIcsFDirRecs = array of TIcsFDirRec ;   // lots of records
    PTIcsFDirRec = ^TIcsFDirRec ;           // pointer once record added to TList

// 20 May 2013 - file copying progress, current file and session, V8.71 updated tick counters to Int64
    TIcsCopyProgress = record
        LogLevel: TIcsCopyLogLevel ;  // log level
        Info: String ;                // simple display information
        ProgMessBase: String ;        // partial progess info
        CurSrcName: String ;          // current copy source file
        CurTarName: String ;          // current copy target file
        CurDelName: String ;          // current delete file
        CurOKDone: int64 ;            // bytes copied of current file
        CurFileBytes: Int64 ;         // size of file being copied
        CurFileStartPos: Int64;       // V8.71 resume transfer start position, needed for speed and duration calculation
        CurStartTick: Int64 ;         // when current file started copying or deleting in ticks (from when PC booted)
        CurEstimateTicks: Int64 ;     // current file estimated copying time based on first 5 seconds in ticks (1,000th/sec)
        CurDonePercent: integer ;     // current file percentage copied
        CurDoneTicks: Int64 ;         // current file copying duration in ticks
        TotProcFiles: integer ;       // total files to process in session
        TotDoneNr: integer ;          // how many files processed so far
        TotProcBytes: int64 ;         // total size of files to copy in session
        TotDelFiles: integer ;        // total number of files to delete
        TotDelBytes: int64 ;          // total size of files to delete in session
        SessStartTick: Int64 ;        // when session started in ticks (from when PC booted)
        SessEstimateTicks: Int64 ;    // session estimated time updated after each file in ticks (1,000th/sec)
        SessDonePercent: integer ;    // total files percentage copied
        SessDoneTicks: Int64 ;        // total files copying duration in ticks
        SessAvSpeed: integer ;        // total files average speed so far
        ProcOKFiles: integer ;        // total files copied OK in session
        DelOKFiles: integer ;         // total files deleted OK in session
        ProcFailFiles: integer ;      // total files failed copy in session
        SkippedFiles: integer ;       // total files skipped copy in session
        ProcBytesDone: int64 ;        // total size of files copied OK or failed in session
        ProcBytesLast: int64 ;        // total bytes copied up to start of current file
        ProcOKBytes: int64 ;          // total size of files copied OK in session
        DelOKBytes: int64 ;           // total size of files deleted in session
        LastProgTick: Int64 ;         // used to suppress progress messages for ProgressSecs
    end ;

// copying event, allowing main program to log and display stuff, and cancel
    TBulkCopyEvent = Procedure (LogLevel: TIcsCopyLogLevel ; Info: string ;
                                              var Cancel: boolean) of object ;
    PBulkCopyEvent = ^TBulkCopyEvent ;

    TProgressEvent = Procedure (Sender: TObject ; CopyProgress: TIcsCopyProgress ;     // 20 May 2013
                                                    var Cancel: boolean) of object ;
    PProgressEvent = ^TProgressEvent ;

    TThreadEvent = Procedure (LogLevel: TIcsCopyLogLevel ; const Id, Info: String ;
                                              var Cancel: boolean) of object ;      // 16 Feb 2011

// main multi file copy function
  TIcsFileCopy = class(TIcsWndControl) { V8.60 to use message pump}
  protected
    { Protected declarations }
        fCancelFlag: boolean ;
        fMultiDirList: String ;
        fSrcDir: String ;
        fSrcDirList: TStringList ;
        fSrcFName: String ;
        fTarDir: String ;
        fTarDirList: TStringList ;
        fCopyType: TIcsFileCopyType ;
        fMultiDir: Boolean ;
        fSubDirs: Boolean ;
        fDelDone: Boolean ;
        fDelOldTar: Boolean ;
        fMask: Boolean ;
        fPrev: Boolean ;
        fRepl: TIcsFileCopyRepl ;
        fReplRO: boolean ;
        fSafe: Boolean ;
        fLocalHost: string ;
        fCopyEvent: TBulkCopyEvent ;
        fReqResponse: string ;
        fIgnoreFileExt: string ;
        fUseUTC: Boolean ;
        fCopyLoDT: TDateTime ;
        fCopyHiDT: TDateTime ;
        fUserName: string;
        fPassword: string;
        fLocalName: string;
        fRemoteName: string;
        fRemConnected: string ;
        fProgressSecs: integer ;     // 5 Sept 2005
//        fMultiThreaded: boolean ;    // 16 Sept 2010, gone V8.64
        fEmptyDirs: boolean ;        // 7 Feb 2011
        fIgnorePaths: String ;       // 22 May 2013
        fCopyProg: TIcsCopyProgress ;   // 22 May 2013 replaces most fTotxx/fProcxx variables
        fWow64RedirDisable: boolean ; // 22 May 2013
        fProgressEvent: TProgressEvent ;  // 22 May 2013
     {$IFDEF Zipping}
        fZipDownDel: Boolean ;
        fZipped: Boolean ;
        fZipType: TIcsZipType ;
        fZipPath: TIcsZipPath ;
        fZipDir: String ;
        procedure ZipProgressEvent(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);   { V8.70 }
     {$ENDIF}

        procedure SetMultiDirList (Const Value: string) ;
        procedure SetSrcDirList (Value: TStringList) ;
        procedure SetTarDirList (Value: TStringList) ;
   //     procedure EndUnZipEvent (Sender: TObject; FileIndex: Integer; FName: String) ;         { V8.70 VclZip gone }
   //     procedure UnZipHandleMessage(Sender: TObject;
   //               const MessageID: Integer; const Msg1, Msg2: String;
   //               const flags: Cardinal; var Return: Integer) ;

  public
    { Public declarations }

    SrcFiles: TIcsFDirRecs  ;
    SrcFileList: TIcsFindList ;
    TarFiles: TIcsFDirRecs  ;
    TarFileList: TIcsFindList ;
    TotSrcFiles: integer ;
    TotTarFiles: integer ;
    CurProcFiles: integer ;
    CurSkippedFiles: integer ;
    CurDelFiles: integer ;

    constructor Create(Aowner:TComponent); override;
    destructor Destroy; override;
    procedure doCopyEvent (const LogLevel:TIcsCopyLogLevel ; const Info: string) ;
    function SelCopyFiles (const CheckFiles: boolean): TIcsTaskResult ;
    function SelectFiles (const CurSrcDir, CurTarDir: string): TIcsTaskResult ;
    function CopyFiles (const CurSrcDir, CurTarDir: string): TIcsTaskResult ;
    function DeleteFiles (const CheckFiles: boolean): TIcsTaskResult ;
    function DeleteEmptyDirs (RootDir: string; DirList: TStringList): integer ;
    function CopyOneFile (const Fnamesrc, Fnametar: string ;
        Replopt: TIcsFileCopyRepl; const Safe: boolean; var Fsize: Int64) : TIcsTaskResult ;
    procedure Cancel ;
    procedure ClearTarList ;
    procedure ClearSrcList ;
    function BuildDirList2 (LocDir, LocPartName: string; const SubDirs: boolean;
                Level, InitDLen: integer ; const LoDT, HiDT: TDateTime ; var TotFiles: integer;
                                var LocFiles: TIcsFDirRecs ; const ListDirs: boolean = false): boolean ;
    function BuildDirList (LocDir, LocPartName: string; const SubDirs: boolean;
                 Level, InitDLen: integer; var TotFiles: integer;
                    var LocFiles: TIcsFDirRecs ; const ListDirs: boolean = false): boolean ;
    function GetDirList (LocDir, LocPartName: string; const SelOpt: TIcsFileCopyType;
              const SubDirs: boolean; LoDT, HiDT: TDateTime ; var LocFiles: TIcsFDirRecs ;
                        var LocFileList: TIcsFindList; const ListDirs: boolean = false): integer ;
    function DispLocFiles (const SrcDir, SrcFile: string ;
              const CopyType: TIcsFileCopyType ; const SubDirs: boolean; const UTCFlag: boolean;
                                                const ListDirs: boolean = false): string ;
    function SelectCopyFileList (const SrcFileList, TarFileList: TIcsFindList; const fname: string;
              const selopt: TIcsFileCopyType; const replopt: TIcsFileCopyRepl; const DiffStampMins: integer;   { V8.70 was DiffStamp }
              const IgnoreOldTime: boolean; IgnoreExt: string ; var skipped: integer;
                                    const UTCFlag: boolean; const IgnorePaths: String = ''): integer ;
    function Connect: boolean ;
    function Disconnect (AForce: boolean): boolean ;
    function GetConnection (ALocalName: string): string ;

  published
    { Published declarations }

    property CancelFlag: boolean       read fCancelFlag     write fCancelFlag ;
    property MultiDirList: String      read fMultiDirList   write SetMultiDirList ;
    property SrcDir: String            read fSrcDir         write fSrcDir ;
    property SrcDirList: TStringList   read fSrcDirList     write SetSrcDirList ;
    property SrcFName: String          read fSrcFName       write fSrcFName ;
    property TarDir: String            read fTarDir         write fTarDir ;
    property TarDirList: TStringList   read fTarDirList     write SetTarDirList ;
    property CopyType: TIcsFileCopyType   read fCopyType       write fCopyType ;
    property MultiDir: Boolean         read fMultiDir       write fMultiDir ;
    property SubDirs: Boolean          read fSubDirs        write fSubDirs ;
    property DelDone: Boolean          read fDelDone        write fDelDone ;
    property DelOldTar: Boolean        read fDelOldTar      write fDelOldTar ;
    property Mask: Boolean             read fMask           write fMask ;
    property Prev: Boolean             read fPrev           write fPrev ;
    property Repl: TIcsFileCopyRepl       read fRepl           write fRepl ;
    property ReplRO: boolean           read fReplRO         write fReplRO ;
    property Safe: Boolean             read fSafe           write fSafe ;
    property LocalHost: string         read fLocalHost      write fLocalHost ;
    property ProgressSecs: integer     read fProgressSecs   write fProgressSecs ;   // 5 Sept 2005
    property CopyEvent: TBulkCopyEvent read fCopyEvent      write fCopyEvent ;
    property TotProcFiles: integer     read fCopyProg.TotProcFiles ;
    property ProcOKFiles: integer      read fCopyProg.ProcOKFiles ;
    property DelOKFiles: integer       read fCopyProg.DelOKFiles ;
    property ProcFailFiles: integer    read fCopyProg.ProcFailFiles ;
    property SkippedFiles: integer     read fCopyProg.SkippedFiles ;
    property ReqResponse: string       read fReqResponse ;
    property TotProcBytes: int64       read fCopyProg.TotProcBytes ;
    property ProcOKBytes: int64        read fCopyProg.ProcOKBytes ;
    property DelOKBytes: int64         read fCopyProg.DelOKBytes ;
    property IgnoreFileExt: string     read fIgnoreFileExt  write fIgnoreFileExt ;
    property UseUTC: Boolean           read fUseUTC         write fUseUTC ;
    property CopyLoDT: TDateTime       read fCopyLoDT       write fCopyLoDT ;
    property CopyHiDT: TDateTime       read fCopyHiDT       write fCopyHiDT ;
    property UserName: string          read fUserName       write fUserName ;
    property Password: string          read fPassword       write fPassword ;
    property LocalName: string         read fLocalName      write fLocalName ;
    property RemoteName: string        read fRemoteName     write fRemoteName ;
    property RemConnected: string      read fRemConnected ;
//  property MultiThreaded : Boolean   read fMultiThreaded  write fMultiThreaded;   // 16 Sept 2010 gone V8.64
    property EmptyDirs: Boolean        read fEmptyDirs      write fEmptyDirs ;  // 7 Feb 2011
    property IgnorePaths: String       read fIgnorePaths    write fIgnorePaths ;    // 22 May 2013
    property CopyProg: TIcsCopyProgress  read fCopyProg ;                             // 22 May 2013
    property Wow64RedirDisable: boolean read fWow64RedirDisable write fWow64RedirDisable ; // 22 May 2013
    property ProgressEvent: TProgressEvent read fProgressEvent write fProgressEvent; // 22 May 2013
{$IFDEF Zipping}
    property ZipDownDel: Boolean       read fZipDownDel     write fZipDownDel ;
    property Zipped: Boolean           read fZipped         write fZipped ;
    property ZipType: TIcsZipType      read fZipType        write fZipType ;
    property ZipPath: TIcsZipPath      read fZipPath        write fZipPath ;
    property ZipDir: String            read fZipDir         write fZipDir ;
{$ENDIF}
  end ;

// various public file handling and copying functions
// also used by OverbyteIcsFtpMulti and OverbyteIcsHttpMulti

function IcsGetMaskedName (const mask: string; const PrevFlag: boolean; const host: string): string ;
function IcsFmtFileDir (const FileRecs: TIcsFDirRecs ; const UTCFlag: boolean): string ;
function IcsFmtFileDirList (const LocFileList: TIcsFindList; const UTCFlag: boolean): string ;
function IcsCompareFNext (Item1, Item2: Pointer): Integer ;
function IcsAttrStr (const Attr: longword): string ;
function IcsGetTaskResName (TaskResult: TIcsTaskResult): string ;
function IcsCheckDirAny (LocDir: string): boolean ;
function IcsCheckReplace (const replopt: TIcsFileCopyRepl;
           const IgnoreOldTime: boolean; const AllowDiffDT: TDateTime;
            const SrcSize, TarSize: Int64; SrcDT, TarDT: TDateTime): boolean ;
procedure IcsCopyProgClearAll (var CopyProg: TIcsCopyProgress) ;    // 16 May 2013
procedure IcsCopyProgClearCur (var CopyProg: TIcsCopyProgress) ;    // 16 May 2013
procedure IcsCopyProgDuration (var CopyProg: TIcsCopyProgress; TicksPerFile: Longword = 50) ; // 16 May 2013
function IcsCalcSpeed (DurTicks, FSize: int64): int64 ;  // 22 May 2013 moved from MagentaFTP
function IsWin64: boolean ;   // 22 July 2011
function IsWow64: boolean ;   // 14 Dec 2009
function DisableWow64Redir (var OldRedir: boolean): boolean ;  // 17 May 2013
function RevertWow64Redir (OldRedir: boolean): boolean ;       // 17 May 2013
{$IFDEF Zipping}
function WinFileDateToDateTime(FileDate: UInt32; out DateTime: TDateTime): Boolean;     { V8.70 }
{$ENDIF}

var

    AppProcMessCount: integer = 0 ;
    AppProgressCount: integer = 0 ;
    MaxFNameLen: integer = 32 ;     // maximum file name length to display in listings

const
    AppProcMessMax = 50 ;  // how often to use Application.ProcessMessages, every 50 files
    AppProgressMax = 20 ;  // how often to report progress and check cancelled, 20*50=1000
    AppTicksPerFile = 20 ;   // 22 May 2013 millisecs to open file when calculation session duration
    AppDurationWait = 5000 ; // 22 May 2013 millisecs to wait before estimating duration and speed

{$ENDIF MSWINDOWS}

implementation

{$IFDEF MSWINDOWS}

{$IFDEF Zipping}
 { V8.70 borrowed from System.Zip because not made public }
 { similar to FileDateToDateTime which is Windows only, more error handlinh }
function WinFileDateToDateTime(FileDate: UInt32; out DateTime: TDateTime): Boolean;
var
  LDate: TDateTime;
  LTime: TDateTime;
begin
  Result := TryEncodeDate(
    LongRec(FileDate).Hi shr 9 + 1980,
    LongRec(FileDate).Hi shr 5 and 15,
    LongRec(FileDate).Hi and 31,
    LDate);

  if Result then
  begin
    Result := TryEncodeTime(
      LongRec(FileDate).Lo shr 11,
      LongRec(FileDate).Lo shr 5 and 63,
      LongRec(FileDate).Lo and 31 shl 1, 0, LTime);

    if Result then
      DateTime := LDate + LTime;
  end;
end;
{$ENDIF}

function IsWin64: boolean ;   // 3 August 2011
begin
{$IFDEF CPUX64}
    result := true ;
{$ELSE}
    result := false ;
{$ENDIF}
end ;

// 14 Dec 2009 are we running under a 64-bit windows OS
function IsWow64: boolean ;
{$IFDEF MSWINDOWS}
var
    IsWow64Process: TIsWow64Process;
    flag: Boolean;
{$ENDIF}
begin
    result := false ;
{$IFDEF MSWINDOWS}
    IsWow64Process := GetProcAddress (GetModuleHandle ('kernel32'), 'IsWow64Process') ;
    if Assigned(IsWow64Process) then
    begin
        flag := false ;  // warning, returns false for 64-bit application under 64-bit windows
        if IsWow64Process (GetCurrentProcess(), flag) then result := flag ;
    end ;
{$ENDIF}
end ;


// 17 May 2013, for Win32 apps on Win64 OS, disable WOW64 file system redirection, follow with RevertWow64Redir
function DisableWow64Redir (var OldRedir: boolean): boolean ;
{$IFDEF MSWINDOWS}
var
    Wow64DisableWow64FsRedirection: TWow64DisableWow64FsRedirection ;
{$ENDIF}
begin
    result := false ;
{$IFDEF MSWINDOWS}
    if IsWin64 then exit ;
    if NOT IsWow64 then exit ;
    Wow64DisableWow64FsRedirection := GetProcAddress (GetModuleHandle ('kernel32'), 'Wow64DisableWow64FsRedirection') ;
    if Assigned (Wow64DisableWow64FsRedirection) then
    begin
        result := Wow64DisableWow64FsRedirection (OldRedir) ;
    end ;
{$ENDIF}
end;

// 17 May 2013, for Win32 apps on Win64 OS, revert WOW64 file system redirection after DisableWow64Redir
function RevertWow64Redir (OldRedir: boolean): boolean ;
{$IFDEF MSWINDOWS}
var
    Wow64RevertWow64FsRedirection: TWow64RevertWow64FsRedirection ;
{$ENDIF}
begin
    result := false ;
{$IFDEF MSWINDOWS}
    if IsWin64 then exit ;
    if NOT IsWow64 then exit ;
    Wow64RevertWow64FsRedirection := GetProcAddress (GetModuleHandle ('kernel32'), 'Wow64RevertWow64FsRedirection') ;
    if Assigned (Wow64RevertWow64FsRedirection) then
    begin
        result := Wow64RevertWow64FsRedirection (OldRedir) ;
    end ;
{$ENDIF}
end;

// internal convert TFileTime to Int64

function FileTimeToInt64 (const FileTime: TFileTime): Int64 ;
begin
    Move (FileTime, result, SizeOf (result)) ;
end;

// internal convert Int64 to TFileTime

function Int64ToFileTime (const FileTime: Int64): TFileTime ;
begin
    Move (FileTime, result, SizeOf (result)) ;
end;

// convert TFileTime to TDateTime

function IcsFileTimeToDateTime(const FileTime: TFileTime): TDateTime;
begin
    Result := FileTimeToInt64 (FileTime) / FileTimeStep ;
    Result := Result + FileTimeBase ;
end;

// convert TDateTime to TFileTime

function IcsDateTimeToFileTime(DateTime: TDateTime): TFileTime;
var
  E: Extended;
begin
  E := (DateTime - FileTimeBase) * FileTimeStep;
  result := Int64ToFileTime (Round(E)) ;
end;


function IcsGetTaskResName (TaskResult: TIcsTaskResult): string ;
begin
    result := '' ;
    if TaskResult > TaskRunning then exit ;
    result := IcsTaskResultNames [Ord (TaskResult)] ;
end ;

// 16 May 2013 clear current file progress stuff

procedure IcsCopyProgClearCur (var CopyProg: TIcsCopyProgress) ;
begin
    with CopyProg do
    begin
        CurStartTick := 0 ;
        CurEstimateTicks := 0 ;
        CurSrcName := '' ;
        CurTarName := '' ;
        CurDelName := '' ;
        CurOKDone := 0 ;
        CurFileBytes := 0 ;
        CurFileStartPos := 0;   { V8.71 }
        LastProgTick := 0 ;
        CurDonePercent := 0 ;
        CurDoneTicks := 0 ;
    end;
end;

// 16 May 2013 clear all progress stuff

procedure IcsCopyProgClearAll (var CopyProg: TIcsCopyProgress) ;
begin
    IcsCopyProgClearCur (CopyProg) ;
    with CopyProg do
    begin
        Info := '' ;
        ProgMessBase := '' ;
        TotProcFiles := 0 ;
        ProcBytesDone := 0 ;
        TotProcBytes := 0 ;
        TotDelFiles := 0 ;
        TotDelBytes := 0 ;
        SessStartTick := 0 ;
        SessEstimateTicks := 0 ;
        ProcOKFiles := 0 ;
        DelOKFiles := 0 ;
        ProcFailFiles := 0 ;
        SkippedFiles := 0 ;
        ProcOKBytes := 0 ;
        DelOKBytes := 0 ;
        SessDonePercent := 0 ;
        SessDoneTicks := 0 ;
        SessAvSpeed := 0 ;
    end;
end;

// 22 May 2013  moved from MagentaFTP

function IcsCalcSpeed (DurTicks, FSize: int64): int64 ;
begin
    result := 0 ;
    if (FSize < 1000) or (DurTicks < 1000) then exit ;
    result := (1000 * FSize) div DurTicks;
end ;

// 16 May 2013 calculate file progress stuff

procedure IcsCopyProgDuration (var CopyProg: TIcsCopyProgress; TicksPerFile: Longword = 50) ;
var
    ActualSize, ActualDone: Int64;
begin
    with CopyProg do
    begin
        ActualSize := CurFileBytes - CurFileStartPos;
        ActualDone := CurOKDone - CurFileStartPos;
        if (CurStartTick > 0) and (CurFileBytes > 0) then
        begin
         // how long it's taken so far in millisecs
            CurDoneTicks := IcsElapsedTicks64 (CurStartTick) ;     { V8.71 }
         // percent based on file size, V8.71 percent now only shows resumed size not total size
            CurDonePercent := (ActualDone * 100) div ActualSize ;
         // estimate how long file will take to copy in millisecs
            if (CurDoneTicks >= AppDurationWait) and (CurDonePercent > 1) then
                    CurEstimateTicks := (CurDoneTicks * 100) div CurDonePercent ;
        end;
        if (SessStartTick > 0) and (TotProcBytes > 0) then
        begin
         // how long it's taken so far in millisecs
            SessDoneTicks := IcsElapsedTicks64 (SessStartTick) ;     { V8.71 }
         // percent based on total of all file sizes
            SessDonePercent := ((ProcBytesDone - CurFileStartPos) * 100) div TotProcBytes ;
            if (SessDoneTicks > AppDurationWait) and (SessDonePercent > 1) then
            begin
             // estimate how long session will take in millisecs, add extra for file opening overhead
                SessEstimateTicks := ((SessDoneTicks * 100) div SessDonePercent) + ((TotProcFiles - TotDoneNr) * Int64(TicksPerFile)) ;
             // average speed, allowing for file opening overhead
                SessAvSpeed := IcsCalcSpeed (SessDoneTicks, ProcBytesDone - CurFileStartPos) ;  // bytes/sec
            end;
        end;
    end;
end;

constructor TIcsFileCopy.Create(Aowner:TComponent);
begin
    inherited create(AOwner);
    fSrcDirList := TStringList.Create ;
    fTarDirList := TStringList.Create ;
    SrcFileList := TIcsFindList.Create ;
    TarFileList := TIcsFindList.Create ;
    ClearTarList ;
    ClearSrcList ;
    fCancelFlag := false ;
    fUseUTC := true ;
    fProgressSecs := 2 ;   // update progress every two seconds default
    IcsCopyProgClearAll (fCopyProg) ;  // 22 May 2013
end ;

destructor TIcsFileCopy.Destroy;
begin
{ V8.71 Ensure inherited destroy called }
    try
        ClearTarList ;
        ClearSrcList ;
        FreeAndNil(fSrcDirList) ;
        FreeAndNil(fTarDirList) ;
        FreeAndNil(SrcFileList) ;
        FreeAndNil(TarFileList) ;
    finally
        inherited Destroy;
    end;
end;

function TIcsFileCopy.Connect: boolean ;
var
    NetResource: TNetResource;
    dwFlags: DWORD;
begin
    result := false ;
{$IFDEF MSWINDOWS}
    NetResource.dwType := RESOURCETYPE_DISK ;
    NetResource.lpLocalName := PChar (FLocalName) ;   // ie N: for drive N
    NetResource.lpRemoteName := PChar (FRemoteName) ; // ie \\sysname\share UNC name
    NetResource.lpProvider := Nil ;
    dwFlags := 0 ;
    if WNetAddConnection2 (NetResource, PChar (FPassword), PChar (FUserName), dwFlags) <> NO_ERROR then
    begin
        fReqResponse := 'Failed to Connect to Remote Name: ' +
                              FRemoteName + '; Error: ' + SysErrorMessage (GetLastError) ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
    end
    else
    begin
        result := true ;
        fRemConnected := FRemoteName ;
        fReqResponse := 'Connected OK to Remote Name: ' + FRemoteName ;
        if FLocalName <> '' then fReqResponse := fReqResponse + ' as Drive ' + FLocalName ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
    end
{$ENDIF MSWINDOWS}
end;

function TIcsFileCopy.Disconnect (AForce: boolean): boolean ;
var
    Device: string;
begin
    result := false ;
{$IFDEF MSWINDOWS}
    if Trim (FLocalName) <> '' then
        Device := FLocalName
    else
        Device := FRemoteName;
    if WNetCancelConnection2 (PChar (Device), CONNECT_UPDATE_PROFILE, AForce) <> NO_ERROR then
    begin
        fReqResponse := 'Failed to Disconnect: ' + Device + '; Error: ' + SysErrorMessage (GetLastError) ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
    end
    else
    begin
        result := true ;
        fRemConnected := '' ;
        fReqResponse := 'Disconnected OK: ' + Device ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
    end
{$ENDIF MSWINDOWS}
end;

function TIcsFileCopy.GetConnection (ALocalName: string): string ;
var
    Buffer: array[0..1024] of Char;
    BufSize: DWORD ;
begin
    result := '' ;
{$IFDEF MSWINDOWS}
    BufSize := 1024 ;
    if WNetGetConnection (PChar (ALocalName), Buffer, BufSize) <> NO_ERROR then
    begin
        fReqResponse := 'Connection Not Found: ' + ALocalName + '; Error: ' + SysErrorMessage (GetLastError) ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
    end
    else
    begin
        result := Buffer ;
        fReqResponse := ALocalName + ' is Connected to ' + result ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
    end
{$ENDIF MSWINDOWS}
end;


// free memory used by Src lists

procedure TIcsFileCopy.ClearSrcList ;
begin
    TotSrcFiles := 0 ;
    SrcFiles := Nil ;
    try
        SrcFileList.Clear ;
    except
    end ;
end ;

// free memory used by Tars lists

procedure TIcsFileCopy.ClearTarList ;
begin
    TotTarFiles := 0 ;
    TarFiles := Nil ;
    try
        TarFileList.Clear ;
    except
    end ;
end ;


// multi directories are passed as srcdir+tab+tardir+recsep (repeat)
// parse them into simple stringlists

procedure TIcsFileCopy.SetMultiDirList (const Value: string) ;
{$IFDEF COMPILER10_UP}   { V8.65 only supported D2006 and later }
var
    I: integer ;
    aitems, afields: TStringList ;
{$ENDIF}
begin
    fSrcDirList.Clear ;
    fTarDirList.Clear ;
    if value = '' then exit ;
{$IFDEF COMPILER10_UP}   { only supported D2006 and later }
    afields := TStringList.Create ;
    aitems := TStringList.Create ;
    aitems.Delimiter := IcsRECSEP ;
    aitems.StrictDelimiter := True;
    afields.Delimiter := IcsTAB ;
    afields.StrictDelimiter := True;
    try
        aitems.DelimitedText := value ;
        if aitems.Count = 0 then exit ;
        for I := 0 to aitems.Count - 1 do
        begin
            afields.DelimitedText := aitems [I] ;
            if afields.Count >= 2 then
            begin
                if (afields [0] <> '') and (afields [1] <> '') then
                begin
                    fSrcDirList.Add (afields [0]) ;
                    fTarDirList.Add (afields [1]) ;
                end ;
            end ;
        end ;
    finally
        afields.Free ;
        aitems.Free ;
    end;
{$ENDIF}
end ;

procedure TIcsFileCopy.SetSrcDirList (Value: TStringList) ;
begin
    if fSrcDirList.Text <> Value.Text then
        fSrcDirList.Assign(Value) ;
end;

procedure TIcsFileCopy.SetTarDirList (Value: TStringList) ;
begin
    if fTarDirList.Text <> Value.Text then
        fTarDirList.Assign(Value) ;
end;


function IcsMaskMatches(const Pattern: string; const aString: string; CaseSensitive: Boolean = false): Boolean;
{ Returns true if astring matches the Pattern, which may contain   }
{ wildcards * and ?. }
{ Copied from the "matcher" component for Delphi32.                                 }
{ Copyright 1996, Patrick Brisacier and Jean-Fabien Connault.  All Rights Reserved. }

  function RMatch(s: PChar; i: Integer; p: PChar; j: Integer): Boolean;
  Var
   matched        : Boolean;
   k              : Integer;
    begin
     if p[0]=#0 then
      RMatch :=  TRUE
     else while TRUE do
     if (s[i]=#0) and (p[j]=#0) then begin
      RMatch :=  TRUE;
      exit
    end
    else
      if p[j] = #0 then begin
        RMatch :=  FALSE;
        exit
      end
      else if (p[j] = '*') then begin
        k := i;
        if (p[j + 1] = #0) then begin
          RMatch :=  TRUE;
          exit
        end
        else while TRUE do begin
          matched := RMatch(s, k, p, j + 1);
          if matched OR (s[k] = #0) then begin
            RMatch :=  matched;
            exit;
          end; {if}
          inc(k);

        end; {while}
      end {if}
       else
       begin
        if not CaseSensitive then
         begin
          p[j] := UpCase(p[j]);
          s[i] := UpCase(s[i]);
         end;

        if ((p[j] = '?') and (s[i] <> #0)) OR (p[j] = s[i]) then
         begin
          inc(i);
          inc(j);
         end
        else
         begin
          RMatch :=  FALSE;
          exit
         end;
       end;
    end;

begin {Match}
    result := RMatch(PChar (aString), 0, PChar(Pattern), 0);
end;

function IcsGetMaskedName (const mask: string; const PrevFlag: boolean;
                                            const host: string): string ;
begin
    result := trim (mask) ;
    if result = '' then result := '*.*' ;
    if result = '*.*' then exit ;
    result := StringReplace (result, '$H', host, [rfReplaceAll]) ;
    if PrevFlag then
        result := FormatDateTime (result, Now - 1)
    else
        result := FormatDateTime (result, Now) ;
end ;


// returns a string with ASCII file attributes

function IcsAttrStr (const Attr: longword): string ;
begin
    Result := '';
    if Attr = $FFFFFFFF then exit ;
    if (Attr AND File_Attribute_Directory)  > 0 then Result := Result + 'D' ;
    if (Attr AND File_Attribute_Archive)    > 0 then Result := Result + 'A' ;
    if (Attr AND File_Attribute_Readonly)   > 0 then Result := Result + 'R' ;
    if (Attr AND File_Attribute_System)     > 0 then Result := Result + 'S' ;
    if (Attr AND File_Attribute_Hidden)     > 0 then Result := Result + 'H' ;
    if (Attr AND File_Attribute_Temporary)  > 0 then Result := Result + 'T' ;
    if (Attr AND File_Attribute_Compressed) > 0 then Result := Result + 'C' ;
{$IFDEF COMPILER11_UP}   { not Delphi 7}
    if (Attr AND File_Attribute_Encrypted)  > 0 then Result := Result + 'E' ;
{$ENDIF}
end ;

// format file directory from arrays

function IcsFmtFileDir (const FileRecs: TIcsFDirRecs ; const UTCFlag: boolean): string ;
var
    I, tot, len: integer ;
    totfsize: int64 ;
    temp1, temp2, temp3, linemask: string ;
    listing: TIcsStringBuild ;
begin
    result := '' ;
    linemask := sDirLine ;
    temp1 := IntToStr (MaxFNameLen) ;
    linemask [3] := temp1 [1] ;
    linemask [4] := temp1 [2] ;
    totfsize := 0 ;
    tot := Length (FileRecs) ;
    listing := TIcsStringBuild.Create (tot * 100) ;  // allocate memory for each line
    try
    if tot <> 0 then
    begin
        for I := 0 to Pred (tot) do
        begin
            with FileRecs [I] do
            begin
                temp1 := FrFileName ;
                if temp1 = '' then temp1 := '???' ;  // should never happen
                len := Length (temp1) ;
                if len > MaxFNameLen then
                    temp1 := Copy (temp1, 1, MaxFNameLen - 3) + '...' ;  // too long to display
                inc (totfsize, FrFileBytes) ;
                if FrExtra = sDirLit then
                    temp2 := FrExtra
                else
                    temp2 := IcsInt64ToCStr (FrFileBytes) ;
                temp3 := '' ;
                if FrfileDT <> 0 then
                begin
                    if UTCFlag then
                        temp3 := DateToStr (FrfileUDT) + ' ' + TimeToStr (FrfileUDT)
                    else
                        temp3 := DateToStr (FrfileDT) + ' ' + TimeToStr (FrfileDT) ;
                end ;
                listing.AppendLine (Format (linemask, [temp1, temp2, IcsAttrStr (FrFileAttr), temp3, FrSubDirs])) ;
             end ;
        end ;
    end ;
    listing.AppendLine ('Total Files Found: '+ IcsIntToCStr (tot) + ', Total size: ' + IntToKByte (totfsize, true)) ;
    Result := listing.GetString ;
    finally
        listing.Free ;
    end ;
end ;

// format file directory for list (pointers to arrays)

function IcsFmtFileDirList (const LocFileList: TIcsFindList; const UTCFlag: boolean): string ;
var
    I, tot, len: integer ;
    totfsize: int64 ;
    temp1, temp2, temp3, linemask: string ;
    FileRecX: PTIcsFDirRec ;
    listing: TIcsStringBuild ;
begin
    tot := LocFileList.Count ;
    result := '' ;
    linemask := sDirLine ;
    listing := TIcsStringBuild.Create (tot * 100) ;  // allocate memory for each line
    temp1 := IntToStr (MaxFNameLen) ;
    linemask [3] := temp1 [1] ;
    linemask [4] := temp1 [2] ;
    totfsize := 0 ;
    try
    try
    if tot <> 0 then
    begin
        for I := 0 to Pred (tot) do
        begin
            if LocFileList [I] = Nil then continue ;
            FileRecX := LocFileList [I] ;
            with FileRecX^ do
            begin
                temp1 := FrFileName ;
                if temp1 = '' then temp1 := '???' ;  // should never happen
                len := Length (temp1) ;
                if len > MaxFNameLen then temp1 := Copy (temp1, 1, MaxFNameLen - 3) + '...' ;  // too long to display
                inc (totfsize, FrFileBytes) ;
                if FrExtra = sDirLit then
                    temp2 := sDirLit
                else
                    temp2 := IcsInt64ToCStr (FrFileBytes) ;
                temp3 := '' ;
                if FrfileDT <> 0 then
                begin
                    if UTCFlag then
                        temp3 := DateToStr (FrfileUDT) + ' ' + TimeToStr (FrfileUDT)
                    else
                        temp3 := DateToStr (FrfileDT) + ' ' + TimeToStr (FrfileDT) ;
                end ;
                listing.AppendLine (Format (linemask, [temp1, temp2, IcsAttrStr (FrFileAttr), temp3, FrSubDirs])) ;
            end ;
       end ;
    end ;
    except
        listing.AppendLine ('Exception accessing file record') ;
    end ;
    listing.AppendLine ('Total Files Found: ' + IcsIntToCStr (tot) + ', Total size: ' + IntToKByte (totfsize, true)) ;
    Result := listing.GetString ;
    finally
        listing.Free ;
    end ;
end ;

// check if directory has at least one file or subdirectory
// V8.60 this version uses sysutils FindFirst for Posix compatbility

function IcsCheckDirAny (LocDir: string): boolean ;
var
    SearchRec: TSearchRec ;
    retcode: integer;
    CurName: string;
begin
    result := false ;
    LocDir := IncludeTrailingBackslash (LocDir) + '*.*' ;
    try
    try

    // loop through directory until a real name found
        retcode := FindFirst (Pchar(LocDir), faAnyFile, SearchRec);
        while (retcode = 0) do
        begin
            CurName := SearchRec.Name;
            if ((CurName <> '.') and (CurName <> '..')) then
            begin
                result := true ;
                Exit;
            end
            else
                retcode := FindNext (SearchRec);
        end;
    except
        result := false;
    end;
    finally
        FindClose(SearchRec);
    end;
end;

// builds list of files in a directory and sub directories, optional search path
// Level and InitDLen should be 0, except when called recursively
// LocFiles array should be set to length zero, generally
// returns false for error or if cancelled from copyevent
function TIcsFileCopy.BuildDirList2 (LocDir, LocPartName: string; const SubDirs: boolean;
                Level, InitDLen: integer ; const LoDT, HiDT: TDateTime ;
                                var TotFiles: integer; var LocFiles: TIcsFDirRecs ;
                                                        const ListDirs: boolean = false) : boolean ;
var
    SearchRec: TSearchRec ;
    retcode: integer;
    fullname, CurName: string;
    CurAttr: integer;
    CheckMatch, keepflag, succflag, DateCheckFlag: boolean;
    localDT: TDateTime ;
    OldWow64: Boolean ;        // 22 May 2013

    procedure AddFiletoArray ;  // 9 Feb 2011 commonise code
    begin
        inc(TotFiles);
// see if allocating more array memory
        if Length(LocFiles) <= TotFiles then SetLength(LocFiles, TotFiles * 2);  // 11 Feb 2004, was Length

// keep details
        with LocFiles[pred(TotFiles)] do
        begin
            FrFileName := CurName;
            FrSubDirs := Copy(LocDir, InitDLen, 255);
            FrFullName := LocDir + CurName;
            FrDirLevel := Level;
            FrDirBaseLen := Pred (InitDLen);
            FrFileAttr := CurAttr;
            FrFileDT := localDT ;
{$IFDEF MSWINDOWS}
            FrFileUDT := IcsFileTimeToDateTime (SearchRec.FindData.ftLastWriteTime) ; // UTC time Windows only
{$ELSE}
            FrFileUDT := FrFileDT;
{$ENDIF}
            FrFileBytes := SearchRec.Size;
            FrExtra := '';
            if ((CurAttr and faDirectory) = faDirectory) then
            begin
                FrFileBytes := 0;
                FrExtra := sDirLit ;
            end;
            FrFileCopy := FCStateNone;
            FrLinks := '';
        end;
    end;

begin
    if (Length (LocFiles) = 0) then SetLength (LocFiles, 1000) ;
    if fCancelFlag then
    begin
        result := false ;
        exit ;
    end ;
    result := true ;
    LocPartName := Trim (AnsiLowerCase (LocPartName)) ;
    LocDir := IncludeTrailingBackslash (LocDir) ;  // 7 Jan 2001, keep case
    if InitDLen = 0 then InitDLen := Length(LocDir) ;
    CheckMatch := true;
    DateCheckFlag := (LoDT <> 0) and (HiDT <> 0) ;
    if (LocPartName = '*.*') or (LocPartName = '') then CheckMatch := false;
{$IFDEF MSWINDOWS}
    OldWow64 := false ; // 22 May 2013
    if fWow64RedirDisable then DisableWow64Redir (OldWow64) ; // 22 May 2013
{$ENDIF}
    try
        try

    // loop through directory getting all file names in directory
            fullname := IcsAddLongPath(LocDir) + '*.*';                    { V8.70 paths longer than 255 odd }
            retcode := FindFirst(fullname, faAnyFile, SearchRec);
            while (retcode = 0) do
            begin
                CurAttr := SearchRec.Attr;
                CurName := SearchRec.Name;
                if ((CurName <> '.') and (CurName <> '..')) then
                begin
            // found another directory, recursively call this function to process it
                    if ((CurAttr and faDirectory) = faDirectory) then
                    begin
                        if ListDirs then  // 9 Feb 2011 keep directory names
                        begin
{$IFNDEF MSWINDOWS}
                            localDT := SearchRec.TimeStamp;       { V8.70 returns UTC time on Windows not local, so useless!!! }
{$ELSE}
                            localDT := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.FileDateToDateTime (SearchRec.Time);
{$ENDIF}
                            AddFiletoArray ;
                        end ;
                        if SubDirs then
                        begin
                            succflag := BuildDirList2 (LocDir + CurName, LocPartName,
                                  SubDirs, succ(Level), InitDLen, loDT, hiDT, TotFiles, LocFiles, ListDirs) ;
                            if not succflag then exit;
                        end;
                    end
                    else
                    begin
                   // if file matches mask, add record with all file details to dynamic array
                        if CheckMatch then
                        begin
                            keepflag := IcsMaskMatches (LocPartName, AnsiLowerCase (CurName), true) ;
                        end
                        else
                           keepflag := true;

                   // see if checking any time stamps
                        localDT := 0 ;
                        if keepflag then
                        begin
{$IFNDEF MSWINDOWS}
                            localDT := SearchRec.TimeStamp;   { V8.70 returns UTC time on Windows not local, so useless!!! }
{$ELSE}
                            localDT := {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.FileDateToDateTime (SearchRec.Time);
{$ENDIF}
                            if DateCheckFlag then
                            begin
                                if (localDT < LoDT) or (localDT > HiDT) then keepflag := false;
                            end ;
                        end ;

                   // keep file details in dynamic array
                        if keepflag then AddFiletoArray ;
                    end;
                end;
                retcode := FindNext (SearchRec);
                if AppProcMessCount <= 0 then      // make sure applications remains responsive
                begin
                    MessagePump ; // 16 Sept 2010
                    AppProcMessCount := AppProcMessMax ;
                    if AppProgressCount <= 0 then
                    begin
                        doCopyEvent (LogLevelProg, 'Listing Files, Total Found ' + IcsIntToCStr (TotFiles)) ; // 20 May 2013
                        if fCancelFlag then
                        begin
                            result := false ;
                            exit ;
                        end ;
                        AppProgressCount := AppProgressMax ;
                    end ;
                    dec (AppProgressCount)
                end ;
                dec (AppProcMessCount) ;
            end;
        except
            result := false;
        end;
    finally
        FindClose(SearchRec);
{$IFDEF MSWINDOWS}
        if fWow64RedirDisable then RevertWow64Redir (OldWow64) ; // 22 May 2013
{$ENDIF}
    end;
end;

// backward compatible version, no dates

function TIcsFileCopy.BuildDirList (LocDir, LocPartName: string; const SubDirs: boolean;
                 Level, InitDLen: integer ; var TotFiles: integer;
                                        var LocFiles: TIcsFDirRecs  ; const ListDirs: boolean = false) : boolean ;
begin
    fCancelFlag := false ;
    AppProgressCount := 0 ;
    result := BuildDirList2 (LocDir, LocPartName, SubDirs, Level, InitDLen, 0, 0, TotFiles, LocFiles, ListDirs) ;
end ;

// called by TIcsFindList for sort and find comparison of file records - case insensitive

function IcsCompareFNext (Item1, Item2: Pointer): Integer;
// Compare returns < 0 if Item1 is less than Item2, 0 if they are equal
// and > 0 if Item1 is greater than Item2.
var
    Sort1, Sort2: string ;
begin
// using fullname might be faster, ! as last path delim makes files sort before dirs
    Sort1 := PTIcsFDirRec (Item1).FrSubDirs + '!' + PTIcsFDirRec (Item1).FrFileName ;
    Sort2 := PTIcsFDirRec (Item2).FrSubDirs + '!' + PTIcsFDirRec (Item2).FrFileName ;
    result := CompareText (Sort1, Sort2) ;  // case insensitive
end ;

// builds sorted list of files in a directory and sub directories, optional search path
// returns total files, or -1 for error or cancelled from CopyEvent

function TIcsFileCopy.GetDirList (LocDir, LocPartName: string; const SelOpt: TIcsFileCopyType;
              const SubDirs: boolean; LoDT, HiDT: TDateTime ;
                    var LocFiles: TIcsFDirRecs ; var LocFileList: TIcsFindList; const ListDirs: boolean = false): integer ;
var
    I, totfiles: integer ;
    flag: boolean ;
begin
    fCancelFlag := false ;
    AppProgressCount := 0 ;
    if SubDirs then  // 7 Sept 2008 was 1000 more if subdirs, avoids too much copying
        SetLength (LocFiles, 25000)
    else
        SetLength (LocFiles, 5000) ;
    totfiles := 0 ;
    if NOT Assigned (LocFileList) then LocFileList := TIcsFindList.Create ;
    LocFileList.Clear ;
    if (SelOpt = FCTypeAllDir) or (SelOpt = FCTypeArchDir) or (LocPartName = '') then LocPartName := '*.*' ;
    if (SelOpt <> FCTypeDates) or (LoDT >= HiDT) then
    begin
        LoDT := 0 ;
        HiDT := 0 ;
    end ;
    flag := BuildDirList2 (LocDir, LocPartName, SubDirs, 0, 0, LoDT, HiDT, totfiles, LocFiles, ListDirs) ;
    if NOT flag then
    begin
        SetLength (LocFiles, 0) ;
        result := -1 ;
        exit ;
    end ;
    result := totfiles ;
    SetLength (LocFiles, totfiles) ;
    if result = 0 then exit ;

// build list and sort it
    LocFileList.Capacity := totfiles ;
    for I := 0 to Pred (totfiles) do LocFileList.Add (@LocFiles [I]) ;
    LocFileList.Sort (IcsCompareFNext) ;
    LocFileList.Sorted := true ; // 11 June 2008 not sure if really needed
end ;

// build displayable file directory

function TIcsFileCopy.DispLocFiles (const SrcDir, SrcFile: string ; const CopyType:
        TIcsFileCopyType ; const SubDirs: boolean; const UTCFlag: boolean;
                                        const ListDirs: boolean = false): string ;
var
    DirSrcFiles: TIcsFDirRecs  ;
    DirSrcFileList: TIcsFindList ;
    totfiles: integer ;
begin
    result := '' ;
    fCancelFlag := false ;
    DirSrcFileList := TIcsFindList.Create ;
    try
        totfiles := GetDirList (SrcDir, SrcFile, CopyType, SubDirs, 0, 0, DirSrcFiles, DirSrcFileList, ListDirs) ;
        if totfiles = -1 then
            result := 'Error Locating Files - ' + IcsGetExceptMess (ExceptObject)
        else
            result := IcsFmtFileDirList (DirSrcFileList, UTCFlag) ;

    finally
        DirSrcFileList.Free ;
        SetLength (DirSrcFiles, 0) ;
    end ;
end ;

function IcsCheckReplace (const replopt: TIcsFileCopyRepl;
           const IgnoreOldTime: boolean; const AllowDiffDT: TDateTime;
            const SrcSize, TarSize: Int64; SrcDT, TarDT: TDateTime): boolean ;
var
    realdiffDT: TDateTime ;
begin
    result := true ;
    if (replopt = FCReplAlways) then exit ;
    if (replopt = FCReplNever) then
    begin
         result := false ;
         exit ;
    end ;
    if ((replopt <> FCReplDiff) and (replopt <> FCReplNewer)) then exit ;

// see if FTP timestamp without time
    if IgnoreOldTime then
    begin
        if Frac (TarDT) = 0 then
            SrcDT := Int (SrcDT)
        else if Frac (SrcDT) = 0 then
            TarDT := Int (TarDT) ;
    end ;

// check time stamp difference, if close don't select
    if SrcDT >= TarDT then
        realdiffDT := SrcDT - TarDT
    else
        realdiffDT := TarDT - SrcDT ;

// replace if different date or size
    if (replopt = FCReplDiff) and (SrcSize = TarSize) then  // 18 July 2003
    begin
        if realdiffDT <= AllowDiffDT then result := false ; // 18 July 2003
    end
 // replace if newer date
    else if (replopt = FCReplNewer) then
    begin
        if realdiffDT <= AllowDiffDT then result := false
        else if (SrcDT <= TarDT) then result := false ;
     // unless target is empty (18 Aug 2004)
        if (SrcSize <> 0) and (TarSize = 0) then result := true ;
    end ;
end ;

// select files to copy from one place to another
// also used to select files not in target directory to delete

function TIcsFileCopy.SelectCopyFileList (const SrcFileList, TarFileList: TIcsFindList;
        const Fname: string; const Selopt: TIcsFileCopyType; const Replopt: TIcsFileCopyRepl;
            const DiffStampMins: integer; const IgnoreOldTime: boolean; IgnoreExt: string ;            { V8.70 was DiffStamp }
              var Skipped: integer; const UTCFlag: boolean; const IgnorePaths: String = ''): integer ;  // 16 May 2013 added IgnorePath
var
    I, J, TotSrc, TotTar: integer ;
    srchBytes: int64 ;
    srchDT, oldDT, realdiffDT, allowdiffDT: TDateTime ;
    compname, extn: string ;
    flag, xlatsrc, xlattar: boolean ;
    SrcFileRec, TarFileRec: PTIcsFDirRec ;
    SrchFileRec: TIcsFDirRec ;
    NewSelopt: TIcsFileCopyType ;
    IgnorePathList: TStringList ;    // 16 May 2013
begin
    result := 0 ;
    skipped := 0 ;
    AppProgressCount := 0 ;
    fCancelFlag := false ;
    totSrc := SrcFileList.Count ;
    totTar := TarFileList.Count ;
    if totSrc = 0 then exit ;
    IgnoreExt := AnsiLowerCase (IgnoreExt) ;  // list, ie tmp;bak
    compname := AnsiLowerCase (trim (fname)) ;
    NewSelopt := Selopt ;
    if compname = '*.*' then NewSelopt := FCTypeAllDir ; // 9 Feb 2011 faster and allow files without an extension to be selected
    if DiffStampMins <= 0 then             { V8.70 was -1 }
        allowdiffDT := OneSecondDT * 2 // allow for errors on file time stamps
    else
        allowdiffDT := DiffStampMins / (SecsPerDay / 60) ; // diff in minutes

 // ignore files where source has specific partial path, list is c:\temp;c:\temp2;c:\temp3;etc
    IgnorePathList := TStringList.Create ;
{$IFDEF COMPILER10_UP}   { only supported D2006 and later }
    IgnorePathList.Delimiter := ';';
    IgnorePathList.StrictDelimiter := True;
    IgnorePathList.DelimitedText := AnsiLowerCase (FIgnorePaths) ;
    IgnorePathList.Sort;
{$ELSE}
    IgnorePathList.DelimitedText := '"' + StringReplace(AnsiLowerCase(FIgnorePaths),           { V8.71 lines got reversed }
    IgnorePathList.Delimiter, '"' + IgnorePathList.Delimiter + '"', [rfReplaceAll]) + '"';     { V8.63 for D7 }
{$ENDIF}
    if (IgnorePathList.Count > 0) then  { V8.60 }
    begin
        for J := 0 to Pred (IgnorePathList.Count) do
        begin
            IgnorePathList [J] := Trim(IgnorePathList [J]) ;  // cleanup
        end;
    end;

// test first file in both lists to see if path translation needed
    try
        SrcFileRec := SrcFileList [0] ;
        xlatsrc := (Pos ('/', SrcFileRec^.FrFullName) > 0) ;
        if totTar <> 0 then
        begin
            TarFileRec := TarFileList [0] ;
            xlattar := (Pos ('/', TarFileRec^.FrFullName) > 0) ;
            if xlattar and xlatsrc then   // if both UNIX, don't translate
            begin
                xlatsrc := false ;
                xlattar := false ;
            end
        end
        else
            xlattar := false ;

// check each source file to see if it's being copied
        for I := 0 to Pred (totSrc) do
        begin
            if AppProcMessCount <= 0 then      // make sure applications remains responsive
            begin
                MessagePump ; // 16 Sept 2010
                AppProcMessCount := AppProcMessMax ;
                if AppProgressCount <= 0 then
                begin
                    doCopyEvent (LogLevelProg, 'Checking Files, Done ' + IcsIntToCStr (I)) ;  // 20 May 2013
                    if fCancelFlag then
                    begin
                        result := 0 ;
                        exit ;
                    end ;
                    AppProgressCount := AppProgressMax ;
                end ;
                dec (AppProgressCount) ;
            end ;
            dec (AppProcMessCount) ;
            flag := false ;
            SrcFileRec := SrcFileList [I] ;
            with SrcFileRec^ do
            begin
                if { (FrExtra <> sDirLit) and } (FrLinks = '') then    // 9 Feb 2011 don't ignore directories
                begin
                    if NewSelopt = FCTypeAllDir then flag := true
                    else if NewSelopt = FCTypeDates then flag := true
                    else if NewSelopt = FCTypeSingle then
                    begin
                        if FrFileName = compname then flag := true ;
                    end
                    else if NewSelopt = FCTypeMaskDir then
                    begin
                        if IcsMaskMatches (compname, AnsiLowerCase (FrFileName), true) then flag := true ;
                    end
                    else if NewSelopt = FCTypeArchDir then
                    begin
                        if (FrFileAttr AND File_Attribute_Archive) > 0 then flag := true ;
                    end ;

               // ignore files with specific extensions - list is tmp;ftp;xxx;etc
                    if flag and (Length (IgnoreExt) <> 0) then
                    begin
                        extn := AnsiLowerCase (ExtractFileExt (FrFileName)) ;
                        if Length (extn) >= 4 then
                        begin
                            if Pos (Copy (extn, 2, 99), IgnoreExt) > 0 then flag := false ;
                        end ;
                    end ;

              // ignore files where source has specific partial path, list is c:\temp;c:\temp2;c:\temp3;etc
                    if flag and (IgnorePathList.Count > 0) then  // 16 May 2013
                    begin
                        for J := 0 to IgnorePathList.Count - 1 do
                        begin
                            if Pos (IgnorePathList [J], AnsiLowerCase (FrFullName)) = 1 then flag := false ;
                        end;
                    end;
                end ;
            end ;

       // see if file already exists and whether we can skip downloading it again
            if flag and (totTar <> 0) and (replopt <> FCReplAlways) then
            begin
                SrchFileRec := SrcFileRec^ ;   // copy record to see can mess with it
                with SrchFileRec do
                begin
                    if UTCFlag then
                        srchDT := FrFileUDT
                    else
                        srchDT := FrFileDT ;
                    srchBytes := FrFileBytes ;
                end ;
            // allow for comparing DOS and UNIX file paths
                if xlatsrc then SrchFileRec.FrSubDirs := IcsPathUnixToDos (SrchFileRec.FrSubDirs) ;
                if xlattar then SrchFileRec.FrSubDirs := IcsPathDosToUnix (SrchFileRec.FrSubDirs) ;
                if TarFileList.Find (@SrchFileRec, IcsCompareFNext, J) then
                begin
                   // check if we can ignore the file
                    TarFileRec := TarFileList [J] ;
                    with TarFileRec^ do
                    begin
                     // 9 Feb 2011 directory exists, can not replace it
                        if ((FrFileAttr and faDirectory) = faDirectory) then
                            flag := false
                     // never replace
                        else if (replopt = FCReplNever) then
                            flag := false
                        else
                        begin
                            if ((replopt = FCReplDiff) or (replopt = FCReplNewer)) then // 18 July 2003
                            begin
                           // see if FTP timestamp without time
                                if UTCFlag then
                                    oldDT := FrFileUDT
                                else
                                    oldDT := FrFileDT ;
                                if IgnoreOldTime then
                                begin
                                    if Frac (oldDT) = 0 then
                                        srchDT := Int (srchDT)
                                    else if Frac (srchDT) = 0 then
                                        oldDT := Int (oldDT) ;
                                end ;

                            // check time stamp difference, if close don't select
                                if srchDT >= oldDT then
                                    realdiffDT := srchDT - oldDT
                                else
                                    realdiffDT := oldDT - srchDT ;

                            // replace if different date or size
                                if (replopt = FCReplDiff) and (srchBytes = FrFileBytes) then  // 18 July 2003
                                begin
                                    if realdiffDT <= allowdiffDT then flag := false ; // 18 July 2003
                                end
                             // replace if newer date
                                else if (replopt = FCReplNewer) then
                                begin
                                    if realdiffDT <= allowdiffDT then flag := false
                                    else if (srchDT <= oldDT) then flag := false ;
                                 // unless target is empty (18 Aug 2004)
                                    if (srchBytes <> 0) and (FrFileBytes = 0) then flag := true ;
                                end ;
                            end ;
                        end ;
                    end ;
                end ;
                if NOT flag then inc (skipped) ;
            end ;
            if flag then
            begin
                SrcFileRec^.FrFileCopy := FCStateSelect ;  // yes, we want file
                inc (result) ;
            end ;
        end ;
        doCopyEvent (LogLevelProg, '') ;
    finally
        IgnorePathList.Free;
    end ;
end ;

// called by Windows CopyFileEx API

function CopyProgressRoutine (TotalFileSize, TotalBytesTransferred, StreamSize,
    StreamBytesTransferred: Int64; dwStreamNumber, dwCallbackReason: DWORD;
       hSourceFile, hDestinationFile: THandle; lpData: Pointer): DWORD ; stdcall;
var
    xCancelFlag: boolean ;
    Obj: TObject;
    info: String ;
begin
    result := PROGRESS_CONTINUE ;
    xCancelFlag := false ;
    Obj := lpData ;      // 16 Sept 2010
    TIcsFileCopy (Obj).MessagePump ; // 16 Sept 2010
    if (dwCallbackReason = CALLBACK_CHUNK_FINISHED) or (dwCallbackReason = CALLBACK_STREAM_SWITCH) then  // 16 May 2013 switch is first call
    begin
        try
            with TIcsFileCopy (Obj) do
            begin
                fCopyProg.CurFileBytes := TotalFileSize ;     // 20 May 2013
                fCopyProg.CurOKDone := TotalBytesTransferred ;
                fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + TotalBytesTransferred ;
                if (IcsElapsedSecs64 (fCopyProg.LastProgTick) > fProgressSecs) or                      {  V8.71 Int64 }
                                                    (fCopyProg.LastProgTick = 0) or (TotalBytesTransferred >= TotalFileSize) then
                begin
                    info := fCopyProg.ProgMessBase + ', ' + IntToKByte (TotalBytesTransferred) + ' of ' + IntToKByte (TotalFileSize) ;
                    fCopyProg.LastProgTick := IcsGetTickCount64 ;             {  V8.71 Int64 }
                    if Assigned (fCopyEvent) then
                        fCopyEvent (LogLevelProg, info, xCancelFlag) ;
                    if Assigned (fProgressEvent) then   // 22 May 2013
                    begin
                        IcsCopyProgDuration (fCopyProg, AppTicksPerFile) ;
                        fCopyProg.LogLevel := LogLevelProg ;
                        fCopyProg.Info := info ;
                        fProgressEvent (Obj, fCopyProg, xCancelFlag) ;
                    end;
                end;
             end ;
        except
        end ;
    end ;
    if xCancelFlag then result := PROGRESS_CANCEL ;
end ;

procedure TIcsFileCopy.doCopyEvent (const LogLevel:TIcsCopyLogLevel ; const Info: string) ;
begin
    if Assigned (fCopyEvent) then
        fCopyEvent (LogLevel, Info, fCancelFlag) ;
    if Assigned (fProgressEvent) then   // 22 May 2013
    begin
        fCopyProg.LogLevel := LogLevel ;
        fCopyProg.Info := Info ;
        fProgressEvent (Self, fCopyProg, fCancelFlag) ;
    end;
end ;

{$IFDEF Zipping}
procedure TIcsFileCopy.ZipProgressEvent(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);   { V8.70 }
begin
{    if Assigned (fProgressEvent) then
    begin
        fCopyProg.LogLevel := LogLevelProg ;
        fCopyProg.Info := 'zipping...' ;                    // pending
        fProgressEvent (Self, fCopyProg, fCancelFlag) ;
    end;    }
end;
{$ENDIF}


// get lists of files to copy

function TIcsFileCopy.SelectFiles (const CurSrcDir, CurTarDir: string): TIcsTaskResult ;
var
    newfname, newsrcdir: string ;
    nodeltot: integer ;
begin
    ClearTarList ;
    ClearSrcList ;
    CurProcFiles := 0 ;
    CurSkippedFiles := 0 ;
    CurDelFiles := 0 ;
    doCopyEvent (LogLevelInfo, 'Locating Files to Copy:' + IcsCRLF + 'Source: ' + CurSrcDir + IcsCRLF + 'Target: ' + CurTarDir) ;

// don't delete target files unless processing full directories
    if fDelOldTar then
    begin
        if NOT (fCopyType in [FCTypeArchDir, FCTypeAllDir, FCTypeDates]) then fDelOldTar := false ;
    end ;

// build list of source files
    newsrcdir := CurSrcDir ;
    newfname := '*.*' ;
    if fCopyType in [FCTypeMaskDir, FCTypeDates] then
    begin
        if fMultiDir then
        begin
            newfname := ExtractFileName (CurSrcDir) ;   // check found a file name
            if (Pos ('.', newfname) > 1) and (Pos ('*', newfname) > 0) then
                newsrcdir := ExtractFilePath (CurSrcDir)
            else
                newfname := '*.*' ;
        end
        else
            newfname := fSrcFName ;   /// partial masked directory
    end ;
    if (NOT fMultiDir) and fMask then newfname := '*.*' ;  // date mask
    doCopyEvent (LogLevelProg, 'Listing Files') ;
    TotSrcFiles := GetDirList (newsrcdir, newfname, fCopyType, fSubDirs, fCopyLoDT, fCopyHiDT, SrcFiles, SrcFileList, fEmptyDirs) ; // 8 Feb 2011
    doCopyEvent (LogLevelProg, '') ;
    MessagePump ; // 16 Sept 2010
    if fCancelFlag then
    begin
        result := TaskResAbort ;
        exit ;
    end ;
    if TotSrcFiles = -1  then
    begin
        result := TaskResFail ;
        fReqResponse := 'Error Locating Source Files - ' + IcsGetExceptMess (ExceptObject) ;
        exit ;
    end ;
    if TotSrcFiles <= 0  then
    begin
        result := TaskResOKNone ;
        fReqResponse := 'No Source Files Found' ;
        exit ;
    end ;

// build list of target files, unless replacing everything or deleting old stuff
    if (fRepl <> FCReplAlways) or fDelOldTar then
    begin
        doCopyEvent (LogLevelProg, 'Listing Files') ;
        TotTarFiles := GetDirList (CurTarDir, '*.*', fCopyType,  fSubDirs, 0, 0, TarFiles, TarFileList, fEmptyDirs) ; // 8 Feb 2011
        doCopyEvent (LogLevelProg, '') ;
        MessagePump ; // 16 Sept 2010
        if fCancelFlag then
        begin
            result := TaskResAbort ;
            exit ;
        end ;
        if TotTarFiles = -1  then
        begin
            result := TaskResFail ;
            fReqResponse := 'Error Locating Target Files - ' + IcsGetExceptMess (ExceptObject) ;
            exit ;
        end ;
    end ;

// compare source and target files, see what to copy
    if NOT fMultiDir then
    begin
        if fMask then
            newfname := IcsGetMaskedName (fSrcFName, fPrev, fLocalHost)
        else if (fCopyType = FCTypeSingle) then
            newfname := fSrcFName ;
    end ;
    CurProcFiles := SelectCopyFileList (SrcFileList, TarFileList, newfname,
                               fCopyType, fRepl, -1, false, fIgnoreFileExt, CurSkippedFiles, fUseUTC, fIgnorePaths) ;  // 22 May 2013 added IgnorePaths
    doCopyEvent (LogLevelProg, '') ;
    MessagePump ; // 16 Sept 2010
    if fCancelFlag then
    begin
        result := TaskResAbort ;
        exit ;
    end ;

// see if deleting old target files no longer in source directories
    if fDelOldTar and (TotTarFiles <> 0) then
    begin
        CurDelFiles := SelectCopyFileList (TarFileList, SrcFileList, '*.*',
                              FCTypeAllDir, FCReplNever, -1, false, fIgnoreFileExt, nodeltot, fUseUTC, fIgnorePaths) ;  // 22 May 2013 added IgnorePaths, V8.70 diff mins was 0 not -1
    end ;
    doCopyEvent (LogLevelProg, '') ;
    if (CurProcFiles = 0) then
    begin
        result := TaskResOKNone ;
        fReqResponse := 'No Source Files Selected to Copy' ;
        exit ;
    end ;
    Result := TaskResOKNew ;
end ;

function TIcsFileCopy.CopyOneFile (const Fnamesrc, Fnametar: string ;
                Replopt: TIcsFileCopyRepl; const Safe: boolean; var Fsize: Int64) : TIcsTaskResult ;
var
    newdir, code, errmess: UnicodeString ;
    fnamesrc2, fnametar2, fnamecopy2: UnicodeString ; { V8.70 copies with appended path extender, if needed }
    ret, flag: boolean ;
    retval: integer ;
    duration: longword ;
    SrcFSize, TarFSize: Int64 ;
    SrcFileDT, TarFileDT: TDateTime;
    OldWow64: Boolean ;        // 22 May 2013
begin
    result := TaskResFail ;
    Fsize := -1 ;
    IcsCopyProgClearAll (fCopyProg) ;  // 22 May 2013 clear all progress stuff
    OldWow64 := false ; // 22 May 2013
    if fWow64RedirDisable then DisableWow64Redir (OldWow64) ; // 22 May 2013
    try  // finally

// V8.70 see if trying to copy very long file names, add \\?\ to bypass file system checks
    fnamesrc2 := IcsAddLongPath(fnamesrc);      { V8.70 }
    fnametar2 := IcsAddLongPath(fnametar);      { V8.70 }

// check source exists
    if NOT IcsGetUAgeSizeFile (Fnamesrc2, SrcFileDT, SrcFSize) then
    begin
        doCopyEvent (LogLevelInfo, 'Copy Failed: ' + fnamesrc + ' - File Not Found') ;
        doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Copy Failed: File Not Found|0|0') ;
        exit ;
    end ;

// create target directory
    if Safe then
        fnamecopy2 := ChangeFileExt (Fnametar2, '.TMP')
    else
        fnamecopy2 := Fnametar2 ;
    newdir := ExtractFileDir (fnamecopy2) ;
    if (NOT IcsForceDirsEx (newdir)) then
    begin
        doCopyEvent (LogLevelInfo, 'Can Not Create Directory: ' + newdir) ;
        exit ;
    end ;

// see if replacing existing file
    if IcsGetUAgeSizeFile (Fnametar2, TarFileDT, TarFSize) and (replopt <> FCReplAlways) then
    begin
        flag := IcsCheckReplace (replopt, false, OneSecondDT * 2, SrcFSize, TarFSize, SrcFileDT, TarFileDT) ;
        if NOT flag then
        begin
            result := TaskResOKNone ;
            doCopyEvent (LogLevelInfo, 'Copy Skipped: ' + fnamesrc + code) ;
            doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Copy Skipped|0|0') ;
            exit ;
        end ;
    end ;

// try and delete existing file before copying, removing read only if necessary
    retval := IcsDeleteFile (fnamecopy2, fReplRO) ;
    if retval = 1 then
    begin
        doCopyEvent (LogLevelInfo, 'Can Not Replace Read Only File: ' + fnametar) ;
        doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Can Not Replace Read Only File|0|0') ;
        exit ;
    end
    else if retval > 1 then
    begin
        doCopyEvent (LogLevelInfo, 'Delete File Failed: ' + fnametar + ' - ' + SysErrorMessage (retval)) ;
        doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Delete File Failed|0|0') ;
        exit ;
    end ;

// 16 May 2013 prepare current file progress info
    fCopyProg.CurStartTick := IcsGetTickCount64 ;
    fCopyProg.SessStartTick := IcsGetTickCount64 ;
    fCopyProg.CurSrcName := fnamesrc ;
    fCopyProg.CurTarName := fnametar ;
    fCopyProg.CurFileBytes := SrcFSize ;
    fCopyprog.CurFileStartPos := 0;   { V8.71 }
    fCopyProg.TotProcBytes := SrcFSize ;
    fCopyProg.TotDoneNr := 1 ;
    fCopyProg.TotProcFiles := 1 ;
    fCopyProg.ProcBytesDone := 0 ;
    fCopyProg.ProcBytesLast := fCopyProg.ProcBytesDone ;
    fCopyProg.ProgMessBase := 'Copying ' + fnamesrc  + ' to ' + fnametar ;
    doCopyEvent (LogLevelFile, fCopyProg.ProgMessBase) ;  // 16 May 2013 was earlier

// copy file and it's attributes
    errmess := '';
{$IFDEF POSIX}
    try
        TFile.Copy(fnamesrc2, fnamecopy2, False);     { V8.64 MacOS compatible }
        Ret := True;
    except
        Ret := False;
        errmess := IcsGetExceptMess (ExceptObject)
    end;
{$ELSE}
    ret := CopyFileExW (PWideChar (fnamesrc2), PWideChar (fnamecopy2), Pointer (@CopyProgressRoutine), Pointer (Self), Nil, 0) ;  // Unicode
    if NOT ret then errmess := SysErrorMessage (GetLastError) ;
{$ENDIF}
    duration := IcsElapsedTicks64 (fCopyProg.CurStartTick) ;    { V8.71 }
    doCopyEvent (LogLevelProg, '') ;  // clear progress display
    IcsCopyProgClearCur (fCopyProg) ;  // 16 May 2013 clear current since copying done
    IcsCopyProgDuration (fCopyProg, AppTicksPerFile) ;
    if fCancelFlag then
    begin
        result := TaskResAbort ;
        exit ;
    end ;
    if NOT ret then
    begin
        doCopyEvent (LogLevelInfo, 'Copy Failed: ' + fnametar + ' - ' +  errmess) ;
        doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Copy Failed: ' + errmess + '|0|0') ;
        exit ;
    end ;

// check new file same size, if not see if original has changed size
    if duration = 0 then duration := 10 ;  // special case of copy OK but duration too short to measure
    ret := IcsGetUAgeSizeFile (fnamecopy2, TarFileDT, TarFSize) ;
    if (NOT ret) or (SrcFSize <> TarFSize) then
    begin
        Fsize := -1 ;
        doCopyEvent (LogLevelInfo, 'Copy Failed: ' + fnametar + ' - Size Mismatch') ;
        doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Size Mismatch|0|0') ;
        exit ;
    end ;
    Fsize := TarFSize ;

// see if renaming temporary file
    if Safe then
    begin
        retval := IcsRenameFile (fnamecopy2, fnametar2, true, fReplRO) ;
        if retval <> 0 then
        begin
            if (retval = 1) then
            begin
                doCopyEvent (LogLevelInfo, 'Can Not Replace Read Only File: ' + fnametar) ;
                doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Can Not Replace Read Only File|0|0') ;
            end
            else
            begin
                doCopyEvent (LogLevelInfo, 'Final File Rename Failed: ' + fnametar + ' - ' +  SysErrorMessage (retval)) ;
                doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Final File Rename Failed: ' +
                                                                                SysErrorMessage (retval)+ '|0|0') ;
            end ;
            doCopyEvent (LogLevelInfo, 'File Copied as: ' + fnamecopy2) ;
            exit ;
        end ;
    end ;

// copy OK
    result := TaskResOKNew ;
    fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + Fsize ;
    doCopyEvent (LogLevelFile, 'Copy Succeeded, size ' + IntToKByte (Fsize, true)) ;
    doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|' +
                                IntToStr (Fsize) + '|1|0|OK|' + IntToStr (duration) + '|' + IntToStr (Fsize)) ;
    finally
        if fWow64RedirDisable then RevertWow64Redir (OldWow64) ; // 22 May 2013
    end;
end ;

function TIcsFileCopy.CopyFiles (const CurSrcDir, CurTarDir: string): TIcsTaskResult ;
var
    fnamesrc, fnametar, curdir, newdir, deldir, errmess: UnicodeString ;
    fnamesrc2, fnametar2, fnamecopy2, newdir2: UnicodeString ;     { V8.70 copies with appended path extender, if needed }
 { this unit still compiles on non-Unicode compilers which is why UnicodeString is still used here }
    ret: boolean ;
    I, J, oldFail, retval: integer ;
    newsize, srcfsize: int64 ;
    SrcFileRec, DelFileRec: PTIcsFDirRec ;
    duration: longword ;
    OldWow64: Boolean ;        // 22 May 2013
{$IFDEF Zipping}
    info: string ;
    MyZipFile: TZipFile;         { V8.70 VclZip gone }
    MyZipName, MyDestDir, MyNewFullFile: String;   { V8.70 }
    MyRecreateDirs: Boolean;     { V8.70 }
    MyDateDT: TDateTime;         { V8.70 }
    MyDoneNr: Integer;
{$ENDIF}
    DelDirSrcList, DelDirTarList: TStringList ;    { V8.70 looking for corruption }

    procedure DeleteTempZip ;
    begin
{$IFDEF Zipping}
        if fZipped and (fZipType in [TypeSrcAddX, TypeSrcReplX]) then
        begin
            if fnamesrc = MyZipName then
                IcsDeleteFile (fnamesrc, true) ;
        end ;
{$ENDIF}
    end ;

begin
    result := TaskResOKNew ;
    DelDirSrcList := Nil;      { V8.71 }
    DelDirTarList := Nil;      { V8.71 }
    if fDelOldTar or fDelDone then                { V8.70 don't always need these }
    begin
        DelDirSrcList := TStringList.Create ;
        DelDirSrcList.Sorted := true ;
        DelDirSrcList.Duplicates := dupIgnore ;
        DelDirSrcList.CaseSensitive := false ;
        DelDirTarList := TStringList.Create ;
        DelDirTarList.Sorted := true ;
        DelDirTarList.Duplicates := dupIgnore ;
        DelDirTarList.CaseSensitive := false ;
    end;
{$IFDEF Zipping}
    MyZipFile := Nil;
    if fZipped then
    begin
        MyZipFile := TZipFile.Create;
        MyZipFile.OnProgress := ZipProgressEvent;
    end ;
{$ENDIF}
    OldWow64 := false ; // 22 May 2013
    if fWow64RedirDisable then DisableWow64Redir (OldWow64) ; // 22 May 2013
    try  // finally

    // see if deleting old target files first
        if fDelOldTar and (CurDelFiles <> 0) then
        begin
            doCopyEvent (LogLevelInfo, 'Deleting Old Target Files: ' + CurTarDir) ;
            for I := 0 to Pred (TotTarFiles) do
            begin
                DelFileRec := TarFileList [I] ;
                with DelFileRec^ do
                begin
                    if FrFileCopy = FCStateSelect then
                    begin
                        if ((FrFileAttr and faDirectory) = faDirectory) then continue ;  // 8 Feb 2011 ignore directory
                        doCopyEvent (LogLevelFile, 'Deleting: ' + FrFullName) ;
                        retval := IcsDeleteFile (FrFullName, true) ;
                        if retval <= 0 then
                        begin
                            doCopyEvent (LogLevelDelimFile, '|' + FrFullName + '|0|0|0|Old Target File Deleted|0|0') ;
                            inc (fCopyProg.DelOKBytes, FrFileBytes) ;
                            FrFileCopy := FCStateOK ;
                            inc (fCopyProg.DelOKFiles) ;

                         // add directory to list we'll try and delete later, 24 Apr 2003
                            deldir := AnsiLowerCase (Trim (FrSubDirs)) ;
                            if deldir <> '' then
                            begin
                                if NOT DelDirTarList.Find (deldir, J) then
                                    DelDirTarList.Add (deldir) ;
                            end ;
                        end
                        else
                        begin
                            doCopyEvent (LogLevelInfo, 'File Delete Failed: ' + FrFullName + ' - ' +  SysErrorMessage (retval)) ;
                            FrFileCopy := FCStateFailed ;
                            inc (fCopyProg.ProcFailFiles) ;
                        end ;
                        MessagePump ; // 16 Sept 2010
                        if fCancelFlag then exit ;
                    end ;
                end ;
            end ;
        end ;
        ClearTarList ;  // don't need target list any more, get memory back
        MessagePump ; // 16 Sept 2010
        if fCancelFlag then
        begin
            result := TaskResAbort ;
            exit ;
        end ;

    // ensure some minimal variables have been set
        if (TotSrcFiles <= 0) or (SrcFileList.Count <> TotSrcFiles) or (CurProcFiles = 0) then
        begin
            result := TaskResOKNone ;
            fReqResponse := 'No Files Selected to Copy' ;
            exit ;
        end ;

    // make sure the destination is available
        if NOT IcsForceDirsEx (CurTarDir) then
        begin
            result := TaskResFail ;
            inc (fCopyProg.ProcFailFiles, CurProcFiles) ;
            fReqResponse := 'Can Not Create Destination Directory: ' + CurTarDir ;
            exit ;
        end ;
        doCopyEvent (LogLevelInfo, 'Copying Files:' + IcsCRLF + 'Source: ' + CurSrcDir + IcsCRLF + 'Target: ' + CurTarDir) ;

    // start real copying
        curdir := '.,.,.' ;  // illegal directory
        oldfail := fCopyProg.ProcFailFiles ;
        for I := 0 to Pred (TotSrcFiles) do
        begin
            MessagePump ; // 16 Sept 2010
            if fCancelFlag then
            begin
                result := TaskResAbort ;
                exit ;
            end ;
            IcsCopyProgClearCur (fCopyProg) ;  // 22 May 2013 clear current about to start a file
            SrcFileRec := SrcFileList [I] ;
            with SrcFileRec^ do
            begin
                if FrFileCopy <> FCStateSelect then continue ;
                inc (fCopyProg.TotDoneNr) ;
                fnamesrc := FrFullName ;  // could build from CurSrcDir, FrSubDirs and FrFileName

            // V8.70 see if trying to copy very long file names, add \\?\ to bypass file system checks
                fnamesrc2 := IcsAddLongPath(fnamesrc);     { V8.70 }
                if FrSubDirs = '' then FrSubDirs := '\' ;
                if ((FrFileAttr and faDirectory) = faDirectory) then   // 8 Feb 2011 create directory, might be empty with no files
                begin
                    curdir := '.,.,.' ;  // 8 July 2011, illegal directory to ensure newdir reset
                    newdir := CurTarDir + FrSubDirs + FrFileName + '\' ;
                    if (NOT IcsForceDirsEx (newdir)) then
                    begin
                        inc (fCopyProg.ProcFailFiles) ;
                        FrFileCopy := FCStateFailed ;
                        doCopyEvent (LogLevelInfo, 'Can Not Create Empty Directory: '+ newdir + ' - ' + GetWindowsErr(GetLastError)) ;
                        doCopyEvent (LogLevelInfo, '!!! Long Directory: '+ IcsAddLongPath(newdir)) ;      // !!! TEMP debugging
                        continue ;
                    end ;

                    FrFileCopy := FCStateOK ;
                    inc (fCopyProg.ProcOKFiles) ;
                    doCopyEvent (LogLevelFile, 'Created Directory OK: ' + newdir) ;
                    doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + newdir + '|0|1|0|OK|0|0') ;
{$IFDEF MSWINDOWS}
                    newdir2 := IcsAddLongPath(newdir);                 { V8.70 }
                    retval := FileSetAttr (newdir2, FrFileAttr) ;      { V8.70 }
{$ENDIF MSWINDOWS}
                    if retval > 0 then doCopyEvent (LogLevelInfo,
                                'Failed to Set Directory Attributes: '+ newdir + ' - ' + SysErrorMessage (retval)) ;
                    continue ;
                end ;
                if (curdir <> FrSubDirs) then
                begin
                    curdir := FrSubDirs ;
                    if curdir [1] <> '\' then
                        newdir := CurTarDir + '\' + curdir
                    else
                        newdir := CurTarDir + curdir ;
                    if (NOT IcsForceDirsEx (newdir)) then
                    begin
                        curdir := '.,.,.' ;  // V8.60, illegal directory to ensure newdir reset
                        inc (fCopyProg.ProcFailFiles) ;
                        FrFileCopy := FCStateFailed ;
                        doCopyEvent (LogLevelInfo, 'Can Not Create Directory: '+ newdir + ' - ' + GetWindowsErr(GetLastError)) ;
                        doCopyEvent (LogLevelInfo, '!!! Long Directory: '+ IcsAddLongPath(newdir)) ;      // !!! TEMP debugging
                        continue ;
                    end ;
                end ;
                fnametar := newdir + FrFileName ;
                fnametar2 := IcsAddLongPath(fnametar);     { V8.70 }
                srcfsize := FrFileBytes ;

            // see if zipping temporary file, V8.70 using System.Zip instead of VclZip
     {$IFDEF Zipping}
                MyZipName:= '';
                if fZipped and Assigned (MyZipFile) and (fZipType in [TypeSrcAddX, TypeSrcReplX]) and
                                                      (AnsiLowerCase (ExtractFileExt (fnamesrc)) <> '.zip') then
                begin
                    doCopyEvent (LogLevelFile, 'Compressing ' + fnamesrc) ;
                    MyZipName := IcsAddLongPath(IncludeTrailingBackslash (IcsGetTempPath) + FrFileName) ;
                    srcfsize := -1;
                    try
                        MyZipFile.Open(MyZipName, zmWrite) ;
                        MyZipFile.Add (fnamesrc2, ExtractFileName(fnamesrc)) ;
                        MyZipFile.Comment := 'Zipped by Sync Files Task' ;
                        MyZipFile.Close ;
                        srcfsize := IcsGetFileSize(MyZipName) ;
                        if (srcfsize <= 0) then
                        begin
                            doCopyEvent (LogLevelFile, 'Zip Compression Failed ' + fnamesrc) ;
                        end
                        else
                        begin
                            if fZipType = TypeSrcReplX then
                                fnametar := ChangeFileExt (fnametar, '.zip')
                            else
                                fnametar := fnametar + '.zip' ;
                            fnametar2 := IcsAddLongPath(fnametar);    { V8.70 }
                            doCopyEvent (LogLevelFile, 'Compressed ' + fnamesrc + ' from ' +
                                                IntToKByte (FrFileBytes, true)  + ' to ' + IntToKByte (srcfsize, true)) ;
                            fnamesrc := MyZipName ;
                            fnamesrc2 := MyZipName ;   { V8.70 }
                        end ;
                    except
                        doCopyEvent (LogLevelFile, 'Zip Compression Failed ' + fnamesrc + ' - ' + IcsGetExceptMess (ExceptObject)) ;
                    end;
                end ;
    {$ENDIF}

            // safe copying, will rename later
                FrFileCopy := FCStateCopying ;
                if fSafe then
                    fnamecopy2 := ChangeFileExt (fnametar2, '.TMP')
                else
                    fnamecopy2 := fnametar2 ;

            // try and delete existing file before copying, removing read only if necessary
                retval := IcsDeleteFile (fnamecopy2, fReplRO) ;
                if retval > 0 then
                begin
                    if retval = 1 then
                    begin
                        doCopyEvent (LogLevelInfo, 'Can Not Replace Read Only File: ' + fnametar) ;
                        doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Can Not Replace Read Only File|0|0') ;
                    end
                    else
                    begin
                        doCopyEvent (LogLevelInfo, 'Delete File Failed: ' + fnametar + ' - ' + SysErrorMessage (retval)) ;
                        doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Delete File Failed|0|0') ;
                    end ;
                    FrFileCopy := FCStateFailed ;
                    inc (fCopyProg.ProcFailFiles) ;
                    continue ;
                end ;

                // 22 May 2013 prepare current file progress info
                fCopyProg.CurStartTick := IcsGetTickCount64 ;     { V8.71 }
                fCopyProg.CurSrcName := fnamesrc ;
                fCopyProg.CurTarName := fnametar ;
                fCopyProg.CurFileBytes := SrcFSize ;
                fCopyProg.ProcBytesLast := fCopyProg.ProcBytesDone ;
                fCopyProg.ProgMessBase := 'Copying ' + IcsIntToCStr (fCopyProg.TotDoneNr) + ' of ' +
                                        IcsIntToCStr (fCopyProg.TotProcFiles) + ' - ' + fnamesrc  + ' to ' + fnametar ;
                doCopyEvent (LogLevelFile, fCopyProg.ProgMessBase) ;

            // actual file copy
                errmess := '';
    {$IFDEF POSIX}
                try
                    TFile.Copy(fnamesrc2, fnamecopy2, False);     { V8.64 MacOS compatible, no progress }
                    Ret := True;
                except
                    Ret := False;
                    errmess := IcsGetExceptMess (ExceptObject)
                end;
    {$ELSE}
                Ret := CopyFileExW (PWideChar(fnamesrc2), PWideChar(fnamecopy2), Pointer (@CopyProgressRoutine), Pointer (Self), Nil, 0) ;  // Unicode V8.70 simplified
                if NOT Ret then
                    errmess := SysErrorMessage (GetLastError) ;
    {$ENDIF}
                duration := IcsElapsedTicks64 (fCopyProg.CurStartTick) ;    { V8.71 }
                doCopyEvent (LogLevelProg, '') ;  // clear progress display
                IcsCopyProgClearCur (fCopyProg) ;  // 16 May 2013 clear current since copying done
                IcsCopyProgDuration (fCopyProg, AppTicksPerFile) ;
                if NOT Ret then
                begin
                    doCopyEvent (LogLevelInfo, 'Copy Failed: ' + fnametar2 + ' - ' +  errmess) ;
                    doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar2 + '|0|0|1|Copy Failed: ' + errmess + '|0|0') ;
                    inc (fCopyProg.ProcFailFiles) ;
                    fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + SrcFSize ;
                    FrFileCopy := FCStateFailed ;
                    DeleteTempZip ;
                    continue ;
                end ;

            // check new file same size, if not see if original has changed size
                if duration = 0 then duration := 10 ;  // special case of copy OK but duration too short to measure
                newsize := IcsGetFileSize (fnamecopy2) ;
                if newsize <> srcfsize then
                begin
                    if newsize <> IcsGetFileSize (fnamesrc2) then
                    begin
                        doCopyEvent (LogLevelInfo, 'Copy Failed: ' + fnametar + ' - Size Mismatch') ;
                        doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Size Mismatch|0|0') ;
                        inc (fCopyProg.ProcFailFiles) ;
                        fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + SrcFSize ;
                        FrFileCopy := FCStateFailed ;
                        DeleteTempZip ;
                        continue ;
                    end ;
                end ;

            // see if renaming temporary file
                if fSafe then
                begin
                    retval := IcsRenameFile (fnamecopy2, fnametar2, true, fReplRO) ;
                    if (retval > 0) then
                    begin
                        if retval = 1 then
                        begin
                            doCopyEvent (LogLevelInfo, 'Can Not Replace Read Only File: ' + fnametar) ;
                            doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Can Not Replace Read Only File|0|0') ;
                        end
                        else
                        begin
                            doCopyEvent (LogLevelInfo, 'Final File Rename Failed: ' + fnametar + ' - ' +  SysErrorMessage (retval)) ;
                            doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Final File Rename Failed: ' +
                                                                                            SysErrorMessage (retval)+ '|0|0') ;
                        end ;
                        doCopyEvent (LogLevelInfo, 'File Copied as: ' + fnamecopy2) ;
                        FrFileCopy := FCStateFailed ;
                        inc (fCopyProg.ProcFailFiles) ;
                        fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + SrcFSize ;
                        DeleteTempZip ;
                        continue ;
                    end ;
                end ;

           // copy OK
                inc (fCopyProg.ProcOKBytes, newsize) ;
                fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + newsize ;
                FrFileCopy := FCStateOK ;
                inc (fCopyProg.ProcOKFiles) ;
                doCopyEvent (LogLevelFile, 'Copy Succeeded, size ' + IntToKByte (newsize, true)) ;
                doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|' +
                                     IntToStr (newsize) + '|1|0|OK|' + IntToStr (duration) + '|' + IntToStr (newsize)) ;

           // see if unzipping it, V8.70 now using System.Zip instead of VclZip
               {$IFDEF Zipping}
                if fZipped and Assigned (MyZipFile) and (fZipType = TypeUnzip) and (AnsiLowerCase (ExtractFileExt (fnametar)) = '.zip') then
                begin
                    MyZipName := fnametar2 ;    // set the zip filename
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
                            MyDestDir := ExtractFileDir (fnametar2) ;     // Set destination directory
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
                                    IcsDeleteFile (fnametar2, true) ;
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
                DeleteTempZip ;  // if zipped before copy

            // see if deleting source file
                if fDelDone then
                begin
                    retval := IcsDeleteFile (fnamesrc, fReplRO) ;     // 30 Jan 2003, was fnamesrc
                    if retval = 0 then
                    begin
                        doCopyEvent (LogLevelDelimFile, FrFullName + '| |0|0|0|Source File Deleted After Copy|0|0') ;
                        deldir := AnsiLowerCase (Trim (FrSubDirs)) ;
                        if deldir <> '' then
                        begin
                            if NOT DelDirSrcList.Find (deldir, J) then
                                DelDirSrcList.Add (deldir) ;
                        end ;
                    end
                    else
                    begin
                        doCopyEvent (LogLevelFile, 'Failed to Delete File - ' + SysErrorMessage (retval)) ;
                        doCopyEvent (LogLevelDelimFile, FrFullName + '| |0|0|0|Source File Delete Failed - ' + SysErrorMessage (retval) + '|0|0') ;
                        continue ;
                    end ;
                end ;
            end ;
        end ;
        if oldfail <> fCopyProg.ProcFailFiles then
        begin
            result := TaskResFail ;
            fReqResponse := 'Some Errors Encountered' ;
        end ;
        doCopyEvent (LogLevelInfo, 'Copy Files Completed') ;      { V8.70 }

    // see if any old empty directories to delete
        if fDelDone and (DelDirSrcList.Count <> 0) then      { V8.70 }
            DeleteEmptyDirs (CurSrcDir, DelDirSrcList) ;
        if fDelOldTar and (DelDirTarList.Count <> 0) then     { V8.70 }
            DeleteEmptyDirs (CurTarDir, DelDirTarList) ;
        doCopyEvent (LogLevelProg, '') ;  // clear progress display

    finally
{$IFDEF Zipping}
        if fZipped and Assigned (MyZipFile) then
            MyZipFile.Free ;
{$ENDIF}
        if fDelOldTar or fDelDone then               { V8.71 exception even though not created }
        begin
            FreeAndNil(DelDirSrcList) ;
            FreeAndNil(DelDirTarList) ;
        end;
        if fWow64RedirDisable then
            RevertWow64Redir (OldWow64) ; // 22 May 2013
    end ;
end;

// select and copy multiple local files from one location to another

function TIcsFileCopy.SelCopyFiles (const CheckFiles: boolean): TIcsTaskResult ;
var
    I, totsecs: integer ;
    errflag: boolean ;
    starttick: Int64 ;   { V8.71 }
    xProcOKBytes: int64 ;   // 29 Jan 2003, was integer and wrapped
    xProcOKFiles, xProcFailFiles: integer ;
    TaskResult: TIcsTaskResult ;

// get file totals, optionally list all files that will be copied
    procedure DispSel ;
    var
        I: integer ;
        newsize, delsize: int64 ;
        info: string ;
        FileRec: PTIcsFDirRec ;
        listing: TIcsStringBuild ;
    begin

    // find size of stuff to copy
        newsize := 0 ;
        delsize := 0 ;
        listing := TIcsStringBuild.Create ;
        try
        if fDelOldTar and (CurDelFiles <> 0) then
        begin
            if CheckFiles then
            begin
                listing.Capacity (TotSrcFiles * 50) ;  // no real idea yet
                listing.AppendLine ('Old Files Selected for Deletion are: ') ;
            end;
            for I := 0 to Pred (TotTarFiles) do
            begin
                FileRec := TarFileList [I] ;
                with FileRec^ do
                begin
                    if FrFileCopy = FCStateSelect then
                    begin
                        inc (delsize, FrFileBytes) ;
                        if CheckFiles then listing.AppendLine (FrFullName + ', Size ' + IcsInt64ToCStr (FrFileBytes)) ;
                    end ;
                end ;
            end ;
        end ;
        if CheckFiles then
        begin
            listing.Capacity (TotSrcFiles * 50) ;   // no real idea yet
            listing.AppendLine ('Files Selected for Copying are: ') ;
        end;
        for I := 0 to Pred (TotSrcFiles) do
        begin
            FileRec := SrcFileList [I] ;
            with FileRec^ do
            begin
                if FrFileCopy = FCStateSelect then
                begin
                    inc (newsize, FrFileBytes) ;
                    if CheckFiles then
                    begin
                        if ((FrFileAttr and faDirectory) = faDirectory) then   // 8 Feb 2011 display directory
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
        finally
            listing.Free ;
        end;
        doCopyEvent (LogLevelInfo, 'Source Files Skipped ' + IcsIntToCStr (CurSkippedFiles)) ;
        info := 'Selected Total Files ' + IcsIntToCStr (CurProcFiles) + ', Total size ' + IntToKByte (newsize, true) ;
        if CurDelFiles <> 0 then
            info := info + IcsCRLF + 'Old Files to Delete ' +
                                    IcsIntToCStr (CurDelFiles) + ', Total size ' + IntToKByte (delsize, true) ;
        doCopyEvent (LogLevelInfo, info) ;
        inc (fCopyProg.SkippedFiles, CurSkippedFiles) ;
        inc (fCopyProg.TotProcFiles, CurProcFiles) ;
        inc (fCopyProg.TotDelFiles, CurDelFiles) ;
        inc (fCopyProg.TotProcBytes, newsize) ;
        inc (fCopyProg.TotDelBytes, delsize) ;
    end ;

begin
//    result := true ;
    IcsCopyProgClearAll (fCopyProg) ;   // 16 May 2013 clear all progress information
    fCopyProg.SessStartTick := IcsGetTickCount64 ;    { V8.71 }
    fReqResponse := 'No Errors' ;
    totsecs := 0 ;
    try
    fCancelFlag := false ;

// multiple directories are done one at a time, to save memory
    if fMultiDir then
    begin
// some simple validation
        if (fSrcDirList.Count = 0) or (fTarDirList.Count = 0) or (fSrcDirList.Count <> fTarDirList.Count) then
        begin
            result := TaskResFail ;
            fReqResponse := 'Multiple Directories Must Be Specified' ;
            exit ;
        end ;
        errflag := false ;
        result := TaskResOKNone ;  // true if one file copied OK
        for I := 0 to Pred (fSrcDirList.Count) do
        begin
            doCopyEvent (LogLevelInfo, ' ') ;
            doCopyEvent (LogLevelInfo, 'Processing Multiple Directory ' + IcsIntToCStr (succ (I)) + ' of ' + IcsIntToCStr (fSrcDirList.Count)) ;
            ClearTarList ;
            ClearSrcList ;
            fSrcDirList [I] := ExcludeTrailingBackslash (fSrcDirList [I]) ;
            fTarDirList [I] := ExcludeTrailingBackslash (fTarDirList [I]) ;
            TaskResult := SelectFiles (fSrcDirList [I], fTarDirList [I]) ;
            MessagePump ; // 16 Sept 2010
            if fCancelFlag then
            begin
                ClearSrcList ;
                ClearTarList ;
                exit ;
            end ;
            if TaskResult = TaskResOKNew then
            begin
                DispSel ;    // shows selection info, maybe list of files
                if NOT CheckFiles then
                begin
               // start real copying
                    xProcOKBytes := fCopyProg.ProcOKBytes ;
                    xProcOKFiles := fCopyProg.ProcOKFiles ;
                    xProcFailFiles := fCopyProg.ProcFailFiles ;
                    starttick := IcsGetTickCount64 ;      { V8.71 }
                    TaskResult := CopyFiles (fSrcDirList [I], fTarDirList [I]) ;
                    if (result = TaskResOKNone) and (TaskResult <> TaskResOKNone) then
                        result := TaskResult ;
                    totsecs :=  totsecs + IcsElapsedSecs64 (starttick) ;     { V8.71 }
                    doCopyEvent (LogLevelDelimTot, fSrcDirList [I] + '|' + fTarDirList [I] + '|' +
                                IntToStr (fCopyProg.ProcOKBytes - xProcOKBytes) + '|' + IntToStr (fCopyProg.ProcOKFiles -
                                     xProcOKFiles) + '|' + IntToStr (fCopyProg.ProcFailFiles - xProcFailFiles) + '|Totals|') ;
               end ;
            end ;
            if TaskResult = TaskResOKNone then
            begin
                doCopyEvent (LogLevelInfo, fReqResponse) ;  // nothing to copy
            end ;
            if TaskResult = TaskResFail then
            begin
                errflag := true ;
                doCopyEvent (LogLevelInfo, fReqResponse) ;  // failed, say why
            end ;
            MessagePump ; // 16 Sept 2010
            if fCancelFlag then
            begin
                ClearSrcList ;
                exit ;
            end ;
        end ;
        if errflag then
            fReqResponse := 'Some Errors Encountered'
        else
            fReqResponse := 'No Errors' ;
        ClearSrcList ;

    // show totals for all directories - checking only, no copying
        if CheckFiles then
        begin
            doCopyEvent (LogLevelInfo, ' ') ;
            doCopyEvent (LogLevelInfo, 'All Directories Files Skipped ' + IcsIntToCStr (fCopyProg.SkippedFiles)) ;
            doCopyEvent (LogLevelInfo, 'All Directories Total Files ' + IcsIntToCStr (fCopyProg.TotProcFiles) + ', Total size ' + IntToKByte (fCopyProg.TotProcBytes, true)) ;
            fReqResponse := '' ;
            result := TaskResOKNone ;
            exit ;
        end ;
    end
    else

// single directory pair
    begin
        ClearTarList ;
        ClearSrcList ;
        if (fSrcDir = '') or (fTarDir = '') then
        begin
            result := TaskResFail ;
            fReqResponse := 'Source and Target Directories Must Be Specified' ;
            exit ;
        end ;
        doCopyEvent (LogLevelInfo, ' ') ;
        fSrcDir := ExcludeTrailingBackslash (fSrcDir) ;
        fTarDir := ExcludeTrailingBackslash (fTarDir) ;
        TaskResult := SelectFiles (fSrcDir, fTarDir) ;

    // don't clear source list, application may want to look at it
        MessagePump ; // 16 Sept 2010
        if fCancelFlag or (TaskResult <> TaskResOKNew) then
        begin
            result := TaskResult ;
            ClearTarList ;
            ClearSrcList ;
            exit ;
        end ;

        DispSel ;    // shows selection info, maybe list of files
        if CheckFiles then    // test only
        begin
            fReqResponse := '' ;
            result := TaskResOKNone ;
            exit ;
        end ;
    // start real copying
        starttick := IcsGetTickCount64 ;         { V8.71 }
        result := CopyFiles (fSrcDir, fTarDir) ;
        totsecs := IcsElapsedSecs64 (starttick) ;    { V8.71 }
        doCopyEvent (LogLevelDelimTot, fSrcDir + '|' + fTarDir + '|' + IntToStr (fCopyProg.ProcOKBytes) +
                             '|' + IntToStr (fCopyProg.ProcOKFiles) + '|' + IntToStr (fCopyProg.ProcFailFiles) + '|Totals|') ;
    end ;

// common to single and multi dirs
    if NOT fCancelFlag then
    begin
   //   doCopyEvent (LogLevelInfo, ' ') ;   { V8.70 }
        doCopyEvent (LogLevelInfo, 'Finished, files copied OK: ' + IcsIntToCStr (fCopyProg.ProcOKFiles) +
                         ', failed: ' + IcsIntToCStr (fCopyProg.ProcFailFiles) + ', skipped: ' + IcsIntToCStr (fCopyProg.SkippedFiles)) ;
        doCopyEvent (LogLevelInfo, 'Total size copied ' + IntToKByte (fCopyProg.ProcOKBytes, true) +
                           ', duration ' + IcsSecsToStr (totsecs) + ', average speed ' +
                              IntToKByte (IcsCalcSpeed (fCopyProg.SessDoneTicks, fCopyProg.ProcOKBytes)) + '/sec') ;
        if fCopyProg.DelOKFiles <> 0 then doCopyEvent (LogLevelInfo, 'Old target files deleted OK: ' +
                          IcsIntToCStr (fCopyProg.DelOKFiles) + ', Total size deleted ' + IntToKByte (fCopyProg.DelOKBytes, true)) ;
    end ;
    doCopyEvent (LogLevelProg, '') ;  // clear progress display
    finally
        if fCancelFlag then
        begin
            result := TaskResAbort ;
            fReqResponse := 'Cancelled Copying Files' ;
        end ;
   end ;
end ;

// delete list of empty directories, including higher level directories (but not root)

function TIcsFileCopy.DeleteEmptyDirs (RootDir: string; DirList: TStringList): integer ;
var
    I, J: integer ;
    curdir: string ;
begin
    result := 0 ;
    if NOT Assigned (DirList) then exit ;
    if DirList.Count = 0 then exit ;
    doCopyEvent (LogLevelInfo, 'Checking for Directories: ' + RootDir) ;
    RootDir := trim (ExcludeTrailingBackslash (RootDir)) ;
    for I := 0 to Pred (DirList.Count) do
    begin
        if fCancelFlag then exit ;
        if Length (Trim (DirList [I])) <= 1 then continue ;   // 19 March 2003
        curdir := RootDir + Trim (DirList [I]) ;
        J := Length (curdir) ;
        while J >= 2 do     // loop deleting directories
        begin
            if curdir [J] = '\' then dec (J) ;
            curdir := copy (curdir, 1, J) ;
            if curdir = RootDir then break ;   // stop at root directory  // 19 March 2003
            doCopyEvent (LogLevelDiag, 'Checking Directory Empty: ' +  curdir) ;
            if NOT IcsCheckDirAny (curdir) then
            begin
             // doCopyEvent (LogLevelDiag, 'Will Delete Dir: ' + curdir) ;
                if RemoveDir (curdir) then
                begin
                    inc (result) ;
                    doCopyEvent (LogLevelFile, 'Removed Directory OK: ' + curdir) ;
                end
                else
                    doCopyEvent (LogLevelFile, 'Failed to Remove Directory: ' + curdir) ;
                while J >= 2 do   // search for lower level directory
                begin
                    dec (J) ;
                    if curdir [J] = '\' then break ;
                end ;
            end
            else
                break ;
        end ;
    end ;
end ;

// delete specified files, use fSrcDir, fCopyType, fSubDirs, fSrcFName,
//   fCopyLoDT, fCopyHiDT
// V8.68 Zipped means file is zipped or archieved with .zip extension before being deleted

function TIcsFileCopy.DeleteFiles (const CheckFiles: boolean): TIcsTaskResult ;
var
    newfname, newsrcdir, curdir, info, desc: String ;
    I, J, retval: integer ;
    newsize: int64 ;
    DelFileRec: PTIcsFDirRec ;
    DelDirList: TStringList ;
    OldWow64: Boolean ;        // 22 May 2013
    listing: TIcsStringBuild ;
{$IFDEF Zipping}
    MyZipFile: TZipFile;         { V8.70 VclZip gone }
    MyZipName: String;           { V8.70 }
    StartTick: Int64;       { V8.71 }
    Duration: Integer;
{$ENDIF}
begin
    result := TaskResNone ;
    DelDirList := TStringList.Create ;
    DelDirList.Sorted := true ;
    DelDirList.CaseSensitive := false ;
    DelDirList.Duplicates := dupIgnore ;
    OldWow64 := false ; // 22 May 2013
    if fWow64RedirDisable then DisableWow64Redir (OldWow64) ; // 22 May 2013
    desc := 'Delete';   { V8.68 }
{$IFDEF Zipping}
    MyZipFile := Nil;
    if fZipped then
    begin
        desc := 'Archieve and Delete';
        MyZipFile := TZipFile.Create;
        MyZipFile.OnProgress := ZipProgressEvent;
    end ;
{$ENDIF}

    try  // finally
    ClearSrcList ;
    IcsCopyProgClearAll (fCopyProg) ;   // 16 May 2013 clear all progress information
    fReqResponse := 'No Errors' ;
    doCopyEvent (LogLevelInfo, 'Locating Files to ' + desc + ': ' + fSrcDir) ;

// build list of files to delete
    newsrcdir := fSrcDir ;
    newfname := '*.*' ;
    if fCopyType in [FCTypeMaskDir, FCTypeDates] then
    begin
        if fMultiDir then
        begin
            newfname := ExtractFileName (fSrcDir) ;   // check found a file name
            if (Pos ('.', newfname) > 1) and (Pos ('*', newfname) > 0) then
                newsrcdir := ExtractFilePath (fSrcDir)
            else
                newfname := '*.*' ;
        end
        else
            newfname := fSrcFName ;   /// partial masked directory
    end ;
    if (NOT fMultiDir) and fMask then newfname := '*.*' ;  // date mask
    TotSrcFiles := GetDirList (newsrcdir, newfname, fCopyType, fSubDirs, fCopyLoDT, fCopyHiDT, SrcFiles, SrcFileList) ;
    if TotSrcFiles = -1  then
    begin
        result := TaskResFail ;
        fReqResponse := 'Error Locating Files - ' + IcsGetExceptMess (ExceptObject) ;
        exit ;
    end ;
    if TotSrcFiles <= 0  then
    begin
        result := TaskResOKNone ;
        fReqResponse := 'No Source Files Found' ;
        exit ;
    end ;
    MessagePump ; // 16 Sept 2010
    if fCancelFlag then
    begin
        result := TaskResAbort ;
        exit ;
    end ;

// list files for deletion
    listing := TIcsStringBuild.Create ;
    try
    if CheckFiles then
     begin
        listing.Capacity (TotSrcFiles * 50) ;   // no real idea yet
        listing.AppendLine ('Files Selected for ' + desc + ' are: ') ;
     end;
    newsize := 0 ;
    for I := 0 to Pred (TotSrcFiles) do
    begin
        DelFileRec := SrcFileList [I] ;
        with DelFileRec^ do
        begin
            inc (newsize, FrFileBytes) ;
            if CheckFiles then
            begin
                if ((FrFileAttr and faDirectory) = faDirectory) then   // 8 Feb 2011 display directory
                    listing.AppendLine (FrFullName + IcsSpace + sDirLit)
                else
                    listing.AppendLine (FrFullName + ', Size ' + IcsInt64ToCStr (FrFileBytes)) ;
            end;
        end ;
    end ;
    if CheckFiles then
    begin
        info := listing.GetString ;
        doCopyEvent (LogLevelInfo, info) ;
        info := '' ;
    end;
    finally
        listing.Free ;
    end;
    info := 'Will ' + desc + ' Total Files ' + IcsIntToCStr (TotSrcFiles) + ', Total size ' + IntToKByte (newsize, true) ;
    fCopyProg.TotProcFiles := TotSrcFiles ;
    fCopyProg.TotProcBytes := newsize ;    // total size of all files
    doCopyEvent (LogLevelInfo, info) ;
    if CheckFiles then    // test only
    begin
        fReqResponse := '' ;
        result := TaskResOKNone ;
        exit ;
    end ;

// delete files
    doCopyEvent (LogLevelInfo,  desc + ' Files: ' + newsrcdir) ;
    for I := 0 to Pred (TotSrcFiles) do
    begin
        DelFileRec := SrcFileList [I] ;
        with DelFileRec^ do
        begin
            if ((FrFileAttr and faDirectory) = faDirectory) then   // 8 Feb 2011 display directory
            begin
                DelDirList.Add (AnsiLowerCase (Trim (FrFullName))) ;
                FrFileCopy := FCStateOK ;
                inc (fCopyProg.DelOKFiles) ;
                continue ;
            end;

        // V8.68 see if zipping file before deleting original, always add .zip extension
{$IFDEF Zipping}
            if fZipped and Assigned (MyZipFile) and (AnsiLowerCase (ExtractFileExt (FrFullName)) <> '.zip') then
            begin
                doCopyEvent (LogLevelFile, 'Compressing ' + FrFullName) ;
                try
                    MyZipName := IcsAddLongPath(FrFullName + '.zip') ;
                    IcsDeleteFile (MyZipName, fReplRO) ;
                    MyZipFile.Open(MyZipName, zmWrite) ;
                    StartTick := IcsGetTickCount64;        { V8.71 }
                    MyZipFile.Add (IcsAddLongPath(FrFullName), FrFileName) ;   { V8.70 }
                    MyZipFile.Comment := 'Zipped by Cleanup Files Task - ' + FrFullName ;
                    MyZipFile.Close ;
                    newsize := IcsGetFileSize (MyZipName) ;
                    if newsize > 0 then
                    begin
                        UpdateFileAge (MyZipName, FrFileDT) ;  // set time stamp same as file we zipped
                        Duration := IcsElapsedTicks64 (StartTick) ;   { V8.71 }
                        if Duration = 0 then Duration := 1;
                        doCopyEvent (LogLevelDelimFile, '|' + MyZipName + '|'+ IntToStr (newsize) + '|1|0|File Zipped OK|' + IntToStr(Duration) + '|0' ) ;
                        doCopyEvent (LogLevelFile, 'Compressed ' + MyZipName + ' from ' + IntToKByte (FrFileBytes, true)  + ' to ' + IntToKByte (newsize, true)) ;
                    end
                    else
                    begin
                        doCopyEvent (LogLevelFile, 'Zip Compression Failed: ' + FrFullName) ;
                        doCopyEvent (LogLevelDelimFile, '|' + FrFullName + '|0|0|1|Zip Compression Failed|0|0') ;
                    end;
                except
                    doCopyEvent (LogLevelFile, 'Zip Compression Failed: ' + FrFullName + ' - ' + IcsGetExceptMess (ExceptObject)) ;
                    doCopyEvent (LogLevelDelimFile, '|' + FrFullName + '|0|0|1|Zip Compression Failed|0|0') ;
                    continue;
                end ;
            end ;
{$ENDIF}

            doCopyEvent (LogLevelFile, 'Deleting: ' + FrFullName) ;
            inc (fCopyProg.TotDoneNr) ;
            retval :=  IcsDeleteFile (FrFullName, fReplRO) ;
            if retval = 0 then
            begin
                doCopyEvent (LogLevelDelimFile, '|' + FrFullName + '|'+ IntToStr (FrFileBytes) +'|1|0|File Deleted|0|0') ;   { V8.68 add size deleted }
                inc (fCopyProg.DelOKBytes, FrFileBytes) ;
                FrFileCopy := FCStateOK ;
                inc (fCopyProg.DelOKFiles) ;

             // add directory to list we'll try and delete later
                curdir := AnsiLowerCase (Trim (FrSubDirs)) ;
                if curdir <> '' then
                begin
                    if NOT DelDirList.Find (curdir, J) then DelDirList.Add (curdir) ;
                end ;
            end
            else
            begin
                doCopyEvent (LogLevelInfo, 'File Delete Failed: ' + FrFullName+ ' - ' +  SysErrorMessage (retval)) ;
                doCopyEvent (LogLevelDelimFile, '|' + FrFullName + '|0|0|1|Delete Failed|0|0') ;                            { V8.68  }
                FrFileCopy := FCStateFailed ;
                inc (fCopyProg.ProcFailFiles) ;
            end ;
            MessagePump ; // 16 Sept 2010
            if fCancelFlag then exit ;
        end ;
    end ;

// see if any old empty directories to delete
    if DelDirList.Count <> 0 then
        DeleteEmptyDirs (newsrcdir, DelDirList) ;

// report totals
    if NOT fCancelFlag and (fCopyProg.DelOKFiles <> 0) then
    begin
        doCopyEvent (LogLevelInfo, 'Files ' + desc + ' OK: ' + IcsIntToCStr (fCopyProg.DelOKFiles) +
                                                 ', Total size deleted ' + IntToKByte (fCopyProg.DelOKBytes, true)) ;
        doCopyEvent (LogLevelDelimTot, '|' + newsrcdir + '\' + newfname + '|' + IntToStr (fCopyProg.DelOKBytes) +
                             '|' + IntToStr (fCopyProg.DelOKFiles) + '|' + IntToStr (fCopyProg.ProcFailFiles) + '|Total Deleted|') ;    { V8.68 add totals }
        Result := TaskResOKNew ;
    end ;
    doCopyEvent (LogLevelProg, '') ;  // clear progress display
    finally
       DelDirList.Free ;
        if fCancelFlag then
        begin
            result := TaskResAbort ;
            fReqResponse := 'Cancelled ' + desc + ' Files' ;
        end ;
{$IFDEF Zipping}
        if fZipped then
        begin
            if Assigned (MyZipFile) then MyZipFile.Free ;
        end ;
{$ENDIF}
    end ;
end ;

procedure TIcsFileCopy.Cancel ;
begin
    fCancelFlag := true ;
end ;
{$ENDIF MSWINDOWS}

end.
