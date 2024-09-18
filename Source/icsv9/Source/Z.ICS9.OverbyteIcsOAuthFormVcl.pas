{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  OAuth authentication browser window VCL form.
Creation:     Dec 2022
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

Updates:
Dec 14, 2022 - V8.71 - baseline.
Aug 08, 2023 V9.0  Updated version to major release 9.

This unit displays browser pages to handle OAuth login to cloud services, rather
than using an external browser.

It supports two browser engines with Delphi 10.4 and later, TEdgeBrowser Chromium
based browser introduced in 2020 for Windows 10 and 11 which should be on most
recent PCs provided Windows Update is used, and the older TWebBrowser using
Microsoft's Shell Doc Object and Control Library (SHDOCVW.DLL), part of Internet
Explorer, which is removed when Edge is installed, but still seems to be available,
and is used on older Windows versions.  Edge Chromium can be installed on Windows
7 and later. The unit checks for Edge in the registry and for the WebView2Loader.dll,
otherwise it uses TWebBrowser.

For Delphi 10.3 and earlier, only the older TWebBrowser is supported.

Beware the properties of TWebBrowser changed in Delphi XE2 so a lot of conditions here,
and onShowScriptError added much later. The 11.2 version has SelectedEngine property
to support Edge, but we ignore that and use separate EdgeBrowser.

WARNING !! If this form is edited in modern versions of Delphi, the newer properties
will need removing to compile in older versions of Delphi.

Note Google no longer supports authentication using TWebBrowser, you will get
script errors and a warning to use another browser, and announced it would no
longer support embedded browsers atall, but a year later Edge still seems to work:
https://developers.googleblog.com/2021/06/upcoming-security-changes-to-googles-oauth-2.0-authorization-endpoint.html

Officially the Microsoft.Web.WebView2 runtime (from GetIt) must be installed for Edge
Chromium to work, but in practice copying WebView2Loader.dll into the same directory as
the executable seems to work, there are Win32 and Win64 versions of this DLL with the
same name, you need the correct version for the build!







}

unit Z.ICS9.OverbyteIcsOAuthFormVcl;

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$H+}           { Use long strings                    }
{$J+}           { Allow typed constant to be modified }
{$I Include\Z.ICS9.OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$IFDEF COMPILER12_UP}
    { These are usefull for debugging !}
    {$WARN IMPLICIT_STRING_CAST       OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS  ON}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
    {$DEFINE USE_BUFFERED_STREAM}
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

interface

uses
 {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
 {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
 {$IFDEF RTL_NAMESPACES}System.Variants{$ELSE}Variants{$ENDIF},
 {$IFDEF RTL_NAMESPACES}System.Win.Registry{$ELSE}Registry{$ENDIF},
 {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
 {$IFDEF RTL_NAMESPACES}Winapi.ActiveX{$ELSE}ActiveX{$ENDIF},
 {$IFDEF RTL_NAMESPACES}Vcl.Forms{$ELSE}Forms{$ENDIF},
 {$IFDEF RTL_NAMESPACES}Vcl.StdCtrls{$ELSE}StdCtrls{$ENDIF},
 {$IFDEF RTL_NAMESPACES}Vcl.ExtCtrls{$ELSE}ExtCtrls{$ENDIF},
 {$IFDEF RTL_NAMESPACES}Vcl.Controls{$ELSE}Controls{$ENDIF},
 {$IFDEF RTL_NAMESPACES}Vcl.Clipbrd{$ELSE}Clipbrd{$ENDIF},
 {$IFDEF RTL_NAMESPACES}Vcl.OleCtrls{$ELSE}OleCtrls{$ENDIF},
 {$IFDEF COMPILER16_UP}System.IOUtils,{$ENDIF}
{$IFDEF COMPILER27_UP}
 Vcl.Edge,
{$ENDIF}
 Z.ICS9.OverbyteIcsWndControl,
 Z.ICS9.OverbyteIcsSslHttpOAuth,
 SHDocVw;

type

  TOAuthFormLogEvent = procedure(const Line: String) of object;
  TOAuthFormRedirEvent = procedure(const NewUrl: String; var HtmlBody: String; var CanClose: Boolean) of object;

  TOAuthBrowser = class(TIcsWndControl)
  private
  public
    function     BrowserCheckEmbedded: Boolean;
    function     BrowserEmbedded(RestOAuth: TRestOAuth; Owner: TComponent; const URL: String): Boolean;
    destructor   Destroy; override;
  end;

  TOAuthLoginForm = class(TForm)
    Panel1: TPanel;
    LabelTitle: TLabel;
    doClose: TButton;
    TimerClose: TTimer;
    LabelAccount: TLabel;
    PanelClient: TPanel;
    PanelWebBrowser: TPanel;
    WebBrowser: TWebBrowser;
{$IFDEF COMPILER27_UP}      // 10.4
    procedure EdgeBrowserFrameNavigationCompleted(Sender: TCustomEdgeBrowser; IsSuccess: Boolean; WebErrorStatus: TOleEnum);
    procedure EdgeBrowserCreateWebViewCompleted(Sender: TCustomEdgeBrowser; AResult: HRESULT);
    procedure EdgeBrowserWindowCloseRequested(Sender: TObject);
    procedure EdgeBrowserNavigationStarting(Sender: TCustomEdgeBrowser; Args: TNavigationStartingEventArgs);
    procedure EdgeBrowserDocumentTitleChanged(Sender: TCustomEdgeBrowser; const ADocumentTitle: string);
    procedure EdgeBrowserSourceChanged(Sender: TCustomEdgeBrowser; IsNewDocument: Boolean);
    procedure EdgeBrowserProcessFailed(Sender: TCustomEdgeBrowser; ProcessFailedKind: TOleEnum);
    procedure EdgeBrowserWebResourceRequested(Sender: TCustomEdgeBrowser; Args: TWebResourceRequestedEventArgs);
{$ENDIF}
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure doCloseClick(Sender: TObject);
    procedure Log(Line: String);
    procedure TimerCloseTimer(Sender: TObject);
    procedure WebBrowserTitleChange(ASender: TObject; const Text: WideString);
{$IFDEF COMPILER16_UP}     // XE2
    procedure WebBrowserBeforeNavigate2(ASender: TObject; const pDisp: IDispatch; const URL, Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
    procedure WebBrowserNavigateComplete2(ASender: TObject; const pDisp: IDispatch; const URL: OleVariant);
    procedure WebBrowserNavigateError(ASender: TObject; const pDisp: IDispatch; const URL, Frame, StatusCode: OleVariant; var Cancel: WordBool);
{$ELSE}
    procedure WebBrowserBeforeNavigate2(ASender: TObject; const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
    procedure WebBrowserNavigateComplete2(ASender: TObject; const pDisp: IDispatch; var URL: OleVariant);
    procedure WebBrowserNavigateError(ASender: TObject; const pDisp: IDispatch; var URL, Frame, StatusCode: OleVariant; var Cancel: WordBool);
{$ENDIF}
{$IFDEF COMPILER27_UP}   // 10.4, probably
    procedure WebBrowserShowScriptError(ASender: TObject; const AErrorLine, AErrorCharacter, AErrorMessage, AErrorCode, AErrorUrl: OleVariant; var AOut: OleVariant; var AHandled: Boolean);
{$ENDIF}
  private
    { Private declarations }
{$IFDEF COMPILER27_UP}      // 10.4
    FEdgeBrowser: TEdgeBrowser;
{$ENDIF}
 //   FWebBrowser: TWebBrowser;
    FAuthURL: String;
    FRedirDone: Boolean;
    FonLogEvent: TOAuthFormLogEvent;
    FonRedirEvent: TOAuthFormRedirEvent;
    procedure HandleWBRedirect;
  public
    { Public declarations }
    NewURL: String;
    NewTitle: String;
    function GotoURL(const URL, FormTitle, LoginHint, CacheDir: String; IgnoreEdge: Boolean = False): Boolean;
    property onLogEvent: TOAuthFormLogEvent        read FonLogEvent    write FonLogEvent;
    property onRedirEvent: TOAuthFormRedirEvent    read FonRedirEvent  write FonRedirEvent;
  end;

var
  OAuthLoginForm: TOAuthLoginForm;
  OALoginFormShowing: Boolean;

const
  MSEdgeVer = '\Software\Microsoft\Edge\BLBeacon';

  function IcsCheckEdgeBrowser (var ErrInfo: String): Boolean;

implementation

{$R *.DFM}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ try and check if Edge is available before creating the form }
function IcsCheckEdgeBrowser (var ErrInfo: String): Boolean;
{$IFDEF COMPILER27_UP}
var
    EdgeVer: String;
{$ENDIF}
begin
    Result := False;
    ErrInfo := '';
{$IFDEF COMPILER27_UP}

// see if Edge Chromium Browser is installed
    with TRegistry.Create do
    try
        RootKey := HKEY_CURRENT_USER;
        if NOT OpenKeyReadOnly(MSEdgeVer) then begin
            ErrInfo := 'Edge Chromium Browser Not Found';
            Exit;
        end;
        EdgeVer := ReadString('version');
        if EdgeVer = '' then begin
            ErrInfo := 'Edge Chromium Browser Not Found';
            Exit;
         // pending, do we need to check actual version of legacy or chromium browser??
         // currently version=107.0.1418.42
        end;
    finally
        Free;
    end;

// and for the loader DLL we should have installed
// pending, is it Win32 or Win64 to match our build?
    if NOT FileExists('WebView2Loader.dll') then begin
        ErrInfo := 'WebView2Loader.dll Not Found';
        Exit;
    end;
    Result := True;
{$ENDIF}
end;

procedure TOAuthLoginForm.Log(Line: String);
begin
    if Assigned(FonLogEvent) then
        FonLogEvent(Line);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOAuthLoginForm.TimerCloseTimer(Sender: TObject);
begin
    TimerClose.Enabled := False;
    Log('OAuth Login Form Closing on Timeout');
    Self.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOAuthLoginForm.FormCreate(Sender: TObject);
begin
    OALoginFormShowing := True;
    FRedirDone := False;
    ModalResult := mrCancel;
//    FWebBrowser := Nil;
{$IFDEF COMPILER27_UP}
    FEdgeBrowser := Nil;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOAuthLoginForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    OALoginFormShowing := False;
{$IFDEF COMPILER27_UP}
    if Assigned(FEdgeBrowser) then begin
        FEdgeBrowser.Free;
        FEdgeBrowser := Nil;
    end;
    Action := TCloseAction.caFree;
{$ELSE}
    Action := caFree;
{$ENDIF}
 {   if Assigned(FWebBrowser) then begin
        FWebBrowser.Free;
        FWebBrowser := Nil;
    end;  }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// call this before showing the form
function TOAuthLoginForm.GotoURL(const URL, FormTitle, LoginHint, CacheDir: String; IgnoreEdge: Boolean = False): Boolean;
{$IFDEF COMPILER27_UP}
var
    ErrInfo, CacheFolder: String;
{$ENDIF}
begin
    Result := False;
{$IFDEF COMPILER27_UP}  // 10.4
    if NOT IgnoreEdge then begin
        if NOT IcsCheckEdgeBrowser(ErrInfo) then begin
            Log(ErrInfo);
            IgnoreEdge := True;
        end;
        CacheFolder := CacheDir;
{$IFDEF COMPILER28_UP}
        if Pos(':', CacheFolder) = 0 then
                CacheFolder := TPath.GetTempPath + '\ics-edgecache';
        if NOT ForceDirectories(CacheFolder) then begin
            Log('Cache Directory Not Found: ' + CacheFolder);
            Log('Unable to use Edge Browser');
            IgnoreEdge := True;
       end;
{$ENDIF}
    end;
{$ELSE}
    IgnoreEdge := True;
{$ENDIF}
    FAuthURL := URL;

 // see if user has passed an email address or login name hint
    if LoginHint<> '' then begin
        Clipboard.SetTextBuf(PChar(LoginHint));   // copy login name to clipboard
        LabelAccount.Caption := 'Login Hint: ' + LoginHint + ' (copied to clipboard)';
    end
    else
        LabelAccount.Caption := '';
    if FormTitle <> '' then
        Self.Caption := Self.Caption + ' - ' + FormTitle;
    Log(Self.Caption);
    Log(URL);

 // display Edge Browser
    if NOT IgnoreEdge then begin
        PanelWebBrowser.Visible := False;
{$IFDEF COMPILER27_UP}
        FEdgeBrowser := TEdgeBrowser.Create(Self);
        FEdgeBrowser.Parent := PanelClient;
        FEdgeBrowser.Align := alClient;
{$IFDEF COMPILER28_UP}
        FEdgeBrowser.UserDataFolder := CacheFolder;    // Delphi 11 and later
{$ENDIF}
        FEdgeBrowser.OnCreateWebViewCompleted := EdgeBrowserCreateWebViewCompleted;
        FEdgeBrowser.OnDocumentTitleChanged := EdgeBrowserDocumentTitleChanged;
        FEdgeBrowser.OnFrameNavigationCompleted := EdgeBrowserFrameNavigationCompleted;
        FEdgeBrowser.OnNavigationStarting := EdgeBrowserNavigationStarting;
        FEdgeBrowser.OnProcessFailed := EdgeBrowserProcessFailed;
        FEdgeBrowser.OnSourceChanged := EdgeBrowserSourceChanged;
        FEdgeBrowser.OnWebResourceRequested := EdgeBrowserWebResourceRequested;
        FEdgeBrowser.OnWindowCloseRequested := EdgeBrowserWindowCloseRequested;
        try
            FEdgeBrowser.CreateWebView;
            Log('Creating EdgeBrowser WebView');
        // wait 10 secs for browser to be created, or assume dead
            TimerClose.Interval := 10 * 1000;
            TimerClose.Enabled := True;
            Result := True;
        except
            on E:Exception do
                Log('Exception EdgeBrowser WebView - ' + E.Message);
        end;
{$ENDIF}
    end

 // display MSIE Web Browser
    else begin
        PanelWebBrowser.Visible := True;
        PanelWebBrowser.Align := alClient;

     { can not assign TWebBrowser events in code in Delphi 2007, only some later versions, so must drop on form
        FWebBrowser := TWebBrowser.Create(Self);
        FWebBrowser.Parent := PanelClient;
        FWebBrowser.Left := PanelClient.Left;
        FWebBrowser.Top := PanelClient.Top;
        FWebBrowser.Width := PanelClient.Width;
        FWebBrowser.Height := PanelClient.Height;
        FWebBrowser.OnTitleChange := WebBrowserTitleChange;
        FWebBrowser.OnBeforeNavigate2 := WebBrowserBeforeNavigate2;
        FWebBrowser.OnNavigateComplete2 := WebBrowserNavigateComplete2;
        FWebBrowser.OnNavigateError := WebBrowserNavigateError;   }
{$IFDEF COMPILER27_UP}
       WebBrowser.onShowScriptError := WebBrowserShowScriptError;    // Delphi 10.4, stops pop-up error window
       // 11.2 also has SelectedEngine property to support Edge, but we use separate edgebrowser component
{$ENDIF}
        try
            WebBrowser.Navigate(URL);
            Log('Creating WebBrowser');
            Result := True;
        except
            Log('Exception WebBrowser');
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOAuthLoginForm.doCloseClick(Sender: TObject);
begin
    ModalResult := mrCancel;
    Self.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// following events from TEdgeBrowser using Chrome engine, Windows 10 and 11 only with updates
{$IFDEF COMPILER27_UP}
procedure TOAuthLoginForm.EdgeBrowserCreateWebViewCompleted(Sender: TCustomEdgeBrowser; AResult: HRESULT);
begin
    TimerClose.Enabled := False;
    if Succeeded(AResult) then begin
        Log('EdgeBrowser WebView Created');
        if NOT FEdgeBrowser.Navigate(FAuthURL) then
            Log('EdgeBrowser Navigate Failed to ' + FAuthURL);
    end
    else begin
        if AResult = HResultFromWin32(ERROR_FILE_NOT_FOUND) then
            Log('EdgeBrowser Not Available')
        else
            Log('EdgeBrowser Failed to Create WebView');
        ModalResult := mrCancel;
        Self.Close;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// new page loading
procedure TOAuthLoginForm.EdgeBrowserDocumentTitleChanged(Sender: TCustomEdgeBrowser; const ADocumentTitle: string);
begin
    NewTitle := ADocumentTitle;
    LabelTitle.Caption := NewTitle;
    Log('EdgeBrowser Title: ' + NewTitle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// new page loaded
procedure TOAuthLoginForm.EdgeBrowserFrameNavigationCompleted(Sender: TCustomEdgeBrowser; IsSuccess: Boolean; WebErrorStatus: TOleEnum);
begin
   if IsSuccess then
        Log('EdgeBrowser Navigate OK to ' + Sender.LocationURL)
   else
        Log('EdgeBrowser Navigate Failed to ' + Sender.LocationURL);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// event called before page loading starts, check URL for redirect with authentication code argument
procedure TOAuthLoginForm.EdgeBrowserNavigationStarting(Sender: TCustomEdgeBrowser; Args: TNavigationStartingEventArgs);
var
    Url: PChar;
    CanCancel: Boolean;
    HtmlBody: String;
begin
    Args.ArgsInterface.Get_uri(Url);   // hard way to get the URL to which navigation is starting
    NewURL := Url;
    CoTaskMemFree(Url);
    Log('EdgeBrowser Navigate Starting to: ' + NewURL);
    if Assigned(FonRedirEvent) then begin
        CanCancel := False;
        HtmlBody := '';
        FonRedirEvent(NewURL, HtmlBody, CanCancel);
        if CanCancel then begin
            Log('EdgeBrowser Authenication Completed Successfully');
            FRedirDone := True;
            Args.ArgsInterface.Set_Cancel(Integer(LongBool(True)));  // try and stop redirection
            ModalResult := mrOk;
            Self.Close;
        end
        else begin
            if HtmlBody <> '' then begin
                FRedirDone := True;
                Args.ArgsInterface.Set_Cancel(Integer(LongBool(True)));  // try and stop redirection
                Log('EdgeBrowser Loading Local Error Page');
                if NOT FEdgeBrowser.NavigateToString(HtmlBody) then
                    Log('EdgeBrowser Navigate Failed Local Page');
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOAuthLoginForm.EdgeBrowserProcessFailed(Sender: TCustomEdgeBrowser; ProcessFailedKind: TOleEnum);
begin
   Log('EdgeBrowser Process Failed');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// page URL changed
procedure TOAuthLoginForm.EdgeBrowserSourceChanged(Sender: TCustomEdgeBrowser; IsNewDocument: Boolean);
begin
   NewURL := Sender.LocationURL;
   Log('EdgeBrowser Source URL Changed to: ' + NewURL);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOAuthLoginForm.EdgeBrowserWebResourceRequested(Sender: TCustomEdgeBrowser; Args: TWebResourceRequestedEventArgs);
begin
//
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOAuthLoginForm.EdgeBrowserWindowCloseRequested(Sender: TObject);
begin
   Log('EdgeBrowser Close Window');
   Close;
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// following events from olde TWebBrowser using Microsoft's Shell Doc Object and Control Library (SHDOCVW.DLL), part of Internet Explorer
// note events called are not consistent, so look for redirect URL in lots of them.
procedure TOAuthLoginForm.HandleWBRedirect;
var
    aStream: TMemoryStream;
    CanCancel: Boolean;
    HtmlBody: String;
    ABody: AnsiString;
begin
    if Assigned(FonRedirEvent) then begin
        CanCancel := False;
        HtmlBody := '';
        FonRedirEvent(NewURL, HtmlBody, CanCancel);
        if CanCancel then begin
            Log('WebBrowser Authenication Completed Successfully');
            FRedirDone := True;
            ModalResult := mrOk;
            WebBrowser.Stop;  // try and stop redirection
            Self.Close;
        end
        else begin
            if HtmlBody <> '' then begin
                FRedirDone := True;
                WebBrowser.Stop;  // try and stop redirection
                Log('WebBrowser Loading Local Error Page');
                aStream := TMemoryStream.Create;
                try
                    Try
                        ABody := AnsiString(HtmlBody);
                        aStream.WriteBuffer (Pointer (ABody)^, Length (ABody));
                        aStream.Position := 0;
                        (WebBrowser.Document as IPersistStreamInit).Load (TStreamAdapter.Create(aStream)) ;
                    except
                        Log('WebBrowser Failed to Load Error Page');
                    End;
                finally
                    aStream.Free;
                end;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER16_UP}     // XE2
procedure TOAuthLoginForm.WebBrowserBeforeNavigate2(ASender: TObject; const pDisp: IDispatch; const URL, Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
{$ELSE}
procedure TOAuthLoginForm.WebBrowserBeforeNavigate2(ASender: TObject; const pDisp: IDispatch; var URL, Flags, TargetFrameName,  PostData, Headers: OleVariant; var Cancel: WordBool);
{$ENDIF}
begin
    if FRedirDone then Exit;
    TimerClose.Enabled := False;
    NewURL := URL;
    Log('WebBrowser Navigate Starting to: ' + NewURL);
    HandleWBRedirect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER16_UP}     // XE2
procedure TOAuthLoginForm.WebBrowserNavigateComplete2(ASender: TObject; const pDisp: IDispatch; const URL: OleVariant);
{$ELSE}
procedure TOAuthLoginForm.WebBrowserNavigateComplete2(ASender: TObject; const pDisp: IDispatch; var URL: OleVariant);
{$ENDIF}
begin
    if FRedirDone then Exit;
    TimerClose.Enabled := False;
    Log('WebBrowser Navigate OK to ' + URL);
    NewURL := URL;
    HandleWBRedirect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER16_UP}     // XE2
procedure TOAuthLoginForm.WebBrowserNavigateError(ASender: TObject; const pDisp: IDispatch; const URL, Frame, StatusCode: OleVariant; var Cancel: WordBool);
{$ELSE}
procedure TOAuthLoginForm.WebBrowserNavigateError(ASender: TObject; const pDisp: IDispatch; var URL, Frame, StatusCode: OleVariant;  var Cancel: WordBool);
{$ENDIF}
begin
    if FRedirDone then Exit;
    TimerClose.Enabled := False;
    Log('WebBrowser Navigate Error to ' + URL);
    NewURL := URL;
    HandleWBRedirect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER27_UP}  // probably 10.4
procedure TOAuthLoginForm.WebBrowserShowScriptError(ASender: TObject; const AErrorLine, AErrorCharacter, AErrorMessage, AErrorCode,
  AErrorUrl: OleVariant; var AOut: OleVariant; var AHandled: Boolean);
begin
   Log('WebBrowser Script Error to ' + AErrorURL + ' - ' + AErrorMessage);
   AHandled := True;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOAuthLoginForm.WebBrowserTitleChange(ASender: TObject; const Text: WideString);
begin
    NewTitle := Text;
    LabelTitle.Caption := NewTitle;
    Log('WebBrowser Title: ' + NewTitle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TOAuthBrowser.Destroy;                                                     { V8.71 }
begin
    if OALoginFormShowing and Assigned(OAuthLoginForm) then begin
        OAuthLoginForm.Close;
        OAuthLoginForm := Nil;
        OALoginFormShowing := False;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOAuthBrowser.BrowserCheckEmbedded: Boolean;                                  { V8.71 }
{$IFDEF COMPILER27_UP}
{$IFNDEF FMX}
var
    ErrInfo: String;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF COMPILER27_UP}
{$IFNDEF FMX}
    Result := IcsCheckEdgeBrowser(ErrInfo);
{$ELSE}
    Result := False;
{$ENDIF}
{$ELSE}
    Result := False;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Display browser window with URL, supports Edge and MSIE engines, if available, and
  if supported by the authenication server, Google seems to dislike MSIE.
  Currently Delphi 10.4 and later only which support Edge, but older compilers planned.
  Edge should be available on Windows 10 and 11 provided updates have not been skipped. }

function TOAuthBrowser.BrowserEmbedded(RestOAuth: TRestOAuth; Owner: TComponent; const URL: string): Boolean;    { V8.71 }
var
    IgnoreEdge: Boolean;
    ErrInfo, MyTitle: String;
begin
    Result := False;
    ErrInfo := '';
{$IFDEF COMPILER27_UP}
    if NOT IcsCheckEdgeBrowser(ErrInfo) then begin
        RestOAuth.LogEvent('Embedded Edge Browser Error - ' + ErrInfo);
        IgnoreEdge := True;
    end
    else
        IgnoreEdge := False;
{$ELSE}
        IgnoreEdge := True;
{$ENDIF}
    MyTitle := RestOAuth.AccName;
    if MyTitle = '' then
        MyTitle := 'Please Login';

 // create modal OAuth Login Form, wait for user to login to cloud
    OAuthLoginForm := TOAuthLoginForm.Create(Owner);
    OAuthLoginForm.onLogEvent := RestOAuth.FormLogEvent;
    OAuthLoginForm.onRedirEvent := RestOAuth.FormRedirEvent;
    if NOT OAuthLoginForm.GotoURL(URL, MyTitle, RestOAuth.LoginHint, RestOAuth.EdgeCacheDir, IgnoreEdge) then begin
        if OALoginFormShowing then begin
            OAuthLoginForm.Close;
            OAuthLoginForm := Nil;
            OALoginFormShowing := False;
        end;
    end
    else begin
        if (OAuthLoginForm.ShowModal = mrOk) or (RestOAuth.AccToken <> '') then
            Result := True
        else
            RestOAuth.SetError(OAuthErrCancelled, 'OAuth Login Window Closed');
    end;
end;


end.
