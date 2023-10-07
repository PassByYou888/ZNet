{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  OAuth authentication browser window FMX form.  May use MSIE or
              Edge on Windows with 10.4 and later, probably.
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
Dec 13, 2022 - V8.71 - baseline.
Aug 08, 2023 V9.0  Updated version to major release 9.



Officially the Microsoft.Web.WebView2 runtime (from GetIt) must be installed for Edge
Chromium to work, but in practice copying WebView2Loader.dll into the same directory as
the executable seems to work, there are Win32 and Win64 versions of this DLL with the
same name, you need the correct version for the build!

With FMX, the IE is automatically used if Edge is not available
}

unit Z.ICS9.OverbyteIcsOAuthFormFmx;

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
{$DEFINE FMX}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.WebBrowser, FMX.Controls.Presentation, FMX.Platform, FMX.Clipboard,
  Z.ICS9.Ics.Fmx.OverbyteIcsWndControl,
  Z.ICS9.Ics.Fmx.OverbyteIcsSslHttpOAuth,
  Z.ICS9.OverbyteIcsUtils;

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
    PanelTitle: TPanel;
    LabelTitle: TLabel;
    PanelBrowser: TPanel;
    doClose: TButton;
    LabelAccount: TLabel;
    procedure doCloseClick(Sender: TObject);
    procedure WebBrowserShouldStartLoadWithRequest(ASender: TObject; const URL: string);
    procedure WebBrowserDidFinishLoad(ASender: TObject);
    procedure WebBrowserDidFailLoadWithError(ASender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FAuthURL: String;
    FRedirDone: Boolean;
    FonLogEvent: TOAuthFormLogEvent;
    FonRedirEvent: TOAuthFormRedirEvent;
    WebBrowser: TWebBrowser;
    procedure Log(Line: String);
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

implementation

{$R *.FMX}

procedure TOAuthLoginForm.Log(Line: String);
begin
    if Assigned(FonLogEvent) then
        FonLogEvent(Line);
end;

procedure TOAuthLoginForm.doCloseClick(Sender: TObject);
begin
    Close;
end;

procedure TOAuthLoginForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    OALoginFormShowing := False;
    if Assigned(WebBrowser) then begin
        WebBrowser.Free;
        WebBrowser := Nil;
    end;
    Action := TCloseAction.caFree;
end;

procedure TOAuthLoginForm.FormCreate(Sender: TObject);
begin
    OALoginFormShowing := True;
    FRedirDone := False;
    ModalResult := mrCancel;
    WebBrowser := Nil;
end;

// call this before showing the form
function TOAuthLoginForm.GotoURL(const URL, FormTitle, LoginHint, CacheDir: String; IgnoreEdge: Boolean = False): Boolean;
var
    MyClipBoard: IFMXClipboardService;
begin
    Result := False;
    FAuthURL := URL;

// create component in code, since new Delphi WebBrowser versions may have new properties
    WebBrowser := TWebBrowser.Create(Self);
    with WebBrowser do begin
        Parent := PanelBrowser;
        Top := Trunc(PanelBrowser.Position.X);
        Left := Trunc(PanelBrowser.Position.Y);
        Size.Width := PanelBrowser.Size.Width;
        Size.Height := PanelBrowser.Size.Height;
        OnDidFinishLoad := WebBrowserDidFinishLoad;
        OnDidFailLoadWithError := WebBrowserDidFailLoadWithError;
        OnShouldStartLoadWithRequest := WebBrowserShouldStartLoadWithRequest;
    end;

 // see if user has passed an email address or login name hint
    if LoginHint<> '' then begin
        if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, MyClipboard) then begin
            MyClipBoard.SetClipboard(LoginHint);
            LabelAccount.Text := 'Login Hint: ' + LoginHint + ' (copied to clipboard)';
        end;
    end
    else
        LabelAccount.Text := '';
    LabelTitle.Text := '';
    if FormTitle <> '' then
        Self.Caption := Self.Caption + ' - ' + FormTitle;
    Log(Self.Caption);
    Log(URL);

    try
{$IFDEF COMPILER28_UP}
{$IF Defined(MSWINDOWS)}
        if IgnoreEdge then begin
            WebBrowser.WindowsEngine := TWindowsEngine.IEOnly;
      end
        else begin
            WebBrowser.WindowsEngine := TWindowsEngine.EdgeIfAvailable;
     //   WebBrowser.UserDataFolder := CacheDir;    // Delphi 11 and later missing from FMX
      end;
{$ENDIF}
{$ENDIF}
        WebBrowser.Navigate(URL);
        Log('Creating WebBrowser');
        WebBrowser.SetFocus;
        Result := True;
    except
        on E:Exception do
            Log('Exception WebBrowser - ' + E.Message);
    end;
end;


procedure TOAuthLoginForm.HandleWBRedirect;
var
    CanCancel: Boolean;
    HtmlBody: String;
begin
    if Assigned(FonRedirEvent) then begin
        CanCancel := False;
        HtmlBody := '';
        FonRedirEvent(NewURL, HtmlBody, CanCancel);
        if CanCancel then begin
            Log('WebBrowser Authenication Completed Successfully');
            FRedirDone := True;
            WebBrowser.Stop;  // try and stop redirection
            ModalResult := mrOk;
            Self.Close;
        end
        else begin
            if HtmlBody <> '' then begin
                FRedirDone := True;
                WebBrowser.Stop;  // try and stop redirection
                Log('WebBrowser Loading Local Error Page');
                WebBrowser.LoadFromStrings(HtmlBody, '/');
            end;
        end;
    end;
end;


procedure TOAuthLoginForm.WebBrowserDidFailLoadWithError(ASender: TObject);
begin
    Log('WebBrowser Process Failed to ' +  WebBrowser.URL);
end;


procedure TOAuthLoginForm.WebBrowserDidFinishLoad(ASender: TObject);
begin
    NewURL := WebBrowser.URL;
 // how do we get the document title for LabelTitle ???
    Log('WebBrowser Navigate OK to ' +  NewURL);
    HandleWBRedirect;
end;


procedure TOAuthLoginForm.WebBrowserShouldStartLoadWithRequest(ASender: TObject; const URL: string);
begin
    NewURL := Url;
    Log('WebBrowser Navigate Starting to: ' + NewURL);
{$IFDEF COMPILER28_UP}
{$IF Defined(MSWINDOWS)}
    if WebBrowser.WindowsActiveEngine = TWindowsActiveEngine.IE then
        Log('WebBrowser using IE Engine')
    else if WebBrowser.WindowsActiveEngine = TWindowsActiveEngine.Edge then
        Log('WebBrowser using Edge Engine')
    else
        Log('WebBrowser using Unknown Engine');
{$ENDIF}
{$ENDIF}
   HandleWBRedirect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TOAuthBrowser.Destroy;
begin
    if OALoginFormShowing and Assigned(OAuthLoginForm) then begin
        OAuthLoginForm.Close;
        OAuthLoginForm := Nil;
        OALoginFormShowing := False;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOAuthBrowser.BrowserCheckEmbedded: Boolean;
begin
    Result := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Display browser window with URL, supports Edge and MSIE engines, if available, and
  if supported by the authenication server, Google seems to dislike MSIE.
  Currently Delphi 10.4 and later only which support Edge, but older compilers planned.
  Edge should be available on Windows 10 and 11 provided updates have not been skipped. }

function TOAuthBrowser.BrowserEmbedded(RestOAuth: TRestOAuth; Owner: TComponent; const URL: string): Boolean;
var
    IgnoreEdge: Boolean;
    ErrInfo, MyTitle: String;
    CacheDir: String;
begin
    CacheDir := '';
    ErrInfo := '';
    IgnoreEdge := False;  // not used, FMX version of TWebBrowser makes choice of Edge.
    Result := False;
    MyTitle := RestOAuth.AccName;
    if MyTitle = '' then
        MyTitle := 'Please Login';

 // create modal OAuth Login Form, wait for user to login to cloud
    OAuthLoginForm := TOAuthLoginForm.Create(Owner);
    OAuthLoginForm.onLogEvent := RestOAuth.FormLogEvent;
    OAuthLoginForm.onRedirEvent := RestOAuth.FormRedirEvent;
    if NOT OAuthLoginForm.GotoURL(URL, MyTitle, RestOAuth.LoginHint, CacheDir, IgnoreEdge) then begin
        if OALoginFormShowing then begin
            OAuthLoginForm.Close;
            OAuthLoginForm := Nil;
            OALoginFormShowing := False;
        end;
    end
    else begin
        if (OAuthLoginForm.ShowModal = idOk {mrOk} ) or (RestOAuth.AccToken <> '') then  { avoid forms unit for mrOK }
            Result := True
        else
            RestOAuth.SetError(OAuthErrCancelled, 'OAuth Login Window Closed');
    end;
end;


end.
