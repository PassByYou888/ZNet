{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Author:       Arno Garrels
Description:  A few header translations from MS mlang.h.
              Requires Internet Explorer 5 or better, all functions fail
              with HRESULT E_NOTIMPL if the library isn't loaded.
Creation:     March 19, 2010
Version:      V9.0
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) Microsoft Corporation.  All Rights Reserved.
              Translator Arno Garrels <arno.garrels@gmx.de>

History:
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Oct 14, 2021 - V8.68 - Trying to keep C++ happy
Aug 08, 2023 V9.0  Updated version to major release 9.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit Z.ICS9.OverbyteIcsMLang;

{$I Include\Z.ICS9.OverbyteIcsDefs.inc}

{$IFNDEF BCB}      { V8.68 }
  {$WEAKPACKAGEUNIT}
{$ENDIF}

interface

{$IFDEF MSWINDOWS}

uses
  {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF};

function ConvertINetString(
        var lpdwMode: DWORD;
        dwSrcEncoding: DWORD;
        dwDstEncoding: DWORD;
        lpSrcStr: LPCSTR;
        var lpnSrcSize: Integer;
        lpDstStr: PBYTE;
        var lpnDstSize: Integer
    ): HRESULT;

function ConvertINetMultibyteToUnicode(
        var lpdwMode: DWORD;
        dwSrcEncoding: DWORD;
        lpSrcStr: LPCSTR;
        var lpnMultiCharCount: Integer;
        lpDstStr: LPWSTR;
        var lpnWideCharCount: Integer
    ): HRESULT;

function ConvertINetUnicodeToMultibyte(
        var lpdwMode: DWORD;
        dwEncoding: DWORD;
        lpSrcStr: LPCWSTR;
        var lpnWideCharCount: Integer;
        lpDstStr: LPSTR;
        var lpnMultiCharCount: Integer
    ): HRESULT;

function IsConvertINetStringAvailable(
      dwSrcEncoding: DWORD;
      dwDstEncoding: DWORD
    ): HRESULT;

function Load_MLang: Boolean;

{$ENDIF MSWINDOWS}

implementation

{$IFDEF MSWINDOWS}

type
    TConvertINetString = function(
        var lpdwMode: DWORD;
        dwSrcEncoding: DWORD;
        dwDstEncoding: DWORD;
        lpSrcStr: LPCSTR;
        var lpnSrcSize: Integer;
        lpDstStr: PBYTE;
        var lpnDstSize: Integer
    ): HRESULT; stdcall;

    TConvertINetMultibyteToUnicode = function(
        var lpdwMode: DWORD;
        dwSrcEncoding: DWORD;
        lpSrcStr: LPCSTR;
        var lpnMultiCharCount: Integer;
        lpDstStr: LPWSTR;
        var lpnWideCharCount: Integer
    ): HRESULT; stdcall;

    TConvertINetUnicodeToMultibyte = function(
        var lpdwMode: DWORD;
        dwEncoding: DWORD;
        lpSrcStr: LPCWSTR;
        var lpnWideCharCount: Integer;
        lpDstStr: LPSTR;
        var lpnMultiCharCount: Integer
    ): HRESULT; stdcall;

    TIsConvertINetStringAvailable = function(
      dwSrcEncoding: DWORD;
      dwDstEncoding: DWORD
    ): HRESULT; stdcall;

var
    fptrConvertINetString             : TConvertINetString = nil;
    fptrConvertINetMultibyteToUnicode : TConvertINetMultibyteToUnicode = nil;
    fptrConvertINetUnicodeToMultibyte : TConvertINetUnicodeToMultibyte = nil;
    fptrIsConvertINetStringAvailable  : TIsConvertINetStringAvailable = nil;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Load_MLang: Boolean;
var
    h : HMODULE;
begin
    if Assigned(fptrConvertINetString) then
        Result := TRUE
    else begin
        h := LoadLibrary('mlang.dll');
        if h = 0 then
            Result := FALSE
        else begin
            fptrConvertINetString := GetProcAddress(h, 'ConvertINetString');
            fptrConvertINetMultiByteToUnicode := GetProcAddress(h, 'ConvertINetMultiByteToUnicode');
            fptrConvertINetUnicodeToMultiByte := GetProcAddress(h, 'ConvertINetUnicodeToMultiByte');
            fptrIsConvertINetStringAvailable := GetProcAddress(h, 'IsConvertINetStringAvailable');
            Result := TRUE;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ S_OK    Performed the conversion successfully.                              }
{ S_FALSE The specified conversion is not supported on the system.            }
{ E_FAIL  An error has occurred.                                              }
function ConvertINetString(
        var lpdwMode: DWORD;
        dwSrcEncoding: DWORD;
        dwDstEncoding: DWORD;
        lpSrcStr: LPCSTR;
        var lpnSrcSize: Integer;
        lpDstStr: PBYTE;
        var lpnDstSize: Integer
    ): HRESULT;
begin
    if Assigned(fptrConvertINetString) or Load_MLang then
        Result := fptrConvertINetString(lpdwMode, dwSrcEncoding, dwDstEncoding,
                                    lpSrcStr, lpnSrcSize, lpDstStr, lpnDstSize)
    else
        Result := E_NOTIMPL;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ S_OK    Performed the conversion successfully.                              }
{ S_FALSE The specified conversion is not supported on the system.            }
{ E_FAIL  An error has occurred.                                              }
function ConvertINetMultibyteToUnicode(
        var lpdwMode: DWORD;
        dwSrcEncoding: DWORD;
        lpSrcStr: LPCSTR;
        var lpnMultiCharCount: Integer;
        lpDstStr: LPWSTR;
        var lpnWideCharCount: Integer
    ): HRESULT;
begin
    if Assigned(fptrConvertINetString) or Load_MLang then
        Result := fptrConvertINetMultibyteToUnicode(lpdwMode, dwSrcEncoding,
                     lpSrcStr, lpnMultiCharCount, lpDstStr, lpnWideCharCount)
    else
        Result := E_NOTIMPL;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ S_OK    Performed the conversion successfully.                              }
{ S_FALSE The specified conversion is not supported on the system.            }
{ E_FAIL  An error has occurred.                                              }
function ConvertINetUnicodeToMultibyte(
        var lpdwMode: DWORD;
        dwEncoding: DWORD;
        lpSrcStr: LPCWSTR;
        var lpnWideCharCount: Integer;
        lpDstStr: LPSTR;
        var lpnMultiCharCount: Integer
    ): HRESULT;
begin
    if Assigned(fptrConvertINetString) or Load_MLang then
        Result := fptrConvertINetUnicodeToMultibyte(lpdwMode, dwEncoding,
                 lpSrcStr, lpnWideCharCount, lpDstStr, lpnMultiCharCount)
    else
        Result := E_NOTIMPL;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ S_OK          The function can perform the conversion.                      }
{ S_FALSE       The conversion is not supported on the system.                }
{ E_INVALIDARG  One or more arguments are invalid.                            }
function IsConvertINetStringAvailable(
      dwSrcEncoding: DWORD;
      dwDstEncoding: DWORD
    ): HRESULT;
begin
    if Assigned(fptrConvertINetString) or Load_MLang then
        Result := fptrIsConvertINetStringAvailable(dwSrcEncoding, dwDstEncoding)
    else
        Result := E_NOTIMPL;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF MSWINDOWS}
end.
