{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Fran�ois PIETTE
Description:
Creation:     April 2004
Version:      V9.0
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2004-2023 by Fran�ois PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.

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
Apr 10, 2009 Arno changed TBytes to an alias of SysUtils.TBytes in D2007 and
             better. Added alias EAbort.
Dec 03, 2009 Arno added some of the polymorphic integer types from
             Windows.pas/BaseTsd.h.
May 07, 2010 Arno added a few declarations.
Apr 15, 2011 Arno prepared for 64-bit.
May 06, 2011 Arno added TThreadID
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Feb 19, 2021 - V8.66 Added PUInt64.
Aug 08, 2023 V9.0  Updated version to major release 9.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Z.ICS9.OverbyteIcsTypes;

interface

{$I Include\Z.ICS9.OverbyteIcsDefs.inc}

uses
{$IFDEF MSWINDOWS}
  {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
  {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF};

const
  OverbyteIcsTypesVersion = 900;
  CopyRight : String      = ' OverbyteIcsTypes (c) 2004-2023 F. Piette V9.0 ';

type
{$IFNDEF COMPILER12_UP}
    UnicodeString = WideString;
    RawByteString = AnsiString;
{$ENDIF}
{$IFDEF COMPILER11_UP} // D2007 and better
  TBytes                    = {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.TBytes;
{$ELSE} // D7 - D2006
  TBytes                    = array of Byte;
{$ENDIF}

{$IFNDEF COMPILER15_UP}
  TThreadID                 = LongWord;
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$EXTERNALSYM size_t}
  {$IFDEF CPUX64}
    size_t                    = UInt64;
  {$ELSE}
    size_t                    = LongWord;
  {$ENDIF}
    Psize_t                   = ^size_t;
    PUInt64                   = ^UInt64;   { V8.66 }

  {$IFDEF COMPILER14_UP} // D2010 and better
      {$EXTERNALSYM HANDLE_PTR}
      HANDLE_PTR                = {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.HANDLE_PTR;
  {$ELSE} // D7 - D2009
      {$EXTERNALSYM HANDLE_PTR}
      HANDLE_PTR                = type LongWord;
  {$ENDIF}

  {$IFDEF COMPILER11_UP} // D2007 and better
      {$EXTERNALSYM INT_PTR}
      INT_PTR                   = {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.INT_PTR;
      {$EXTERNALSYM LONG_PTR}
      LONG_PTR                  = {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.LONG_PTR;
      {$EXTERNALSYM UINT_PTR}
      UINT_PTR                  = {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.UINT_PTR;
      {$EXTERNALSYM ULONG_PTR}
      ULONG_PTR                 = {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.ULONG_PTR;
      {$EXTERNALSYM DWORD_PTR}
      DWORD_PTR                 = {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.DWORD_PTR;
  {$ELSE} // D7 - D2006
      // From BaseTsd.h
      {$EXTERNALSYM INT_PTR}
      INT_PTR                   = Integer;
      {$EXTERNALSYM LONG_PTR}
      LONG_PTR                  = Longint;
      {$EXTERNALSYM UINT_PTR}
      UINT_PTR                  = Cardinal;
      {$EXTERNALSYM ULONG_PTR}
      ULONG_PTR                 = LongWord;
      {$EXTERNALSYM DWORD_PTR}
      DWORD_PTR                 = ULONG_PTR;
  {$ENDIF}
  {$EXTERNALSYM PINT_PTR}
  PINT_PTR                  = ^INT_PTR;
  {$EXTERNALSYM PUINT_PTR}
  PUINT_PTR                 = ^UINT_PTR;
{$ENDIF MSWINDOWS}

implementation

end.
