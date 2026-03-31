{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Description:  Posix types for Android and Linux
Version:      V9.2
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2024 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.

              This software is freeware and provided 'as-is', without any
              express or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of this
              software.

              The following restrictions apply:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.


History:
Jun 03, 2024 V9.2  Added Ics.Posix.EpollTypes and Ics.Posix.EpollApi, various changes to
                     TIcsEventQueue adding Linux and Android support in addition to MacOS
                     and IOS, not tested or supported yet.



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

 unit Z.ICS9.Ics.Posix.EpollApi;

interface

{$IF DEFINED(FPC)}
{$LINKLIB c}
{$ENDIF}

{$IF DEFINED(LINUX) or DEFINED(ANDROID)}

uses
  Posix.Base, {Posix.StdDef,} Posix.SysTypes, Posix.Signal,
  Z.ICS9.Ics.Posix.EpollTypes;
  { open an epoll file descriptozr }

function epoll_create(size: Integer): Integer; cdecl;
  external {.$IFDEF DELPHI29}libc name 'epoll_create'{.$ENDIF};

{ control interface for an epoll descriptor }
function epoll_ctl(epfd, op, fd: Integer; event: pepoll_event): Integer; cdecl;
  external libc name 'epoll_ctl';

{ wait for an I/O event on an epoll file descriptor }
function epoll_wait(epfd: Integer; events: pepoll_event; maxevents, timeout: Integer): Integer; cdecl;
  external libc name 'epoll_wait';

{ create a file descriptor for event notification }
function eventfd(initval: Cardinal; flags: Integer): Integer; cdecl;
  external libc name 'eventfd';

function __write(Handle: Integer; Buffer: Pointer; Count: size_t): ssize_t; cdecl;
  external libc name 'write';

{$ENDIF}

implementation

end.
