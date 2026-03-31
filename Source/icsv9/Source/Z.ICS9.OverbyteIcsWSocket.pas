{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  TWSocket class encapsulate the Windows Socket paradigm
Creation:     April 1996
Updated:      Jul 2025
Version:      V9.5
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 1996-2025 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
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
If not otherwise noted, changes are by Francois Piette
Jul 18, 1996  Move all low level socket to winsock to be Delphi 2.x compatible
Sep 18, 1996  Use structured exception for handling errors
Sep 19, 1996  Check csDestroying before invoking event handler
Nov 04, 1996  Better error handling
Jan 31, 1997  Changed property assignation for Addr, Port and Proto
              Added notification handler
Feb 14, 1997  Corrected bug in property assignation for Addr, Port and Proto
Mar 26, 1997  Make UDP protocol work correctly
              Enable UDP broadcasting by using addr 255.255.255.255
Apr 1, 1997   Added class function when independent of any open socket
              Moved InitData as global
              Added ReceivedFrom function
              Added ResolveHost function
Jul 22, 1997  Adapted to Delphi 3 which has a modified winsock.accept
Aug 13, 1997  'sin' member made public
Aug 24, 1997  Create the only help
              Makes writing HSocket the same as calling Dup.
Sep 5, 1997   Version 2.01, added WinsockInfo function
Sep 21, 1997  Version 2.02, make it really thread safe
                            created global WSocketVersion
Sep 25, 1997  Version 2.04, port to C++Builder
Sep 27, 1997  Version 2.05. All class methods converted to global
              procedure or function because C++Builder do not like
              class method very much.
              Old class method              New global function
              ----------------              -------------------
              WinsockInfo                   WinsockInfo
              SocketErrorDesc               WSocketErrorDesc
              GetHostByAddr                 WSocketGetHostByAddr
              GetHostByName                 WSocketGetHostByName
              ResolveHost                   WSocketResolveHost
              HostName                      LocalHostName
Oct 02, 1997  V2.06 Added a check in destructor to avoid calling WSACleanup at
              design time which crashes the excellent Eagle Software CDK.
Oct 16, 1997  V2.07 Added PortNum property with numeric value for Port.
              Added RcvdCount property to return the number of
              characters received in the buffer but not read yet. Do not
              confuse with ReadCount which returns the number of chars
              already received.
              Added a check for FWait assignation in front of ReadLine
              Prefixed each TSocketState value by 'ws' to avoid name conflict.
              Moved FHSocket member to private section because the property
              HSocket does the right job.
              Added a check for state closed when changing Port, Proto and Addr.
Oct 22, 1997  V2.08 Added Flush method (asked by john@nexnix.co.uk) and
              FlushTimeout property (default to 60 seconds).
Oct 22, 1997  V2.09 Added SendFlags property to enable sending in or out of
              band data (normal or urgent, see RFC-1122)
Oct 28, 1997  V2.10 Added an OnLineTooLong event and code to handle the case
              where ReadLine has been called and the buffer overflowed (line
              long)
Oct 29, 1997  V2.11 Added DnsLookup functionnality (DnsLookup method, DnsResult
              property and DnsLookupDone event).
              Calling the connect method with a hostname work well except that
              it could block for a long period (ie: 2 minutes) if DNS do not
              respond. Calling the connect method with a numeric IP address will
              never block. So you can call DnsLookup to start hostname
              resolution in the background, after some time you evenutually
              receive the OnDnsLookupDone event. The copy the DnsResult property
              to the Addr property and call connect.
Oct 30, 1997  V2.12 added a check in DnsLookup to handel numeric IP which do
              not require any lookup. The numeric IP is treated immediately
              and immediately trigger the DnsLookupDone event.
              I modified the code to be compatible with Delphi 1.
Oct 31, 1997  V2.13 added CancelDnsLookup procedure.
Nov 09, 1997  V2.14 add LocalIPList function to get the list of local IP
              addresses (you have two IP addresses when connected to a LAN
              and an ISP).
Nov 11, 1997  V2.15 Made TCustomWSocket with virtual functions. This will
              allow to easily descend a new component from TCustomWSocket.
              Make ReadLine stop when the connection is broken.
Nov 12, 1997  V2.16 Corrected bug (Justin Yunke <yunke@productivity.org>)
              in LocalIPList: phe should be checked for nil.
Nov 18, 1997  Added ReceiveStr function (Suggested by FLDKNHA@danisco.com)
Nov 30, 1997  V2.18 Added a call to OnDnsLookupDone when canceling.
Dec 04, 1997  V2.19 Added LocalPort property and SessionConnected event
              for UDP socket.
              V2.20 Modified MessageLoop and ProcessMessages to process not
              only the socket messages, but all messages (necessary if the
              thread has several TWSocket for example).
Dec 09, 1997  V2.21 Corrected a minor bug in ReceiveStr. Detected by
              david@e.co.za (David Butler).
Dec 10, 1997  V2.22 Corrected a minor bug in Send which now correctly
              returns the number of bytes sent. Detected by
              james.huggins@blockbuster.com
Dec 16, 1997  V2.23 Corrected a bug which prevented the receiving of datagram
              from a UDP socket.
              Thank to Mark Melvin (melvin@misrg.ml.org) for pointing it.
Dec 20, 1997  V2.24 Added the PeekData function as suggested by Matt Rose
              mcrose@avproinc.com
Dec 26, 1997  V2.25 Added the Text property as suggested by Daniel P. Stasinski
              <dse@pacific.net>. Made GetXPort work even when listening as
              suggested by is81024@cis.nctu.edu.tw.
Jan 10, 1998  V2.26 Check for null hostname in DNSLookup
              Added DnsResultList with all IP addresses returned form DNS
Jan 13, 1998  V2.27 a Added MultiThreaaded property to tell the component that
              it is working in a thread and should take care of it (call
              internal ProcessMessages in place of Application.ProcessMessages,
              and do not use the WaitCtrl object).
Jan 15, 1998  V2.28 WMAsyncSelect revisited to work properly with NT winsock 2.
Feb 10, 1998  V2.29 Added an OnError event. If not assigned, then the component
              raise an exception when the error occurs.
Feb 14, 1998  V2.30 Published Text property
Feb 16, 1998  V2.31 Added virtual methods to trigger events
              Renamed all event handler variable to begin with FOn
Feb 26, 1998  V2.32 Added procedure PutDataInSendBuffer and PutStringInSendBuffer
              Using PutDataInSendBuffer you can place data in the send buffer
              without actualy trying to send it. This allows to place several
              (probably small) data chunk before the component attempt to send
              it. This prevent small packet to be sent. You can call
              Send(nil, 0) to force the component to begin to send data.
              If the buffer was not empty, PutDataInSendBuffer will just queue
              data to the buffer. This data will be sent in sequence.
Mar 02, 1998  V2.33 Changed the error check with WSAstartup as pointed out by
              Donald Strenczewilk (dstrenz@servtech.com)
Mar 06, 1998  V2.34 Added a runtime property to change the buffer size.
Mar 27, 1998  V2.35 Adapted for C++Builder 3
Apr 08, 1998  V2.36 Made SetDefaultValue virtual
Apr 13, 1998  V2.37 Reset FDnsLookupHandle to 0 after a failed call to
              WSACancelAsyncRequest
Apr 22, 1998  V2.38 Published AllSent property to let outside know if our
              buffer has some data unsent.
Apr 28, 1998  V2.39 Added LingerOnOff and LingerTimeout. Default values are
              wsLingerOn and timeout = 0 to behave by default as before.
              This value is setup just before Connect. Call SetLingerOption to
              set the linger option on the fly (the connection must be
              established to set the option). See winsock.closesocket on line
              help (winsock.hlp or win32.hlp) for a dsicussion of this option
              usage.
May 06, 1998  V2.40 Added a workaround for Trumpet winsock inet_addr bug.
              Thanks to Andrej Cuckov <andrej@cuckov.com> for his code.
May 18, 1998  V2.41 Jan Tomasek <xtomasej@feld.cvut.cz> found that Trumpet
              Winsock (Win 3.11) has some bugs and suggested a workaround in
              TryToSend procedure. This workaround makes TWSocket blocking in
              some cases. A new property enables the workaround. See code.
Jun 01, 1998  V2.42 In finalization section, check for not assigned IPList.
Jun 15, 1998  V2.43 Added code to finalization section to unload winsock if
              still loaded at that point (this happend if no socket where
              created but WinsockInfo called). Suggested by Daniel Fazekas
              <fdsoft@dns.gyor-ph.hu>
Jun 27, 1998  V2.44 Added checks for valid arguments in SetPort, SetProto
              and SetAddr. Deferred address resolution until Connect or Listen.
Jul 08, 1998  V2.45 Adadpted for Delphi 4
Jul 20, 1998  V2.46 Added SetWindowLong(FWindowHandle, 0, 0) in the destructor
              and a check for TWSocket class in XSocketWindowProc.
              Added virtual method RealSend.
Jul 23, 1998  V2.47 Added a TriggerSessionClosed from TryToSend in case of
              send error. This was called before, but with a nul error argument.
              Now it correctly gives the error number.
              Added a trashcan to receive data if no OnDataAvailable event
              handler is installed. Just receive the data and throw it away.
              Added reverse dns lookup asynchronous code (IP -> HostName).
              Thanks to Daniel Fazekas <fdsoft@dns.gyor-ph.hu> for his code.
Jul 30, 1998  V2.48 Changed local variable "error" by FLastError in SocketError
              to make it available from the OnError handler. Thanks to
              dana@medical-info.com for finding this bug.
              In Abort procedure, deleted all buffered data because it was send
              the next time the socket is opened !
              Added CancelDnsLookup in Abort procedure.
Aug 28, 1998  V2.49 Made InternalClose and ReceiveStr virtual
Sep 01, 1998  V2.50 Ignore CancelDnsLookup exception during destroy
Sep 29, 1998  V2.51 In InternalClose, protect AssignDefaultValue with
              try/except because SessionClosed event handler may have destroyed
              the component.
Oct 11, 1998  V2.52 Changed Shutdown(2) to Shutdown(1) in Internal Close to
              prevent data lost on send. You may have to call Shutdown(2) in
              your own code before calling Close to have the same behaviour as
              before.
              Changed argument type for ASyncReceive and passed 0 from FD_CLOSE
              message handler.
Oct 28, 1998  V2.53 Made WSocketLoadWinsock and WSocketUnloadWinsock public.
Nov 11, 1998  V2.54 Added OnDisplay event for debugging purpose
Nov 16, 1998  V2.55 Ignore WSANOTINITIALIZED error calling CloseSocket. This
              occurs when using TWSocket from a DLL and the finalization
              section is called before destroying TWSocket components (this is
              a program logic error).
              Made some properties and methods protected instead of private.
              Made some methods virtual.
              Added an Error argument to InternalClose.
              Added DoRecv virtual function.
              Added WSocketResolvePort
              Added WSocketResolveProto
              Deferred port and protocol resolution until really needed
              Transformed Listen to procedure (in case of failure Listen
              always calls SocketError which triggers an exception or the
              OnError event).
Nov 22, 1998  V3.00 Skipped from V2.55 to V3.00. Socks support is major update!
              Added SOCKS5 (RFC-1928) support for TCP connection and
              simple usercode passwword authentication.
              Consider the socks code as beta !
              New properties: SocksServer, SocksPort, SocksUsercode,
              SocksPassword, FSocksAuthentication. New events: OnSocksError,
              OnSocksConnected, OnSocksAuthState.
              I used WinGate 2.1d to test my code. Unfortunately WinGate do
              not correctly handle user authentication, so the code here is
              just untested...
Dec 05, 1998  V3.10 Removed ReadLine feature using TWait component.
              Added new TCustomLineWSocket and TCustomSyncWSocket.
              Those modifications implies that the ReadLine functionnality is
              slightly changed. Notably, the end of line marker is now
              configurable and remains in the received line unless a timeout
              occurs or the buffer is too small.
Dec 10, 1998  V3.11 Added missing code to resolve port in the Listen method.
Dec 12, 1998  V3.12 Added write method for LocalPort property. Thanks to
              Jan Tomasek <xtomasej@feld.cvut.cz> for his code.
              Added background exception handling.
              Fixed a bug in TCustomLineWSocket.TriggerDataAvailable which was
              not calling the inherited function when it actually should.
              Added a check on multithreaded in WaitForClose to call the
              correct ProcessMessages procedure.
              Added SOCKS4 support (only tcp connect is supported).
Dec 28, 1998  V3.13 Changed WSocketResolveHost to check for invalid numeric
              IP addresses whitout trying to use them as hostnames.
Dec 30, 1998  V3.14 Changed SetPort to SetRemotePort to solve the SetPort
              syndrome with BCB. Also chnaged GetPort to be consistant.
Jan 12, 1999  V3.15 Introduced DoRecvFrom virtual function. This correct a bug
              introduced in V3.14 related to UDP and RecvFrom.
Jan 23, 1999  V3.16 Changed FRcvdFlag computation in DoRecv and DoRecvFrom
              because it caused problems with HTTP component and large blocks.
              Removed modification by Jan Tomasek in TriggerDataAvailable
Jan 30, 1999  V3.17 Added WSocketResolveIp function.
              Checked for tcp protocol before setting linger off in abort.
              Moved a lot of variables from private to protected sections.
              Removed check for Assigned(FOnDataSent) in WMASyncSelect.
Feb 03, 1999  V3.18 Removed useless units in the uses clause.
Feb 14, 1999  V4.00 Jump to next major version number because lots of
              fundamental changes have been done. See below.

              Use runtime dynamic link with winsock. All winsock functions
              used by TWSocket are linked at runtime instead of loadtime. This
              allows programs to run without winsock installed, provided program
              doesn't try to use TWSocket or winsock function without first
              checking for winsock installation.
              Removed WSocketLoadWinsock and all use to DllStarted because it
              is no longer necessary because winsock is automatically loaded
              and initialized with the first call to a winsock function.

              Added MessagePump to centralize call to the message pump.
              It is a virtual procedure so that you can override it to
              cutomize your message pump. Also changed slightly ProcessMessages
              to closely match what is done in the forms unit.

              Removed old stuff related to WaitCtrl (was already excluded from
              compilation using a conditional directive).

              Added NOFORMS conditional compilation to exclude the Forms unit
              from wsocket. This will reduce exe or dll size by 100 to 150KB.
              To use this feature, you have to add NOFORMS in your project
              options in the "defines" edit box in the "directory/conditional"
              tab. Then you must add a message pump to your application and
              call it from TWSocket.OnMessagePump event handler. TWSocket really
              need a message pump in order to receive messages from winsock.
              Depending on how your application is built, you can use either
              TWSocket.MessageLoop or TWSocket.ProcessMessages to quickly build
              a working message pump. Or you may build your own custom message
              pump taylored to your needs. Your message pump must set
              TWSocket.Terminated property to TRUE when your application
              terminates or you may experience long delays when closing your
              application.
              You may use NOFORMS setting even if you use the forms unit (GUI
              application). Simply call Application.ProcessMessages in the
              OnMessagePump event handler.
              OnMessagePump event is not visible in the object inspector. You
              must assign it at run-time before using the component and after
              having created it (in a GUI application you can do that in the
              FormCreate event, in a console application, you can do it right
              after TWSocket.Create call).
Feb 17, 1999  V4.01 Added LineEcho and LineEdit features.
Feb 27, 1999  V4.02 Added TCustomLineWSocket.GetRcvdCount to make RcvdCount
              property and ReceiveStr work in line mode.
Mar 01, 1999  V4.03 Added conditional compile for BCB4. Thanks to James
              Legg <jlegg@iname.com>.
Mar 14, 1999  V4.04 Corrected a bug: wsocket hangup when there was no
              OnDataAvailable handler and line mode was on.
Apr 21, 1999  V4.05 Added H+ (long strings) and X+ (extended syntax)
              compilation options
May 07, 1999  V4.06 Added WSAECONNABORTED to valid error codes in TryToSend.
Jul 21, 1999  V4.07 Added GetPeerPort method, PeerPort and PeerAddr propertied
              as suggested by J. Punter <JPunter@login-bv.com>.
Aug 20, 1999  V4.05 Changed conditional compilation so that default is same
              as latest compiler (currently Delphi 4, Bcb 4). Should be ok for
              Delphi 5.
              Added LocalAddr property as suggested by Rod Pickering
              <fuzzylogic123@yahoo.com>. LocalAddr default to '0.0.0.0' and is
              intended to be used by a client when connecting to a server, to
              select a local interface for multihomed computer. Note that to
              select an interface for a server, use Addr property before
              listening.
              LocalAddr has to be an IP address in dotted form. Valid values are
              '0.0.0.0' for any interface, '127.0.0.1' for localhost or any
              value returned by LocalIPList.
              Replaced loadtime import for ntohs and getpeername by runtime
              load.
              Revised check for dotted numeric IP address in WSocketResolveHost
              to allow correct handling of hostnames beginning by a digit.
              Added OnSendData event. Triggered each time data has been sent
              to winsock. Do not confuse with OnDataSent which is triggered
              when TWSocket internal buffer is emptyed. This event has been
              suggested by Paul Gertzen" <pgertzen@livetechnology.com> to
              easyly implement progress bar.
              Corrected WSocketGetHostByAddr to make it dynamically link to
              winsock.
Sep 5, 1999   V4.09 Added CloseDelayed method.
              Make sure that TriggerSessionClosed is called from WMASyncSelect
              and InternalClose, even if there is no OnSessionClosed event
              handler assigned. This is required to make derived components
              work correctly.
              Created message WM_TRIGGER_EXCEPTION to help checking background
              exception handling (OnBgException event).
              Corrected bug for Delphi 1 and ReallocMem.
Oct 02, 1999  V4.10 Added Release method.
Oct 16, 1999  V4.11 Corrected a bug in TCustomLineWSocket.DoRecv: need to move
              data in front of buffer instead of changing buffer pointer which
              will crash the whole thing at free time.
Oct 23, 1999  V4.12 Made WSocketIsDottedIP a public function
Nov 12, 1999  V4.13 removed 3 calls to TriggerSocksAuthState because it was
                    called twice. By A. Burlakov <alex@helexis.com>.
Jan 24, 1999  V4.14 Call Receive instead of DoRecv from ReceiveStr to be sure
              to set LastError correctly. Thanks to Farkas Balazs
              <megasys@www.iridium.hu>
              Suppressed FDllName and used winsocket constant directly. I had
              troubles with some DLL code and string handling at program
              termination.
Apr 09, 2000 V4.15 Added error number when resolving proto and port
Apr 29, 2000 V4.16 Added WSocketForceLoadWinsock and
             WSocketCancelForceLoadWinsock. Thanks to Steve Williams.
             Created variable FSelectEvent to store current async event mask.
             Added ComponentOptions property with currently only one options
             wsoNoReceiveLoop which disable a receive loop in AsyncReceive.
             This loop breaking was suggested by Davie <smatters@smatters.com>
             to lower resource usage with really fast LAN and large transfers.
             By default, this option is disabled so there is no change needed
             in current code.
May 20, 2000 V4.17 Made TSocket = u_int (same def as in winsock.pas)
             Moved bind after setting options.
             Thanks to Primoz Gabrijelcic <fab@siol.net>
Jul 15, 2000 V4.18 Alon Gingold <gingold@hiker.org.il> changed
             TCustomSocksWSocket calls to inherited triggers of
             TriggerSessionConnected and TriggerDataAvailable.
             Now, it calls the trigger directly. This solves the problem
             of descendent classes with overridden triggers, not being
             called when a REAL connection was established, and when real
             data starts coming in. Special care MUST be taken in such
             overridden triggers to ONLY call the inherited trigger AND
             IMMEDIATELY EXIT when FSocksState <> socksData to avoid loopback
Jul 22, 2000 V4.19 John Goodwin <john@jjgoodwin.com> found a failure in the
             logic for DnsLookup. He also implemented a workaround.
             See DnsLookup comments for explanation.
Aug 09, 2000 V4.20 Alon Gingold <gingold2@mrm-multicat.com> found a bug in
             SOCKS4 implementation where a nul byte was incorrectly added
             (it should be added only with SOCKS4A version, not straith
             SOCKS4).
Sep 17, 2000 V4.21 Eugene Mayevski <Mayevski@eldos.org> added TWndMethod for
             NOFORMS applications in other components.
Oct 15, 2000 V4.22 Added method GetXAddr which returns local IP address to
             which a socket has been bound. There was already a GetXPort.
             Thanks to Wilfried Mestdagh <wilfried_sonal@compuserve.com>
             and Steve Williams <stevewilliams@kromestudios.com>.
Nov 08, 2000 V4.23 Moved FSelectEvent from private to protected section.
Nov 11, 2000 V4.24 Added LineLimit property and OnLineLimitExceeded event.
             When using line mode, line length is checked as each data block is
             comming. If the length is greater than the limit, then the event
             is triggered. You have the opportunity to close the socket or
             change the limit to a higher value. Thus you can prevent a hacker
             from locking your system by sending unlimited line which otherwise
             would eat up all system resources.
             Changed line handling variables to LongInt
             Checked all length involved in StrPCopy calls.
Nov 26, 2000 V4.25 Do not trust GetRcvdCount. Always call Receive to check for
             incomming data (sometime NT4 will hang if we don't do that).
Jan 24, 2001 V4.26 Blaine R Southam <bsoutham@iname.com> fixed out of bound
             error in TCustomLineWSocket.TriggerDataAvailable
Feb 17, 2001 V4.27 Davie <smatters@smatters.com> fixed a bug causing byte lost
             when closing (related to wsoNoReceiveLoop option).
May 04, 2001 V4.28 Fixed W2K bug (winsock message ordering)
Jun 18, 2001 V4.29 Added AllocateHWnd and DeallocateHWnd from Forms unit to
             avoid warning from Delphi 6 in all other components.
Jul 08, 2001 V4.30 Fixed small bug related to NOFOMRS and V4.29
Jul 26, 2001 V4.31 Checked csDesigning in GetRcvdCount so that Delphi 6 does'nt
             crash when object inspector wants to display RcvdCount value.
             Added multicast capability and UDP ReuseAddr. Thanks to Mark
             G. Lewis <Lewis@erg.sri.com> for his code.
             Added TriggerSessionClosed to SocketError as suggested by Wilfried
             Mestdagh <wilfried_sonal@compuserve.com>
Jul 28, 2001 V4.32 New option wsoTcpNoDelay implemented. Code by Arnaldo Braun
             <abraun@th.com.br>
Jul 30, 2001 V4.33 Corrected at few glitches with Delphi 1
Sep 08, 2001 V4.34 Added ThreadAttach and related functions
Nov 27, 2001 V4.35 Added type definition for in_addr and Delphi 2 (Yes there are
             still some peoples who wants to use it. Don't ask me why !).
Dec 02, 2001 V4.36 david.brock2@btinternet.com found a bug in SOCKS4 where
             error check incorrectly checked "FRcvBuf[1] = #$90" instead of
             "FRcvBuf[1] <> #90". He also found a bug when receiving domain name
             where length of name was incorrectly copyed to the buffer.
Dec 23, 2001 V4.37 Removed bWrite, nMoreCnt, bMoreFlag and nMoreMax which where
             not more really used. Thanks to Al Kirk <akirk@pacific.net> for
             showing that.
Feb 24, 2002 V4.38 Wilfried Mestdagh <wilfried@mestdagh.biz> added ThreadDetach
             and a property editor for LineEnd. XSocketDeallocateHWnd made a
             function.
             I created a new unit WSocketE.pas to put Wilfried's property
             editor so that it works correctly with Delphi 6.
Apr 24, 2002 V4.39 Removed OnLineTooLong event which was not used anywhere.
             Use OnLineLimitExceeded event if you used this event.
             Thanks to Alex Kook <cookis@mail.ru> for finding this one.
Apr 27, 2002 V4.40 Added procedure WSocketUnregisterClass to be able to
             unregister hidden window. This is necessary when TWSocket is
             used within a DLL which is unloaded and reloaded by applications,
             specially when running with Windows-XP. Thanks to Jean-Michel Aliu
             <jmaliu@jmasoftware.com> who provided a test case.
Jun 02, 2002 V4.41 allow SOCK_RAW in Connect method for any protocol which is
             not TCP or UDP. Thanks to Holger Lembke <holger@hlembke.de>.
Jun 04, 2002 V4.42 Do not call Listen for SOCK_RAW.
             Thanks to Holger Lembke <holger@hlembke.de>.
Jun 08, 2002 V4.43 Add a dummy Register procedure for BCB1.
             Thanks to Marc-Alexander Prowe <listen@mohajer.de>.
Jul 07, 2002 V4.44 Added code in Connect method to check if socket still opened
             after OnChangeState event. If not, trigger an error WSAINVAL.
Sep 16, 2002 V4.45 Exposed RcvdPtr and RcvdCnt readonly properties.
Sep 17, 2002 V4.46 Used InterlockedIncrement/InterlockedDecrement to Inc/Dec
             socket count safely when TWSocket is used within a thread. This
             was proposed by Matthew Meadows <matthew.meadows@inquisite.com>
Sep 28, 2002 V4.47 Changed DnsLookup so that a hostname is checked for dotted
             IP addresse and resolve it numerically. Thanks to Bogdan Calin
             <soul4blade@yahoo.com> who found this bug. Alos loaded the result
             list with the address to be consistant with real lookup result.
Nov 17, 2002 V4.48 Roland Klabunde <roland.klabunde@gmx.net> found a bug in
             multicast code: listening on a specific interface was ignored.
             He fixed Listen and Connect.
Nov 27, 2002 V4.49 Added ListenBacklog property, default to 5.
Dec 17, 2002 V4.50 Moved code to virtual function to permit SSL implementation.
Jan 19, 2003 V5.00 First pre-release for ICS-SSL. New major version number
             V5.01 Gabi Slonto <buffne01@gmx.net> found a bug in DnsLookup
             when hostname was actulally a dotted IP address.
Mar 18, 2003 V5.02 Fixed WSocketIsDottedIP: reordering of boolean expressions
             involaving a string. Thanks to Ian Baker <ibaker@codecutters.org>
Apr 30, 2003 V5.03 Replaced all calls to setsockopt by calls to
             WSocket_setsockopt to avoid statically linked winsock DLL.
             Thanks to Piotr Dalek <enigmatical@interia.pl>.
             Also replaced inet_addr by WSocket_inet_addr.
Aug 27, 2003 V5.04 Marco van de Voort <marcov@stack.nl> added FreePascal (FPC)
             conditional compilation. Please contact him for any FPC support
             question.
Aug 28, 2003 V5.05 Fixed a multithreading issue related to windows class
             registration. Now using a critical section around the code.
             Thanks to Bogdan Ureche <bureche@omnivex.com> for his precious help.
Aug 31, 2003 V5.06 Added warning about deprecated procedures Synchronize,
             WaitUntilReady and ReadLine. Do not use them in new applications.
Sep 03, 2003 V5.07 Bogdan Ureche <bureche@omnivex.com> added a critical section
             to avoid problem when winsock.dll is unloaded by a thread while
             another thread is still using some TWSocket.
Sep 15, 2003 V5.08 Fixed finalization section to no free critical section if
             a TWSocket is still existing. This happend for example when a
             TWSocket is on a form and Halt is called from FormCreate event.
             Changed SendStr argument to const.
Nov 09, 2003 V5.09 Added manifest constants for Shutdown
             Added TCustomLineWSocket.SendLine method.
Jan 16, 2004 V5.10 Added "const" in front of all method using strings.
Jan 17, 2004 V5.11 Modified TriggerDataAvailable so that when in LineMode, we
             check if a line is still in the buffer of already received data.
             Also updated WMTriggerDataAvailable to avoid infinite loops.
             Introduced FLineFound to flag when a line has been found.
             See "OLD_20040117" to find this code.
Jan 21, 2004 V5.12 Checked null string in PutStringInSendBuffer and null
             pointer in PutDataInSendBuffer.
Jan 26, 2004 V5.13 Conditional compilation for BCB for constants for Shutdown.
             Reordered uses clause for FPC compatibility.
             Fixed TCustomLineWSocket.TriggerDataAvailable to deliver data
             already received while in line mode but after component user
             turned line mode off in the middle of the way. This could occur
             for example in a HTTP application where line mode is used to
             receive HTTP header line and turned off when last header line is
             found. At that point, if posted data (HTTP document) was completely
             in the same packet as the last header line, that data was not
             delivered until the next packet comes, which could never occur !
Mar 20, 2004 V5.14 Added partial support for RAW socket.
             To use RAW sockets, set Proto to 'raw_ip', 'raw_icmp', ...
             Set Port to '0' or whatever value is useful for the protocol.
             When using IP protocol, you can add option wsoSIO_RCVALL so that
             your program receive ALL datagrams when you listen on a given
             interface (You can't use 0.0.0.0).
             Do not use Connect with RAW socket. Always use Listen and then
             use SendTo to send datagrams use the socket.
             Added ReqVerHigh and ReqVerLow properties to be able to select
             which winsock version you want to load. Default to 1.1 but need
             2.2 for RAW sockets to be used.
Mar 24, 2004 V5.15 Changed WSocket_Synchronized_ResolveProto to hard code
             protocol number for tcp, udp and raw.
Apr 17, 2004 V6.00 New major release started. Move all platform and runtime
             dependencies to separate units. New base component for handling
             component with window handle.
Jun 20, 2004 V 5.16 John Mulvey <john@mulvey.eurobell.co.uk> fixed error message
             in GetPeerAddr which incorrectly reported an error about
             GetPeerName.
May 23, 2005 V5.17 PutDataInSendBuffer set bAllSent to false.
Jun 03, 2005 V5.18 Added SocketSndBufSize property which gives the size of
             winsock internal send buffer. When using TCP, you must make sure
             you never use a BufSize equal or greater than this value or
             you'll experience bad performances. See description in MSDN
             http://support.microsoft.com/default.aspx?scid=kb;en-us;823764
             Default value for BufSize is 1460 and SocketSndBufSize is 8192 so
             there is no problem when not changing those values.
Jun 18, 2005 V5.19 Made TCustomSocksWSocket.Connect accept 'tcp' as well as '6'
             for protocol. By Piotr "Hellrayzer" Dalek.
             Renamed event OnDisplay to OnDebugDisplay.
Sept 4, 2005 V5.20 added BufferedByteCount property used to ensure winsock has sent
             data, currently used in TFtpCli to check a put has finished correctly
             Thanks to Tobias Giesen <tobias@tgtools.de> for the fix
Dec 27, 2005 V6.00a Updated new release with change done in the old release.
Dec 31, 2005 V6.00b added new debug and logging event and log levels, replacing
             conditional debug code with optional code to avoid rebuilding apps.
             Works in combination with new component TIcsLogger.
             This is controlled by the new LogOptions property:
               loDestEvent - write to OnIcsLogEvent (called from higher level protocols)
               loDestFile - write to file debug_out.myprog.txt
               loDestOutDebug - write to OutputDebugString (shown in Debugger Event Log window)
               loAddStamp - time stamp each log line (accurate only to about 18ms)
               loWsockErr - log wsocket errors
               loWsockInfo - log wsocket general information
               loWsockDump - log wsocket data (not implemented yet)
               loSslErr - log SSL errors
               loSslInfo - log SSL general information
               loSslDump - log SSL packets and data
               loProtSpecErr - log protocol specific error
               loProtSpecInfo - log protocol specific general information
               loProtSpecDump - log protocol specific data and packets
Jan 22, 2006 V6.00c Added some KeepAlive stuff (seems winsock is bugged and
             doesn't care any setting done !).
Jan 28, 2006 V6.00d Gerhard Rattinger fixed SetKeepAliveOption for Delphi 3
Mar 09, 2006 V6.00e Arno made properties to select keepalive parameters.
             He also fixed ReverseDnsLookup to return a list of
             host names (aliases) instead of just the first entry. Added func.
             ReverseDnsLookupSync.
Apr 27, 2006 V6.00f Roger Tinembart <tinembart@brain.ch> added a critical section
             around the list of sendbuffers (FBufHandler) to avoid problems when
             the data is placed in the sendbuffer (for example with SendStr)
             by a different thread than the one that is effectively sending the
             data with TryToSend
June 11, 2006 V6.01 Use new TIcsBufferHandler.
Aug 06, 2006 V6.02 Angus added GetWinsockErr to give alpha and numeric winsock
             errors and improved many other error messages,
             and fixed FReadCount for 64-bit downloads
             added some EXTERNALSYM for BCB compatiblity
Aug 18, 2006 V6.03 Fixed a bug in ASyncReceive(). This bug caused data loss.
Oct 28, 2006 V6.04 Added setter for SocketSndBufSize and SocketRcvBufSize
Dec 22, 2006 V6.05 Oliver Grahl fixed SendLine to properly count LineEnd characters.
Jan 18, 2007 V6.06 Fixed constructor and DeleteBufferedData to behave correctly
             when an exception occur in AllocateSocketHWnd.
Mar 23, 2007 V6.07 Removed FD_CONNECT from dup().
Apr 04, 2007 V6.08 Arno Garrels updated SetKeepAliveOption
Mar 10, 2008 V6.09 Francois Piette & Arno Garrels made some changes to
                   prepare code for Unicode
                   WSocket_gethostname conversion from String to AnsiString
                   WSocketGetProc and WSocket2GetProc use AnsiString
                   GetAliasList simplified and use AnsiString
Apr 25, 2008 V6.10 A. Garrels, added some getters/setters to store and use some
             string-property-values as AnsiString internally.
             This reduced number of string casts with potential data loss to 17.
             These ansi-values are used to call winsock API that doesn't provide
             W functions. Modified depending code including some type changes
             from PChar to PAnsiChar. Made some casts Unicode => Ansi with
             potential data loss *explicit* casts (conditionally compiled) some
             unicode strings with only 7 bit ASCII characters are casted using
             new function UnicodeToAscii() in new unit OverbyteIcsUtils which
             should be fast and reliable and doesn't produce compiler warnings.
             Added new warning symbols.
Apr 30, 2008 V6.11 A. Garrels - Function names adjusted according to changes in
             OverbyteIcsLibrary.pas.
May 11, 2008 V6.12 USchuster removed local atoi implementation (atoi is now in
             OverbyteIcsUtils.pas)
May 15, 2008 V6.13 AGarrels type change of some published String properties
             to AnsiString, this is an attempt to avoid too many implicit
             string casts, only a few higher level components have been adjusted
             accordingly so far.
Jun 30, 2008 A.Garrels made some changes to prepare SSL code for Unicode.
Jul 04, 2008 V6.11 Rev.58 SSL - Still lacked a few changes I made last year.
Jul 13, 2008 V6.12 Added SafeWSocketGCount
Aug 03, 2008 V6.16 A. Garrels removed packed from record TExtension.
Jul 07, 2008 V6.17 Still a small fix from December 2007 missing in SSL code.
Aug 11, 2008 V6.18 A. Garrels - Type AnsiString rolled back to String.
             Two bugs fixed in SSL code introduced with Unicode change.
             Socks was not fully prepared for Unicode.
Sep 19, 2008 V6.19 A. Garrels changed some AnsiString types to RawByteString.
Sep 21, 2008 V6.20 A. Garrels removed BoolToStr(), available since D7
Oct 22, 2008 V7.21 A. Garrels removed the const modifier from parameter Data
             in function SendTo to fix a bug in C++ Builder.
Nov 03, 2008 V7.22 Added property Counter, a class reference to TWSocketCounter
             which provides some useful automatic counters. By default property
             Counter is unassigned and has to be enabled by a call to
             CreateCounter.
Apr 24, 2009 V7.23 A. Garrels added *experimental* OpenSSL engine support which
             is not compiled in by default. You have to uncomment conditional
             define OPENSSL_NO_ENGINE in OverbyteIcsSslDefs.inc and rebuild your
             packages to get it included. With engine support included a new
             published property AutoEnableBuiltinEngines of TSslContext has to
             be set to TRUE in order to enable OpenSSL's built-in hardware
             accelerators support, that's all.

             ******************************************************************
             * Due to the lack of hardware this feature is completely untested*
             ******************************************************************

             Any feedback and fixes are welcome, please contact the ICS mailing
             list. The OpenSSL engine documentation can be found here:
             http://openssl.org/docs/crypto/engine.html

             Additionally a new component TSslEngine is installed on the palette.
             Its purpose is to control (dynamic) engines.

             Typically control commands of an OpenSC dynamic pkcs11 engine
             (SmartCard) are :

             Cmds.Add('SO_PATH=d:\opensc\bin\engine_pkcs11.dll');
             Cmds.Add('ID=pkcs11');
             Cmds.Add('LIST_ADD=1');
             Cmds.Add('LOAD=');
             Cmds.Add('MODULE_PATH=d:\opensc\bin\opensc-pkcs11.dll');
             Cmds.Add('INIT='); <= Special ICS-control command to initialize the engine

             Sample test code (Dod couldn't get it working :(

             It assumes that the X509 certificate has been exported from
             the SmartCard to PEM file that is available in property
             SslCertFile. It's also assumed that SslEngine1 is created
             dynamically at run-time in this sample.
             We are in new event TSslContext.OnBeforeInit:

             if not Assigned(SslEngine1) then
             begin
                SslEngine1 := TSslEngine.Create(Self);
                try
                  SslEngine1.NameID := 'dynamic';

                  // The SmartCard holds the private key.
                  // Next two lines advise SslContext to load the key
                  // from the engine instead from PEM file.
                  TSslContext(Sender).CtxEngine := SslEngine1;
                  SslEngine1.CtxCapabilities := [eccLoadPrivKey];

                  // The PIN code is expected in property SslPassPhrase
                  TSslContext(Sender).SslPassPhrase := 'ics';

                  // Tell the engine which key to use.
                  SslEngine1.KeyID := KeyIdEdit.Text;

                  // At first open the engine
                  if not SslEngine1.Open then
                      raise Exception.Create(FEngine.LastErrorMsg);

                  // Now send our vendor specific control commands
                  for I := 0 to Cmds.Count -1 do
                  begin
                    if not SslEngine1.Control(Cmds.Names[I],
                                              Cmds.ValueFromIndex[I]) then
                        raise Exception.Create(SslEngine1.LastErrorMsg);
                  end;

                  Display('Engine set up and loaded successfully');
                except
                    FreeAndNil(SslEngine1);
                    raise;
                end;
             end;

Jun 12, 2009 V7.24 Angus added WriteCount property, how many bytes sent since
                     connection opened
                   Only reset ReadCount when connection opened, not closed
Jul 16, 2009 V7.25 Arno fixed and changed SetCounterClass()
Jul 19, 2009 V7.26 Arno - SSL code ignored FPaused flag, the change is in
                   TCustomSslWSocket.TriggerEvent.
Sep 04, 2009 V7.27 Set option TCP_NODELAY in Dup as well as provide a public
                   method to set this option, similar as suggested by
                   Samuel Soldat.
Sep 08, 2009 V7.28 Arno - Minor Unicode bugfix in TX509Base.GetExtension().
Sep 09, 2009 V7.29 Arno - Added new public methods TX509Base.WriteToBio() and
                   TX509Base.ReadFromBio(). Method SafeToPemFile got an arg.
                   that adds human readable certificate text to the output.
                   InitializeSsl inlined. Removed a Delphi 1 conditional.
Sep 17, 2009 V7.30 Anton Sviridov optimized setting of SSL options.
Sep 17, 2009 V7.31 Arno fixed a Unicode bug in TX509Base.GetExtension and
                   a general bug in TX509Base.GetSha1Hash (AnsiString as
                   digest buffer should really be avoided)
Sep 18, 2009 V7.32 Arno changed visibility of TX509Base.WriteToBio() and
                   TX509Base.ReadFromBio() to protected.
Nov 01, 2009 V7.33 Arno fixed a memory overwrite bug in
                   TCustomSocksWSocket.DoRecv().
Nov 07, 2009 V7.34 OpenSSL V0.9.8L disables session renegotiation due to
                   TLS renegotiation vulnerability.
Dec 20, 2009 V7.35 Arno added support for SSL Server Name Indication (SNI).
                   SNI has to be turned on in OverbyteIcsSslDefs.inc, see define
                   "OPENSSL_NO_TLSEXT". Exchanged symbol "NO_ADV_MT" in the
                   SSL source by "NO_SSL_MT" (This and SNI was sponsored by
                   Fastream Technologies).
                   SNI Howto: In SSL server mode assign event OnSslServerName,
                   it triggers whenever a client sent a server name in the TLS
                   client helo. From the event handler read public property
                   SslServerName, lookup and pass a matching, valid and
                   initialized SslContext instance associated with the server name.
                   In SSL client mode, if property SslServerName was not empty
                   this server name is sent to the server in the TLS client helo.
                   Currently IE 7 and FireFox >= V2 support SNI, note that both
                   browers don't send both "localhost" and IP addresses as
                   server names, this is specified in RFC.
Dec 24, 2009 V7.36 SSL SNI - Do not switch context if not initialized.
Dec 26, 2009 V7.37 Arno fixed TCustomSyncWSocket.ReadLine for Unicode. It
                   now takes an AnsiString buffer. Since this method is highly
                   deprecated it's also marked as "deprecated". Do not use it
                   in new applications.
May 08, 2010 V7.38 Arno Garrels added support for OpenSSL 0.9.8n. Read comments
                   in OverbyteIcsLIBEAY.pas for details.
May 16, 2010 V7.39 Arno Garrels reenabled check for nil in WMAsyncGetHostByName.
Jun 10, 2010 V7.40 Arno Garrels added experimental timeout and throttle feature
                   to TWSocket. Currently both features have to be enabled
                   explicitly with conditional defines BUILTIN_TIMEOUT
                   and/or BUILTIN_THROTTLE (see OverbyteIcsDefs.inc )
Aug 02, 2010 V7.41 Arno removed an option to send plain UTF-16 strings with
                   SendStr() and SendLine() by passing 1200 (CP_UTF16) in the
                   codepage parameter. Changed SendLine() to return correct
                   number of bytes written.
Aug 08, 2010 V7.42 FPiette prevented socket close in TCustomWSocket.Destroy when
                   socket state is wsInvalidState (this happend when an
                   exception is raise early in the constructor).
Sep 05, 2010 V7.43 Arno fixed a bug in the experimental throttle and timeout
                   source which made it impossible to use both features at the
                   same time. Renamed conditionals EXPERIMENTAL_THROTTLE and
                   EXPERIMENTAL_TIMEOUT to BUILTIN_THROTTLE and BUILTIN_TIMEOUT.
                   It's now possible to either enable them in OverbyteIcsDefs.inc
                   or define them in project options.
Sep 08, 2010 V7.44 Arno reworked the experimental timeout and throttle code.
                   Method names of TCustomTimeoutWSocket **changed**, they all
                   got prefix "Timeout". Removed the crappy TCustomTimerWSocket
                   class, both throttle and timeout use their own TIcsThreadTimer
                   instance now.
Sep 08, 2010 V7.45 Fixed a typo in experimental throttle code.
Sep 11, 2010 V7.46 Arno added two more SSL debug log entries and a call to
                   RaiseLastOpenSslError in TCustomSslWSocket.InitSSLConnection.
                   Added function OpenSslErrMsg.
Sep 23, 2010 V7.47 Arno fixed a bug in the experimental throttle code and made
                   it more accurate. Thanks to Angus for testing and reporting.
                   Method Resume with SSL enabled did not always work.
Oct 10, 2010 V7.48 Arno - MessagePump changes/fixes.
Oct 14, 2010 V7.49 Arno - Abort TCustomLineWSocket as soon as possible.
Oct 15, 2010 V7.50 Arno - Made function IsSslRenegotiationDisallowed available.
Oct 16, 2010 V7.51 Arno removed dummy ancestor TBaseParentWSocket, it was not
                   required to make D7's structure view happy.
Nov 08, 2010 V7.52 Arno improved final exception handling, more details
                   in OverbyteIcsWndControl.pas (V1.14 comments).
Dec 06, 2010 V7.53 Arno added thread-safe TSslContext.FreeNotification and
                   TSslContext.RemoveFreeNotification.
Dec 07, 2010 V7.54 Arno - TSslBaseComponent, thread-safe removal of IcsLogger's
                   free notification.
Feb 04, 2011 V7.55 Angus - allow BandwidthLimit to be changed during connection
Feb 09, 2011 V7.56 Arno added HTTP V1.1 transparent proxy support including
                   Basic and NTLM authentication implemented as
                   TCustomHttpTunnelWSocket. This feature is still in beta
                   state, mainly because I only tested against WinGate proxy
                   server. My goal was to make it as fast as possible, there
                   are no or just very few protected methods available to
                   override so far. Best performance is achieved with one of the
                   authentication types "htatNone", "htatBasic" or "htatNtlm".
                   With "htatDetect" the last successful authentication type is
                   cached until the proxy server name changes or the TWSocket
                   object is freed.
Feb 09, 2011 V7.57 Arno added public property HttpTunnelLastResponse to
                   TCustomHttpTunnelWSocket. HTTP status codes are no longer
                   triggered as is but added to a base of global const
                   "ICS_HTTP_TUNNEL_BASEERR".
Feb 13, 2011 V7.58 Arno - HTTP tunnel accepts LF as end-of-line marker.
                   In both SOCKS and HTTP tunnel trigger an error from
                   SessionClosed if the proxy drops the connection unexpectedly.
                   New functions to get error messages from error codes.
                   WSocketHttpTunnelErrorDesc, WSocketSocksErrorDesc
                   WSocketProxyErrorDesc, WSocketErrorMsgFromErrorCode
                   WSocketGetErrorMsgFromErrorCode. The latter two are general
                   purpose functions that currently try to translate winsock and
                   proxy error codes to string.
Feb 14, 2011 V7.59 Arno - SessionConnected triggered twice when a connection to
                   proxy could not be established, introduced in V7.58.
                   Added function WSocketIsProxyErrorCode(): Boolean.
Feb 14, 2011 V7.60 Arno - SOCKS4/SOCKS4A Unicode bug fixed in
                   TCustomSocksWSocket.SocksDoConnect.
Feb 15, 2011 V7.61 Arno - Workaround a false version number (5) in WinGate's
                   SOCKS5 authentication response when user credentials are
                   wrong (reports an authentication error now rather than a
                   SOCKS version error). WSocketProxyErrorDesc and
                   WSocketGetErrorMsgFromErrorCode added a hyphen as separator
                   to the message. Removed a string cast warning.
Feb 16, 2011 V7.62 Arno fixed a memory overwrite bug in TCustomHttpTunnelWSocket
                   DataAvailable. Improved keep-alive handling and removed the
                   hard check for correct HTTP version. It still supports
                   HTTP/1.1 only, however is now partly compatible with 3Proxy.
                   It has been tested successfully with WinGate, Sambar,
                   Fastream IQ Proxy Server and 3Proxy. Note that detection of
                   authentication type works only with persistent connections,
                   with 3Proxy you have to explicitly set the authentication type.
Feb 20, 2011 V7.63 Arno added digest authentication to TCustomHttpTunnelWSocket.
                   Define "NO_HTTP_TUNNEL_AUTHDIGEST" to exclude it from a build,
                   so far it's tested with ICS-based IQ Proxy Server only. Some
                   new property setters.
Feb 21, 2011 V7.64 Arno - New ComponentOption "wsoNoHttp10Tunnel" treats HTTP/1.0
                   responses as errors. If "wsoNoHttp10Tunnel" is not set send
                   "Keep-Alive" header with NTLM message #1 in order to make
                   HTTP/1.0 proxies happy (MS Proxy Server 2.0 tested).
Feb 26, 2011 V7.65 Arno - TCustomHttpTunnelWSocket strongly improved.
                 - Bugfix: Ensure that internally buffered application data
                   is read by the upper layer by posting a fake FD_READ message
                   if required.
                 - Better support of HTTP/1.0 proxies.
                 - Performs an internal reconnect when the connection has to
                   be closed after a 407 status code and the server provides a
                   supported authentication method, as a result htatDetect
                   works with non-persistent connections as well.
                 - Tested with Squid 2.7.STABLE8 for Windows successfully.
                 - Tested with CSM proxy 4.2 successfully after adding a
                   workaround (requires at least two header lines being sent).
                 - ISA Server 2006 tested successfully except with digest auth.
                   I wasn't able to get digest authentication working neither IE
                   nor Firefox got authenticated. Interesting is that ISA uses
                   digest algorithm "MD5-sess" that I enabled in
                   OverbyteIcsDigestAuth.pas now, untested!
Feb 28, 2011 V7.66 Arno - TCustomHttpTunnelWSocket bugfix, do not explicitly
                   close the connection in HttpTunnelTriggerResultOrContinue
                   when FD_CLOSE notification from winsock has been received yet.
Feb 28, 2011 V7.67 Arno - TCustomHttpTunnelWSocket HttpTunnelGetNtlmMessage3,
                   send challenge domain name only if user code doesn't
                   include one.
Mar 16, 2011 V7.68 Anton S. added two debug messages.
Mar 21, 2011 V7.69 Method Abort no longer triggers an exception nor event OnError
                   if a call to winsock API WSACancelAsyncRequest in method
                   CancelDnsLookup failed for some reason.
Apr 10, 2011 V7.70 Arno added property SslVerifyFlags to the TSslContext. This enables
                   the component user to include certificate revocation lists (CRL)
                   added thru SslCRLFile and SslCRLPath in the certificate verification
                   process. However enabling CRL-checks needs some additional
                   action in the OnSslVerifyPeer event since OpenSSL will the
                   trigger any error related to CRLs. If there's, for instance,
                   no CRL available or it has expired etc.. You find the possible
                   error codes in OverbyteIcsLibeay.pas search i.e. for
                   X509_V_ERR_UNABLE_TO_GET_CRL.
Apr 15, 2011 V7.71 Arno prepared for 64-bit.
Apr 21, 2011 V7.72 Éric Fleming Bonilha found a bug in SetSocketRcvBufSize.
Apr 23, 2011 V7.73 Arno added support for OpenSSL 0.9.8r and 1.0.0d.
Apr 24, 2011 V7.74 Arno fixed compatibility with OpenSSL 1.0.0d.
Apr 26, 2011 V7.75 Anton S. found that TrashCanSize was not set correctly in
                   non .NET environments, only important if OnDataAvailable
                   is not assigned with non-listening sockets, which should
                   never be the case.
May 03, 2011 V7.76 Arno improved error messages on loading OpenSSL libs.
                   Added method Insert to TX509List. Removed a few useless
                   type casts.
May 08, 2011 v7.77 Arno added TX509List.SortChain and use of new function
                   f_ERR_remove_thread_state(nil) with OpenSSL v1.0.0+
May 11, 2011 v7.78 Arno made the SSL peer certificate available as property
                   TSslWSocket.SslPeerCert. The SslPeerCert is instantiated
                   lazily once and usable after a SSL handshake including
                   the SSL session was reused from cache until the next SSL
                   connection is initialized. It is usable when SslPeerCert.X508
                   is non-nil. In previous versions the PeerCert passed to
                   OnSslHandshakeDone could be just a reference to one of the
                   certs in the SslCertChain which was eval.
May 17, 2011 v7.79 Arno added Sha1Hex, Sha1Digest, IssuedBy, IssuerOf and
                   SelfSigned to TX509Base. Deprecated Sha1Hash since it was
                   buggy storing binary in strings. Return values of Sha1Hex
                   and Sha1Digest are cached. Reworked TX509List and added new
                   methods.
Jun 08, 2011 v7.80 Arno added x64 assembler routines, untested so far.
Jun 18, 2011 v7.81 aguser removed some compiler hints.

May 21, 2011 V7.82 Arno - Make sure receipt of a SSL shutdown notification
                   closes the connection if no bidirectional SSL shutdown is
                   wanted. There are servers in the wild expecting a SSL
                   shutdown confirmation before they close the connection.
Jul 22, 2011 V7.83 Arno - OEM NTLM changes.
Sep 26, 2011 V7.84 Angus - Set SocketSndBufSize and SocketRcvBufSize for
                   Listen sockets, note only worth increasing sizes for UDP
Feb 17, 2012 V7.86 Arno added NTLMv2 and NTLMv2 session security (basics),
                   read comment "HowTo NTLMv2" in OverbyteIcsNtlmMsgs.pas.
Apr 30, 2012 V7.87 Arno - Some SSL debug log strings adjusted.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
                   added property SocketFamily, sfAny = System default preference,
                     sfAnyIPv4 = IPv4 preference, sfAnyIPv6 = IPv6 preference,
                     sfIPv4 = explicit IPv4, sfIPv6 = explicit IPv6.
                   added functions WSocketIPv4ToStr, WSocketIPv6ToStr,
                     WSocketStrToIPv4, WSocketStrToIPv6, WSocketStrToMappedIPv4,
                     WSocketIsIPv4, WSocketIsIP (finds SocketFamily from string)
Aug 5, 2012 V8.01 - Angus added WSocketIsIPEx (finds SocketFamily from string,
                     including AnyIPv4/IPv6), added SocketFamilyNames
Feb 16. 2013 V8.02 Angus - WSocketResolveIp no exception for IPv6 lookups
Mar 16, 2013 V8.03 Arno added new property LocalAddr6. This is a breaking change
                   if you ever assigned some IPv6 to property LocalAddr in
                   existing code. LocalAddr6 should be assigned a local IPv6,
                   LocalAddr should be assigned a local IPv4, provided it is
                   actually required to bind the socket to a particular interface.
                   By default both properties LocalAddr and LocalAddr6 do not
                   require a value.
Jun 03, 2013 V8.04 FPiette added unit "Types" so that some inlines are
                   expanded.
Jun 03, 2013 V8.05 Eric Fleming Bonilha found a serious bug with closing the
                   socket. The problem was that winsock may continue to post
                   notification messages after closesocket() has been called.
Aug 18, 2013 V8.06 Arno added some default property specifiers.
Oct 22. 2013 V8.07 Angus - Added SendTo6 and ReceiveFrom6 for IPv6 UDP
Dec 24. 2013 V8.08 Francois - fixed range check error in various PostMessages
Feb 12, 2014 V8.09 Angus - fixed TX509Base.PostConnectionCheck to check multiple
                   DNS or IP entries
Jul 9, 2014  V8.10 Angus - added sslCiphersXXX literals some taken from Mozilla
                   with much higher security for servers
Aug 15, 2014 V8.11 Angus made WriteCount public in descendent components
Oct 20, 2014 V8.12 Angus added sslCiphersMozillaSrvInter which excludes SSLv3
Nov 8,  2014 V8.13 Eugene Kotlyarov added namespace for all RTL units for XE2 and
                    later, note only this unit has bumped version
Dec 11, 2014 V8.14 Angus made LastOpenSslErrMsg public for better error reporting (dump=true)
                   Added SslHandshakeRespMsg with friendly error or success message
                   Added SslHandshakeErr with full SSL error code (event error is reason only)
                   Added SslCipherDesc with OpenSsl long cipher description
                   Added SslEncryption, SslKeyExchange and SslMessAuth extracted from SslCipherDesc
                   Stop SSL reporting handshaking steps as errors and report real SSL errors
                   Other various SSL error reporting improvements
Mar 16, 2015 V8.15 Angus added more SslOptions: sslOpt_NO_COMPRESSION, sslOpt_TLSEXT_PADDING,
                     sslOpt_SAFARI_ECDHE_ECDSA_BUG, sslOpt_CISCO_ANYCONNECT, sslOpt_NO_TLSv1_1
                     and sslOpt_NO_TLSv1_2
                   Added more SslVersionMethods: sslTLS_V1_1, sslTLS_V1_2 and sslBestVer which
                     is eqivalent to sslV23 and actually means any of SSLV3, TLS1, TLS1.1 or TLS1.2
                   To disable some versions, use sslBestVer and disable specific ones using SslOptions
                   To force only one version, set SslVersionMethod to that version
                   Choosing a specific TLS version will fail if matching Ciphers are not available
                   OPENSSL_NO_TLSEXT removed so SSL Server Name Identification is always supported
                   Added SslDHParamFile to load a DH Parameters for Diiffie-Hellman DH and EDH key ciphers
                     DH param files may have key lengths of 512,1024,2048,4096 bits and currently need
                     to be generated using the opensll.exe utility (or use those that come with ICS)
                   Added SslECDHMethod to select Elliptic Curves to support ECDH and EECDH key ciphers
                   Note, only OpenSSL 1.0.1 and later are now supported since this added TLS 1.1/1.2
Mar 26, 2015 V8.16 Angus, the OpenSSL version check is relaxed so minor versions with a letter suffix
                      are now supported up to the next major version, so now support up to 1.0.2z
May 08, 2015 V8.17 Angus, added SslOpt_SINGLE_ECDH_USE
                   check for SslECDHMethodAuto after SSL initialised since need version
Jun 05, 2015 V8.18 Angus, enabled SSL engine support, which are cryptographic modules adding extra algorithms
                   ICS packages are now include OverbyteIcsMsSslUtils and OverbyteIcsSslX509Utils
                     which include certificate display and validation functions, and which were
                     previously only in the SSL samples directory
Oct 25, 2015 V8.19 Angus, version bump only for SSL changes in other units
Nov 3, 2015  V8.20 Angus, SslECDHMethod defaults to sslECDHAuto since web sites are increasingly needing ECDH
                   added two more protocols to sslCiphersMozillaSrvInter according to latest Mozilla update
Nov 23, 2015 V8.21 Eugene Kotlyarov fix MacOSX compilation and compiler warnings
Feb 1, 2016  V8.22 Fixed SSL bug where two consecutive requests from a client would leave a server in
                     a waiting state and not process any other requests, thanks to AviaVox for the fix
Feb 23, 2016 V8.23 Angus, version bump only for changes in other units
Mar 3, 2016  V8.24 Angus, OpenSSL 1.0.2g and 1.0.1s, and later, no longer generally support SSLv2
                   Added define OPENSSL_ALLOW_SSLV2 which must be enabled to allow SSLv2 methods t
                      to be specifically selected for older DLLs or new ones that specifically
                      have SSLv2 support compiled.
                   Don't attempt to set DH, EC and SNI that SSLv2 does not support
Mar 17, 2016  V8.25 Angus, updated sslCiphersMozillaSrvxxx cipher literals to latest versions,
                    but left old versions with suffix 38 for backward compatibility
                    OverbyteIcsSslWebServ1 has cipher menu selection to allow comparison testing
Mar 22, 2016  V8.26 Angus, OnSslServerName event error now defaults to OK instead of
                      ERR_ALERT_WARNING which prevented Java clients connecting with SSL.
May 24, 2016  V8.27 Angus, initial support for OpenSSL 1.1.0, new DLL file names, old exports gone
                    Add public variable GSSLEAY_DLL_IgnoreNew which should be set to TRUE before calling
                      any SSL functions if OpenSSL 1.1.0 should be ignored.  Otherwise libcrypto32.dll
                      found in the PATH will override libeay32.dll in the local directory
                    Added public variable GSSL_BUFFER_SIZE defaults to 16384, previously fixed
                      at 4096, may improve SSL performance if larger
                    Added public variable GSSL_DLL_DIR if set before OpenSSL loaded,
                      will use this directory for DLLs, must have trailing \
                    SslContext adds SslMinVersion and SslMaxVersion properties to
                      specify the minimum and maximum SSL/TLS versions supported from:
                      sslVerSSL3,sslVerTLS1,sslVerTLS1_1,sslVerTLS1_2,sslVerTLS1_3,sslVerMax,
                      note 1.3 is not yet supported.  Although introduced for 1.1.0,
                      these properties have also been implemented for 1.0.1/1.0.2 by
                      internally using Options.  SslVersionMethod is ignored for 1.1.0
                      and SslMinVersion > sslVerSSL3 or SslMaxVersion < sslVerMax.
                   SslContext now allows SSL certificates, private keys, CA bundles and
                      DHParams to be loaded from strings instead of files, allowing
                      them to be saved or created in the application without using any
                      files.  New properties SslCertLines, SslPrivKeyLines, SslCALines
                      and SslDHParamLines allow PEM formatted certificates and keys to
                      be saved with the form or loaded as TStrings.  SslCertLines may
                      be a single certificate or a bundle including one or more
                      intermediates, but must not include the private key.  There
                      are new public methods LoadCertFromString, LoadPKeyFromString,
                      LoadCAFromString and LoadDHParamsFromString that can be used to
                      update the certificates after SslContext is initialised.
                    SslContext has a new method SslGetAllCiphers that returns a multi
                      line list of the ciphers supported by OpenSSL although some may
                      be unusable if the correct protocols,  EC and DHParams are not set.
                    TSslWSocket has a new method SslGetSupportedCiphers (Supported, Remote)
                      that returns a multi line list of ciphers. Supported=True is only for
                      1.1.0 and later and returns the actual ciphers available for the
                      session allowed by the protocols, EC and DHParams.  Remote=True for
                      list received by server from remote client, Remote=False is list
                      supported by client or server. Supported=False is list of all ciphers.
                    Added sslDHParams2048 and sslDHParams4096 constants, the latter is used
                      as SslDHParamLines default so applications support DH and ECDH ciphers
                      without needed a DHParams file.  Still better to generate your own
                      DHParams and load them.
                    Added sslRootCACertsBundle constant as a Root CA Certs Bundle of about
                      30 PEM certificates extracted from Windows 2012 R2 server by
                      OverbyteIcsPemtool, assign this to SslContext.SslCALines.Text to
                      verify remote SSL certificates in client applications, not for servers.
                      This is not used as a default to avoid linking the list unless needed.
                    Many SslOptions are no longer supported for 1.1.0 and are now ignored.
                    Cleaned up SSL initialisation
                    SSL debug logging has been improved by logging SSL certificate
                      subjects when loaded from lines, and logging ciphers when
                      SslContext is initialised.
                    SslECDHMethod is ignored for 1.1.0, always enabled.
                    Various internal SSL changes to accommodate new or removed functions
                       with 1.1.0.
                    X509Base has new methods LoadFromText and PrivateKeyLoadFromText
                      that load a PEM SSL certificate and private key from strings.
May 27, 2016  V8.28 Angus corrected SslMinVersion and SslMaxVersion setting protocols
                      for OSLL 1.0.1 and 1.0.2
                    Debug list all SSL Options
June 26, 2016 V8.29 Angus check for WSAESHUTDOWN in TryToSend and clean close down
                      instead of exception
                    Implement GSSL_DLL_DIR properly to report full file path on error
July 7, 2016  V8.30 Angus corrected FCounter.FLastRecvTick not updated in DoRecvFrom
                       or in SSL DoRecv, and FCounter.FLastSendTick not in SentTo
                       so timeouts did not always work
                    Corrected ReadCount/WriteCount with SSL so xmit no longer includes
                       encryption overhead (recv ignored overhead)
Aug 5, 2016   V8.31 Angus, testing OpenSSL 1.1.0 beta 6
Aug 27, 2016  V8.32 Angus, suuport final release OpenSSL 1.1.0
                    OpenSSL 64-bit DLLs have different file names with -x64 added
                    Fix sslRootCACertsBundle long constant would not compile under
                       C++ Builder, by aplitting smaller and making function
Aug 29, 2016  V8.33 Angus, free GLIBEAY_DLL_Handle before GSSLEAY_DLL_Handle to avoid exception
Sept 5, 2016  V8.34 Angus, correct next OpenSSL release is 1.1.1 not 1.1.0a
                    Added public variable GSSLEAY_DLL_IgnoreOld so only OpenSSL 1.1.0 and later are loaded
Oct 18, 2016  V8.35 Angus, major rewrite to simplify loading OpenSSL DLL functions
                    Reversed V8.34 fix so this release only supports 1.1.0 not 1.1.1
                    OPENSSL_ALLOW_SSLV2 gone with all SSLv2 functions
                    stub more removed functions to save some exceptions
                    moved all imports from OverbyteIcsLibeayEx to OverbyteIcsLibeay to make
                        maintenance and use easier, OverbyteIcsLibeayEx may be removed
                    EVP_CIPHER_CTX_xx is now backward compatible with 1.1.0
Oct 26, 2016  V8.36 Now using new names for imports renamed in OpenSSL 1.1.0
                    Added ExclusiveAddr property to stop other applications listening on same socket
                    Added extended exception information, set SocketErrs = wsErrFriendly for
                      some more friendly messages (without error numbers)
                    ESocketException has several more properties to detail errors
Nov 04, 2016  V8.37 Fixed memory leak in RaiseException in last build
Nov 15, 2016  V8.38 Don't hide detailed load SSL exceptions
                    Added IcsVerifyTrust to check authenticode code signing digital
                      certificate and hash on EXE and DLL files, note currently
                      ignores certificate revoke checking since so slow
                    Added public variable GSSL_SignTest_Check to check OpenSSL
                      DLLs are digitally signed, and GSSL_SignTest_Certificate to
                      check for a valid certificate, both default to false
Nov 23, 2016  V8.39 Minimum OpenSSL support is now 1.0.2 (1.0.1 support ceases Dec 2016)
                    Added functions to check certificate params using X509_VERIFY_PARAM,
                      which means the peer certificate common name is now checked against
                      the host set as SslServerName during handshaking instead of needing
                      to use PostConnectionCheck in the handshake event.
                    Added more SslVerifyFlags for extra certificate verification options
                    Added SslCheckHostFlags to context to control host name checking
                    Added SslCertPeerName property set after successfull SSL handshake
                      and peer certificate check that returns matched name from certificate
                    Combined TX509Ex properties into TX509Base from OverbyteIcsSslX509Utils
                    TX509Base has new methods CheckHost, CheckEmail and CheckIPaddr as
                      alternatives to PostConnectionCheck using OpenSSL APIs
                    Added SslGetAllCerts to context to get list of certificates from
                      context store, may be used to check CA certificates have been
                      correctly loaded from files, 1.1.0 and later
                    TX509List has new LoadAllFromFile method to load all certificates
                      from a bundle file
                    Added IcsSslOpenFileBio and IcsSslLoadStackFromInfoFile which were
                       previously methods in TSslContext so they can be used elsewhere
                    Added IcsUnwrapNames which changes multi-line string to comma string
                    Added SslCertX509 to context which returns last certificate loaded
                       for reporting purposes (can not be set yet)
Jan 27, 2017  V8.40 TX509Base can now read and save all common X509 certificate file formats:
                       .PEM, .CER, .CRT - Base64 encoded DER - LoadFromPemFile/SaveToPemFile
                       .DER, .CER, .CRT - binary DER - LoadFromPEMFile/SaveToDERFile
                       .P7B, .P7R, .SPC - PKCS#7 - LoadFromP7BFile/SaveToP7BFile
                       .PFX, .P12 - PKCS#12 - LoadFromP12File/SaveToP12File
                       PEM/P12 may have certificate and private key
                       PEM/P7B/P12 may have extra intermediate certificates
                    LoadFromFile/SaveToFile uses file extension to choose file format
                    Extra options for TX509Base methods to load and save files to
                      specify if private key should be read or saved and the password
                    TX509Base will now encrypt private keys with a password
                    CheckCertAndPKey in TX509Base checks cert and pkey match
                    SslCertX509 in context now public not published, returns certificate and
                       private key from the context rather than what was loaded, and may be
                       used to set both, overriding any previous settings
                    Fixed bug in TX509Base.SerialNumHex to show correct serial, note most
                       browsers display serial in hex not a numeric value,
                    Added SerialNumHexto GetCertInfo
                    Fixed bug in TX509Base.SerialNum to clear SSL error for an illegal serial,
                        to avoid handshaking later failing with 'asn1_get_uint64:too large'
                        (mainly when logging was enabled)
                    TX509Base.SerialNum now returns an int64 instead of integer, but only
                        correctly with 1.1.0 and later (or use SerialNumHex instead)
                    Added SslSecLevel to context to set security level (1.1.0 and later),
                       defaults to sslSecLevel80bits, set sslSecLevelNone for SSLv3
                    Added CheckPrivateKey to context which checks loaded certificate and
                       private key match
                    Added OnSslProtoMsg event which receives SSL protocol messages
                        (in binary, need decoding) for debugging purposes
                    Added loSslDevel to ICS logger which replaces many loSslInfo logging
                       lines for BIO. read and write which are really internal ICS development
                       use to make logging more readable, added more useful loSslInfo lines
                    Added SslBuildCertChain to context for servers to validate correct
                       certificates loaded OK  (not sure how useful yet)
                    Added ReadOnly option to IcsSslOpenFileBio since mostly we don't
                       want to update certificates
                    GetKeyInfo now support both RSA and EC keys, displays curve names
                    Added GetPKeyRawText to display private key raw values
                    Added PrivateKeyInfo to display private key type and size
                    Added CertPolicies, AuthorityKeyId, SubjectKeyId and CRLDistribution
                      extended certificate properties
                    Added ExtendedValidation returns true for EV certificates
Feb 26, 2017  V8.41 Fix bug in last build with TX509Base PEM cert error handling
                    Simplified checks for base64 certificates
                    Binary format certificate files are now saved correctly
                    Implemented intermediate certificate support in TX509Base which
                      includes loading and saving them from file formats supporting
                      them, and LoadIntersFromPemFile, LoadIntersFromString,
                      SaveIntersToToPemFile, GetIntersList and ListInters.
                    Implemented CA certificate support in TX509Base mainly for
                      chain verification, LoadCAFromPemFile, LoadCAFromString.
                    KeyInfo displays correct key length and curve for certificates
                    CertInfo has Brief option for shorter description
                    ValidateCertChain in TX509Base checks and reports cert and inters
                       and can save a lot of cert problems in servers
                    Added AllCertInfo to TX509List that reports all certificates and
                       can save code in clients reporting certificates
                    Added sslCiphersMozillaSrvInterFS with only forward security ciphers
                    Added IsCertLoaded, IsPkeyLoaded and IsInterLoaded to TX509Base
                    Added SslKeyAuth property to get cipher key authentication
                    Added SslGetCerts to context to get cert, key and intermediates
                    Added SslSetCertX509 to context which sets cert, key and
                       intermediates from FSslCertX509 to load all together
                    If FSslCertX509 in Context has cert loaded when context is
                      initialised, context file properties are ignored and
                      SslSetCertX509 called to load them.
                    Made InitializeSsl public in TSslBaseComponent for more control
                      over SSL loading and unloading
Mar 3, 2017  V8.42 Angus couple of cross platform fixes, thanks to Bill Florac
                   Fixed a change of behaviour made in V8.22 to only effect SSL,
                     beware this means non-SSL applications that break ICS design
                     rules by calling the message handler in the OnDataAvailable
                     event risking re-entrancy may again fail (which was the
                     case before V8.22).
Mar 7, 2017  V8.43  Added new ComponentOptions wsoAsyncDnsLookup and
                        wsoIcsDnsLookup, thanks to ant_s@rambler.ru.
                    Setting wsoAsyncDnsLookup causes Connect to use async DNS
                      lookups instead of blocking sync without needing to call
                      DnsLookUp first and then Connect.
                    Setting wsoIcsDnsLookup causes DnsLookUp to use a thread for
                       async DNS lookups for IPv4 (previously only IPv6) to avoid
                       a windows limitation of one active DNS lookup per thread.
Mar 14, 2017  V8.44 ReverseDnsLookup supports wsoIcsDnsLookup to use thread
                    SslSetCertX509 did not load Inter unless Cert set
Apr 11, 2017  V8.45 Added multiple SSL host support to TSslWSocketServer.
                    CertInfo shows expiry date for brief (it's important).
                    Added TriggerSslServerName so it can be overriden.
                    Added TSslSrvSecurity SSL server security level, used by
                       TIcsHost, sets protocol, cipher and SslSecLevel.
Apr 20, 2017  V8.46 Added sslCiphersNoDH which blocks DH and DHE ciphers,
                       needed for forums.embarcadero.com
                    Adjusted a V8.42 cross platform fix
May 15, 2017  V8.47 Fixed ValidateCertChain ignoring some warnings
May 22, 2017  V8.48 Added added wsDnsLookup SocketState during wsoAsyncDnsLookup
                    Cancel async DNS if close called before connect
June 26, 2017 V8.49 SSL changes in other units, MacOS fixes in other units
Sep 21, 2017  V8.50 LoadFromP12File correctly supports croYes as well as croTry
                    PrivKeyECX25519 is now correctly PrivKeyEd25519
Nov 23, 2017  V8.51 Testing OpenSSL 1.1.1 that adds TLS/1.3, not enabled yet.
                    Added SHA3 digests to TEvpDigest
                    Added RSS-PSS keys to TSslPrivKeyType
                    Better reporting of ED25519 and RSS-PSS keys, report number
                      of security bits for private keys.
                    Fixed SslContext Options being ignored with 1.1.0 due to macros
                       being changed to exported functions.  This meant some features
                       like server cipher preference never worked with 1.1.0.
                    Added SslIntOptions2 to replace SslIntOptions with 1.1.0 and
                       later removing all obsolete options and adding new, set
                       in SslContext as SslOptions2.
                    Changed the way SSlContext Options are set since it did not
                      work properly until OpenSSL was loaded with 1.1.0 and later
                    Added SslContext SslCryptoGroups for 1.1.1 to set which
                       curve groups are supported in preference order.
                    Improved debug diagnostics and added more for SSL.
Feb 19, 2018 V8.52  LocalIpList only uses GetHostByName for Windows XP, 2003 and
                       earlier which also solves a MacOS issue.
                    Renamed PublicKey property to X509PublicKey to avoid confusion.
                    Added PublicKeySaveToText saves public part of private key.
                    Added PublicKeyLoadFromText loads public key into private key
                    Fixed a problem with BIO_get_flags and 1.1.1 that caused SSL to
                      fail, thanks to Rui for finding this.
                    Added sslCipherTLS13 server cipher suites for TLSv1.3.
                    Cleaned up SSL handshake message for TLSv1.3 .
                    IcsSslOpenFileBio now checks PEM files not empty to avoid
                      strange ASN1 errors parsing them.
                    Fixed IcsSslGetEVPDigest to work with 1.1.1
Apr 06, 2018 V8.53  CertInfo shows OU if available, but less in brief mode
                    ValidateCertChain checks issuer OU for duplicate roots
                    Added sanity check to GetSha1Hex if certificate not loaded
                    Ignore CliNewSession event with TLSv1.3 since session not yet
                      established until after handshake.
                    Ignore second handshake start in InfoCallback with TLSv1.3
                       since renegotiation not supported by protocol.
May 21, 2018 V8.54  Added TSslCliSecurity similar to TSslSrvSecurity
                    Added SslCliSecurity to SslContext which if set to other
                      than sslCliSecIgnore sets the protocols, security and
                      ciphers to standardised settings, may be changed without
                      reinitialising the context.
                    Improved SSL handshake failed error message with protocol
                      state information instead of just saying closed unexpectedly.
                    CertInfo shows Valid From date
Jun 27, 2018 V8.55  Server also ignores second handshake start in InfoCallback with
                        TLSv1.3, so it works again.
                    Prevent multiple SslHandshakeDone events for TLSv1.3 which broke
                       FTP client and possibly other protocols.
                    Use NewSessionCallback for clients as well as servers.
                    TX509Base adds SslPWUtf8 to use UTF8 for private key passwords,
                       defaults to true for OSSL 1.1.0 and later, otherwise ANSI.
                    sslSrvSecInter/FS, sslCliSecInter now requires TLS1.1, PCI
                      council EOF TLS1.0 30 June 2018.
                    Added SslCliSecurityNames for TSslCliSecurity dialogs and
                      SslSrvSecurityNames for TSslSrvSecurity dialogs.
                    OnSslHandshakeDone is now called if StartSslHandshake or
                      AcceptSslHandshake raises an exception to report the SSL
                      error (previously only in the debug log).
                    Added sslCliSecDefault and sslSrvSecDefault recommended
                       security defaults, two more client security levels
                    Ensure that SSL alerts are logged with more detail.
Jul 14, 2018 V8.56  Support SSL application layer protocol negotiation (ALPN)
                     extension which is sent with the initial SSL hello.
                    For clients, SslAlpnProtocols sets SslContext with a list of
                      protocols the application supports (ie http/1.1, h2), and
                      SslAlpnProtocol property after connection returns whatever
                      the server selected (if any).
                    For servers, there is a new OnSslAlpnSelect event that has
                       the list of protocols from the client, from which one
                       may be selected (ie H2 to support HTTP/2).
                    Added IPv6 support for TCustomSocksWSocket and
                       TCustomHttpTunnelWSocket, thanks to Max Terentiev.
Oct 5, 2018  V8.57  Tidy up UnwrapNames.
                    WriteIntersToBio now ignores self signed certificate which
                       are roots not an intermediate.
                    Added TSslCliCertMethod so an SSL server asks a client to
                       send an SSL certificate.
                    Support OpenSSL 1.1.1 final with TLS/1.3.
                    CertInfo returns blank if no certificate loaded.
                    ValidateCertChain ExpireDays warning now configurable,
                      defaults to 30 days, used to order new certificates.
                    Moved some SSL types and lits to OverbyteIcsSSLEAY.
                    Fixed compiler hints in GetProc
Nov 2, 2018  V8.58 Increased ListenBacklog property default to 15 to handle
                      higher server loads before rejecting new connections.
                   Corrected some debug error loSslInfo to loSslErr.
Dec 11, 2018 V8.59 Too many lines in SSL diags for errors only, bug in V8.55
Mar 18, 2019 V8.60 Added AddrResolvedStr read only resolved IPv4 or IPv6 address
                     set during Connect method after DNS lookup.
                   Clean-up old commented out code.
                   Made SocketFamilyNames more descriptive.
                   Moved OnDNSLookupDone before internal Connect attempt so
                      user can change DNS result, bug in V8.43.
                   DnsResult can now be updated in OnDNSLookupDone event.
                   Added TLS version to SslSrvSecurityNames.
                   Added sslSrvSecTls12Less and sslSrvSecTls13Only to disable
                     in server IcsHosts if TLS1.3 fails.
Apr 16, 2019 V8.61 Fixed ValidateCertChain to check certificate start and expiry
                      dates in UTC time instead of local time.
Aug 07, 2019 V8.62 Added SslCtxPtr to SslContext to allow use of OpenSSL functions
                     outside this unit.
                   DHParams only needed for servers, don't use if using client
                     security to avoid issues with high security levels.
                   Raise background exception for user exceptions in OnDataAvailable
                     event rather than silently ignoring them.
                   SSL ALPN now properly tested, for client SslAlpnProtocol property
                     returns what the server selects (if anything), for server the
                     selected protocol is now correctly sent.
                   Moved FIcsLogger to TIcsWndControl ao that unit can log errors.
                   Added source to HandleBackGroundException so we know where
                       errors come from, when using IcsLogger.
Nov 18, 2019 V8.63 Corrected fix for user exceptions in OnDataAvailable in last
                     version to break receive loop after exception handling.
                   Added LoadFromP12Buffer to load PFX certificate from buffer to
                     TX509Base, thanks to Mitzi.
                   GetSelfSigned now has better check for self signed certificates.
                   Added Sha256Digest and Sha256Hex to TX509Base.
                   CertInfo in TX509Base shows SHA256 fingerprint instead of SHA1.
May 18, 2020 V8.64 Added support for International Domain Names for Applications (IDNA),
                     i.e. using accents and unicode characters in domain names.
                   DnsLookup now converts Unicode IDN into A-Label (Punycode ASCII)
                     so accented and non-ansi domains lookup correctly.  PunycodeHost
                     is read only with the converted name for display, i.e.
                     éxàmpl?ftptest.co.uk converts to xn--xmpl-0na6cm.ftptest.co.uk.
                   ReverseDnsLookup converts a A-Label (Punycode ASCII) domain name
                     to Unicode, if the ACE prefix xn-- is found in a name.
                   Added new ComponentOptions: wsoUseSTD3AsciiRules will cause
                     DnsLookup to fails if there are illegal symbols in domain names,
                     wsoIgnoreIDNA uses old behaviour for ANSI domain names so
                     no Unicode support, only limited ASNI, sometimes.
                   X509 certificate A-Label domain names converted to Unicode.
                   OpenSSL uses A-Label (Punycode ASCII) in host names, not UTF8.
                   Open and save SSL certificate files with Unicode names not ANSI.
                   Sample OverbyteIcsBatchDnsLookup has lots of ISN test names.
                   Support new OpenSSL 1.1.1 ClientHelloCallback which mostly
                     replaces ServerName callback setting all client negotiation
                     information before the SSL session starts in the CliHelloData
                     property.  The onSslServerName event is still called, from
                     where you can access CliHelloData and SslServerName.
                   WSocketGetCliHelloStr function returns description of CliHelloData.
                   Allows the server to choose an SSL context appropriate to the
                     client's capabilities, such as for Let'S Encrypt acme-tls/1
                     challenge which did not work before, or ECC SSL certificates
                     which are smaller than RSA but not always supported.
                   The onSslAlpnSelect event is now only needed to tell the client
                     which ALPN to choose, generally for HTTP/2 only.
                   Ignore any errors with SSL APLN during handshake.
                   Fixed a problem with SSL ALPN server handshake that may have
                     caused unexpected exceptions mainly on 64-bit applications.
                   Removed TlsExtension_cb callback which wrote a lot to the
                     debug log, same information now in CliHelloData property
                     from ClientHelloCallback.
                   Another attempt to improve handshake error messages.
                   Removed X509CATrust and related properties and methods from
                      TX509Base, it was used by the ValidateCertChain method which
                      now accepts a shared TX509List instead for efficiency.
                   Fixed memory leak in SslGetAllCerts, and bad declarations of
                      SslProtoMsgCallback and CheckIPaddr thanks to Ralf Junker.
Dec 17, 2020 V8.65 No exception trying to change winsock version with SetReqVerLow.
                   GetPKeyRawText has flag to show public key only.
                   FPiette fixed he code so that the exception related to wrong
                       OpenSSL flows correctly up to the application instead of
                       triggering successive access violation exceptions.
                   CertInfo has ISO date/time instead of local date formst.
                   Write comments as UTF8 to certificate PEM file.
                   When reading and loading SSL/TLS certificates and private keys,
                     ICS now parses the PEM bundle one at a time and optionally
                     lists errors about bad certificatss instead of just failing
                     with a stack error. ICS no longer passes extra comments in
                     the bundles to OpenSSL to avoid problems with 8-bit and UTF8
                     characters.
                   Internally simplified reading PEM and similar files by reading
                     them into an ANSI string rather than a File BIO, so common
                     code is used for reading certificates from files or strings
                     for TX509, TX509List and TSslContext.
                   TX509List has new LoadAllFromFileEx and LoadAllFromStringEx
                     which return a list of certificate errors rathern than an
                     exception on the first error.
                   TX509 has new LoadFromPemFileEx, LoadFromFileEx, LoadFromTextEx
                      which return a list of certificate errors rathern than an
                      exception on the first error.  LoadFromFile now returns an
                      exception for unknown file extensions instead of ignoring them.
                   No longer using Ics.InterlockedApi.inc (for a long time).
                   MacOS64 does not support inline assembler so set PUREPASCAL.
                   Changed Longint to Integer and LongWord to Cardinal to keep
                     MacOS64 happy since it thinks both are 64-bits.
                   Added note:  About TCP Window Size, Buffer Size and Performance Tuning.
                   Ignore exception in Destroy freeing winsock.
                   Added ICS_LOCAL_HOST_NAME
                   TriggerSendData now virtual.
                   Prevent overflow in GetMaskBits.
                   Fixed several memory leaks in OpenSSL API calls, thanks to Ralf Junker.
                   Fixed a possible pointer out of scope issue in IcsSslOpenFileBio
                     opening SSL certificate files, seems to effect some Delphi versions
                     more than others, thanks to Linden Roth and Alexander Pastuhov.
                   Fixed more pointer out of scope issues with certificate passwords.
                   Renamed Ics.Posix.Messages.pas to Ics.Posix.PXMessages.pas
                     to avoid conflicts with projects loading it instead of
                     WinApi.Messages.
                   Fixed PrivateKeyLoadFromText got broken recently.
Mar 26, 2021 V8.66 Corrected integer overflow in WSocketIsDottedIP with full numeric
                      IP address which is not supported.
                   Renamed all OpenSSL functions to original names removing ICS
                     f_ prefix.
                   Removed support for OpenSSL 1.0.2 and 1.1.0 whose support ceased
                     Dec 2019, also Fips mode pending 3.0.
                   SslContext SslOptions now ignored, use SslOptions2 instead which
                      has more modern options.  Can not remove SslOptions because it's
                      saved on too many DFMs.
                   Added support for YuOpenSSL which provides OpenSSL in a pre-built
                     DCU linked into applications, rather than using external DLLs.
                   Ssl Renegotiation disabled, no longer recommended.
                   New TSslWSocket method SslOK is TLS/SSL negotiated OK.
                   LoadVerifyLocations no longer lets OpenSSL use default CA folder
                      locations which is potentially a security risk, also fixes
                      a problem with YuOpenSSL, thanks to Ralf Junker for the fix.
                   Made SslVerifyResult public.
                   After SSL handshake, ignore OpenSSL errors if certificate chain
                     verified OK.
                   Socks proxy and tunneling again support host names, broken in V8.56,
                     thanks to Fr0sT.Brutal.  Also made HttpTunnelState property readable,
                   In 2021 Mozilla now recommends TLSv1.3 as modern ciphers and TLSv1.2/1.3
                     as Intermediate supporting all browsers from last five years, so IcsHosts
                     now uses sslCiphersMozillaSrvTLS12 as Intermediate level, also Mozilla
                     recommends no cipher server preference so changed that.
                   Added ProxyURL property to set SOCKS or HTTP Tunnel proxy settings using
                     single URL, ie proto://[user:password@]host:port where proto = socks5 or
                     http. Also new methods SetSocks and SetHTTPTunnel to set proxy properties
                     in a single call, thanks to Fr0sT.Brutal.
Sep 27, 2021 V8.67 SendTo and SendTo6 now set LastError on failure (Send, SendStr, etc
                      already did so).
                   TX509Base.ValidateCertChain reports CA roots for multiple certificate
                     verification paths with two or more intermediate certificates, rather
                     than only the last.
                   Added PKCS12 certificate encryption of AES256 CBC, since previous
                     default 3DES is not supported by OpenSSL 3.0 unless the legacy module
                     is loaded. Beware AES256 not recognised until Windows Server 2016 v1709
                     and Windows 10 v1709, so Server 2012, Windows 10 RTM and earlier won't
                     load AES keys.  Tweaked default parameters for backward compatibility.
                   OpenSSL 3.0 unload any providers we loaded.
                   Added TX509Base.SaveToP12Buf.
                   Added TX509Base.CertMainInfo provides a single line with the main
                     certificate information.
                   Added TX509Base.Comments/CertName/KeyName for certificate information when
                     exporting Windows store certificates, or building bundles.
                   Fail to open passworded private key, retry without password.
                   Added TX509Base.ComparePkey to compare supplied private key with ours.
Jan 04, 2022 V8.68 Support OpenSSL 3.0 for YuOpenSSL.
                   Updated Load/SaveP7BFile for OpenSSL 3.0.
                   Improved OpenSSL error handling to say 'No error returned' instead of
                      error:00000000:lib(0):func(0):reason(0).
                   ValidateCertChain no longer reports an error for the expired 'DST Root CA
                      X3' CA root, since some platforms accept expired roots.
                   Clear LastError in Listen and Connect methods to avoid false errors later.
                   Improved source description for some fatal background errors.
                   AbortComponent has Exception argument so higher level components can report
                     errors better, such as out of memory without needing BgException handler.
May 26, 2022 V8.69 Restored TlsExtension_cb callback for client debugging, ClientHelloCallback
                     only works with servers.
                   Added TX509Base.UrlOcsp and TX509Base.UrlIssuer properties to parse the
                     certificate authority extension and get useful URLs.
                   Added TX509List.FindSubject and TX509List.IndexOfSubj to find certificate
                     by SubjectOneLine, to find issuer certificate in bundle.
                   Added TX509List.SaveToStack which saves all the list certificates to a
                     Stack to pass to OpenSSL functions.
                   Added TX509List.SetX509Store which sets the property X509Store with all
                     the list certificates for use with OpenSSL chain verification functions.
                   Added OCSP (Online Certificate Status Protocol) support to TX509List to
                     create requests and process responses, used to check certificates are
                     legitimately issued and not revoked (instead of Certificate Revocation
                     Lists).  OCSP uses HTTP POST requests, but these are handled using the
                     TOcspHttp component in the OverbyteIcsSslHttpRest unit.
                     BuildOCSPCertId builds a CertId for the certificate and intermediate in
                     the list, BuildOCSPReq builds an OCSP request for the CertId, CheckOCSPResp
                     checks the OCSP response against a CertId and verifies it against the
                     intermediate issuer and X509Store setting various properties, OcspRespStatus,
                     OcspRespStatusStr, OcspCertStatus, OcspCertStatusStr, OcspCertReason,
                     OcspCertReasonStr, OcspRevokeDT, OcspUpdateDT and OcspNextUpdDT, with the
                     full diagnostic request and response being returned as strings by OCSPReqInfo
                     and OCSPRespInfo.  Applications should use TOcspHttp.CheckOcspRevoked instead
                     of these TX509List functions.
                   Added OCSP Stapling TSslContext.SslOcspStatus=True, which for clients causes
                     the remote server to send an OCSP response as part of the initial handshake
                     to avoid contacting an OCSP HTTP server directly.  For servers, we get the
                     OCSP response using HTTP and send it as part of the handshake, in
                     TSslWSocketServer.  TSslContext.GetX509Store returns an X509STORE with all
                     the context certificates for use with OpenSSL chain verification functions.
                     TSslWSocket.OcspStapleRaw returns the OCSP response from the client
                     handshake which can be passed to TOcspHttp.CheckOcspRevoked for checking,
                     or for servers can be set with the response to be sent back to clients.
                     For clients, OcspStapleStatus is set to response status (before
                     certificate checking).
                   Default SSL context option SslOpt2_LEGACY_SERVER_CONNECT needed for servers
                     that don't support secure renegotiation (but which are a security risk).
                   Default sslOpt2_ALLOW_UNSAFE_LEGACY_RENEGOTIATION for clients to connect to
                     such servers.
                   Added SendTB(const Data: TBytes; Len: Integer = -1) where Len is optional,
                     also similar SendToTB and SendToTB6 with TBytes.
                   Moved some common OpenSSL functions like LoadSsl to OverbyteIcsLIBEAY as
                     IcsLoadSsl so they can be used in projects without WSocket.
Oct 08, 2022 V8.70 Handle background exception in ResetSSL, and more logging.
                   Added new ComponentOptions wsoNoSendException which stops an exception
                     being raised if Send is called when the socket is not connected, which
                     can happen if the connection drops unexpectedly.
                   Use HasOption instead of in for all ComponentOptions checks.
                   Improved Unicode compatibility by adding UnicodeString overloads of various
                     AnsiString functions: WSocketGetHostByAddr, WSocketGetHostByName,
                     WSocketResolveIp, WSocketResolveHost, WSocketResolvePort, WSocketResolveProto,
                     WSocketIsDottedIP, WSocket_WSAAsyncGetHostByName, WSocket_WSAAsyncGetHostByAddr,
                     WSocket_getservbyname, WSocket_getprotobyname, WSocket_gethostbyname,
                     WSocket_setsockopt, WSocket_getsockopt, WSocket_inet_add,
                     WSocket_Synchronized_ResolveHost, WSocket_Synchronized_inet_addr,
                     WSocket_Synchronized_ResolvePort, WSocket_Synchronized_ResolveProto.
                     Added GetLocalHostName: String; and WSocket_inet_ntoaW.
                   Added ReceiveTB(var Data : TBytes; MaxLen : Integer = -1) : Integer; where MaxLen
                     is optional, to receive TCP data into a TBytes dynamic array of bytes. Also
                     ReceiveFromTB and ReceiveFrom6TB for UDP datagrams.
Jul 10, 2023 V8.71 Protect inherited class destroys from exceptions at higher levels, thanks to
                     Jaroslav Kulísek for these changes.
                   Stop Winsock being unloaded if socket creation fails, using FWSocketGCountIncremented,
                     thanks to Jaroslav Kulísek.
                   Protect close socket errors in destroy.
                   Fix range check error in ReceiveFromTB.
                   Using Int64 ticks.
                   TWSocketCounter and TCustomTimeoutWSocket now use Int64 counters, if updated with
                     GetTickCount in other units should be changed to IcsGetTickCount64.
                   Added WSocketSockAddrToStr to convert TSockAddrIn6 with IPv4 or PIv6 address to
                     string, used elsewhere to simplify code.
                   Added WSocketIPAddrToSocAddr to convert IPv4 or IPv6 address into TSockAddrIn6,
                     result family AF_UNSPEC for failure.
                   Added function WSocketIPv4ToSocIn6 (TSockAddrIn6) to remove horrible casting in
                     various units.
                   Added function WSocketFamilyToAF to find family for Windows APIs from TSocketFamily.
                   ReceiveTB no longer fails if buffer not initialised.
                   Added overloaded ReceiveTB that returns a TByte instead of the received size.
                   Added function WSocketIPv6Same to compare two TIcsIPv6Address.
                   Added functions IcsReverseIP and IcsReverseIPv6 from DnsQuery.
                   Fixed range check error in WSocket_Synchronized_ResolveHost with IPv4 address
                     with last byte higher than 127.
                   Implemented Assign in TSslBaseComponent and TX509Base, thanks to uso.
                   SetX509Inters in TX509Base now correctly duplicates certificates instead
                     of just pointers which could cause crashes on free.
Aug 08, 2023 V9.0  Updated version to major release 9.
Mar 07, 2024 V9.1  Moved TSslContext, TSslBaseComponent, TX509Base and TX509List to OverbyteIcsSslBase
                     to simplify this unit, left SslContext callbacks here since they need access to
                     it, now set in InitSSLConnection instead of InitContext.
                   No longer supporting defines OPENSSL_USE_DELPHI_MM (never used),
                     OPENSSL_NO_ENGINE (deprecated, never used), OPENSSL_USE_RESOURCE_STRINGS
                     (never used), NO_OSSL_VERSION_CHECK (dangerous), DEFINE OPENSSL_NO_TLSEXT
                     (TLS needed everywhere), LOADSSL_ERROR_FILE (better debugging now).
                   If a connection fails, don't change State to wsConnected briefly before
                      changing it again to wsClosed.
                   Added TSslWsocket SslAlpnProtocols property to specify a list of protocols
                     for clients to send to servers, instead of a similar SslContext property.
                   Published AddrFormat, resolved family AF_INET or AF_INET6 after connection.
                   Fixed AddrFormat not set properly if AddrResolved by DnsLookup.
Jun 06, 2024 V9.2  Added Ics.Posix.EpollTypes and Ics.Posix.EpollApi, various changes to
                     TIcsEventQueue adding Linux and Android support in addition to MacOS
                     and IOS, not tested or supported yet.
Sep 20, 2024 V9.3  Moved many types and constants to OverbyteIcsTypes for consolidation.
                   Moved various IPv4/6 conversion functions to OverbyteIcsUtils so they can
                     be used without sockets.
                   Made TIcsEventQueue public for testing.
                   Note, MacOS support is disabled pending new implementation.
                   Moved IsIPv6APIAvailable and IsIPv6Available here from Winsock.
                   Report EC Group Name during handshake, OpenSSL 3.2 and later.
                   Added TSslCertVerifyEvent for TCertVerMethod = CertVerOwnEvent.
Feb 03, 2025 V9.4  Fixed a potential problem using multiple threads where a new connection
                     opened very quickly (ie localhost) and then stalled due to an
                     unexpected connection state, thanks to Roger Tinembart for the fix.
                   When opening a new connection, set state to wsConnected before calling
                     connect in case the new connection opens very quickly (ie localhost)
                     and then stalls due to an unexpected connection state in the event
                     handler triggered.
                   Changed u_long to LongWord from Longint since this is what Windows
                     uses, and IPv4 addresses are never negative, should prevent some
                     integer overflow errors.
                     This effects TInAddr, in_add, TSockAddrIn, sockaddr_in
                   Changed TIcsIPv4Address and in_addr6.u6_addr32 to LongWord from Integer.
                   Updated Base64 encoding functions to IcsBase64 functions.
                   Fixed a nasty SSL close down issue in Do_FD_CLOSE that could cause an
                     endless loop if SSL data was received after close down was completed
                     but before the handles were reset, only seen with a recent Nginx web
                     server. Improved some debug logging.
                   Made DataToString unicode compatible, only used for diagnostic dump logs.
Sep 10, 2025 V9.5  Added property SessionIpInfo a TIcsSessIpInfo record set after connection
                     with the local and remote IP addresses and ports from the socket, also
                     socket type and protocol, as internal and string versions.  Easier to
                     use than various GetPeer methods. Set for accepted listen connections.
                   Fixed a missing inherited DupConnected that meant counters did not get reset.
                   Removed duplicate CliHelloStr functions previously moved to SslBase.
                   SSL/TLS server name extension does not allow raw IP addresses, convert then
                     to domain names, ie 217.146.102.139 becomes 139.102.146.217.in-addr.arpa
                   Moved IcsInitializeAddr and IcsSizeOfAddr to Utils for use elsewhere.



Use of certificates for SSL clients:
Client SSL applications will usually work without any certificates because all
the encryption is done by the server.  If a client needs to confirm the identity
of a server, set SslVerifyPeer=true and specify a certificate authority root
bundle as SslCAFile, SslCAPath or SslCALines, that contains the certificates
used to sign the server certificate or intermediate certificate, to confirm
they are trusted.  To permanently trust an unknown certificate, save it to
the CA file or path, or add it temporarily using TrustCert.

More rarely in high security operations, the server will need
a client to identify itself with a private certificate before granting access,
and this is where a client SSL certificate and private key are needed.  Client
certificate checking is controlled by the server.  An SslPassPhrase is only
needed if the private key is password protected.

Use of certificates for SSL servers:
Server SSL applications always require an SSL certificate and matching private
key because these control the SSL encryption.  The certificate may also confirm
the identity of the web site using the domain name and often the company name.
To be trusted by browsers and other applications, the SSL certificate needs to
be signed by a root certificate available for local checking.  SSL certificates
are often signed by intermediate certificates rather than root certificates, and
these also need to sent by the server as part of a chain, the intermediate will
have been signed by a trusted root certificate.  To configure an SSL server,
SslCertFile or SslCertLines specify the SSL certificate and optionally
intermediate certificates in same file as a bundle; SslPrivKeyFile or
SslPrivKeyLines specify the private key used to generate the certificate, which
may be optionally password protected by SslPassPhrase; and SslCAFile, SslCAPath
or SslCALines specifies the intermediate certificates if not in the certificate
bundle file.  Also, SslDHParamFile or SslDHParamLines should specify DHParams
which are a secondary encryption key used for some ciphers, ICS has default
DHParams but ideally applications should use unique DHParams.

Sometimes SSL certificates are withdrawn due to misuse such as being stolen
and appear in Certificate Revocation Lists (CRL) that are published by SSL
certificate issuers.  Such lists in PEM format may be loaded by
LoadCrlFromFile or LoadCrlFromPath.

Rarely, a server may want to check the identify of clients by requesting a
client SSL certificate by setting SslVerifyPeerModes=SslVerifyMode_PEER.
AddClientCAFromFile and SetClientCAListFromFile are used to set acceptable
CAs For the client certificate.

OpenSSL 1.1.0 and later support security levels, as follows:
    TSslSecLevel = (
         sslSecLevelAny,        // 0 - anything allowed, old compatibility
         sslSecLevel80bits,     // 1 - default, RSA/DH keys=>1024, ECC=>160, no MD5
         sslSecLevel112bits,    // 2 - RSA/DH keys=>2048, ECC=>224, no RC4, no SSL3, no SHA1 certs
         sslSecLevel128bits,    // 3 - RSA/DH keys=>3072, ECC=>256, FS forced, no TLS/1.0
         sslSecLevel192bits,    // 4 - RSA/DH keys=>7680, ECC=>384, no SHA1 suites, no TLS/1.1
         sslSecLevel256bits);   // 5 - RSA/DH keys=>15360, ECC=>512


About multithreading and event-driven:
    TWSocket is a pure asynchronous component. It is non-blocking and
    event-driven. It means that when you request an operation such as connect,
    the component start the operation your requested and give control back
    immediately while performing the operation in the background automatically.
    When the operation is done, an event is triggered (such as
    OnSessionConnected if you called Connect).

    This asynchronous non-blocking behaviour is very high performance but a
    little bit difficult to start with. For example, you can't call Connect and
    immediately call SendStr the line below. If you try, you'll have an
    exception triggered saying you are not connected. Calling connect will start
    connection process but will return long before connection is established.
    Calling SendStr at the next line will not work because the socket is not
    connected yet. To make it works the right way, you have to put your SendStr
    in the OnSessionConnected event.

    The asynchronous operation allows you to do several TCP/IP I/O
    simultaneously. Just use as many component as you need. Each one will
    operate independently of the other without blocking each other ! So you
    basically don't need multi-threading with TWSocket, unless YOUR processing
    is lengthy and blocking.

    If you have to use multithreading, you have two possibilities:
    1) Create your TWSocket from your thread's Execute method
    2) Attach a TWSocket to a given thread using ThreadAttach.
    In both cases, you must set MultiThreaded property to TRUE.
    If you don't use one of those methods, you'll end up with a false
    multithreaded program: all events will be processed by the main tread !
    For both methods to work, you MUST have a message loop withing your thread.
    Delphi create a message loop automatically for the main thread (it's in
    the Forms unit), but does NOT create one in a thread ! For your convenience,
    TWSocket has his own MessageLoop procedure. You can use it from your thread.

    Sample program MtSrv uses first method while ThrdSrv uses second method.
    Sample program TcpSrv is much the same as ThrdSrv but doesn't use any
    thread. You'll see that it is able to server a lot of simultaneous clients
    as well and it is much simpler.

 About TCP Window Size, Buffer Size and Performance Tuning
    The TCP protocol has a window size field to control flow of data, the amount
    of data that can be sent without an acknowledgement that it has been received
    safely and does not need to be repeated.  Back in dial-up days, the window
    size was small since corruption and resending was common.

    Early applications would increase the window size up to the original TCP
    maximum of 64K to improve performance with registry changes or using a TCP
    option that ICS exposes as SocketRcvBufSize and SocketSndBufSize properties
    in TWSocket. In ICS, only the FTP client and server components ever used this
    feature since they did large file transfers. Unfortunately the default buffer
    size never kept up with faster internet speeds which has been fixed with V8.65.

    Windows sets the default TCP window size according to network adaptor speed,
    8K for less than 1Mb, 17K for 1Mb to 100Mb, 64K for 100Mb to 10Gb, 128k
    for 10Gb and faster.

    The total achievable throughput for a connection is window size in bytes
    divided by connection latency or Round Trip Time (RTT) in seconds, with a
    result in bytes per second. So a 64KB window size with 10ms latency allows
    51Mb/sec speed (6.3MB/sec), 20ms is 25Mb, 50ms is 10Mb.

    So the default 64K window size limits performance on low latency networks.
    With Windows Vista/2008 and later, TCP Auto Tuning was added (and the registry
    ignored) using the TCP window scale option (RFC 7323) that allows increased
    window size beyond 64K, up to 16M for Windows.   Auto tuning automatically
    increases the window size dynamically to achieve the best performance,
    rather than being fixed by a predefined window size.

    Windows provides control over auto tuning, type 'netsh interface tcp show
    global' should say AutoTuningLevel normal, but it can be disabled or
    restricted for poor connections.

    Note some older firewalls and routers did not support TCP scaling and
    removed it causing unstable connections and unexplained problems.

    So for best performance, do not change the SocketRcvBufSize and SocketSndBufSize
    properties, let Windows do auto tuning.  With V8.65 for FTP it is no longer
    possible to reduce to the buffer size below that of the Windows default which
    means existing applications setting it below 64K will speed up without change.
    If your application sets buffer sizes for HTTP or other  protocols (which
    ICS never did), we suggest you remove it.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit Z.ICS9.OverbyteIcsWSocket;
{$ENDIF}

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$H+}           { Use long strings                    }
{$J+}           { Allow typed constant to be modified }
{$ALIGN 8}
{$I Include\Z.ICS9.OverbyteIcsDefs.inc}
{$IFDEF USE_SSL}
{.I Include\OverbyteIcsSslDefs.inc  V9.1 gone }
{$ENDIF}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$IFDEF COMPILER12_UP}
    { These are usefull for debugging !}
    {$WARN IMPLICIT_STRING_CAST       ON}
    {$WARN IMPLICIT_STRING_CAST_LOSS  ON}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{$IFDEF BCB}
    {$ObjExportAll On}
{$ENDIF}
{$IFDEF POSIX}                 { V8.65 }
    {$DEFINE PUREPASCAL}
{$ENDIF}
{$DEFINE OPENSSL_NO_ENGINE}       { V9.1 deprecated in OpenSSL 3.0 }

interface

uses
{$IFDEF MSWINDOWS}
  {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
  {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
  Z.ICS9.OverbyteIcsWinsock,
{$ENDIF}
  {$IFDEF RTL_NAMESPACES}System.Types{$ELSE}Types{$ENDIF},    { V8.21 }
  {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Contnrs{$ELSE}Contnrs{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.SyncObjs{$ELSE}SyncObjs{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.SysConst{$ELSE}SysConst{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.TypInfo{$ELSE}TypInfo{$ENDIF},  { V8.40 }
//  {$IfDEF Rtl_Namespaces}System.StrUtils{$ELSE}StrUtils{$ENDIF},{ V8.65 }
{$IFDEF POSIX}
  System.Generics.Collections,
  Posix.Pthread,
  Posix.SysTypes,
  Posix.Base,
  Posix.Errno,
  Posix.SysSocket,
  Posix.NetinetIn,
  Posix.NetinetIp6,
  Posix.NetinetTCP,
  {Posix.SysSelect}
  Posix.SysTime,
  Posix.ArpaInet,
  Posix.NetDB,
  Posix.UniStd,
  Posix.Fcntl,
{$WARN UNIT_PLATFORM OFF}
  Posix.StrOpts,
{$WARN UNIT_PLATFORM ON}
  Z.ICS9.Ics.Posix.PXMessages,
  {$IF DEFINED(MACOS) or DEFINED(IOS)}    { V9.2}
  Z.ICS9.Ics.Posix.KEventTypes,
  Z.ICS9.Ics.Posix.KEventApi,
  {$ELSEIF DEFINED(LINUX) or DEFINED(ANDROID)}    { V9.2}
  Z.ICS9.Ics.Posix.EpollTypes,
  Z.ICS9.Ics.Posix.EpollApi,
  {$IFEND}
  Z.ICS9.Ics.Posix.WinTypes,
{$ENDIF}
{ You must define USE_SSL so that SSL code is included in the component.    }
{ Either in OverbyteIcsDefs.inc or in the project/package options.          }
{$IFDEF USE_SSL}
  Z.ICS9.OverbyteIcsSSLEAY,
  Z.ICS9.OverbyteIcsLIBEAY,
  {$IFDEF RTL_NAMESPACES}System.Masks{$ELSE}Masks{$ENDIF}, { Masks added AG 06/20/07 }
{$ENDIF}
{$IFNDEF NO_DEBUG_LOG}
  Z.ICS9.OverbyteIcsLogger,
{$ENDIF}
  Z.ICS9.OverbyteIcsUtils,
  Z.ICS9.OverbyteIcsAvlTrees,
{$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
  Z.ICS9.OverbyteIcsDigestAuth,
{$ENDIF}
{$IFDEF FMX}
  {$IF DEFINED(BUILTIN_THROTTLE) or DEFINED(BUILTIN_TIMEOUT)}
    Z.ICS9.Ics.Fmx.OverbyteIcsThreadTimer,
  {$IFEND}
  Z.ICS9.Ics.Fmx.OverbyteIcsWndControl,
  Z.ICS9.Ics.Fmx.OverbyteIcsSslBase,  { V9.1 TX509Base }
{$ELSE FMX} // VCL
  {$IF DEFINED(BUILTIN_THROTTLE) or DEFINED(BUILTIN_TIMEOUT)}
    Z.ICS9.OverbyteIcsThreadTimer,
  {$IFEND}
  Z.ICS9.OverbyteIcsWndControl,
  Z.ICS9.OverbyteIcsSslBase,  { V9.1 TX509Base, TX509List, etc }
{$ENDIF FMX}
  Z.ICS9.OverbyteIcsNtlmMsgs,
  Z.ICS9.OverbyteIcsWSockBuf,
  Z.ICS9.OverbyteIcsMimeUtils,
  Z.ICS9.OverbyteIcsTypes,
  Z.ICS9.OverbyteIcsUrl                                    { V8.66 }
 {$IFDEF YuOpenSSL}, YuOpenSSL{$ENDIF YuOpenSSL},   { V8.66 }
  Z.ICS9.OverbyteIcsTicks64;                               { V8.71 }

//type
  { sfAny = System default preference, sfAnyIPv4 = IPv4 preference,            }
  { sfAnyIPv6 = IPv6 preference, sfIPv4 = explicit IPv4, sfIPv6 = explicit IPv6 }
  { use WSocketFamilyToAF to convert to AF_xx values }
//  TSocketFamily = (sfAny, sfAnyIPv4, sfAnyIPv6, sfIPv4, sfIPv6);    { V9.3 moved to OverbyteIcsUtils }

const
  WSocketVersion            = 905;
  CopyRight    : String     = ' TWSocket (c) 1996-2025 Francois Piette V9.5 ';
  WSA_WSOCKET_TIMEOUT       = 12001;
//  DefaultSocketFamily       = sfIPv4;

{$IFDEF MSWINDOWS}
//type
//  TSockAddrIn    = sockaddr_in;   { V8.46 assists cross platform use }    {V8.65 duplicates decs later }
//  TSockAddrIn6   = sockaddr_in6;
{$ENDIF}


{ V9.3 moved most constants and types to OverbyteIcsTypes }

{$IFDEF POSIX}
function IN6_ADDR_EQUAL(const a: PIn6Addr; const b: PIn6Addr): Boolean;
function IN6ADDR_ISANY(sa: PSockAddrIn6): Boolean;
{$ENDIF POSIX}

type
  TWndMethod         = procedure(var Message: TMessage) of object;
  TBgExceptionEvent  = TIcsBgExceptionEvent; { V7.35 }

  ESocketException   = class(Exception)  { V8.36 more detail }
  private
    FErrorMessage : string;
    FIPStr        : String;
    FPortStr      : String;
    FProtoStr     : String;
    FErrorCode    : Integer;
    FFriendlyMsg  : String;
    FFunc         : String;
  public
    constructor Create(const AMessage       : String;
                       AErrorCode           : Integer = 0;
                       const AErrorMessage  : String = '';
                       const AFriendlyMsg   : String = '';
                       const AFunc          : String = '';
                       const AIP            : String = '';
                       const APort          : String = '';
                       const AProto         : String = '');
    property IPStr        : String  read FIPStr;
    property PortStr      : String  read FPortStr;
    property ProtoStr     : String  read FProtoStr;
    property ErrorCode    : Integer read FErrorCode;
    property ErrorMessage : String  read FErrorMessage;
    property FriendlyMsg  : String  read FFriendlyMsg;
    property Func         : String  read FFunc;
  end;

type
  TNetChangeEvent    = procedure (Sender: TObject; ErrCode: Word) of object;
  TDataAvailable     = procedure (Sender: TObject; ErrCode: Word) of object;
  TDataSent          = procedure (Sender: TObject; ErrCode: Word) of object;
  TSendData          = procedure (Sender: TObject; BytesSent: Integer) of object;
  TSessionClosed     = procedure (Sender: TObject; ErrCode: Word) of object;
  TSessionAvailable  = procedure (Sender: TObject; ErrCode: Word) of object;
  TSessionConnected  = procedure (Sender: TObject; ErrCode: Word) of object;
  TDnsLookupDone     = procedure (Sender: TObject; ErrCode: Word) of object;
  TChangeState       = procedure (Sender: TObject; OldState, NewState : TSocketState) of object;
  TDebugDisplay      = procedure (Sender: TObject; var Msg : String) of object;
  TIcsException      = procedure (Sender: TObject; SocExcept: ESocketException) of object; { V8.36 }
  TWSocketSyncNextProc = procedure of object;

type  { <== Required to make D7 code explorer happy, AG 05/24/2007 }

  TWSocketCounter = class(TObject)
  private
    FConnectDT    : TDateTime;
    FConnectTick  : Int64;       { V8.71 changed from cardinal to Int64 }
    FLastRecvTick : Int64;       { V8.71 }
    FLastSendTick : Int64;       { V8.71 }
    function  GetLastAliveTick : Int64;
  public
    procedure SetConnected; virtual;
    property  ConnectTick   : Int64  read FConnectTick  write FConnectTick;
    property  ConnectDT     : TDateTime read FConnectDT    write FConnectDT;
    property  LastAliveTick : Int64  read GetLastAliveTick;
    property  LastRecvTick  : Int64  read FLastRecvTick write FLastRecvTick;
    property  LastSendTick  : Int64  read FLastSendTick write FLastSendTick;
  end;
  TWSocketCounterClass = class of TWSocketCounter;

{$IFDEF POSIX}
  TIcsAsyncEventState = set of (aesCloseNotified, aesConnectNotified, aesShutDown0Called, aesShutDown1Called);
  IIcsEventSource = interface(IInterface) { AG 8.11.2011 }
  ['{EDA1AB33-D3F0-4C14-AA99-67E6D28A38F3}']
    function  GetEventMask: Cardinal;
    procedure SetEventMask(const AValue: Cardinal);
    function  GetNotifyMessageID: UINT;
    function  GetNotifyWindow: HWND;
    function  GetEventState: TIcsAsyncEventState;
    function  GetFileDescriptor: Integer;
    procedure SetFileDescriptor(const AValue: Integer);
    function  GetObject: TObject;
    procedure SetEventState(const AValue: TIcsAsyncEventState);
    procedure SetNotifyWindow(const AValue: HWND);
    procedure SetNotifyMessageID(const AValue: UINT);
    function  GetObjectID: NativeInt; // Must be > 0 and unique!!

    property  EventMask: Cardinal read GetEventMask write SetEventMask;
    property  NotifyMessageID: UINT read GetNotifyMessageID write SetNotifyMessageID;
    property  NotifyWindow: HWND read GetNotifyWindow write SetNotifyWindow;
    property  EventState: TIcsAsyncEventState read GetEventState write SetEventState;
    property  FileDescriptor: Integer read GetFileDescriptor write SetFileDescriptor;
    property  ObjectID: NativeInt read GetObjectID; // Must be > 0 and unique!!
  end;
{$ENDIF POSIX}

  TCustomWSocket = class(TIcsWndControl {$IFDEF POSIX}, IIcsEventSource {$ENDIF})
  private
    FSocketFamily       : TSocketFamily;
    FOldSocketFamily    : TSocketFamily;
    FCurrentAddrFamily  : Word; // Addr family cache
    Fsin                : TSockAddrIn6;
    FDnsResult          : String;
    FDnsResultList      : TStrings;
    FSendFlags          : Integer;
    FLastError          : Integer;
  {$IFDEF MSWINDOWS}
    FDnsLookupBuffer    : array [0..MAXGETHOSTSTRUCT] of AnsiChar;
  {$ENDIF}
    FInternalDnsActive  : Boolean;      { V8.43 }
    FDnsLookupCheckMsg  : Boolean;
    FDnsLookupTempMsg   : TMessage;
    FCounter            : TWSocketCounter;
    FCounterClass       : TWsocketCounterClass;
    { ThreadID at the time of the first call to one of the IcsAsyncXxxx methods}
    FLookupThreadID     : THandle;
    { Pointer to a TIcsAsyncDnsLookup instance shared by all TWSocket instances}
    { which called one of the IcsAsyncXxxx methods from the same thread context}
    FAsyncLookupPtr     : Pointer;
    FWSocketGCountIncremented : Boolean;  { V8.71 JK }
    { if constructor fails, WSocketGCount is not incremented thus WSocketGCount should not be decremented in destructor }
{$IFDEF POSIX} { IIcsEventSource }
  strict private
    FPxEventMask        : Cardinal;
    FPxFileDescriptor   : Integer;
    FPxEventState       : TIcsAsyncEventState;
    FPxEventMessageID   : UINT;
    FPxEventWindow      : HWND;
    FPxObjectID         : NativeInt;
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
    FHSocket            : TSocket;
    FASocket            : TSocket;               { Accepted socket }

    FMsg_WM_ASYNCSELECT            : UINT;
    FMsg_WM_ASYNCGETHOSTBYNAME     : UINT;
    FMsg_WM_ASYNCGETHOSTBYADDR     : UINT;
    FMsg_WM_CLOSE_DELAYED          : UINT;
    //FMsg_WM_WSOCKET_RELEASE      : UINT;
    FMsg_WM_TRIGGER_EXCEPTION      : UINT;
    FMsg_WM_TRIGGER_DATA_AVAILABLE : UINT;
    FAddrStr            : String;
    FAddrResolved       : Boolean;
    FAddrFormat         : Integer;
    FAddrAssigned       : Boolean;
    FProto              : Integer;
    FProtoAssigned      : Boolean;
    FProtoResolved      : Boolean;
    FLocalPortResolved  : Boolean;
    FProtoStr           : String;
    FPortStr            : String;
    FPortAssigned       : Boolean;
    FPortResolved       : Boolean;
    FPortNum            : Integer;
    FLocalPortStr       : String;
    FLocalPortNum       : Integer;
    FLocalAddr          : String;     { IP address for local interface to use }
    FLocalAddr6         : String;     { IPv6 address for local interface to use }
    FType               : Integer;
    FBufHandler         : TIcsBufferHandler;
    FLingerOnOff        : TSocketLingerOnOff;
    FLingerTimeout      : Integer;              { In seconds, 0 = disabled }
    FKeepAliveOnOff     : TSocketKeepAliveOnOff;
    FKeepAliveTime      : Integer;              { In milliseconds }
    FKeepAliveInterval  : Integer;              { In milliseconds }
    FListenBacklog      : Integer;
    ReadLineCount       : Integer;
    bAllSent            : Boolean;
    FReadCount          : Int64;   { V5.26 }
    FWriteCount         : Int64;   { V7.24 }
    FPaused             : Boolean;
    FCloseInvoked       : Boolean;
    FBufferedByteCount  : Integer;   { V5.20 how man xmit bytes unsent        }
    FFlushTimeout       : Integer;   { This property is not used anymore      }
    FDnsLookupHandle    : THandle;
    { More info about multicast can be found at:                              }
    {    http://ntrg.cs.tcd.ie/undergrad/4ba2/multicast/antony/               }
    {    http://www.tldp.org/HOWTO/Multicast-HOWTO-6.html                     }
    FMultiCast          : Boolean;
    { Multicast addresses consists of a range of addresses from 224.0.0.0 to  }
    { 239.255.255.255. However, the multicast addresses from 224.0.0.0 to     }
    { 224.0.0.255 are reserved for multicast routing information; Application }
    { programs should use multicast addresses outside this range.             }
    FMultiCastAddrStr   : String;
    FMultiCastIpTTL     : Integer;
    FReuseAddr          : Boolean;
    FExclusiveAddr      : Boolean;   { V8.36 }
    FComponentOptions   : TWSocketOptions;
    FState              : TSocketState;
  {$IFDEF MSWINDOWS}
    FConnectThreadId    : TThreadID; { V9.4 }
  {$ENDIF}
    FRcvdFlag           : Boolean;
    FSelectEvent        : Integer;
    FSelectMessage      : WORD;
    FOnSessionAvailable : TSessionAvailable;
    FOnSessionConnected : TSessionConnected;
    FOnSessionClosed    : TSessionClosed;
    FOnChangeState      : TChangeState;
    FOnDataAvailable    : TDataAvailable;
    FOnDataSent         : TDataSent;
    FOnSendData         : TSendData;
    { FOnLineTooLong      : TNotifyEvent; }
    FOnDnsLookupDone    : TDnsLookupDone;
    FOnError            : TNotifyEvent;
    FOnDebugDisplay     : TDebugDisplay;       { 18/06/05 }
    //FThreadId           : THandle;
    FSocketSndBufSize   : Integer;  { Winsock internal socket send buffer size }
    FSocketRcvBufSize   : Integer;  { Winsock internal socket Recv buffer size }
    FOnAddressListChanged : TNetChangeEvent;
    FOnRoutingInterfaceChanged : TNetChangeEvent;
    FSocketErrs         : TSocketErrs;   { V8.36 }
    FonException        : TIcsException; { V8.36 }
    FAddrResolvedStr    : String;        { V8.60 IPv4 or IPv6 address }
    FPunycodeHost       : String;        { V8.64 Puncycode result of last DnsLookup  }
    FSessIpInfo         : TIcsSessIpInfo;  { V9.5 set by GetSocketIpInfo after connection or server WSAAccept method }
{$IFNDEF NO_DEBUG_LOG}
//  FIcsLogger          : TIcsLogger;                        { V5.21, V8.62 moved to TIcsWndControl }
  procedure   SetIcsLogger(const Value : TIcsLogger); virtual;                { V5.21 }
  procedure   DebugLog(LogOption : TLogOption; const Msg : String); virtual;  { V5.21 }
  function    CheckLogOptions(const LogOption: TLogOption): Boolean; virtual; { V5.21 }
{$ENDIF}
    procedure   AbortComponent(E:Exception); override;                        { V8.68 added E to allow reporting }
    procedure   WndProc(var MsgRec: TMessage); override;
    function    MsgHandlersCount: Integer; override;
    procedure   AllocateMsgHandlers; override;
    procedure   FreeMsgHandlers; override;
    procedure   AllocateSocketHWnd; virtual;
    procedure   DeallocateSocketHWnd; virtual;
    procedure   SocketError(sockfunc: String; LastError: Integer = 0;
                                      FriendlyMsg: String = ''); virtual;   { V8.36 added FriendlyMsg }
    procedure   WMASyncSelect(var msg: TMessage); virtual;
    procedure   WMAsyncGetHostByName(var msg: TMessage);
    procedure   WMAsyncGetHostByAddr(var msg: TMessage);
    procedure   WMCloseDelayed(var msg: TMessage);
    procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure   ChangeState(NewState : TSocketState);
    procedure   TryToSend; virtual;
    procedure   ASyncReceive(Error : Word; MySocketOptions : TWSocketOptions);
    procedure   AssignDefaultValue; virtual;
    procedure   InternalClose(bShut : Boolean; Error : Word); virtual;
    procedure   InternalAbort(ErrCode : Word); virtual;
    procedure   InternalCancelDnsLookup(IgnoreErrors: Boolean);
    procedure   SetSendFlags(newValue : TSocketSendFlags);
    function    GetSendFlags : TSocketSendFlags;
    procedure   SetAddr(const InAddr : String);
    procedure   SetCounterClass(const Value: TWSocketCounterClass);
    procedure   SetRemotePort(sPort : String); virtual;
    function    GetRemotePort : String;
    procedure   SetLocalAddr(const sLocalAddr : String);
    procedure   SetLocalAddr6(const sLocalAddr6 : String);
    procedure   SetMultiCastAddrStr(const sMultiCastAddrStr: String);
    procedure   SetLocalPort(const sLocalPort : String);
    procedure   SetProto(sProto : String); virtual;
    procedure   SetSocketFamily(const Value: TSocketFamily);
    procedure   SetOnRoutingInterfaceChanged(const Value: TNetChangeEvent);
    procedure   SetOnAddressListChanged(const Value: TNetChangeEvent);
    function    GetRcvdCount : Integer; virtual;
    procedure   SetBufSize(Value : Integer); virtual;
    function    GetBufSize: Integer; virtual;
    procedure   SetSocketRcvBufSize(BufSize : Integer); virtual;
    procedure   SetSocketSndBufSize(BufSize : Integer); virtual;
    procedure   BindSocket; virtual;
    procedure   SendText(const Str : RawByteString); {$IFDEF COMPILER12_UP} overload;
    procedure   SendText(const Str : UnicodeString); overload;
    procedure   SendText(const Str : UnicodeString; ACodePage : Cardinal); overload;
                                                     {$ENDIF}
    function    RealSend(var Data : TWSocketData; Len : Integer) : Integer; virtual;
    procedure   RaiseException(const Msg : String); overload; virtual;
    procedure   RaiseException(const Msg : String;
                       AErrorCode           : Integer;   { V8.36 more }
                       const AErrorMessage  : String = '';
                       const AFriendlyMsg   : String = '';
                       const AFunc          : String = '';
                       const AIP            : String = '';
                       const APort          : String = '';
                       const AProto         : String = ''); overload; virtual;
    function    GetReqVerLow: BYTE;
    procedure   SetReqVerLow(const Value: BYTE);
    function    GetReqVerHigh: BYTE;
    procedure   SetReqVerHigh(const Value: BYTE);
    procedure   TriggerDebugDisplay(Msg : String); { 18/06/05 }
    procedure   TriggerSendData(BytesSent : Integer); virtual;                  { V8.65 }
    function    TriggerDataAvailable(Error : Word) : Boolean; virtual;
    procedure   TriggerSessionAvailable(Error : Word); virtual;
    procedure   TriggerSessionConnectedSpecial(Error : Word); virtual;
    procedure   TriggerSessionConnected(Error : Word); virtual;
    procedure   TriggerSessionClosed(Error : Word); virtual;
    procedure   TriggerDataSent(Error : Word); virtual;
    procedure   TriggerChangeState(OldState, NewState : TSocketState); virtual;
    procedure   TriggerDNSLookupDone(Error : Word); virtual;
    procedure   TriggerError; virtual;
    procedure   TriggerException (E: ESocketException); virtual;   { V8.36 }
    procedure   TriggerAddressListChanged(ErrCode: Word);
    procedure   TriggerRoutingInterfaceChanged(ErrCode: Word);
    function    DoRecv(var Buffer : TWSocketData;
                       BufferSize : Integer;
                       Flags      : Integer) : Integer; virtual;
    function    DoRecvFrom(FHSocket    : TSocket;
                           var Buffer  : TWSocketData;
                           BufferSize  : Integer;
                           Flags       : Integer;
                           var From    : TSockAddr;
                           var FromLen : Integer) : Integer; virtual;
    procedure Do_FD_CONNECT(var msg: TMessage); virtual;
    procedure Do_FD_READ(var msg: TMessage); virtual;
    procedure Do_FD_WRITE(var msg: TMessage); virtual;
    procedure Do_FD_ACCEPT(var msg: TMessage); virtual;
    procedure Do_FD_CLOSE(var msg: TMessage); virtual;
    procedure Do_FD_ROUTING_INTERFACE_CHANGE(var msg: TMessage); virtual;
    procedure Do_FD_ADDRESS_LIST_CHANGE(var msg: TMessage); virtual;
    procedure DupConnected; virtual;
    procedure SetSin(const Value: TSockAddrIn);
    function  GetSin: TSockAddrIn;
    function  GetCurrentSocketFamily: TSocketFamily;
    { The next three are wrappers around methods of TIcsAsyncDnsLookup. They   }
    { mimic the MS native async DNS looup API that isn't available for IPv6.   }
    function IcsAsyncGetHostByName(AWnd                : HWND;
                                   AMsgID              : UINT;
                                   const ASocketFamily : TSocketFamily;
                                   const AName         : String;
                                   const AProtocol     : Integer): THandle;
    function IcsAsyncGetHostByAddr(AWnd                : HWND;
                                   AMsgID              : UINT;
                                   const ASocketFamily : TSocketFamily;
                                   const AAddr         : String;
                                   const AProtocol     : Integer): THandle;
    function IcsCancelAsyncRequest(const ARequest: THandle): Integer;
    procedure GetSocketIpInfo;                                                     { V9.5 }
    procedure ClearSocketIpInfo;                                                   { V9.5 }
  public
    property sin  : TSockAddrIn read GetSin write SetSin;
    property sin6 : TSockAddrIn6 read Fsin write Fsin;
    property SessIpInfo : TIcsSessIpInfo       read  FSessIpInfo      { V9.5 session IP info after connection }
                                               write FSessIpInfo;
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Connect; virtual;
    procedure   Close; virtual;
    procedure   CloseDelayed; virtual;
    procedure   Abort; virtual;
    procedure   Flush; virtual;
    procedure   WaitForClose; virtual;
    procedure   Listen; virtual;
    function    Accept: TSocket; virtual;
    function    Receive(Buffer : TWSocketData; BufferSize: Integer) : Integer; virtual;
    function    ReceiveTB(var Data : TBytes; MaxLen : Integer = -1) : Integer; overload; virtual;   { V8.70 }
    function    ReceiveTB(MaxLen : Integer = -1) : TBytes; overload; virtual;                       { V8.71 }
    function    ReceiveStr : String; virtual;
    function    ReceiveStrA : AnsiString; virtual;
{$IFDEF COMPILER12_UP}
    function    ReceiveStrW(ACodePage: Cardinal) : UnicodeString; overload; virtual;
    function    ReceiveStrW : UnicodeString; overload; virtual;
{$ENDIF}
    function    ReceiveFrom(Buffer      : TWSocketData;
                            BufferSize  : Integer;
                            var From    : TSockAddr;
                            var FromLen : Integer) : Integer; virtual;
    function    ReceiveFrom6(Buffer      : TWSocketData;   { V8.07 }
                             BufferSize  : Integer;
                             var From    : TSockAddrIn6;
                             var FromLen : Integer) : Integer; virtual;
    function    ReceiveFromTB(var Data : TBytes; var From : TSockAddr;
                              var FromLen : Integer; MaxLen : Integer = -1) : Integer; virtual;   { V8.70 }
    function    ReceiveFrom6TB(var Data : TBytes; var From : TSockAddrIn6;
                              var FromLen : Integer; MaxLen : Integer = -1) : Integer; virtual;   { V8.70 }
    function    PeekData(Buffer : TWSocketData; BufferSize: Integer) : Integer;
    function    Send(Data : TWSocketData; Len : Integer) : Integer; overload; virtual;
    function    Send(DataByte : Byte) : Integer; overload; virtual;
    function    SendTB(const Data: TBytes; Len: Integer = -1) : Integer; virtual;     { V8.69 }
    function    SendTo(Dest       : TSockAddr;
                       DestLen    : Integer;
                       Data       : TWSocketData;
                       Len        : Integer) : Integer; virtual;
    function    SendToTB(Dest       : TSockAddr;
                         DestLen    : Integer;
                         const Data : TBytes;
                         Len: Integer = -1) : Integer; virtual;                       { V8.69 }
    function    SendTo6(Dest       : TSockAddrIn6;       { V8.07 }
                        DestLen    : Integer;
                        Data       : TWSocketData;
                        Len        : Integer) : Integer; virtual;
    function    SendToTB6(Dest       : TSockAddrIn6;       { V8.07 }
                          DestLen    : Integer;
                          const Data : TBytes;
                          Len: Integer = -1) : Integer; virtual;                       { V8.69 }
    function    SendStr(const Str : RawByteString) : Integer; {$IFDEF COMPILER12_UP} overload; {$ENDIF} virtual;
{$IFDEF COMPILER12_UP}
    function    SendStr(const Str : UnicodeString; ACodePage: Cardinal) : Integer; overload; virtual;
    function    SendStr(const Str : UnicodeString) : Integer; overload; virtual;
{$ENDIF}
    procedure   DnsLookup(const AHostName : String); overload; virtual;
    procedure   DnsLookup(const AHostName : String; const AProtocol: Integer); overload; virtual;
    procedure   ReverseDnsLookup(const HostAddr: String); overload; virtual;
    procedure   ReverseDnsLookup(const HostAddr: String; const AProtocol: Integer); overload; virtual;
    procedure   ReverseDnsLookupSync(const HostAddr: String); overload; virtual;  {AG 03/03/06}
    procedure   ReverseDnsLookupSync(const HostAddr: String; const AProtocol: Integer); overload; virtual;
    procedure   CancelDnsLookup; virtual;
    { Sets behavior of the internal DNS lookup object.                         }
    { AMinThreads determines the number of persistent DNS lookup threads while }
    { AMaxThreads determines the maximum number of threads the internal DNS    }
    { lookup object may create per caller's thread context. After an idle time }
    { out of 60 seconds the non-persistent threads exit.                       }
    { Works only with new API, that is when SocketFamily is not sfIPv4.        }
    procedure   SetMinMaxIcsAsyncDnsLookupThreads(AMinThreads, AMaxThreads: Byte);
    { Helper method that assigns FAsyncLookupPtr internally                   }
    procedure   RegisterIcsAsyncDnsLookup;
    procedure   UnregisterIcsAsyncDnsLookup;

    function    GetPeerAddr: String; virtual;
    function    GetPeerPort: String; virtual;
    function    GetPeerName(var Name : TSockAddrIn; NameLen : Integer) : Integer; virtual;
    function    GetXPort: String; virtual;
    function    GetXAddr: String; virtual;
    function    TimerIsSet(var tvp : TTimeVal) : Boolean; virtual;
    procedure   TimerClear(var tvp : TTimeVal); virtual;
    function    TimerCmp(var tvp : TTimeVal; var uvp : TTimeVal; IsEqual : Boolean) : Boolean; virtual;
    function    GetSockName(var saddr : TSockAddrIn; var saddrlen : Integer) : Integer; virtual;
    procedure   SetLingerOption;
    procedure   SetKeepAliveOption;
    function    SetTcpNoDelayOption: Boolean; { V7.27 }
    function    SetRoutingInterfaceChangeNotification: Boolean; virtual;
    function    SetAddressListChangeNotification: Boolean; virtual;
    procedure   Dup(NewHSocket : TSocket); virtual;
    procedure   Shutdown(How : Integer); virtual;
    procedure   Pause; virtual;
    procedure   Resume; virtual;
    procedure   PutDataInSendBuffer(Data : TWSocketData; Len : Integer); virtual;
    function    PutStringInSendBuffer(const Str : RawByteString): Integer; {$IFDEF COMPILER12_UP} overload; {$ENDIF}
{$IFDEF COMPILER12_UP}
    function    PutStringInSendBuffer(const Str : UnicodeString; ACodePage: Cardinal): Integer; overload;
    function    PutStringInSendBuffer(const Str : UnicodeString): Integer; overload;
{$ENDIF}
    procedure   DeleteBufferedData;
    procedure   ThreadAttach; override;
    procedure   ThreadDetach; override;
    procedure   CreateCounter; virtual;
    procedure   DestroyCounter;
    property    BufferedByteCount  : Integer        read FBufferedByteCount;  { V5.20 }
    property    CurrentSocketFamily: TSocketFamily  read GetCurrentSocketFamily;
    property    AddrResolvedStr : String            read FAddrResolvedStr;    { V8.60 IPv4 or IPv6 address }
    property    PunycodeHost : String               read FPunycodeHost;       { V8.64 Puncycode result of last DnsLookup }
    property    AddrFormat : Integer                read FAddrFormat;         { V9.1 resolved AF_INET, AF_INET6 }

  protected
{$IFNDEF NO_DEBUG_LOG}
    property IcsLogger : TIcsLogger                 read  FIcsLogger          { V5.21 }
                                                    write SetIcsLogger;       { V5.21 }
{$ENDIF}
    property PortNum : Integer                      read  FPortNum;
    property FWindowHandle : HWND                   read  FHandle;
    property HSocket : TSocket                      read  FHSocket
                                                    write Dup;
    property Addr : String                          read  FAddrStr
                                                    write SetAddr;
    property Port : String                          read  GetRemotePort
                                                    write SetRemotePort;
    property LocalPort : String                     read  FLocalPortStr
                                                    write SetLocalPort;
    property LocalAddr : String                     read  FLocalAddr
                                                    write SetLocalAddr;
    property LocalAddr6: String                     read  FLocalAddr6
                                                    write SetLocalAddr6;
    property Proto : String                         read  FProtoStr
                                                    write SetProto;
    property MultiCast       : Boolean              read  FMultiCast
                                                    write FMultiCast
                                                    default False;
    property MultiCastAddrStr: String               read  FMultiCastAddrStr
                                                    write SetMultiCastAddrStr;
    property MultiCastIpTTL  : Integer              read  FMultiCastIpTTL
                                                    write FMultiCastIpTTL
                                                    default 1;
    property ReuseAddr       : Boolean              read  FReuseAddr
                                                    write FReuseAddr
                                                    default False;
    property ExclusiveAddr   : Boolean              read  FExclusiveAddr
                                                    write FExclusiveAddr;   { V8.36 }
    property PeerAddr : String                      read  GetPeerAddr;
    property PeerPort : String                      read  GetPeerPort;
    property DnsResult : String                     read  FDnsResult
                                                    write FDnsResult;     { V8.60 }
    property DnsResultList : TStrings               read  FDnsResultList;
    property State : TSocketState                   read  FState;
    property AllSent   : Boolean                    read  bAllSent;
    property ReadCount : Int64                      read  FReadCount;    { V5.26 }
    property WriteCount : Int64                     read  FWriteCount;   { V7.24 }
    property RcvdCount : Integer                    read  GetRcvdCount;
    property LastError : Integer                    read  FLastError
                                                    write FLastError   { V5.20 }
                                                    default 0;
    property ComponentOptions : TWSocketOptions     read  FComponentOptions
                                                    write FComponentOptions;
    property BufSize          : Integer             read  GetBufSize
                                                    write SetBufSize;
    property SocketRcvBufSize : Integer             read  FSocketRcvBufSize
                                                    write SetSocketRcvBufSize;
    property SocketSndBufSize : Integer             read  FSocketSndBufSize
                                                    write SetSocketSndBufSize;
    property ListenBacklog    : Integer             read  FListenBacklog
                                                    write FListenBacklog
                                                    default 5;
    property ReqVerLow       : BYTE                 read  GetReqVerLow
                                                    write SetReqVerLow
                                                    default 2;
    property ReqVerHigh      : BYTE                 read  GetReqVerHigh
                                                    write SetReqVerHigh
                                                    default 2;
    property OnDataAvailable : TDataAvailable       read  FOnDataAvailable
                                                    write FOnDataAvailable;
    property OnDataSent      : TDataSent            read  FOnDataSent
                                                    write FOnDataSent;
    property OnSendData      : TSendData            read  FOnSendData
                                                    write FOnSendData;
    property OnSessionClosed : TSessionClosed       read  FOnSessionClosed
                                                    write FOnSessionClosed;
    property OnSessionAvailable : TSessionAvailable read  FOnSessionAvailable
                                                    write FOnSessionAvailable;
    property OnSessionConnected : TSessionConnected read  FOnSessionConnected
                                                    write FOnSessionConnected;
    property OnChangeState      : TChangeState      read  FOnChangeState
                                                    write FOnChangeState;
    property OnDnsLookupDone    : TDnsLookupDone    read  FOnDnsLookupDone
                                                    write FOnDnsLookupDone;
    property OnError            : TNotifyEvent      read  FOnError
                                                    write FOnError;
    property SocketErrs         : TSocketErrs       read  FSocketErrs
                                                    write FSocketErrs;   { V8.36 }
    property onException        : TicsException     read  FonException
                                                    write FonException;  { V8.36 }
    { FlushTimeout property is not used anymore }
    property FlushTimeout : Integer                 read  FFlushTimeOut
                                                    write FFlushTimeout
                                                    default 60;
    property SendFlags : TSocketSendFlags           read  GetSendFlags
                                                    write SetSendFlags
                                                    default wsSendNormal;
    property Text: String                           read  ReceiveStr
                                                    write SendText;
    property LingerOnOff   : TSocketLingerOnOff     read  FLingerOnOff
                                                    write FLingerOnOff
                                                    default wsLingerOn;
    property LingerTimeout : Integer                read  FLingerTimeout
                                                    write FLingerTimeout
                                                    default 0;
    property KeepAliveOnOff: TSocketKeepAliveOnOff  read  FKeepAliveOnOff
                                                    write FKeepAliveOnOff
                                                    default wsKeepAliveOff;
    property KeepAliveTime : Integer                read  FKeepAliveTime
                                                    write FKeepAliveTime
                                                    default 0;
    property KeepAliveInterval : Integer            read  FKeepAliveInterval
                                                    write FKeepAliveInterval
                                                    default 0;
    property OnDebugDisplay : TDebugDisplay         read  FOnDebugDisplay
                                                    write FOnDebugDisplay;
    property Counter      : TWSocketCounter         read  FCounter;
    property CounterClass : TWsocketCounterClass    read  FCounterClass
                                                    write SetCounterClass;
    property SocketFamily : TSocketFamily           read  FSocketFamily
                                                    write SetSocketFamily
                                                    default DefaultSocketFamily;
    property OnAddressListChanged : TNetChangeEvent read  FOnAddressListChanged
                                                    write SetOnAddressListChanged;
    property OnRoutingInterfaceChanged : TNetChangeEvent
                                                    read  FOnRoutingInterfaceChanged
                                                    write SetOnRoutingInterfaceChanged;
  end;

  THttpTunnelAuthType = (htatDetect, htatNone, htatBasic,
                    {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
                         htatDigest,
                    {$ENDIF}
                         htatNtlm
                         );

  THttpTunnelErrorEvent = procedure(Sender               : TObject;
                                   ErrCode               : Word;
                                   TunnelServerAuthTypes : THttpTunnelServerAuthTypes;
                                   const Msg             : String) of object;

  TCustomHttpTunnelWSocket = class(TCustomWSocket)
  private
      FHttpTunnelAuthChallenge    : AnsiString;
  {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
      FHttpTunnelAuthDigestCached : Boolean;
      FHttpTunnelAuthDigestValid  : Boolean;
      FHttpTunnelAuthDigestHash   : THashHex;
      FHttpTunnelAuthDigestInfo   : TAuthDigestResponseInfo;
  {$ENDIF}
      FHttpTunnelAuthType         : THttpTunnelAuthType;
      FHttpTunnelBuf              : TBytes;
      FHttpTunnelBufSize          : Integer;
      FHttpTunnelChunked          : Boolean;
      FHttpTunnelChunkRcvd        : Integer;
      FHttpTunnelChunkSize        : Integer;
      FHttpTunnelChunkState       : THttpTunnelChunkState;
      FHttpTunnelCloseNotified    : Boolean;
      FHttpTunnelContentLength    : Integer;
      FHttpTunnelCurAuthType      : THttpTunnelAuthType;
      FHttpTunnelKeepsAlive       : Boolean;
      FHttpTunnelLastResponse     : AnsiString; // Also hijacked for internal error messages
      FHttpTunnelLmCompatLevel    : Cardinal;   { V7.86 }
      FHttpTunnelReconnectRequest : THttpTunnelReconnectRequest;
      FHttpTunnelPassword         : String;
      FHttpTunnelPort             : AnsiString;
      FHttpTunnelPortAssigned     : Boolean;
      FHttpTunnelProto            : THttpTunnelProto;
      FHttpTunnelRcvdCnt          : Integer;
      FHttpTunnelRcvdIdx          : Integer;
      FHttpTunnelServer           : AnsiString;
      FHttpTunnelServerAssigned   : Boolean;
      FHttpTunnelServerAuthTypes  : THttpTunnelServerAuthTypes;
      FHttpTunnelState            : THttpTunnelState;
      FHttpTunnelStatusCode       : Word;
      FHttpTunnelUsercode         : String;
      FHttpTunnelWaitingBody      : Boolean;
      FMsg_WM_TUNNEL_RECONNECT    : UINT;
      FOnHttpTunnelConnected      : TSessionConnected;
      FOnHttpTunnelError          : THttpTunnelErrorEvent;
      function  GetHttpTunnelLastResponse: String;
      function  GetHttpTunnelServer: String;
      function  GetHttpTunnelPort: String;
      procedure HttpTunnelClear;
      function  HttpTunnelGetNtlmMessage3: String;
      function  HttpTunnelProcessHdrLine(Data: PAnsiChar; Cnt: Integer): Boolean;
      procedure HttpTunnelSendAuthBasic;
  {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
      procedure HttpTunnelSendAuthDigest;
  {$ENDIF}
      procedure HttpTunnelSendAuthNtlm_1;
      procedure HttpTunnelSendAuthNtlm_3;
      procedure HttpTunnelSendPlainConnect;
      function  HttpTunnelTriggerResultOrContinue: Boolean;
      procedure SetHttpTunnelAuthType(const Value: THttpTunnelAuthType);
      procedure SetHttpTunnelBufferSize(BufSize: Integer);
      procedure SetHttpTunnelServer(const Value: String);
      procedure SetHttpTunnelPassword(const Value: String);
      procedure SetHttpTunnelPort(const Value: String);
      procedure SetHttpTunnelUsercode(const Value: String);
      procedure TriggerHttpTunnelConnected(ErrCode : Word);
      procedure TriggerHttpTunnelError(ErrCode: Word);
  protected
      procedure AllocateMsgHandlers; override;
      procedure AssignDefaultValue; override;
      procedure Do_FD_CLOSE(var msg: TMessage); override;
      function  DoRecv(var Buffer : TWSocketData;
                       BufferSize : Integer;
                       Flags      : Integer) : Integer; override;
      procedure FreeMsgHandlers; override;
      function  GetRcvdCount : Integer; override;
      function  MsgHandlersCount : Integer; override;
      procedure RaiseException(const Msg : String); override;
      function  TriggerDataAvailable(ErrCode : Word) : Boolean; override;
      procedure TriggerSessionClosed(ErrCode : Word); override;
      procedure TriggerSessionConnectedSpecial(ErrCode: Word); override;
      procedure WndProc(var MsgRec: TMessage); override;
      procedure WMHttpTunnelReconnect(var MsgRec: TMessage);

      property  HttpTunnelAuthType   : THttpTunnelAuthType
                                                    read  FHttpTunnelAuthType
                                                    write SetHttpTunnelAuthType
                                                    default htatDetect;
      property  HttpTunnelBufferSize : Integer      read  FHttpTunnelBufSize
                                                    write SetHttpTunnelBufferSize;
      property  HttpTunnelLastResponse : String     read  GetHttpTunnelLastResponse;
      property  HttpTunnelLmCompatLevel : Cardinal  read  FHttpTunnelLmCompatLevel   { V7.86 }
                                                    write FHttpTunnelLmCompatLevel;  { V7.86 }
      property  HttpTunnelPassword   : String       read  FHttpTunnelPassword
                                                    write SetHttpTunnelPassword;
      property  HttpTunnelPort       : String       read  GetHttpTunnelPort
                                                    write SetHttpTunnelPort;
      property  HttpTunnelServer     : String       read  GetHttpTunnelServer
                                                    write SetHttpTunnelServer;
      property  HttpTunnelUsercode   : String       read  FHttpTunnelUsercode
                                                    write SetHttpTunnelUsercode;

      property  HttpTunnelCurrentAuthType : THttpTunnelAuthType
                                                    read FHttpTunnelCurAuthType;

      property  HttpTunnelState : THttpTunnelState  read FHttpTunnelState;  { V8.66 }
      property  OnHttpTunnelError  : THttpTunnelErrorEvent
                                                    read  FOnHttpTunnelError
                                                    write FOnHttpTunnelError;
      property  OnHttpTunnelConnected : TSessionConnected
                                                    read  FOnHttpTunnelConnected
                                                    write FOnHttpTunnelConnected;
  public
      constructor Create(AOwner: TComponent); override;
      procedure Connect; override;
      procedure Listen; override;
  end;

  TSocksAuthStateEvent = procedure(Sender : TObject; AuthState : TSocksAuthState) of object;
  TSocksErrorEvent     = procedure(Sender : TObject; Error : Integer; Msg : String) of Object;


  TCustomSocksWSocket = class(TCustomHttpTunnelWSocket)
  protected
      FSocksState          : TSocksState;
      FSocksServer         : String;
      FSocksLevel          : String;
      FSocksPort           : String;
      FSocksPortAssigned   : Boolean;
      FSocksServerAssigned : Boolean;
      FSocksUsercode       : String;
      FSocksPassword       : String;
      FSocksAuthentication : TSocksAuthentication;
      FSocksAuthNumber     : AnsiChar;
      FBoundAddr           : AnsiString;
      FBoundPort           : AnsiString;
      FRcvBuf              : array [0..127] of Byte;
      FRcvCnt              : Integer;
      FSocksRcvdCnt        : Integer;
      FSocksRcvdPtr        : Integer;
      FOnSocksError        : TSocksErrorEvent;
      FOnSocksConnected    : TSessionConnected;
      FOnSocksAuthState    : TSocksAuthStateEvent;
      FProxyURL            : String;                              { V8.66 }
      procedure   AssignDefaultValue; override;
      procedure   TriggerSessionConnectedSpecial(Error : Word); override;
      procedure   TriggerSocksConnected(Error : Word); virtual;
      procedure   TriggerSessionClosed(Error : Word); override;
      function    TriggerDataAvailable(Error : Word) : Boolean; override;
      function    GetSocksPort: String;
      procedure   SetSocksPort(sPort : String); virtual;
      function    GetSocksServer: String;
      procedure   SetSocksServer(sServer : String); virtual;
      procedure   TriggerSocksError(Error : Integer; Msg : String); virtual;
      procedure   TriggerSocksAuthState(AuthState : TSocksAuthState);
      function    GetRcvdCount : Integer; override;
      procedure   SetSocksLevel(newValue : String);
      function    DoRecv(var Buffer : TWSocketData;
                         BufferSize : Integer;
                         Flags      : Integer) : Integer; override;
      procedure   SocksDoConnect;
      procedure   SocksDoAuthenticate;
      procedure   DataAvailableError(ErrCode : Integer; Msg : String);
      procedure   SetProxyURL(const Value: String);                          { V8.66 }
  public
      constructor Create(AOwner : TComponent); override;
      procedure   SetSocks(const Host: string = ''; const Port: string = '';
                                    const User: string = ''; const Pass: string = '');   { V8.66 }
      procedure   SetHTTPTunnel(const Host: string = ''; const Port: string = '';
                                    const User: string = ''; const Pass: string = '');   { V8.66 }
      procedure   Connect; override;
      procedure   Listen; override;
  protected
      property SocksServer   : String               read  GetSocksServer
                                                    write SetSocksServer;
      property SocksLevel    : String               read  FSocksLevel
                                                    write SetSocksLevel;
      property SocksPort     : String               read  FSocksPort
                                                    write SetSocksPort;
      property SocksUsercode : String               read  FSocksUsercode
                                                    write FSocksUsercode;
      property SocksPassword : String               read  FSocksPassword
                                                    write FSocksPassword;
      property SocksAuthentication : TSocksAuthentication
                                                    read  FSocksAuthentication
                                                    write FSocksAuthentication
                                                    default socksNoAuthentication;
      property ProxyURL : String                    read  FProxyURL
                                                    write SetProxyURL;                    { V8.66 }
      property OnSocksError  : TSocksErrorEvent     read  FOnSocksError
                                                    write FOnSocksError;
      property OnSocksConnected : TSessionConnected read  FOnSocksConnected
                                                    write FOnSocksConnected;
      property OnSocksAuthState : TSocksAuthStateEvent
                                                    read  FOnSocksAuthState
                                                    write FOnSocksAuthState;
  end;

{ You must define USE_SSL so that SSL code is included in the component.    }
{ Either in OverbyteIcsDefs.inc or in the project/package options.          }
{$IFDEF USE_SSL}
    procedure UnloadSsl;     { redirects to OverbyteIcsLIBEAY }
    procedure LoadSsl;

type
  TSslVerifyPeerEvent = procedure (Sender    : TObject;
                                   var Ok    : Integer;
                                   Cert      : TX509Base) of object;
  TSslHandshakeDoneEvent = procedure (Sender            : TObject;
                                      ErrCode           : Word;
                                      PeerCert          : TX509Base;
                                      var Disconnect    : Boolean) of object;

  TSslCertVerifyEvent = procedure (Sender            : TObject;             { V9.3 TCertVerMethod = CertVerOwnEvent }
                                   CertChain         : TX509List;
                                   var VerifyCode    : Integer;
                                   var VerifyInfo    : String;
                                   var Disconnect    : Boolean) of object;

  //Client-side session caching
  TSslCliGetSession         = procedure(Sender          : TObject;
                                        var SslSession  : Pointer;
                                        var FreeSession : Boolean) of object;
  TSslCliNewSession         = procedure(Sender          : TObject;
                                        SslSession      : Pointer;
                                        WasReused       : Boolean;
                                        var IncRefCount : Boolean) of object;

  //Server-side session caching
  TSslSetSessionIDContext   = procedure(Sender                  : TObject;
                                        var SessionIDContext    : TSslSessionIdContext) of object;
  TSslSvrNewSession         = procedure(Sender                  : TObject;
                                        SslSession              : Pointer;
                                        SessId                  : Pointer;
                                        Idlen                   : Integer;
                                        var AddToInternalCache  : Boolean) of object;
  TSslSvrGetSession         = procedure(Sender                  : TObject;
                                        var SslSession          : Pointer;
                                        SessId                  : Pointer;
                                        Idlen                   : Integer;
                                        var IncRefCount         : Boolean) of object;

  TSslCliCertRequest        = procedure(Sender     : TObject;
                                        var Cert   : TX509Base) of object;
  TSslShutDownComplete      = procedure(Sender          : TObject;
                                        Bidirectional   : Boolean;
                                        ErrCode         : Integer) of object;
  TSslServerNameEvent       = procedure(Sender               : TObject;
                                        var Ctx              : TSslContext;
                                        var ErrCode          : TTlsExtError) of object;

 { V8.40 handshake protocol message callback }
  TSslProtoMsgEvent        = procedure (Sender               : TObject;
                                        Info                 : String;
                                        Sending              : integer;
                                        Version              : integer;
                                        ContentType          : integer;
                                        Buffer               : PAnsiChar;
                                        BuffSize             : integer) of object;

 { V8.56 SSL ALPN protocol selection }
  TSslAlpnSelect           = procedure (Sender               : TObject;
                                        ProtoList            : TStrings;
                                        var SelProto         : String;
                                        var ErrCode          : TTlsExtError) of object;


  TCustomSslWSocket = class(TCustomSocksWSocket)
  private
      procedure SslShutDownAsync(How: Integer); { V7.80 }
  protected
        FSslContext                 : TSslContext;
        FOnSslSvrNewSession         : TSslSvrNewSession;
        FOnSslSvrGetSession         : TSslSvrGetSession;
        FOnSslCliGetSession         : TSslCliGetSession;
        FOnSslCliNewSession         : TSslCliNewSession;
        FOnSslSetSessionIDContext   : TSslSetSessionIDContext;
        FOnSslServerName            : TSslServerNameEvent;
        FOnSslCliCertRequest        : TSslCliCertRequest;
        FX509Class                  : TX509Class;
        FSslCertChain               : TX509List;
        FSslPeerCert                : TX509Base;
        FSslMode                    : TSslMode;
        FSslBufList                 : TIcsBufferHandler;
        FExplizitSsl                : Boolean;
        bSslAllSent                 : Boolean;
        FMayTriggerFD_Read          : Boolean;
        FMayTriggerFD_Write         : Boolean;
        FMayTriggerDoRecv           : Boolean;
        FMayTriggerSslTryToSend     : Boolean;
        FCloseCalled                : Boolean;
        FPendingSslEvents           : TSslPendingEvents;
        FSslIntShutDown             : Integer;
        FShutDownHow                : Integer;
        FSslEnable                  : Boolean;
        FLastSslError               : Cardinal;     { V8.66 was Integer }
        FSslInRenegotiation         : Boolean;    // <= 01/01/06
        FSslBioWritePendingBytes    : Integer;
        FSendPending                : Boolean;
        FSslBiShutDownFlag          : Boolean;    // <= 01/08/06
        FOnSslShutDownComplete      : TSslShutDownComplete;
        FNetworkError               : Integer;
        FSslInitialized             : Boolean;
//        FInHandshake                : Boolean;      { V8.55 unused }
        FHandshakeDone              : Boolean;        { only used to trigger event once }
        FHandshakeEventDone         : Boolean;        { V8.55 don't trigger event more than once }
        FSslHandshakeErr            : Integer;       { V8.14  }
        FSslHandshakeRespMsg        : string;        { V8.14  }
        FSslVersNum                 : Integer;        //12/09/05
        FSSLState                   : TSslState;
        FSsl_In_CB                  : Boolean;
        FSsl                        : PSSL;
        FSslBio                     : PBIO;
        FIBio                       : PBIO;
        FNBio                       : PBIO;
        FSslAcceptableHosts         : TStrings;
        FSslVerifyResult            : Integer;
        FSslVersion                 : String;
        FSslCipher                  : String;
        FSslCipherDesc              : String;       { V8.14  }
        FSslEncryption              : String;       { V8.14  }
        FSslKeyExchange             : String;       { V8.14  }
        FSslMessAuth                : String;       { V8.14  }
        FSslCertPeerName            : String;       { V8.39 }
        FSslKeyAuth                 : String;       { V8.41 }
        FSslGroupName               : String;       { V9.3 }
        FSslTotalBits               : Integer;
        FSslSecretBits              : Integer;
        FSslSupportsSecureRenegotiation : Boolean;
        FMsg_WM_TRIGGER_DATASENT    : UINT;
        FMsg_WM_SSL_ASYNCSELECT     : UINT;
        FMsg_WM_RESET_SSL           : UINT;
        FMsg_WM_BI_SSL_SHUTDOWN     : UINT;
        FMsg_WM_TRIGGER_SSL_SHUTDOWN_COMPLETED : UINT;
        FOnSslVerifyPeer            : TSslVerifyPeerEvent;
        FOnSslHandshakeDone         : TSslHandshakeDoneEvent;
        FHandShakeCount             : Integer;
        FSslServerName              : String;
        FOnSslProtoMsg              : TSslProtoMsgEvent;  { V8.40 }
        FOnSslAlpnSelect            : TSslAlpnSelect;     { V8.56 }
        FAlpnProtoAnsi              : AnsiString;         { V8.62 server sending response }
        FSslAlpnProtoRcvd           : String;             { V8.62 client one ALPN server requests, comes during handshake }
        FCliHelloData               : TClientHelloData;   { V8.64 client hello data received by server, includes SNI and Alpn }
        FOcspStapleRaw              : AnsiString;         { V8.69 OCSP staple response from server, or sent to server }
        FOcspStapleStatus           : Integer;            { V8.69 OCSP staple status }
        FSslAlpnProtoList           : TStrings;           { V9.1 client ALPN protocols accepted, sent with request }
        procedure   RaiseLastOpenSslError(EClass          : ExceptClass;
                                          Dump            : Boolean = FALSE;
                                          const CustomMsg : String  = ''); virtual;
        function  SocketDataPending : Boolean;
        procedure InternalShutdown(How: Integer);
        procedure PutDataInSslBuffer(Data: Pointer; Len: Integer);
        procedure DeleteBufferedSslData;
        function  GetRcvdCount : Integer; override;
        procedure WMSslBiShutDown(var msg: TMessage);
        procedure WMSslASyncSelect(var msg: TMessage);
        procedure WMTriggerSslShutDownComplete(var msg: TMessage);
     {   procedure Do_SSL_FD_READ(var Msg: TMessage);    removed V8.22 }
        function  TriggerEvent(Event: TSslEvent; ErrCode: Word): Boolean;

        procedure AssignDefaultValue; override;
        procedure Do_FD_CONNECT(var Msg : TMessage); override;
        procedure Do_FD_READ(var Msg : TMessage); override;
        procedure Do_FD_WRITE(var Msg : TMessage); override;
        procedure Do_FD_CLOSE(var Msg : TMessage); override;
        procedure Do_FD_ACCEPT(var Msg : TMessage); override;
        //procedure WMSslHandshakeDone(var msg: TMessage); message WM_TRIGGER_SSLHANDSHAKEDONE;
        function  SslShutdownCompleted(How: Integer) : Boolean;
        function  DoRecv(var Buffer : TWSocketData;
                         BufferSize : Integer;
                         Flags      : Integer) : Integer; override;
        procedure TryToSend; override;
        procedure InitializeSsl;
        procedure FinalizeSsl;
        procedure InitSSLConnection(ClientMode : Boolean; pSSLContext : PSSL_CTX = nil);
        procedure DupConnected; override;
        procedure InternalClose(bShut : Boolean; Error : Word); override;
        procedure InternalAbort(ErrCode : Word); override;
        procedure WndProc(var MsgRec: TMessage); override;
        procedure SetSslAcceptableHosts(Value : TStrings);
        procedure TriggerEvents;
        procedure TriggerSessionConnected(ErrCode : Word); override;
        procedure TriggerSslHandshakeDone(ErrCode : Word); virtual;
        procedure TriggerSslVerifyPeer(var Ok     : Integer;
                                       Cert       : TX509Base); virtual;
        procedure TriggerSslCliNewSession; virtual;
        procedure SetSslContext(const Value: TSslContext);
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;
        procedure TriggerSslShutDownComplete(ErrCode: Integer); virtual;
        procedure TriggerSslServerName(var Ctx: TSslContext; var ErrCode: TTlsExtError); virtual;  { V8.45 }
        procedure TriggerSslAlpnSelect(ProtoList: TStrings; var SelProto: String; var ErrCode: TTlsExtError); virtual; { V8.56 }
        procedure SslGetAlpnProtocol;                                                              { V8.62 client, protocol server wants }
        procedure SetSslCallbacks(pSSLContext: PSSL_CTX);                                          { V9.1 }
        function  MsgHandlersCount : Integer; override;
        procedure AllocateMsgHandlers; override;
        procedure FreeMsgHandlers; override;
        function  GetSslPeerCert : TX509Base;
        property  X509Class : TX509Class read FX509Class write FX509Class;
    public
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy; override;
        procedure   ResetSSL;
        procedure   Listen; override;
        function    Accept : TSocket; override;
        procedure   Close; override;
        procedure   Dup(NewHSocket: TSocket); override;
        procedure   ThreadAttach; override;
        procedure   Resume; override;
        //procedure DoSslShutdown;
        procedure   ResetSslDelayed;
        procedure   SslBiShutDownAsync;
        function    SslStartRenegotiation : Boolean;
        function    SslRenegotiatePending : Boolean;
        function    SslSessionReused : Boolean;
        procedure   Shutdown(How : Integer); override;
        procedure   PutDataInSendBuffer(Data : TWSocketData; Len : Integer); override;
        procedure   StartSslHandshake;
        procedure   AcceptSslHandshake;
        procedure   SetAcceptableHostsList(const SemiColonSeparatedList : String);
        function    SslGetSupportedCiphers (Supported, Remote: boolean): String;    { V8.27 }
        function    SslBytesToCiphers(const CList: TBytes): String;                 { V8.64 }
        function    SslOK: Boolean;                                                 { V8.66 is TLS/SSL negotiated OK }
        procedure   SetSslAlpnProtocols(ProtoList: TStrings);                       { V9.1 client, protocols we support }

        property    LastSslError       : Cardinal         read FLastSslError;       { V8.66 was Integer }
        property    ExplizitSsl        : Boolean          read  FExplizitSsl
                                                          write FExplizitSsl;
        property    SslServerName      : String           read  FSslServerName
                                                          write FSslServerName;
        property  OnSslShutDownComplete : TSslShutDownComplete
                                               read   FOnSslShutDownComplete
                                               write  FOnSslShutDownComplete;
        property  SSL                :  PSsl              read  FSsl;
                                                          //write FSsl;
        property  SslInRenegotiation : Boolean            read  FSslInRenegotiation; //<= 01/01/06 AG
        property  SslEnable          : Boolean            read  FSslEnable
                                                          write FSslEnable;
        //property  SslEstablished      : Boolean           read  FSslEstablished;
        property  SslState            : TSslState         read  FSslState;
        property  SslContext          : TSslContext       read  FSslContext
                                                          write SetSslContext;
        property  SslCertChain        : TX509List         read  FSslCertChain;

        property  OnSslVerifyPeer : TSslVerifyPeerEvent   read  FOnSslVerifyPeer
                                                          write FOnSslVerifyPeer;
        property  OnSslCliCertRequest : TSslCliCertRequest
                                                          read  FOnSslCliCertRequest
                                                          write FOnSslCliCertRequest;
        property  OnSslHandshakeDone : TSslHandshakeDoneEvent
                                                          read  FOnSslHandshakeDone
                                                          write FOnSslHandshakeDone;
        property  OnSslSvrNewSession : TSslSvrNewSession  read  FOnSslSvrNewSession
                                                          write FOnSslSvrNewSession;
        property  OnSslSvrGetSession : TSslSvrGetSession  read  FOnSslSvrGetSession
                                                          write FOnSslSvrGetSession;
        property  OnSslCliGetSession : TSslCliGetSession
                                                          read  FOnSslCliGetSession
                                                          write FOnSslCliGetSession;
        property  OnSslCliNewSession : TSslCliNewSession  read  FOnSslCliNewSession
                                                          write FOnSslCliNewSession;
        property  OnSslSetSessionIDContext : TSslSetSessionIDContext
                                                          read  FOnSslSetSessionIDContext
                                                          write FOnSslSetSessionIDContext;
        property  OnSslServerName    : TSslServerNameEvent
                                                          read  FOnSslServerName
                                                          write FOnSslServerName;
        property  OnSslAlpnSelect  : TSslAlpnSelect       read  FOnSslAlpnSelect
                                                          write FOnSslAlpnSelect;    { V8.56 }
{$IFNDEF NO_DEBUG_LOG}
        property  OnSslProtoMsg  : TSslProtoMsgEvent      read  FOnSslProtoMsg            { V8.40 }
                                                          write FOnSslProtoMsg;
{$ENDIF}
        property  SslAcceptableHosts : TStrings           read  FSslAcceptableHosts
                                                          write SetSslAcceptableHosts;
        property  SslMode            : TSslMode           read  FSslMode
                                                          write FSslMode;
        property  OcspStapleRaw      : AnsiString         read  FOcspStapleRaw
                                                          write FOcspStapleRaw;        { V8.69 OCSP staple response from server, or sent to server }
        property  OcspStapleStatus   : Integer            read  FOcspStapleStatus;     { V8.69 OCSP staple status }
        property  SslAlpnProtocols : TStrings             read  FSslAlpnProtoList
                                                          write SetSslAlpnProtocols;   { V9.1 ALPN protocols client supports, to server }
        property  SslVersion    : String                  read  FSslVersion;
        property  SslCipher     : String                  read  FSslCipher;
        property  SslTotalBits  : Integer                 read  FSslTotalBits;
        property  SslSecretBits : Integer                 read  FSslSecretBits;
        property  SslPeerCert   : TX509Base               read  GetSslPeerCert;
        property  SslHandshakeErr      : Integer          read  FSslHandshakeErr;        { V8.14  }
        property  SslHandshakeRespMsg  : string           read  FSslHandshakeRespMsg;    { V8.14  }
        property  SslCipherDesc  : String                 read  FSslCipherDesc;          { V8.14  }
        property  SslEncryption  : String                 read  FSslEncryption;          { V8.14  }
        property  SslKeyExchange : String                 read  FSslKeyExchange;         { V8.14  }
        property  SslMessAuth    : String                 read  FSslMessAuth;            { V8.14  }
        property  SslCertPeerName : String                read  FSslCertPeerName;        { V8.39  }
        property  SslKeyAuth     : String                 read  FSslKeyAuth;             { V8.41  }
        property  SslGroupName   : String                 read  FSslGroupName;           { V9.3 }
        property  SslAlpnProto   : String                 read  FSslAlpnProtoRcvd;       { V8.62 ALPN protocol requested, from server }
        property  CliHelloData   : TClientHelloData       read  FCliHelloData;           { V8.64  }
        property  SslVerifyResult : Integer               read  FSslVerifyResult;        { V8.66  }
  private
      function my_WSocket_recv(s: TSocket;
                               var Buf: TWSocketData; len, flags: Integer): Integer;
      function my_RealSend(Buf : TWSocketData; Len : Integer) : Integer;
{$IFNDEF NO_DEBUG_LOG}
      function GetMyBioName(B: PBIO) : String;
{$ENDIF}
      function my_BIO_ctrl_pending(B: PBIO) : integer;
      function my_BIO_read(B: PBIO; Buf: Pointer; Len: Integer): Integer;
      function my_BIO_write(B: PBIO; Buf: Pointer; Len: Integer): Integer;
      function my_BIO_ctrl(bp: PBIO; Cmd: Integer; LArg: Integer; PArg: Pointer): Integer;
      function my_BIO_ctrl_get_write_guarantee(b: PBIO): size_t;    { V8.66 corrected result }
      function my_BIO_ctrl_get_read_request(b: PBIO): size_t;       { V8.66 corrected result }
      function my_BIO_should_retry(b: PBIO): Boolean;
      procedure HandleSslError;
  end;

{$ENDIF} // USE_SSL

type
  TLineLimitEvent = procedure (Sender        : TObject;
                               RcvdLength    : Integer;
                               var ClearData : Boolean) of object;

{ You must define USE_SSL so that SSL code is included in the component.    }
{ Either in OverbyteIcsDefs.inc or in the project/package options.          }
{$IFDEF USE_SSL}
  TCustomLineWSocket = class (TCustomSslWSocket)
{$ELSE}
  TCustomLineWSocket = class (TCustomSocksWSocket)
{$ENDIF}
  protected
      FRcvdPtr             : TWSocketData;
      FRcvBufSize          : Integer;
      FRcvdCnt             : Integer;
      FLineEnd             : AnsiString;
      FLineMode            : Boolean;
      FLineLength          : Integer;    { When a line is available  }
      FLineLimit           : Integer;    { Max line length we accept }
      FLineReceivedFlag    : Boolean;
      FLineFound           : Boolean;
      FLineClearData       : Boolean;
      FLineEcho            : Boolean;    { Echo received data    }
      FLineEdit            : Boolean;    { Edit received data    }
      FTimeout             : Integer;    { Given in milliseconds }
      FTimeStop            : Int64;      { Milliseconds V8.71 was Integer  }
      FOnLineLimitExceeded : TLineLimitEvent;
      procedure   InternalAbort(ErrCode : Word); override; { V7.49 }
      procedure   WndProc(var MsgRec: TMessage); override;
      procedure   WMTriggerDataAvailable(var msg: TMessage);
      function    TriggerDataAvailable(ErrCode : Word) : Boolean; override;
      procedure   TriggerSessionClosed(Error : Word); override;
      procedure   TriggerLineLimitExceeded(Cnt: Integer;
                                           var ClearData : Boolean); virtual;
      procedure   SetLineMode(newValue : Boolean); virtual;
      procedure   EditLine(var Len : Integer); virtual;
      function    GetRcvdCount : Integer; override;
      function    DoRecv(var Buffer : TWSocketData;
                         BufferSize : Integer;
                         Flags      : Integer) : Integer; override;
  public
      constructor Create(AOwner : TComponent); override;
      destructor  Destroy; override;
      function    SendLine(const Str : RawByteString) : Integer; {$IFDEF COMPILER12_UP} overload; {$ENDIF} virtual;
{$IFDEF COMPILER12_UP}
      function    SendLine(const Str : UnicodeString; ACodePage: Cardinal) : Integer; overload; virtual;
      function    SendLine(const Str : UnicodeString) : Integer; overload; virtual;
{$ENDIF}
      property    LineLength : Integer      read  FLineLength;
      property    RcvdPtr    : TWSocketData read  FRcvdPtr;
      property    RcvdCnt    : Integer      read  FRcvdCnt;
  published
      property LineMode : Boolean           read  FLineMode
                                            write SetLineMode
                                            default False;
      property LineLimit : Integer          read  FLineLimit
                                            write FLineLimit
                                            default 65536;
      property LineEnd  : AnsiString        read  FLineEnd
                                            write FLineEnd;
      property LineEcho : Boolean           read  FLineEcho
                                            write FLineEcho
                                            default False;
      property LineEdit : Boolean           read  FLineEdit
                                            write FLineEdit
                                            default False;
      property OnLineLimitExceeded : TLineLimitEvent
                                            read  FOnLineLimitExceeded
                                            write FOnLineLimitExceeded;
  end;



  { DEPRECATED: DO NOT USE Synchronize, WaitUntilReady, ReadLine procedure }
  { for a new application.                                                 }
  TCustomSyncWSocket = class(TCustomLineWSocket)
  protected
      FLinePointer : ^AnsiString;
      function    Synchronize(Proc         : TWSocketSyncNextProc;
                              var DoneFlag : Boolean) : Integer; virtual;
      function    WaitUntilReady(var DoneFlag : Boolean) : Integer; virtual;
      procedure   InternalDataAvailable(Sender: TObject; Error: Word);
  public
      procedure   ReadLine(Timeout : Integer; var Buffer : AnsiString); deprecated
          {$IFDEF COMPILER12_UP}'Do not use in new applications'{$ENDIF};
  end;

{$IFDEF BUILTIN_TIMEOUT}
  TTimeoutReason = (torConnect, torIdle);
  TTimeoutEvent = procedure (Sender: TObject; Reason: TTimeoutReason) of object;
  TCustomTimeoutWSocket = class(TCustomSyncWSocket)
  private
      FTimeoutConnect         : Cardinal;
      FTimeoutIdle            : Cardinal;
      FTimeoutSampling        : Cardinal;
      FOnTimeout              : TTimeoutEvent;
      FTimeoutTimer           : TIcsThreadTimer;
      FTimeoutConnectStartTick: Int64;       { V8.71 was Cardinal }
      FTimeoutOldTimerEnabled : Boolean;
      FTimeoutKeepThreadAlive : Boolean;
      procedure TimeoutHandleTimer(Sender: TObject);
      procedure SetTimeoutSampling(const Value: Cardinal);
      procedure SetTimeoutKeepThreadAlive(const Value: Boolean);
  protected
      procedure TriggerTimeout(Reason: TTimeoutReason); virtual;
      procedure TriggerSessionConnectedSpecial(Error: Word); override;
      procedure TriggerSessionClosed(Error: Word); override;
      procedure DupConnected; override;
  public
      constructor Create(AOwner: TComponent); override;
      procedure Connect; override;
      procedure TimeoutStartSampling;
      procedure TimeoutStopSampling;
      procedure ThreadAttach; override;
      procedure ThreadDetach; override;
      property  TimeoutKeepThreadAlive: Boolean    read  FTimeoutKeepThreadAlive
                                                   write SetTimeoutKeepThreadAlive
                                                   default TRUE;
  //published
      property TimeoutSampling: Cardinal           read  FTimeoutSampling
                                                   write SetTimeoutSampling;
      property TimeoutConnect: Cardinal            read  FTimeoutConnect
                                                   write FTimeoutConnect;
      property TimeoutIdle: Cardinal    read FTimeoutIdle write FTimeoutIdle;
      property OnTimeout: TTimeoutEvent read FOnTimeout write FOnTimeout;
  end;
{$ENDIF}

{$IFDEF BUILTIN_THROTTLE}
  {$IFDEF BUILTIN_TIMEOUT}
  TCustomThrottledWSocket = class(TCustomTimeoutWSocket)
  {$ELSE}
  TCustomThrottledWSocket = class(TCustomSyncWSocket)
  {$ENDIF}
  private
      FBandwidthLimit           : Cardinal;  // Bytes per second, null = disabled
      FBandwidthSampling        : Cardinal;  // Msec sampling interval
      FBandwidthCount           : Cardinal;  // Byte counter
      FBandwidthMaxCount        : Cardinal;  // Bytes during sampling period
      FBandwidthTimer           : TIcsThreadTimer;
      FBandwidthPaused          : Boolean;
      FBandwidthEnabled         : Boolean;
      FBandwidthOldTimerEnabled : Boolean;
      FBandwidthKeepThreadAlive : Boolean;
      procedure BandwidthHandleTimer(Sender: TObject);
      procedure SetBandwidthControl;
      procedure SetBandwidthLimit(const Value: Cardinal);   { V7.55 }
      procedure SetBandwidthSampling(const Value: Cardinal);
      procedure SetBandwidthKeepThreadAlive(const Value: Boolean);
  protected
      procedure DupConnected; override;
      function  RealSend(var Data: TWSocketData; Len : Integer) : Integer; override;
      procedure TriggerSessionConnectedSpecial(Error: Word); override;
      procedure TriggerSessionClosed(Error: Word); override;
  public
      constructor Create(AOwner: TComponent); override;
      function  Receive(Buffer: TWSocketData; BufferSize: Integer) : Integer; override;
      procedure ThreadAttach; override;
      procedure ThreadDetach; override;
      property  TimeoutKeepThreadAlive: Boolean    read  FBandwidthKeepThreadAlive
                                                   write SetBandwidthKeepThreadAlive
                                                   default TRUE;
  //published
      property BandwidthLimit       : Cardinal     read  FBandwidthLimit
                                                   write SetBandwidthLimit;
      property BandwidthSampling    : Cardinal     read  FBandwidthSampling
                                                   write SetBandwidthSampling;
  end;
{$ENDIF}

{$IFDEF BUILTIN_THROTTLE}
  TWSocket = class(TCustomThrottledWSocket)
{$ELSE}
  {$IFDEF BUILTIN_TIMEOUT}
  TWSocket = class(TCustomTimeoutWSocket)
  {$ELSE}
  TWSocket = class(TCustomSyncWSocket)
  {$ENDIF}
{$ENDIF}
  public
    property PortNum;
    property Handle;
    property HSocket;
    property BufSize;
    property Text;
    property AllSent;
    property PeerAddr;
    property PeerPort;
    property State;
    property DnsResult;
    property DnsResultList;
    property ReadCount;
    property WriteCount;            { V8.11 }
    property RcvdCount;
    property SocketRcvBufSize;     {AG 03/10/07}
    property SocketSndBufSize;     {AG 03/10/07}
    property OnDebugDisplay;
    property Counter;
    property HttpTunnelCurrentAuthType;
    property HttpTunnelBufferSize;
    property HttpTunnelLastResponse;
    property HttpTunnelLmCompatLevel;  { V7.86 }
    property OnAddressListChanged;
    property OnRoutingInterfaceChanged;
    property SessIpInfo;         { V9.5 }
  published
    property Addr;
    property SocketFamily;
    property Port;
    property Proto;
    property LocalAddr;
    property LocalAddr6;
    property LocalPort;
    property MultiThreaded;
    property MultiCast;
    property MultiCastAddrStr;
    property MultiCastIpTTL;
    property FlushTimeout;
    property SendFlags;
    property LingerOnOff;
    property LingerTimeout;
    property KeepAliveOnOff;
    property KeepAliveTime;
    property KeepAliveInterval;
    property SocksLevel;
    property SocksServer;
    property SocksPort;
    property SocksUsercode;
    property SocksPassword;
    property SocksAuthentication;
    property LastError;
    property ReuseAddr;
    property ExclusiveAddr;
    property ComponentOptions;
    property ListenBacklog;
    property ReqVerLow;
    property ReqVerHigh;

    property HttpTunnelAuthType;
    property HttpTunnelPassword;
    property HttpTunnelPort;
    property HttpTunnelServer;
    property HttpTunnelUsercode;
    property ProxyURL;                       { V8.66 }
    property OnHttpTunnelError;
    property OnHttpTunnelConnected;

    property OnDataAvailable;
    property OnDataSent;
    property OnSendData;
    property OnSessionClosed;
    property OnSessionAvailable;
    property OnSessionConnected;
    property OnSocksConnected;
    property OnChangeState;
    { property OnLineTooLong; }
    property OnDnsLookupDone;
    property OnError;
    property OnBgException;
    property OnSocksError;
    property OnSocksAuthState;
    property SocketErrs;
    property onException;
{$IFNDEF NO_DEBUG_LOG}
    property IcsLogger;                       { V5.21 }
{$ENDIF}
  end;

  TSocksWSocket = class(TWSocket)
  end;

{ You must define USE_SSL so that SSL code is included in the component.    }
{ Either in OverbyteIcsDefs.inc or in the project/package options.          }
{$IFDEF USE_SSL}
  TSslWSocket = class(TWSocket)
  public
      property  SslVersion;
      property  SslCipher;
      property  SslTotalBits;
      property  SslSecretBits;
      property  X509Class;
      //property  SslEstablished;
      property  SslState;
      property  SslServerName;
      property  OnSslServerName;
  published
{$IFNDEF NO_DEBUG_LOG}
      property IcsLogger;                      { V5.21 }
{$ENDIF}
      property  SslContext;
      property  SslEnable;
      property  SslAcceptableHosts;
      property  SslMode;
      property  OnSslVerifyPeer;
      property  OnSslHandshakeDone;
      property  OnSslCliGetSession;
      property  OnSslCliNewSession;
      property  OnSslSvrNewSession;
      property  OnSslSvrGetSession;
      property  OnSslSetSessionIDContext;
      property  OnSslShutDownComplete;
      property  OnSslCliCertRequest;
  end;
{$ENDIF}

{ V9.3 moved from Winsock }
function  IsIPv6APIAvailable: Boolean;
function  IsIPv6Available: Boolean;

function  HasOption(OptSet : TWSocketOptions; Opt : TWSocketOption) : Boolean;
function  RemoveOption(OptSet : TWSocketOptions; Opt : TWSocketOption) : TWSocketOptions;
function  AddOptions(Opts: array of TWSocketOption): TWSocketOptions;
{$IFDEF MSWINDOWS}
function  WinsockInfo : TWSADATA;
{$ENDIF}

function  WSocketGetHostByAddr(Addr : AnsiString) : PHostEnt; overload;
function  WSocketGetHostByAddr(Addr : UnicodeString) : PHostEnt; overload;   { V8.70 }
function  WSocketGetHostByName(Name : AnsiString) : PHostEnt; overload;
function  WSocketGetHostByName(Name : UnicodeString) : PHostEnt; overload;   { V8.70 }
function  LocalHostName : AnsiString;
function  GetLocalHostName : String;                                         { V8.70 }
function  LocalIPList(const ASocketFamily: TSocketFamily = DefaultSocketFamily; const AProtocol: Integer = IPPROTO_TCP) : TStrings;
procedure GetLocalIPList(AIPList: TStrings; const ASocketFamily: TSocketFamily = DefaultSocketFamily;
                                                                                  const AProtocol: Integer = IPPROTO_TCP);
function  WSocketResolveIp(const IpAddr : AnsiString; const ASocketFamily: TSocketFamily = DefaultSocketFamily;
                                                              const AProtocol: Integer = IPPROTO_TCP) : AnsiString; overload;
function  WSocketResolveIp(const IpAddr : UnicodeString; const ASocketFamily: TSocketFamily = DefaultSocketFamily;
                                                           const AProtocol: Integer = IPPROTO_TCP) : UnicodeString; overload; { V8.70 }
function  WSocketResolveHost(InAddr : AnsiString) : TInAddr; overload;
function  WSocketResolveHost(InAddr : UnicodeString) : TInAddr; overload;    { V8.70 }
procedure WSocketResolveHost(const AHostName: string; var AAddr: TSockAddrIn6; const ASocketFamily: TSocketFamily;
                                                                                             const AProtocol: Integer); overload;
function  WSocketResolvePort(Port : AnsiString; Proto : AnsiString) : Word; overload;       { V8.70 }
function  WSocketResolvePort(Port : UnicodeString; Proto : UnicodeString) : Word; overload; { V8.70 }
function  WSocketResolveProto(sProto : AnsiString) : Integer; overload;      { V8.70 }
function  WSocketResolveProto(sProto : UnicodeString) : Integer; overload;   { V8.70 }

procedure WSocketForceLoadWinsock; {$IFDEF USE_INLINE} inline; {$ENDIF}
procedure WSocketCancelForceLoadWinsock; {$IFDEF USE_INLINE} inline; {$ENDIF}
procedure WSocketUnloadWinsock; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  WSocketIPAddrToSocAddr(const IPAddr: String): TSockAddrIn6;       { V8.71 }
function  WSocket_Synchronized_WSAAccept(s: TSocket; addr: PSockAddr; addrlen: PInteger; lpfnCondition: Pointer;
                                                    dwCallbackData: Pointer): TSocket; {$IFDEF USE_INLINE} inline; {$ENDIF} { V9.5 }
procedure WSocket_Synchronized_WSASetLastError(iError: Integer); {$IFDEF USE_INLINE} inline; {$ENDIF}  { V9.5 }

{$IFDEF MSWINDOWS}
function WSocket_WSAStartup(wVersionRequired: word; var WSData: TWSAData): Integer;
function WSocket_WSACleanup : Integer;
function WSocket_WSACancelAsyncRequest(hAsyncTaskHandle: THandle): Integer;
{$ENDIF MSWINDOWS}
procedure WSocket_WSASetLastError(iError: Integer);
function WSocket_WSAGetLastError: Integer;
{$IFDEF MSWINDOWS}
function WSocket_WSAAsyncGetHostByName(HWindow: HWND; wMsg: u_int; name, buf: PAnsiChar; buflen: Integer): THandle; overload;    { V8.70 }
function WSocket_WSAAsyncGetHostByName(HWindow: HWND; wMsg: u_int; name, buf: UnicodeString; buflen: Integer): THandle; overload;    { V8.70 }
function WSocket_WSAAsyncGetHostByAddr(HWindow: HWND; wMsg: u_int; addr: PAnsiChar; len, Struct: Integer; buf: PAnsiChar;
                                                                                             buflen: Integer): THandle; overload;    { V8.70 }
function WSocket_WSAAsyncGetHostByAddr(HWindow: HWND; wMsg: u_int; addr: UnicodeString; len, Struct: Integer;  buf: UnicodeString;
                                                                                              buflen: Integer): THandle; overload;    { V8.70 }
function WSocket_WSAAsyncSelect(s: TSocket; HWindow: HWND; wMsg: u_int; lEvent: Integer): Integer;
{$ENDIF MSWINDOWS}

{$IFDEF POSIX}
function WSocket_WSAAsyncSelect(IEventSrc: IIcsEventSource; s: TSocket; HWindow: HWND; wMsg: u_int; lEvent: Integer): Integer;
{ Must be called whenever the socket handle was closed }
procedure WSocketSynchronizedRemoveEvents(AEventSource: IIcsEventSource; FdClosed: Boolean = False);
procedure WSocketSynchronizedEnableReadEvent(AEventSource: IIcsEventSource);
procedure WSocketSynchronizedEnableAcceptEvent(AEventSource: IIcsEventSource);
{ Must be called before any call to shutdown() }
procedure WSocketSynchronizedSetShutdownCalled(IEventSrc: IIcsEventSource; How: Integer);
function WSocketGenerateObjectID: NativeInt;
{$ENDIF}

function WSocket_recv(s: TSocket; var Buf: TWSocketData; len, flags: Integer): Integer;
function WSocket_recvfrom(s: TSocket; var Buf: TWSocketData; len, flags: Integer; var from: TSockAddr; var fromlen: Integer): Integer;
function WSocket_getservbyname(name, proto: PAnsiChar): PServEnt; overload;         { V8.70 }
function WSocket_getservbyname(name, proto: UnicodeString): PServEnt; overload;     { V8.70 }
function WSocket_getprotobyname(name: PAnsiChar): PProtoEnt; overload;              { V8.70 }
function WSocket_getprotobyname(name: UnicodeString): PProtoEnt; overload;          { V8.70 }
function WSocket_gethostbyname(name: PAnsiChar): PHostEnt; overload;                { V8.70 }
function WSocket_gethostbyname(name: UnicodeString): PHostEnt; overload;            { V8.70 }
function WSocket_gethostbyaddr(addr: Pointer; len, Struct: Integer): PHostEnt;
function WSocket_gethostname(out name: AnsiString): Integer; overload;              { V8.70 }
function WSocket_gethostname(out name: UnicodeString): Integer; overload;           { V8.70 }
function WSocket_socket(af, Struct, protocol: Integer): TSocket;
function WSocket_shutdown(s: TSocket; how: Integer): Integer;
function WSocket_setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; optlen: Integer): Integer; overload;
function WSocket_setsockopt(s: TSocket; level, optname: Integer; optval: UnicodeString; optlen: Integer): Integer; overload;                    { V8.70 }
function WSocket_setsockopt(s: TSocket; level, optname: Integer; var optval: TLinger; optlen: Integer): Integer; overload;
function WSocket_getsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; var optlen: Integer): Integer; overload;                { V8.70 }
function WSocket_getsockopt(s: TSocket; level, optname: Integer; optval: UnicodeString; var optlen: Integer): Integer; overload;                { V8.70 }
function WSocket_sendto(s: TSocket; var Buf : TWSocketData; len, flags: Integer; var addrto: TSockAddr; tolen: Integer): Integer;
function WSocket_send(s: TSocket; var Buf : TWSocketData; len, flags: Integer): Integer;
function WSocket_ntohs(netshort: u_short): u_short;
function WSocket_ntohl(netlong: u_long): u_long;
function WSocket_listen(s: TSocket; backlog: Integer): Integer;
function WSocket_ioctlsocket(s: TSocket; cmd: DWORD; var arg: u_long): Integer;
{$IFDEF MSWINDOWS}
function WSocket_WSAIoctl(s: TSocket; IoControlCode: DWORD; InBuffer: Pointer; InBufferSize: DWORD; OutBuffer: Pointer;
                  OutBufferSize: DWORD; var BytesReturned: DWORD; Overlapped: POverlapped; CompletionRoutine: FARPROC): Integer;
{$ENDIF}
function WSocket_inet_ntoa(inaddr: TInAddr): AnsiString;
function WSocket_inet_ntoaW(inaddr: TInAddr): UnicodeString;                     { V8.70 }
function WSocket_inet_addr(const cp: AnsiString): u_long; overload;              { V8.70 }
function WSocket_inet_addr(const cp: UnicodeString): u_long; overload;           { V8.70 }
function WSocket_htons(hostshort: u_short): u_short;
function WSocket_htonl(hostlong: u_long): u_long;
function WSocket_getsockname(s: TSocket; var name: TSockAddr; var namelen: Integer): Integer;
function WSocket_getpeername(s: TSocket; var name: TSockAddr; var namelen: Integer): Integer;
function WSocket_connect(s: TSocket; var name: TSockAddr; namelen: Integer): Integer;
function WSocket_closesocket(s: TSocket): Integer;
function WSocket_bind(s: TSocket; var addr: TSockAddr; namelen: Integer): Integer;
function WSocket_accept(s: TSocket; addr: PSockAddr; addrlen: PInteger): TSocket;

{ * Winsock2 *}
{$IFDEF MSWINDOWS}
function  WSocket_GetAddrInfo(NodeName: PChar;ServName: PChar; Hints: PAddrInfo; var Addrinfo: PAddrInfo): Integer;
function  WSocket_GetNameInfo(addr: PSockAddr; namelen: Integer; host: PChar;  hostlen: Cardinal; serv: PChar;
                                                                                    servlen: Cardinal; flags: Integer): Integer;
{$ENDIF}
{$IFDEF POSIX}
function  WSocket_GetAddrInfo(NodeName: PAnsiChar; ServName: PAnsiChar; Hints: PAddrInfo; var Addrinfo: PAddrInfo): Integer;
function  WSocket_GetNameInfo(addr: PSockAddr; namelen: Integer; host: PAnsiChar; hostlen: Cardinal; serv: PAnsiChar;
                                                                                    servlen: Cardinal; flags: Integer): Integer;
{$ENDIF}
procedure WSocket_FreeAddrInfo(ai: PAddrInfo);

function WSocket_ResolveName(const AName: string; const AReverse: Boolean; const AFamily: TSocketFamily;
                                                                  AResultList: TStrings; const AProtocol: Integer): Integer;

{$IFNDEF NO_ADV_MT}
function SafeWSocketGCount : Integer;
{$ENDIF}

{$IFNDEF NO_DEBUG_LOG}
var
    __DataSocket : TCustomWSocket;
{$ENDIF}
{$IFNDEF COMPILER12_UP}
var
    CPUCount     : Integer;
{$ENDIF}

{$IFDEF POSIX}
type
  { It's a singleton running one thread with a kernel event queue per  }
  { application that receives socket events and post them to the right }
  { recipients.                                                        }
  TIcsEventQueue = class;

  TIcsAsyncSocketThread = class(TThread)
  private
    FEventQueue: TIcsEventQueue;
    procedure TerminateThread;
  protected
    procedure Execute; override;
  end;

  {$IF DEFINED(MACOS) or DEFINED(IOS)}         { V9.2 }
  TIcsKEventList = array of TKEvent;
  EIcsEventQueue = class(Exception);
  TIcsEventQueue = class
  strict private
    FChangeList         : TIcsKEventList;
    FThreadChangeList   : TIcsKEventList;
    FEventList          : TIcsKEventList;
  {$ELSEIF DEFINED(LINUX) or DEFINED(ANDROID)}   { V9.2 }
  TIcsEpollList  = array of TEPoll_Event;
  EIcsEventQueue = class(Exception);
  TIcsEventQueue = class
  strict private
    FChangeList         : TIcsEpollList;
    FThreadChangeList   : TIcsEpollList;
    FEventList          : TIcsEpollList;
 {$IFEND}
    FThrdChangeListLen  : Integer;
    FEventListLen       : Integer;
    FPipeFd             : TPipeFd;
    FQueue              : Integer;
    FFreeIndex          : Integer;
    FCapacity           : Integer;
    FAsyncThread        : TIcsAsyncSocketThread;
    FQueueSection       : TIcsCriticalSection;
    FObjIdentList       : TDictionary<NativeInt, TObject>;
    FInLoop             : Boolean;
    FRequireWakeup      : Boolean;
    FInitialized        : Boolean;
 //strict private class threadvar
    //FCurrentEventQueue: TIcsEventQueue;
    procedure Grow;
    procedure AddReadEvent(FD: Integer; UData: NativeInt; Edge: Boolean);
    procedure AddWriteEvent(FD: Integer; UData: NativeInt);
    procedure RemoveReadEvent(FD: Integer; UData: NativeInt);
    procedure RemoveWriteEvent(FD: Integer; UData: NativeInt);
    procedure DisableReadEvent(FD: Integer; UData: NativeInt);
    function EnableReadEvent(FD: Integer; UData: NativeInt): Boolean;
    function Init: Boolean;
    function DeInit: Boolean;
    function CheckChangeEvent(FD: Integer; UData: NativeInt;
      const OldMask: Cardinal; var NewMask: Cardinal): Boolean;
    function Notify(AMsg: Byte): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure InternalRemoveEvents(IEventSrc: IIcsEventSource; FdClosed: Boolean = False);
    function InternalAsyncSelect(IEventSrc: IIcsEventSource; AWndHandle: HWND;
      AMsgID: UINT; AEvents: Cardinal; AWakeupThread: Boolean): Integer;
    procedure RemoveFromObjIdentList(IEventSrc: IIcsEventSource);
    procedure AddToObjIdentList(IEventSrc: IIcsEventSource);
  {$IFDEF NEVER}
    function  KQueueAddReadEvent(FD: Integer; UData: NativeInt; Edge: Boolean): Boolean;
    function  KQueueAddWriteEvent(FD: Integer; UData: NativeInt): Boolean;
    function  KQueueRemoveReadEvent(FD: Integer): Boolean;
    function  KQueueRemoveWriteEvent(FD: Integer): Boolean;
    function  KQueueDisableReadEvent(FD: Integer; UData: NativeInt): Boolean;
    function  KQueueEnableReadEvent(FD: Integer; UData: NativeInt; Edge: Boolean): Boolean;
  {$ENDIF}
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //class function NewInstance: TObject; override;
    //procedure FreeInstance; override;
    //procedure AfterConstruction; override;
    //procedure BeforeDestruction; override;
    function SynchronizedAsyncSelect(IEventSrc: IIcsEventSource;
      FD: Integer; AWndHandle: HWND; AMsgID: UINT; AEvents: Cardinal): Integer;
    function HandleEvents: Boolean;
    function SynchronizedEnableReadEvent(IEventSrc: IIcsEventSource): Boolean;
    function SynchronizedEnableAcceptEvent(IEventSrc: IIcsEventSource): Boolean;
    function SynchronizedRemoveEvents(IEventSrc: IIcsEventSource; FdClosed: Boolean): Boolean;
    procedure SynchronizedSetShutdownCalled(IEventSrc: IIcsEventSource; How: Integer);
    function Wakeup: Boolean;
  end;

var
  GAsyncSocketQueue : TIcsEventQueue = nil;
{$ENDIF POSIX}

implementation

{ R 'OverbyteIcsWSocket.TWSocket.bmp'}
{.I Include\Ics.InterlockedApi.inc}    // V8.65 not using interlocks any longer

var
{$IFNDEF NO_ADV_MT}
    CritSecIpList : TIcsCriticalSection;
{$ENDIF}
    IPList        : TStrings;

{$IFDEF XPOSIX}   // made public for easier debugging
type
  { It's a singleton running one thread with a kernel event queue per  }
  { application that receives socket events and post them to the right }
  { recipients.                                                        }
  TIcsEventQueue = class;

  TIcsAsyncSocketThread = class(TThread)
  private
    FEventQueue: TIcsEventQueue;
    procedure TerminateThread;
  protected
    procedure Execute; override;
  end;

  {$IF DEFINED(MACOS) or DEFINED(IOS)}         { V9.2 }
  TIcsKEventList = array of TKEvent;
  EIcsEventQueue = class(Exception);
  TIcsEventQueue = class
  strict private
    FChangeList         : TIcsKEventList;
    FThreadChangeList   : TIcsKEventList;
    FEventList          : TIcsKEventList;
  {$ELSEIF DEFINED(LINUX) or DEFINED(ANDROID)}   { V9.2 }
  TIcsEpollList  = array of TEPoll_Event;
  EIcsEventQueue = class(Exception);
  TIcsEventQueue = class
  strict private
    FChangeList         : TIcsEpollList;
    FThreadChangeList   : TIcsEpollList;
    FEventList          : TIcsEpollList;
 {$IFEND}
    FThrdChangeListLen  : Integer;
    FEventListLen       : Integer;
    FPipeFd             : TPipeFd;
    FQueue              : Integer;
    FFreeIndex          : Integer;
    FCapacity           : Integer;
    FAsyncThread        : TIcsAsyncSocketThread;
    FQueueSection       : TIcsCriticalSection;
    FObjIdentList       : TDictionary<NativeInt, TObject>;
    FInLoop             : Boolean;
    FRequireWakeup      : Boolean;
    FInitialized        : Boolean;
 //strict private class threadvar
    //FCurrentEventQueue: TIcsEventQueue;
    procedure Grow;
    procedure AddReadEvent(FD: Integer; UData: NativeInt; Edge: Boolean);
    procedure AddWriteEvent(FD: Integer; UData: NativeInt);
    procedure RemoveReadEvent(FD: Integer; UData: NativeInt);
    procedure RemoveWriteEvent(FD: Integer; UData: NativeInt);
    procedure DisableReadEvent(FD: Integer; UData: NativeInt);
    function EnableReadEvent(FD: Integer; UData: NativeInt): Boolean;
    function Init: Boolean;
    function DeInit: Boolean;
    function CheckChangeEvent(FD: Integer; UData: NativeInt;
      const OldMask: Cardinal; var NewMask: Cardinal): Boolean;
    function Notify(AMsg: Byte): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure InternalRemoveEvents(IEventSrc: IIcsEventSource; FdClosed: Boolean = False);
    function InternalAsyncSelect(IEventSrc: IIcsEventSource; AWndHandle: HWND;
      AMsgID: UINT; AEvents: Cardinal; AWakeupThread: Boolean): Integer;
    procedure RemoveFromObjIdentList(IEventSrc: IIcsEventSource);
    procedure AddToObjIdentList(IEventSrc: IIcsEventSource);
  {$IFDEF NEVER}
    function  KQueueAddReadEvent(FD: Integer; UData: NativeInt; Edge: Boolean): Boolean;
    function  KQueueAddWriteEvent(FD: Integer; UData: NativeInt): Boolean;
    function  KQueueRemoveReadEvent(FD: Integer): Boolean;
    function  KQueueRemoveWriteEvent(FD: Integer): Boolean;
    function  KQueueDisableReadEvent(FD: Integer; UData: NativeInt): Boolean;
    function  KQueueEnableReadEvent(FD: Integer; UData: NativeInt; Edge: Boolean): Boolean;
  {$ENDIF}
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //class function NewInstance: TObject; override;
    //procedure FreeInstance; override;
    //procedure AfterConstruction; override;
    //procedure BeforeDestruction; override;
    function SynchronizedAsyncSelect(IEventSrc: IIcsEventSource;
      FD: Integer; AWndHandle: HWND; AMsgID: UINT; AEvents: Cardinal): Integer;
    function HandleEvents: Boolean;
    function SynchronizedEnableReadEvent(IEventSrc: IIcsEventSource): Boolean;
    function SynchronizedEnableAcceptEvent(IEventSrc: IIcsEventSource): Boolean;
    function SynchronizedRemoveEvents(IEventSrc: IIcsEventSource; FdClosed: Boolean): Boolean;
    procedure SynchronizedSetShutdownCalled(IEventSrc: IIcsEventSource; How: Integer);
    function Wakeup: Boolean;
  end;

var
  GAsyncSocketQueue : TIcsEventQueue = nil;
{$ENDIF POSIX}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF POSIX}
{ From OverbyteIcsWinsock.pas }
function IN6_ADDR_EQUAL(const a: PIn6Addr; const b: PIn6Addr): Boolean;
begin
    Result := CompareMem(a, b, SizeOf(TIn6Addr));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IN6ADDR_ISANY(sa: PSockAddrIn6): Boolean;
begin
    if sa <> nil then begin
        with sa^ do begin
            Result := (sin6_family = AF_INET6) and (PCardinal(@sin6_addr.s6_addr[0])^ = 0) and
                      (PCardinal(@sin6_addr.s6_addr[4])^ = 0) and (PCardinal(@sin6_addr.s6_addr[8])^ = 0) and
                      (PCardinal(@sin6_addr.s6_addr[12])^ = 0);
        end;
    end
    else
      Result := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF POSIX}

type
  TIcsAsyncDnsLookupRequestState = (lrsNone, lrsInWork, lrsAlready);
  TIcsAsyncDnsLookupRequest = class(TObject)
  private
    FWndHandle    : HWND;
    FMsgID        : UINT;
    FSocketFamily : TSocketFamily;
    FProtocol     : Integer;
    FState        : TIcsAsyncDnsLookupRequestState;
    FReverse      : Boolean;
    FCanceled     : Boolean;
    FLookupName   : string;
    FResultList   : TStrings;
  public
    property ResultList: TStrings read FResultList;
  end;

  TIcsAsyncDnsLookupThread = class;
  { TIcsAsyncDnsLookup provides async name resolution with new API, IPv6 and IPv4 }
  TIcsAsyncDnsLookup = class(TObject)
  private
    FThreads      : TList;
    FQueue        : TList;
    FMaxThreads   : Integer;
    FMinThreads   : Integer;
    FQueueLock    : TIcsCriticalSection;
    FThreadsLock  : TIcsCriticalSection;
    FDestroying   : Boolean;
    FThreadIdleTimeoutMsec : Cardinal;
    procedure LockQueue;
    procedure UnlockQueue;
    procedure LockThreadList;
    procedure UnlockThreadList;
    function ExecAsync(AWnd: HWND; AMsgID: UINT; ASocketFamily: TSocketFamily;
      const AName: string; AReverse: Boolean; AProtocol: Integer): THandle;
    function GetNextRequest(AThread: TIcsAsyncDnsLookupThread): TIcsAsyncDnsLookupRequest;
    function RemoveRequest(AReq: TIcsAsyncDnsLookupRequest): Boolean;
    function CancelAsyncRequest(AReq: THandle): Integer;
  public
    constructor Create(const AMaxThreads: Integer; const AMinThreads: Integer = 0;
      const AThreadIdleTimeoutSec: Cardinal = 60);
    destructor Destroy; override;
    procedure SetMinMaxThreads(AMinThreads, AMaxThreads: Byte);
  end;

  TIcsAsyncDnsLookupThread = class(TThread)
  private
    FEvent         : TEvent;
    FBusy          : Boolean;
    FDnsLookup     : TIcsAsyncDnsLookup;
    FDnsResultList : TStringList;
  protected
    procedure Execute; override;
  public
    constructor Create(ADnsLookup: TIcsAsyncDnsLookup); reintroduce;
    destructor Destroy; override;
  end;

  TThreadStoreTree = class (TIcsAvlPointerTree)
  protected
    { If Data1 < Data2 return False otherwise True }
    function  CompareData(Data1, Data2: Pointer): Boolean; override;
    { Return True if Data1 equals Data2 otherwise False }
    function  SameData(Data1, Data2: Pointer): Boolean; override;
    procedure Notification(Data: Pointer; Action: TIcsAvlTreeNotification); override;
  end;

  TThreadStoreItem = record
    ThreadID  : THandle;
    RefCnt    : Integer;
    Data      : Pointer;
  end;
  PThreadStoreItem = ^TThreadStoreItem;

  TThreadLocalStore = class // Not really thread local
  private
    FTree : TThreadStoreTree;
    FTemp : TThreadStoreItem;
    FLock : TIcsCriticalSection;
  public
    constructor Create;
    destructor  Destroy; override;
    function  RegisterStore(ThreadID: THandle): PPointer;
    function  UnregisterStore(ThreadID: THandle): Pointer;
    procedure Lock;
    procedure Unlock;
  end;

var
  GThreadLocalStore : TThreadLocalStore = nil;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF POSIX}
procedure WSocketSynchronizedRemoveEvents(AEventSource: IIcsEventSource;
  FdClosed: Boolean = False);
begin
    GAsyncSocketQueue.SynchronizedRemoveEvents(AEventSource, FdClosed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocketSynchronizedEnableReadEvent(AEventSource: IIcsEventSource);
begin
    GAsyncSocketQueue.SynchronizedEnableReadEvent(AEventSource);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocketSynchronizedEnableAcceptEvent(AEventSource: IIcsEventSource);
begin
    GAsyncSocketQueue.SynchronizedEnableAcceptEvent(AEventSource);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocketSynchronizedSetShutdownCalled(IEventSrc: IIcsEventSource;
  How: Integer);
begin
  GAsyncSocketQueue.SynchronizedSetShutdownCalled(IEventSrc, How);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsDigit(Ch : AnsiChar) : Boolean;
begin
    Result := (ch >= '0') and (ch <= '9');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF COMPILER12_UP}
function GetCpuCount: Integer;
var
    SysInfo : TSystemInfo;
begin
    GetSystemInfo(SysInfo);
    Result := SysInfo.dwNumberOfProcessors;
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.3 moved from  Winsock }

function IsIPv6APIAvailable: Boolean;
begin
    Result := True;   { V9.3 assume OK }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Check whether IPv6 is available on the system, requires to load socket API }
{ once, subsequent calls return a cached value.                               }
function IsIPv6Available: Boolean;
var
    s : TSocket;
begin
{$IFDEF POSIX}
    Result := True;      { V9.3 don't need to test }
{$ELSE}
    if GIPv6Available > -1 then
        Result := (GIPv6Available = 1)
    else begin
        EnterCriticalSection(GWSockCritSect);
        try
            s := Ics_socket(AF_INET6, SOCK_DGRAM, IPPROTO_UDP);
            Result := s <> INVALID_SOCKET;
            if Result then begin
                Ics_closesocket(s);
                GIPv6Available := 1;
            end
            else
                GIPv6Available := 0;
            { If no socket created, then unload winsock immediately }
            if WSocketGCount <= 0 then
                UnloadWinsock;
        finally
            LeaveCriticalSection(GWSockCritSect);
        end;
    end;
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_Synchronized_WSAStartup(
    wVersionRequired: word;
    var WSData: TWSAData): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := Ics_WSAStartup(wVersionRequired, WSData)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSACleanup : Integer;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := Ics_WSACleanup;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocket_Synchronized_WSASetLastError(iError: Integer);
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Ics_WSASetLastError(iError);
  {$ENDIF}
  {$IFDEF POSIX}
    SetLastError(IError);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSAGetLastError: Integer;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_WSAGetLastError;
  {$ELSE}
    Result := GetLastError;
    {$IFDEF MACOS} // Likely more mappings are required, add them here for now
      case Result of
          EPIPE : Result := WSAECONNRESET; // ?
      end;
    {$ENDIF}
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_Synchronized_WSACancelAsyncRequest(hAsyncTaskHandle: THandle): Integer;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := Ics_WSACancelAsyncRequest(hAsyncTaskHandle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSAAsyncGetHostByName(
    HWindow: HWND; wMsg: u_int;
    name, buf: PAnsiChar;
    buflen: Integer): THandle; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := Ics_WSAAsyncGetHostByName(HWindow, wMsg, name, buf, buflen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSAAsyncGetHostByAddr(
    HWindow: HWND;
    wMsg: u_int; addr: PAnsiChar;
    len, Struct: Integer;
    buf: PAnsiChar;
    buflen: Integer): THandle; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := Ics_WSAAsyncGetHostByAddr(HWindow, wMsg, addr,
                                                      len, struct, buf, buflen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSAAsyncSelect(
    s: TSocket;
    HWindow: HWND;
    wMsg: u_int;
    lEvent: Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := Ics_WSAAsyncSelect(s, HWindow, wMsg, lEvent);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_getservbyname(name, proto: PAnsiChar): PServEnt;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_getservbyname(name, proto);
  {$ELSE}
    Result := getservbyname(name, proto);
  {$ENDIF}
    (*
    if @Fgetservbyname = nil then
        @Fgetservbyname := WSocketGetProc('getservbyname');
    Result := Fgetservbyname(name, proto);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_getprotobyname(const Name: AnsiString): PProtoEnt;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_getprotobyname(PAnsiChar(Name));
  {$ELSE}
    Result := getprotobyname(PAnsiChar(Name));
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF POSIX}
function internal_gethostbyname(name: PAnsiChar): PHostEnt; cdecl;
  external libc name _PU + 'gethostbyname';
{$ENDIF}

function WSocket_Synchronized_gethostbyname(name: PAnsiChar): PHostEnt;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_gethostbyname(name);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := internal_gethostbyname(name);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF POSIX}
function internal_gethostbyaddr(addr: Pointer; Len: Integer; struct: Integer): PHostEnt; cdecl;
  external libc name _PU + 'gethostbyaddr';
{$ENDIF}

function WSocket_Synchronized_gethostbyaddr(addr: Pointer; len, Struct: Integer): PHostEnt;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_gethostbyaddr(addr, len, Struct);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := internal_gethostbyaddr(addr, len, Struct);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_gethostname(name: PAnsiChar; len: Integer): Integer;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_gethostname(name, len);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := gethostname(name, len);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_socket(af, Struct, protocol: Integer): TSocket;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_socket(af, Struct, protocol);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := socket(af, Struct, protocol);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_shutdown(s: TSocket; how: Integer): Integer;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_shutdown(s, how);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := shutdown(s, how);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_setsockopt(s: TSocket; level, optname: Integer;
  optval: PAnsiChar; optlen: Integer): Integer; overload;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_setsockopt(s, level, optname, optval, optlen);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := setsockopt(s, level, optname, optval^, optlen);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_setsockopt(s: TSocket; level, optname: Integer;
  var optval: TLinger; optlen: Integer): Integer; overload;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_setsockopt(s, level, optname, @optval, optlen);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := setsockopt(s, level, optname, optval, optlen);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_setsockopt(s: TSocket; level, optname: Integer;
  var optval: ip_mreq; optlen: Integer): Integer; overload;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_setsockopt(s, level, optname, @optval, optlen);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := setsockopt(s, level, optname, optval, optlen);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_setsockopt(s: TSocket; level, optname: Integer;
  var optval: Integer; optlen: Integer): Integer; overload;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_setsockopt(s, level, optname, @optval, optlen);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := setsockopt(s, level, optname, optval, optlen);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_setsockopt(s: TSocket; level, optname: Integer;
  var optval: TInAddr; optlen: Integer): Integer; overload;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_setsockopt(s, level, optname, @optval, optlen);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := setsockopt(s, level, optname, optval, optlen);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF POSIX}
  function internal_getsockopt(socket, level, option_name: Integer;
    option_value: PAnsiChar; var option_len: integer): Integer; cdecl;
    external libc name _PU + 'getsockopt';
{$ENDIF}
function WSocket_Synchronized_getsockopt(
    s: TSocket; level, optname: Integer;
    optval: PAnsiChar; var optlen: Integer): Integer;
    {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_getsockopt(s, level, optname, optval, optlen);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := internal_getsockopt(s, level, optname, optval, optlen);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_sendto(
    s          : TSocket;
    const Buf  : TWSocketData;
    len, flags : Integer;
    var addrto : TSockAddr;
    tolen      : Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_sendto(s, Buf^, len, flags, addrto, tolen);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := sendto(s, Buf^, len, flags, Posix.SysSocket.psockaddr(@addrto)^, tolen);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_send(s: TSocket; var Buf : TWSocketData;
  len, flags: Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_send(s, Buf^, len, flags);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := send(s, Buf^, len, flags);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ntohs(netshort: u_short): u_short;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_ntohs(netshort);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := ntohs(netshort);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ntohl(netlong: u_long): u_long;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_ntohl(netlong);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := ntohl(netlong);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_listen(s: TSocket; backlog: Integer): Integer;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_listen(s, backlog);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := listen(s, backlog);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ioctlsocket(s: TSocket; cmd: Cardinal; var arg: u_long): Integer;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_ioctlsocket(s, cmd, arg);
  {$ENDIF}
  {$IFDEF POSIX}
    Result :=  Posix.StrOpts.ioctl(s, cmd, @arg);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_Synchronized_WSAIoctl(
    s                 : TSocket; IoControlCode : DWORD;
    InBuffer          : Pointer; InBufferSize  : DWORD;
    OutBuffer         : Pointer; OutBufferSize : DWORD;
    var BytesReturned : DWORD; Overlapped      : POverlapped;
    CompletionRoutine : FARPROC): Integer;
    {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := Ics_WSAIoctl(s, IoControlCode, InBuffer,
                        InBufferSize, OutBuffer, OutBufferSize, BytesReturned,
                        Overlapped, CompletionRoutine);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_inet_ntoa(inaddr: TInAddr): PAnsiChar;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_inet_ntoa(inaddr);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := inet_ntoa(inaddr);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_inet_addr(const cp: AnsiString): u_long; overload;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_inet_addr(PAnsiChar(cp));
  {$ENDIF}
  {$IFDEF POSIX}
    Result := inet_addr(PAnsiChar(cp));
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_inet_addr(const cp: UnicodeString): u_long; overload;     { V8.70 }
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := WSocket_Synchronized_inet_addr(AnsiString(cp));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_htons(hostshort: u_short): u_short;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_htons(hostshort);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := htons(hostshort);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_htonl(hostlong: u_long): u_long;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_htonl(hostlong);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := htonl(hostlong);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_getsockname(
    s           : TSocket;
    var name    : TSockAddr;
    var namelen : Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_getsockname(s, name, namelen);
  {$ENDIF}
  {$IFDEF POSIX}
  {$IFNDEF ANDROID}
    Result := getsockname(s, Posix.SysSocket.psockaddr(@name)^, Cardinal(namelen));
  {$ENDIF ANDROID}
  {$ENDIF POSIXF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_getpeername(
    s           : TSocket;
    var name    : TSockAddr;
    var namelen : Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_getpeername(s, name, namelen);
  {$ENDIF}
  {$IFDEF POSIX}
  {$IFNDEF ANDROID}
    Result := getpeername(s, Posix.SysSocket.psockaddr(@name)^, Cardinal(namelen));
  {$ENDIF ANDROID}
  {$ENDIF POSIXF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_connect(
    s        : TSocket;
    var name : TSockAddr;
    namelen  : Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_connect(s, name, namelen);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := connect(s, Posix.SysSocket.psockaddr(@name)^, namelen);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_closesocket(s: TSocket): Integer;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_closesocket(s);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := Posix.UniStd.__close(s);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_bind(
    s: TSocket;
    var addr: TSockAddr;
    namelen: Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF POSIX}
    Result := bind(s, Posix.SysSocket.psockaddr(@addr)^, namelen);
  {$ELSE}
    Result := Ics_bind(s, addr, namelen);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF POSIX}
function internal_accept(socket: Integer; address: Psockaddr;
  address_len: PInteger): Integer; cdecl;
  external libc name _PU + 'accept';
{$ENDIF}

function WSocket_Synchronized_accept(
    s: TSocket;
    addr: PSockAddr;
    addrlen: PInteger): TSocket; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_accept(s, addr, addrlen);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := internal_accept(s, addr, addrlen);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSAAccept(s: TSocket; addr: PSockAddr; addrlen: PInteger; lpfnCondition: Pointer;
                                                    dwCallbackData: Pointer): TSocket; {$IFDEF USE_INLINE} inline; {$ENDIF} { V9.5 }
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_WSAAccept(s, addr, addrlen, lpfnCondition, dwCallbackData);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := internal_accept(s, addr, addrlen);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_recv(s: TSocket; var Buf: TWSocketData;
  len, flags: Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_recv(s, Buf^, len, flags);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := recv(s, Buf^, len, flags);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_recvfrom(
    s: TSocket;
    var Buf: TWSocketData; len, flags: Integer;
    var from: TSockAddr;
    var fromlen: Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_recvfrom(s, Buf^, len, flags, from, fromlen);
  {$ENDIF}
  {$IFDEF POSIX}
  {$IFNDEF ANDROID}
    Result := recvfrom(s, Buf^, len, flags, Posix.SysSocket.psockaddr(@from)^, Cardinal(fromlen));
  {$ENDIF ANDROID}
  {$ENDIF POSIXF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_Synchronized_GetAddrInfo(
    NodeName    : PChar;
    ServName    : PChar;
    Hints       : PAddrInfo;
    var Addrinfo: PAddrInfo): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := Ics_GetAddrInfo(NodeName, ServName, Hints, Addrinfo);
end;
{$ENDIF}
{$IFDEF POSIX}
function WSocket_Synchronized_GetAddrInfo(
    NodeName    : PAnsiChar;
    ServName    : PAnsiChar;
    Hints       : PAddrInfo;
    var Addrinfo: PAddrInfo): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := GetAddrInfo(NodeName, ServName, Hints^, Addrinfo);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocket_Synchronized_FreeAddrInfo(ai: PAddrInfo);
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF POSIX}
    FreeAddrInfo(ai^);
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    Ics_FreeAddrInfo(ai);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_Synchronized_GetNameInfo(
    addr    : PSockAddr;
    namelen : Integer;
    host    : PChar;
    hostlen : Cardinal;
    serv    : PChar;
    servlen : Cardinal;
    flags   : Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := Ics_GetNameInfo(addr, namelen, host, hostlen, serv,
                                             servlen, flags);
end;
{$ENDIF}
{$IFDEF POSIX}
function WSocket_Synchronized_GetNameInfo(
    addr    : PSockAddr;
    namelen : Integer;
    host    : PAnsiChar;
    hostlen : Cardinal;
    serv    : PAnsiChar;
    servlen : Cardinal;
    flags   : Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := GetNameInfo(Posix.SysSocket.psockaddr(addr)^, namelen, host,
                          hostlen, serv, servlen, flags);

end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ResolveName(const AName: string;
  const AReverse: Boolean; const AFamily: TSocketFamily;
  AResultList: TStrings; const AProtocol: Integer): Integer; forward;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Winsock is dynamically loaded and unloaded when needed. In some cases     }
{ you may find winsock being loaded and unloaded very often in your app     }
{ This happend for example when you dynamically create a TWSocket and       }
{ destroy a TWSocket when there is no "permanant" TWSocket (that is a       }
{ TWSocket dropped on a persitant form). It is the very inefficiant.        }
{ Calling WSocketForceLoadWinsock will increament the reference count so    }
{ that winsock will not be unloaded when the last TWSocket is destroyed.    }
procedure WSocketForceLoadWinsock;
begin
{$IFDEF MSWINDOWS}
    Z.ICS9.OverbyteIcsWinsock.ForceLoadWinsock;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Cancel the operation done with WSocketForceLoadWinsock.                   }
procedure WSocketCancelForceLoadWinsock;
begin
{$IFDEF MSWINDOWS}
    Z.ICS9.OverbyteIcsWinsock.CancelForceLoadWinsock;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocketUnloadWinsock;
begin
{$IFDEF MSWINDOWS}
    Z.ICS9.OverbyteIcsWinsock.UnloadWinsock;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WinsockInfo : TWSADATA;
begin
    Result := Z.ICS9.OverbyteIcsWinsock.WinsockAPIInfo;
end;

{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF NO_ADV_MT}
procedure SafeIncrementCount;
begin
  {$IFNDEF POSIX}
    EnterCriticalSection(GWSockCritSect);
    Inc(WSocketGCount);
    LeaveCriticalSection(GWSockCritSect);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure SafeDecrementCount;
begin
  {$IFDEF MSWINDOWS}
    EnterCriticalSection(GWSockCritSect);
    Dec(WSocketGCount);
    LeaveCriticalSection(GWSockCritSect);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SafeWSocketGCount : Integer;
begin
  {$IFDEF MSWINDOWS}
    EnterCriticalSection(GWSockCritSect);
    Result := WSocketGCount;
    LeaveCriticalSection(GWSockCritSect);
  {$ELSE}
    Result := 0;
  {$ENDIF}
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_WSAStartup(
    wVersionRequired : WORD;
    var WSData       : TWSAData): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSAStartup(wVersionRequired, WSData);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSACleanup : Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSACleanup;
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;
{$ENDIF MSWINDOWS}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocket_WSASetLastError(iError: Integer);
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        WSocket_Synchronized_WSASetLastError(iError);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSAGetLastError: Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSAGetLastError;
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_WSACancelAsyncRequest(hAsyncTaskHandle: THandle): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSACancelAsyncRequest(hAsyncTaskHandle);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_WSAAsyncGetHostByName(
    HWindow: HWND; wMsg: u_int;
    name, buf: PAnsiChar;
    buflen: Integer): THandle;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSAAsyncGetHostByName(
                      HWindow, wMsg, name, buf, buflen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSAAsyncGetHostByName(HWindow: HWND; wMsg: u_int;
                                      name, buf: UnicodeString;
                                      buflen: Integer): THandle;              { V8.70 }
begin
    Result := WSocket_WSAAsyncGetHostByName(HWindow, wMsg,
                 PAnsiChar(AnsiString(name)), PAnsiChar(AnsiString(buf)), buflen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSAAsyncGetHostByAddr(
    HWindow: HWND;
    wMsg: u_int; addr: PAnsiChar;
    len, Struct: Integer;
    buf: PAnsiChar;
    buflen: Integer): THandle;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSAAsyncGetHostByAddr(
                       HWindow, wMsg, addr, len, struct, buf, buflen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSAAsyncGetHostByAddr(HWindow: HWND;
                                      wMsg: u_int; addr: UnicodeString;
                                      len, Struct: Integer;
                                      buf: UnicodeString;
                                      buflen: Integer): THandle;              { V8.70 }
begin
    Result := WSocket_WSAAsyncGetHostByAddr(HWindow, wMsg,
        PAnsiChar(AnsiString(addr)), len, Struct, PAnsiChar(AnsiString(buf)), buflen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSAAsyncSelect(
    s: TSocket;
    HWindow: HWND;
    wMsg: u_int;
    lEvent: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSAAsyncSelect(
                      s, HWindow, wMsg, lEvent);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;
{$ENDIF MSWINDOWS}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF POSIX}
function WSocket_WSAAsyncSelect(
    IEventSrc: IIcsEventSource;
    s: TSocket;
    HWindow: HWND;
    wMsg: u_int;
    lEvent: Integer): Integer;
begin
    Result := GAsyncSocketQueue.SynchronizedAsyncSelect(IEventSrc, s, HWindow, wMsg, LEvent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSAAsyncSelect(
    IEventSrc: IIcsEventSource;
    s: TSocket;
    HWindow: HWND;
    wMsg: u_int;
    lEvent: Integer): Integer;
begin
    Result := GAsyncSocketQueue.SynchronizedAsyncSelect(IEventSrc, s, HWindow, wMsg, LEvent);
end;
{$ENDIF POSIX}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getservbyname(name, proto: PAnsiChar): PServEnt;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_getservbyname(name, proto);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getservbyname(name, proto: UnicodeString): PServEnt;     { V8.70 }
begin
    Result := WSocket_getservbyname(PAnsiChar(AnsiString(name)),
                                               PAnsiChar(AnsiString(proto)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getprotobyname(name: PAnsiChar): PProtoEnt;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_getprotobyname(name);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getprotobyname(name: UnicodeString): PProtoEnt;           { V8.70 }
begin
    Result := WSocket_getprotobyname(PAnsiChar(AnsiString(name)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_gethostbyname(name: PAnsiChar): PHostEnt;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_gethostbyname(name);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_gethostbyname(name: UnicodeString): PHostEnt;             { V8.70 }
begin
    Result := WSocket_gethostbyname(PAnsiChar(AnsiString(name)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_gethostbyaddr(addr: Pointer; len, Struct: Integer): PHostEnt;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_gethostbyaddr(addr, len, Struct);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_gethostname(out name: AnsiString): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        SetLength(Name, 256);
        Result := WSocket_Synchronized_gethostname(PAnsiChar(name), 256);
        if Result >= 0 then
            // Unicode will convert on the fly
            SetLength(Name, StrLen(PAnsiChar(Name))) // Unicode change
        else
            SetLength(Name, 0);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_gethostname(out name: UnicodeString): Integer;            { V8.70 }
var
    aname: AnsiString;
begin
    Result := WSocket_gethostname(aname);
    name := UnicodeString(aname);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_socket(af, Struct, protocol: Integer): TSocket;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_socket(af, Struct, protocol);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_shutdown(s: TSocket; how: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_Shutdown(s, how);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar;
                            optlen: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_setsockopt(s, level, optname, optval, optlen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_setsockopt(s: TSocket; level, optname: Integer; optval: UnicodeString;
                                                       optlen: Integer): Integer;          { V8.70 }
begin
    Result := WSocket_setsockopt(s, level, optname, PAnsiChar(AnsiString(optval)), optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_setsockopt(s: TSocket; level, optname: Integer; var optval: TLinger;
                            optlen: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_setsockopt(s, level, optname, optval, optlen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getsockopt(s: TSocket; level, optname: Integer;
                                optval: PAnsiChar; var optlen: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_getsockopt(s, level, optname, optval, optlen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getsockopt(s: TSocket; level, optname: Integer; optval: UnicodeString;
                                                 var optlen: Integer): Integer;               { V8.70 }
begin
    Result := WSocket_getsockopt(s, level, optname, PAnsiChar(AnsiString(optval)), optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_sendto(
    s          : TSocket;
    var Buf    : TWSocketData;
    len, flags : Integer;
    var addrto : TSockAddr;
    tolen      : Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_sendto(s, Buf, len, flags, addrto, tolen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_send(s: TSocket; var Buf : TWSocketData; len, flags: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_send(s, Buf, len, flags);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_ntohs(netshort: u_short): u_short;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_ntohs(netshort);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_ntohl(netlong: u_long): u_long;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_ntohl(netlong);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_listen(s: TSocket; backlog: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_listen(s, backlog);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_ioctlsocket(s: TSocket; cmd: DWORD; var arg: u_long): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_ioctlsocket(s, cmd, arg);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_WSAIoctl(
    s                 : TSocket; IoControlCode : DWORD;
    InBuffer          : Pointer; InBufferSize  : DWORD;
    OutBuffer         : Pointer; OutBufferSize : DWORD;
    var BytesReturned : DWORD; Overlapped      : POverlapped;
    CompletionRoutine : FARPROC): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSAIoctl(
                      s, IoControlCode, InBuffer, InBufferSize, OutBuffer,
                      OutBufferSize, BytesReturned, Overlapped, CompletionRoutine);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_inet_ntoa(inaddr: TInAddr): AnsiString;
var
    Temp : PAnsiChar;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Temp := WSocket_Synchronized_inet_ntoa(inaddr);
        if Temp = nil then
            Result := ''
        else
            Result := Temp;
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_inet_ntoaW(inaddr: TInAddr): UnicodeString;                     { V8.70 }
begin
    Result := UnicodeString(WSocket_inet_ntoa(inaddr));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_inet_addr(const cp: AnsiString): u_long;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_inet_addr(PAnsiChar(cp));
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_inet_addr(const cp: UnicodeString): u_long;            { V8.70 }
begin
    Result := WSocket_inet_addr(PAnsiChar(AnsiString(cp)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_htons(hostshort: u_short): u_short;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_htons(hostshort);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_htonl(hostlong: u_long): u_long;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_htonl(hostlong);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getsockname(
    s           : TSocket;
    var name    : TSockAddr;
    var namelen : Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_getsockname(s, name, namelen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getpeername(
    s           : TSocket;
    var name    : TSockAddr;
    var namelen : Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_getpeername(s, name, namelen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_connect(
    s        : TSocket;
    var name : TSockAddr;
    namelen  : Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_connect(s, name, namelen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_closesocket(s: TSocket): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_closesocket(s);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_bind(
    s: TSocket;
    var addr: TSockAddr;
    namelen: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_bind(s, addr, namelen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_accept(
    s: TSocket;
    addr: PSockAddr;
    addrlen: PInteger): TSocket;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_accept(s, addr, addrlen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_recv(s: TSocket; var Buf : TWSocketData; len, flags: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_recv(s, Buf, len, flags);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_recvfrom(
    s: TSocket;
    var Buf: TWSocketData; len, flags: Integer;
    var from: TSockAddr;
    var fromlen: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_recvfrom(s, Buf, len, flags, from, fromlen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_GetAddrInfo(
    NodeName    : PChar;
    ServName    : PChar;
    Hints       : PAddrInfo;
    var Addrinfo: PAddrInfo): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_GetAddrInfo(NodeName, ServName, Hints, Addrinfo);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;
{$ENDIF}

{$IFDEF POSIX}
function WSocket_GetAddrInfo(
    NodeName    : PAnsiChar;
    ServName    : PAnsiChar;
    Hints       : PAddrInfo;
    var Addrinfo: PAddrInfo): Integer;
begin
  Result := WSocket_Synchronized_GetAddrInfo(NodeName, ServName, Hints, Addrinfo);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocket_FreeAddrInfo(ai: PAddrInfo);
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        WSocket_Synchronized_FreeAddrInfo(ai);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_GetNameInfo(
    addr    : PSockAddr;
    namelen : Integer;
    host    : PChar;
    hostlen : Cardinal;
    serv    : PChar;
    servlen : Cardinal;
    flags   : Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_GetNameInfo(addr, namelen, host, hostlen, serv, servlen, flags);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;
{$ENDIF}

{$IFDEF POSIX}
function WSocket_GetNameInfo(
    addr    : PSockAddr;
    namelen : Integer;
    host    : PAnsiChar;
    hostlen : Cardinal;
    serv    : PAnsiChar;
    servlen : Cardinal;
    flags   : Integer): Integer;
begin
  Result := WSocket_Synchronized_GetNameInfo(addr, namelen, host, hostlen, serv, servlen, flags);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.36 extended exception handler }
constructor ESocketException.Create(
                       const AMessage       : String;
                       AErrorCode           : Integer = 0;
                       const AErrorMessage  : String = '';
                       const AFriendlyMsg   : String = '';
                       const AFunc          : String = '';
                       const AIP            : String = '';
                       const APort          : String = '';
                       const AProto         : String = '');
begin
    FErrorCode    := AErrorCode;
    FErrorMessage := AErrorMessage;
    FIPStr        := AIP;
    FPortStr      := APort;
    FProtoStr     := AProto;
    FFriendlyMsg  := AFriendlyMsg;
    FFunc         := AFunc;
    if (FErrorCode > 0) and (FErrorMessage = '') then
                    FErrorMessage := WSocketErrorDesc(FErrorCode);   { V8.42 }
    if FFriendlyMsg = '' then FFriendlyMsg := AMessage;
    inherited Create(AMessage);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.36 extended exception information, set FSocketErrs = wsErrFriendly for
  more friendly messages (without error numbers) }
procedure TCustomWSocket.RaiseException(const Msg : String;
                       AErrorCode           : Integer;
                       const AErrorMessage  : String = '';
                       const AFriendlyMsg   : String = '';
                       const AFunc          : String = '';
                       const AIP            : String = '';
                       const APort          : String = '';
                       const AProto         : String = '');
var
    MyException: ESocketException;
    MyMessage: String;
begin
    if Assigned(FOnError) then
        TriggerError                 { Should be modified to pass Msg ! }
    else begin
        MyMessage := Msg ;
        if (FSocketErrs = wsErrFriendly) and (AFriendlyMsg <> '') then
                                                    MyMessage := AFriendlyMsg;
        MyException := ESocketException.Create(MyMessage, AErrorCode, AErrorMessage,
                                               AFriendlyMsg, AFunc, AIP, APort, AProto);
        if Assigned (FonException) then
        begin
            TriggerException (MyException) ;       { V8.36 }
        end
        else
        begin
            TriggerException (MyException) ;       { V8.37 }
            raise MyException;
        end;
        if Assigned (MyException) then MyException.Free;  { V8.37 }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.RaiseException(const Msg : String);
begin
    RaiseException(Msg, 0, '', '', '', '', '', '');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketCounter.GetLastAliveTick : Int64;   { V8.71 Int64, assembler needs updating }
{x$IFDEF PUREPASCAL}
begin
    if FLastRecvTick > FLastSendTick then
        if FLastRecvTick > FConnectTick then
            Result := FLastRecvTick
        else
            Result := FConnectTick
    else
        if FLastSendTick > FConnectTick then
            Result := FLastSendTick
        else
            Result := FConnectTick;
{x$ELSE}
(* asm
{$IFDEF CPUX64}
    MOV EDX, [RCX].FLastSendTick
    MOV EAX, [RCX].FConnectTick
    MOV ECX, [RCX].FLastRecvTick
    CMP EAX, EDX
    JB  @below
    MOV EDX, ECX
    JMP @more
@below:
    MOV EAX, ECX
@more:
    CMP EAX, EDX
    JB  @done
    RET
@done:
    MOV EAX, EDX
{$ELSE}
    mov ecx, [eax].FLastRecvTick
    mov edx, [eax].FLastSendTick
    mov eax, [eax].FConnectTick
    cmp eax, edx
    jb  @below
    mov edx, ecx
    jmp @more
@below:
    mov eax, ecx
@more:
    cmp eax, edx
    jb  @done
    ret
@done:
    mov eax, edx
{$ENDIF}  *)
{x$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketCounter.SetConnected;
begin
    FLastRecvTick := 0;
    FLastSendTick := 0;
    FConnectTick  := IcsGetTickCount64;    { V8.71 }
    FConnectDT    := Now;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Notification(AComponent: TComponent; Operation: TOperation);
begin
    inherited Notification(AComponent, Operation);
    if operation = opRemove then begin
    {$IFNDEF NO_DEBUG_LOG}
        if AComponent = FIcsLogger then                               { V5.21 }
            FIcsLogger := nil;                                        { V5.21 }
    {$ENDIF}                                                          { V5.21 }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.5 moved to Utils with Ics added }
(*
procedure InitializeAddr(var AAddr: TSockAddrIn6; AIPVersion: TSocketFamily);
    {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    FillChar(AAddr, SizeOf(TSockAddrIn6), 0);
    if AIPVersion = sfIPv6 then
        AAddr.sin6_family := AF_INET6
    else
        AAddr.sin6_family := AF_INET;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SizeOfAddr(const AAddr: TSockAddrIn6): Integer;
    {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    if AAddr.sin6_family = AF_INET6 then
        Result := SizeOf(TSockAddrIn6)
    else
        Result := SizeOf(TSockAddrIn);
end;
*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.AssignDefaultValue;
begin
    IcsInitializeAddr(Fsin, FSocketFamily);    { V9.5 }
    FAddrFormat         := Fsin.sin6_family;
    FCurrentAddrFamily  := AF_UNSPEC;
    FPortAssigned       := FALSE;
    FAddrAssigned       := FALSE;
    FAddrResolved       := FALSE;
    FPortResolved       := FALSE;
    FProtoResolved      := FALSE;
    FLocalPortResolved  := FALSE;

    FProtoAssigned      := TRUE;
    FProto              := IPPROTO_TCP;
    FProtoStr           := 'tcp';
    FType               := SOCK_STREAM;
    FLocalPortStr       := ICS_ANY_PORT;
    FLocalAddr6         := ICS_ANY_HOST_V6;
    FLocalAddr          := ICS_ANY_HOST_V4;
    FLingerOnOff        := wsLingerOn;
    FLingerTimeout      := 0;
    FHSocket            := INVALID_SOCKET;
    FSelectEvent        := 0;
    FState              := wsClosed;
    bAllSent            := TRUE;
    FPaused             := FALSE;
{   FReadCount          := 0;  V7.24 only reset when connection opened, not closed }
    FCloseInvoked       := FALSE;
    FFlushTimeout       := 60;
    FInternalDnsActive  := FALSE;    { V8.43 }
    FAddrResolvedStr    := '';       { V8.60 }
    FPunycodeHost       := '';       { V8.64 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF POSIX}
var
    GlObjectID: NativeInt = 1;
    GLObjectIDSection: TIcsCriticalSection = nil;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketGenerateObjectID: NativeInt;
begin
    { It's not unique but should be OK for our purpose }
    GLObjectIDSection.Enter;
    try
        if GlObjectID = High(NativeInt) then
          GlObjectID := 1
        else
          Inc(GlObjectID);
        Result := GlObjectID;
    finally
        GLObjectIDSection.Leave;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Impl IIcsEventSource }
function TCustomWSocket.GetEventMask: Cardinal;
begin
    Result := FPxEventMask;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetEventMask(const AValue: Cardinal);
begin
    FPxEventMask := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetNotifyMessageID: UINT;
begin
    Result := FPxEventMessageID;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetNotifyMessageID(const AValue: UINT);
begin
    FPxEventMessageID := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetNotifyWindow: HWND;
begin
    Result := FPxEventWindow;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetNotifyWindow(const AValue: HWND);
begin
    FPxEventWindow := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetEventState: TIcsAsyncEventState;
begin
    Result := FPxEventState;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetEventState(const AValue: TIcsAsyncEventState);
begin
    FPxEventState := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetFileDescriptor: Integer;
begin
    Result := FPxFileDescriptor;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetFileDescriptor(const AValue: Integer);
begin
    FPxFileDescriptor := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetObject: TObject;
begin
    Result := Self;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetObjectID: NativeInt;
begin
    Result := FPxObjectID;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF POSIX IIcsEventSource}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.AbortComponent(E:Exception);            { V8.68 added E to allow reporting }
begin
    try
        Abort;
    except
    end;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure handle all messages for TWSocket. All exceptions must be   }
{ handled or the application will be shutted down !                         }
{ If WndProc is overridden in descendent components, then the same exception }
{ handling *MUST* be setup because descendent component code is executed    }
{ before the base class code.                                               }
procedure TCustomWSocket.WndProc(var MsgRec: TMessage);
begin
    try
        with MsgRec do begin
            if Msg = FMsg_WM_ASYNCSELECT then
                WMASyncSelect(MsgRec)
            else if Msg = FMsg_WM_ASYNCGETHOSTBYNAME then
                WMAsyncGetHostByName(MsgRec)
            else if Msg = FMsg_WM_ASYNCGETHOSTBYADDR then
                WMAsyncGetHostByAddr(MsgRec)
            else if Msg = FMsg_WM_CLOSE_DELAYED then
                WMCloseDelayed(MsgRec)
//            else if Msg = FMsg_WM_WSOCKET_RELEASE then
//                WMRelease(MsgRec)
            else if Msg = FMsg_WM_TRIGGER_EXCEPTION then
                { This is useful to check for background exceptions            }
                { In your application, use following code to test your handler }
                { PostMessage(WSocket1.Handle, WM_TRIGGER_EXCEPTION, 0, 0);    }
                raise ESocketException.Create('Test exception in WSocket')
            else
                inherited WndProc(MsgRec);
                //Result := DefWindowProc(Handle, Msg, wParam, LParam);
        end;
    except
        on E:Exception do
            HandleBackGroundException(E, 'TCustomWSocket.WndProc, Msg=' + IntToStr(MsgRec.Msg)) ;  { V8.68 added msg }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.MsgHandlersCount : Integer;
begin
    Result := 6 + inherited MsgHandlersCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.AllocateMsgHandlers;
begin
    inherited AllocateMsgHandlers;
    FMsg_WM_ASYNCSELECT            := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_ASYNCGETHOSTBYNAME     := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_ASYNCGETHOSTBYADDR     := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_CLOSE_DELAYED          := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_TRIGGER_EXCEPTION      := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_TRIGGER_DATA_AVAILABLE := FWndHandler.AllocateMsgHandler(Self);
//  FMsg_WM_WSOCKET_RELEASE        := FWndHandler.AllocateMsgHandler(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.FreeMsgHandlers;
begin
    if Assigned(FWndHandler) then begin
        FWndHandler.UnregisterMessage(FMsg_WM_ASYNCSELECT);
        FWndHandler.UnregisterMessage(FMsg_WM_ASYNCGETHOSTBYNAME);
        FWndHandler.UnregisterMessage(FMsg_WM_ASYNCGETHOSTBYADDR);
        FWndHandler.UnregisterMessage(FMsg_WM_CLOSE_DELAYED);
        FWndHandler.UnregisterMessage(FMsg_WM_TRIGGER_EXCEPTION);
        FWndHandler.UnregisterMessage(FMsg_WM_TRIGGER_DATA_AVAILABLE);
    end;
    inherited FreeMsgHandlers;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.AllocateSocketHWnd;
begin
    inherited AllocateHWnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DeallocateSocketHWnd;
begin
    inherited DeallocateHWnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ThreadAttach;
begin
    inherited ThreadAttach;
    if FHSocket <> INVALID_SOCKET then
        WSocket_Synchronized_WSAASyncSelect(
                                          {$IFDEF POSIX}
                                            Self,
                                          {$ENDIF}
                                            FHSocket,
                                            Handle,
                                            FMsg_WM_ASYNCSELECT, FSelectEvent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ThreadDetach;
begin
    if (IcsGetCurrentThreadID = FThreadID) and (FHSocket <> INVALID_SOCKET) then
        WSocket_Synchronized_WSAASyncSelect(
                                          {$IFDEF POSIX}
                                            Self,
                                          {$ENDIF}
                                            FHSocket,
                                            Handle, 0, 0);
    inherited ThreadDetach;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomWSocket.Create(AOwner: TComponent);
begin
    FWSocketGCountIncremented := False;  { V8.71 JK }
    { if constructor fails, WSocketGCount is not incremented thus WSocketGCount should not be decremented in destructor }

    inherited Create(AOwner);

    FHSocket            := INVALID_SOCKET;           { FP: 18/01/2007 }
    AllocateSocketHWnd;
    FBufHandler         := TIcsBufferHandler.Create(Self);
    FBufHandler.BufSize := 1460; {1514;}             { Default buffer size }
    FDnsResultList      := TStringList.Create;
    FMultiCastIpTTL     := IP_DEFAULT_MULTICAST_TTL;
    ListenBacklog       := 15; { V8.57 was 5 }
    FBufferedByteCount  := 0;  { V5.20 }
    FMultiCastAddrStr   := '';
    FAddrStr            := '';
    FPortStr            := '';
    FCounterClass       := TWSocketCounter;
    FSocketFamily       := DefaultSocketFamily;
    FOldSocketFamily    := FSocketFamily;
    FSocketErrs         := wsErrTech;  { V8.36 }
    AssignDefaultValue;
{$IFDEF MSWINDOWS}
    FConnectThreadId    := 0; { V9.4 }
    EnterCriticalSection(GWSockCritSect);
    try
        Inc(WSocketGCount);
        FWSocketGCountIncremented := True;  { V8.71 JK }
    finally
        LeaveCriticalSection(GWSockCritSect);
    end;
{$ENDIF}
{$IFDEF POSIX}
    FPxObjectID := WSocketGenerateObjectID;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomWSocket.Destroy;
begin
    try                        { V8.71 JK - Inherited Destroy }
        try                    { V8.71 JK - AsyncDNS }
            try
                InternalCancelDnsLookup(TRUE); { Cancel any pending dns lookup      }
            except
                { Ignore any exception here }
            end;
            UnregisterIcsAsyncDnsLookup;
        finally
            if FState <> wsInvalidState then begin              { FPiette V7.42 }
            { wsInvalidState happend when an exception is raised early in the constructor }
            { Close the socket if not yet closed }

                try              { V8.71 JK Close }
                    if FState <> wsClosed then
                        try
                            Close;
                        except  { V8.71 Ignore any exception here }
                        end;
                finally
    {$IFDEF MSWINDOWS}
                    try  { Unload Winsock }
                        EnterCriticalSection(GWSockCritSect);
                        try
                            if FWSocketGCountIncremented then begin  { V8.71 JK }
                                Dec(WSocketGCount);
                                if WSocketGCount <= 0 then begin
                                    WSocketUnloadWinsock;
                     {           WSocketGCount := 0;  // it is set to 0 in WSocketUnloadWinsock }
                                end;
                            end;
                        finally
                            LeaveCriticalSection(GWSockCritSect);
                        end;
                    except  { V8.65 ignore exception }
                    end;
    {$ENDIF}
                end;
            end;
        end;

        if Assigned(FBufHandler) then begin
            FBufHandler.Free;
            FBufHandler := nil;
        end;
        if Assigned(FDnsResultList) then begin
            FDnsResultList.Free;
            FDnsResultList := nil;
        end;

        if Assigned(FCounter) then begin
            FCounter.Free;
            FCounter := nil;
        end;
{$IFNDEF NO_DEBUG_LOG}
     { Removes TIcsLogger's free notification in a thread-safe way }
        SetIcsLogger(nil);
{$ENDIF}
    finally   { V8.71 JK }
        inherited Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.CreateCounter;
begin
    if Assigned(FCounter) then
        FreeAndNil(FCounter);
    FCounter := FCounterClass.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DestroyCounter;
begin
    FreeAndNil(FCounter);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetCounterClass(const Value: TWSocketCounterClass);
var
    NewCounter : TWSocketCounter;
begin
    if Value = nil then
        raise ESocketException.Create('Property CounterClass may not be nil!');
    if Value <> FCounterClass then begin
        FCounterClass := Value;
        if Assigned(FCounter) then begin
            NewCounter              := FCounterClass.Create;
            NewCounter.ConnectDT    := FCounter.ConnectDT;
            NewCounter.ConnectTick  := FCounter.ConnectTick;
            NewCounter.LastRecvTick := FCounter.LastRecvTick;
            NewCounter.LastSendTick := FCounter.LastSendTick;
            FCounter.Free;
            FCounter := NewCounter;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetReqVerLow: BYTE;
begin
  {$IFDEF POSIX}
    Result := 0;
  {$ELSE}
    Result := GReqVerLow;
  {$ENDIF}

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetReqVerLow(const Value: BYTE);
begin
  {$IFDEF MSWINDOWS}
    if GReqVerLow <> Value then begin
      if IsSocketAPILoaded then
  //        SocketError('SetReqVerLow: WinSock version can''t be changed now')
  // V8.65 should never be called, don't support Winsock 1 anyway
        else
            GReqVerLow := Value;
    end;
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetReqVerHigh: BYTE;
begin
  {$IFDEF POSIX}
    Result := 0;
  {$ELSE}
    Result := GReqVerHigh;
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetReqVerHigh(const Value: BYTE);
begin
  {$IFNDEF POSIX}
    if GReqVerHigh <> Value then begin
        if IsSocketAPILoaded then
  //        SocketError('SetReqVerHigh: WinSock version can''t be changed now')
  // V8.65 should never be called, don't support Winsock 1 anyway
        else
            GReqVerHigh := Value;
    end;
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Dup(NewHSocket : TSocket);
var
    iStatus : Integer;
    optlen  : Integer;
  {$IFDEF MACOS}
    optval  : Integer;
  {$ENDIF}
begin
    if (NewHSocket = 0) or (NewHSocket = INVALID_SOCKET) then begin
        WSocket_Synchronized_WSASetLastError(WSAEINVAL);
        SocketError('Dup');
        Exit;
    end;

    if FState <> wsClosed then begin
        iStatus := WSocket_Synchronized_closesocket(FHSocket);
        FHSocket := INVALID_SOCKET;
        if iStatus <> 0 then begin
            SocketError('Dup (closesocket)');
            Exit;
        end;

        ChangeState(wsClosed);
    end;
    FHsocket := NewHSocket;

  {$IFDEF MACOS}
    { No SIGPIPE on writes but EPIPE in errno }
    optlen  := SizeOf(Integer);
    optval  := 1;
    iStatus := WSocket_setsockopt(FHSocket, SOL_SOCKET, SO_NOSIGPIPE,
                                  PAnsiChar(@optval), optlen);
    if iStatus <> 0 then begin
        SocketError('setsockopt(SO_NOSIGPIPE)');
        Exit;
    end;
  {$ENDIF}

    { Get winsock send buffer size }
    optlen  := SizeOf(FSocketSndBufSize);
    iStatus := WSocket_getsockopt(FHSocket, SOL_SOCKET, SO_SNDBUF,
                                  PAnsiChar(@FSocketSndBufSize), optlen);

    if iStatus <> 0 then begin
        SocketError('getsockopt(SO_SNDBUF)');
        Exit;
    end;

    { Get winsock receive buffer size }
    optlen  := SizeOf(FSocketRcvBufSize);
    iStatus := WSocket_getsockopt(FHSocket, SOL_SOCKET, SO_RCVBUF,
                                  PAnsiChar(@FSocketRcvBufSize), optlen);

    if iStatus <> 0 then begin
        SocketError('getsockopt(SO_RCVBUF)');
        Exit;
    end;

    if HasOption(FComponentOptions, wsoTcpNoDelay) and { V7.27 }
                (not SetTcpNoDelayOption) then
        Exit;
    SetLingerOption;
    SetKeepAliveOption;  // AG { 05/23/07)

    { FD_CONNECT is not needed for dup(): The socket is already connected }
    FSelectEvent := FD_READ or FD_WRITE or FD_CLOSE { or FD_CONNECT };
    iStatus      := WSocket_Synchronized_WSAASyncSelect(
                                                      {$IFDEF POSIX}
                                                        Self,
                                                      {$ENDIF}
                                                        FHSocket,
                                                        Handle,
                                                        FMsg_WM_ASYNCSELECT,
                                                        FSelectEvent);
    if iStatus <> 0 then begin
        SocketError('WSAAsyncSelect');
        Exit;
    end;
    DupConnected;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DupConnected;
begin
    if Assigned(FCounter) then
        FCounter.SetConnected;
    FReadCount  := 0;  { 7.24 }
    FWriteCount := 0;  { 7.24 }
    ChangeState(wsConnected);
    if FSessIpInfo.SocFamily = sfAny then    { may have been completed by server client callback }
        GetSocketIpInfo;                  { V9.5 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetBufSize(Value : Integer);
begin
    FBufHandler.BufSize := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetBufSize: Integer;
begin
    Result := FBufHandler.BufSize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Get the number of char received and waiting to be read                    }
function TCustomWSocket.GetRcvdCount : Integer;
var
    Temp : u_long;
begin
    if csDesigning in ComponentState then begin
        Result := -1;
        Exit;
    end;
    if WSocket_Synchronized_ioctlsocket(FHSocket, FIONREAD, Temp) = SOCKET_ERROR then begin
        Result := -1;
        SocketError('ioctlSocket');
        Exit;
    end;
    Result := Integer(Temp);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ChangeState(NewState : TSocketState);
var
    OldState : TSocketState;
begin
    OldState := FState;
    FState   := NewState;
    if OldState <> NewState then       { 20030226 }
        TriggerChangeState(OldState, NewState);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ DoRecv is a simple wrapper around winsock recv function to make it        }
{ a virtual function.                                                       }
function TCustomWSocket.DoRecv(
    var Buffer : TWSocketData;
    BufferSize : Integer;
    Flags      : Integer) : Integer;
begin
{ MoulinCnt := (MoulinCnt + 1) and 3; }
{ Write('R', Moulin[MoulinCnt], #13); }
    Result := WSocket_Synchronized_recv(FHSocket, Buffer, BufferSize, Flags);
  {$IFDEF POSIX}
    if (not FPaused) and ((Result > -1) or (errno = WSAEWOULDBLOCK)) then
        WSocketSynchronizedEnableReadEvent(Self);
  {$ENDIF}
{   FRcvdFlag := (Result > 0);}
    { If we received the requested size, we may need to receive more }
    FRcvdFlag := (Result >= BufferSize);
    if (Result > 0) then begin
        if Flags <> MSG_PEEK then
            FReadCount := FReadCount + Result; { V8.30 was done in Receive }
        if Assigned(FCounter) then
            FCounter.FLastRecvTick := IcsGetTickCount64;    { V8.71 }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ The socket is non-blocking, so this routine will only receive as much     }
{ data as it is available.                                                  }
function TCustomWSocket.Receive(Buffer : TWSocketData; BufferSize: Integer) : Integer;
begin
    Result := DoRecv(Buffer, BufferSize, 0);
    if Result < 0 then
        FLastError := WSocket_Synchronized_WSAGetLastError;
   { else
        FReadCount := FReadCount + Result;  V8.30 done in DoRecv  }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function TCustomWSocket.ReceiveStrW(ACodePage : Cardinal) : UnicodeString;
begin
    Result :=  AnsiToUniCode(ReceiveStrA, ACodePage);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.ReceiveStrW : UnicodeString;
begin
    Result := ReceiveStrW(CP_ACP);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.ReceiveStrA : AnsiString;
var
    lCount : Integer;
begin
    lCount := GetRcvdCount;

    if lCount < 0 then begin  { GetRcvdCount returned an error }
        SetLength(Result, 0);
        Exit;
    end;

    if lCount = 0 then        { GetRcvdCount say nothing, will try anyway }
        LCount := 255;        { some reasonable arbitrary value           }

    SetLength(Result, lCount);
    lCount := Receive(@Result[1], lCount);
    if lCount > 0 then
        SetLength(Result, lCount)
    else
        SetLength(Result, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ receive up to MaxLen of Data into a dynamic array of Bytes, returns array size or -1 for error }
{ generally used to receive TCP streams, not UDP datagrams, see ReceiveFromTB }
function TCustomWSocket.ReceiveTB(var Data : TBytes; MaxLen : Integer = -1) : Integer;              { V8.70 }
begin
  { if NOT Assigned(Data) then begin   V8.71 don't fail
        Result := -1;
        Exit;
    end;    }
    Result := GetRcvdCount;
    if Result < 0 then begin  { GetRcvdCount returned an error }
        SetLength(Data, 0);   { V8.71 empty }
        Exit;
    end;
    if Result = 0 then        { GetRcvdCount say nothing, will try anyway }
        Result := 255;        { some reasonable arbitrary value           }
    if MaxLen > Result then   { restrict receive length }
        Result := MaxLen;
    SetLength(Data, Result);
    Result := Receive(@Data[0], Result);
    if Result > 0 then
        SetLength(Data, Result)
    else
        SetLength(Data, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ receive up to MaxLen of Data into a dynamic array of Bytes, returns array of size zero for error }
{ generally used to receive TCP streams, not UDP datagrams, see ReceiveFromTB }
function TCustomWSocket.ReceiveTB(MaxLen : Integer = -1) : TBytes;              { V8.71 }
begin
    ReceiveTB(Result, MaxLen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Receive as much data as possible into a string                            }
{ You should avoid this function and use Receive. Using string will be      }
{ much slower because data will be copied several times.                    }
{ ReceiveStr will *NOT* wait for a line to be received. It just read        }
{ already received characters and return them as a string.                  }
function TCustomWSocket.ReceiveStr : String;
begin
{$IFDEF COMPILER12_UP}
    Result := ReceiveStrW;
{$ELSE}
    Result := ReceiveStrA;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.DoRecvFrom(
    FHSocket    : TSocket;
    var Buffer  : TWSocketData;     { why is this var ??? }
    BufferSize  : Integer;
    Flags       : Integer;
    var From    : TSockAddr;
    var FromLen : Integer) : Integer;
begin
    Result := WSocket_Synchronized_recvfrom(FHSocket, Buffer, BufferSize, Flags, From, FromLen);
  {$IFDEF POSIX}
    if (not FPaused) and ((Result > -1) or (errno = WSAEWOULDBLOCK)) then
        WSocketSynchronizedEnableReadEvent(Self);
  {$ENDIF}
    FRcvdFlag := (Result >= BufferSize);
    if Result > 0 then begin
        FReadCount := FReadCount + Result;               { V8.30 was in ReceiveFrom }
        if Assigned(FCounter) then
            FCounter.FLastRecvTick := IcsGetTickCount64;    { V8.71 }  { V8.30 was missing }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.ReceiveFrom(
    Buffer      : TWSocketData;
    BufferSize  : Integer;
    var From    : TSockAddr;
    var FromLen : Integer) : Integer;
begin
    Result := DoRecvFrom(FHSocket, Buffer, BufferSize, 0, From, FromLen);
    if Result < 0 then
        FLastError := WSocket_Synchronized_WSAGetLastError;
   { else
        FReadCount := FReadCount + Result;  V8.30 done in DoRecvFrom   }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.ReceiveFrom6(       { V8.07 }
    Buffer      : TWSocketData;
    BufferSize  : Integer;
    var From    : TSockAddrIn6;
    var FromLen : Integer) : Integer;
begin
    Result := DoRecvFrom(FHSocket, Buffer, BufferSize, 0, PSockAddrIn(@From)^, FromLen);
    if Result < 0 then
        FLastError := WSocket_Synchronized_WSAGetLastError;
  {  else
        FReadCount := FReadCount + Result;    V8.30 done in DoRecvFrom   }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ receive up to MaxLen of Data into a dynamic array of Bytes, returns array size or -1 for error }
{ generally used to receive UDP datagrams, not TCP streams, and returns the remote IPv4 address and port }
function TCustomWSocket.ReceiveFromTB(var Data : TBytes; var From : TSockAddr;
                              var FromLen : Integer; MaxLen : Integer = -1) : Integer;  { V8.70 }
var
    Buffer: TWSocketData;
begin
  { if NOT Assigned(Data) then begin   V8.71 don't fail
        Result := -1;
        Exit;
    end;    }
    Result := MaxLen;
    if Result <= 0 then
        Result := 2048;  { only expecting a UDP datagram }
    SetLength(Data, Result);
    Buffer := @Data[0];
    Result := DoRecvFrom(FHSocket, Buffer, Result, 0, From, FromLen);
    if Result < 0 then begin
        FLastError := WSocket_Synchronized_WSAGetLastError;
        SetLength(Data, 0)                 { V8.71 fix range check error }
    end
    else
        SetLength(Data, Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ receive up to MaxLen of Data into a dynamic array of Bytes, returns array size or -1 for error }
{ generally used to receive UDP datagrams, not TCP streams, and returns the remote IPv6 address and port }
function TCustomWSocket.ReceiveFrom6TB(var Data : TBytes; var From : TSockAddrIn6;
                              var FromLen : Integer; MaxLen : Integer = -1) : Integer;   { V8.70 }
var
    Buffer: TWSocketData;
begin
 {   if NOT Assigned(Data) then begin    V8.71 don't fail
        Result := -1;
        Exit;
    end;       }
    Result := MaxLen;
    if Result <= 0 then
        Result := 2048;  { only expecting a UDP datagram }
    SetLength(Data, Result);
    Buffer := @Data[0];
    Result := DoRecvFrom(FHSocket, Buffer, Result, 0, PSockAddrIn(@From)^, FromLen);
    if Result < 0 then begin
        FLastError := WSocket_Synchronized_WSAGetLastError;
        SetLength(Data, 0)                  { V8.71 fix range check error }
    end
    else
        SetLength(Data, Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.PeekData(Buffer : TWSocketData; BufferSize: Integer) : Integer;
begin
    Result := DoRecv(Buffer, BufferSize, MSG_PEEK);
    if Result < 0 then
        FLastError := WSocket_Synchronized_WSAGetLastError;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This function should be used with UDP IPv4 only. Use Send for TCP.        }
function TCustomWSocket.SendTo(
    Dest       : TSockAddr;
    DestLen    : Integer;
    Data       : TWSocketData;
    Len        : Integer) : Integer;
begin
    Result := WSocket_Synchronized_SendTo(FHSocket, Data, Len, FSendFlags, TSockAddr(Dest), DestLen);
    if Result > 0 then begin
        FWriteCount := FWriteCount + Result;  { 7.24 }
        if Assigned(FCounter) then            { V8.30 }
            FCounter.FLastSendTick := IcsGetTickCount64;    { V8.71 }
        TriggerSendData(Result);
        { Post FD_WRITE message to have OnDataSent event triggered }
        if bAllSent and (FType = SOCK_DGRAM) then
            PostMessage(Handle,
                        FMsg_WM_ASYNCSELECT,
                        WParam(FHSocket),            { V8.08 }
                        IcsMakeLong(FD_WRITE, 0));
    end
    else begin
        FLastError := WSocket_Synchronized_WSAGetLastError;  { V8.67 keep error }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This function should be used with UDP IPv6 only. Use Send for TCP.        }
function TCustomWSocket.SendTo6(      { V8.07 }
    Dest       : TSockAddrIn6;
    DestLen    : Integer;
    Data       : TWSocketData;
    Len        : Integer) : Integer;
begin
    Result := WSocket_Synchronized_SendTo(FHSocket, Data, Len, FSendFlags,
                                                   PSockAddrIn(@Dest)^, DestLen);
    if Result > 0 then begin
        FWriteCount := FWriteCount + Result;  { 7.24 }
        if Assigned(FCounter) then            { V8.30 }
            FCounter.FLastSendTick := IcsGetTickCount64;    { V8.71 }
        TriggerSendData(Result);
        { Post FD_WRITE message to have OnDataSent event triggered }
        if bAllSent and (FType = SOCK_DGRAM) then
            PostMessage(Handle,
                        FMsg_WM_ASYNCSELECT,
                        WParam(FHSocket),            { V8.08 }
                        IcsMakeLong(FD_WRITE, 0));
    end
    else begin
        FLastError := WSocket_Synchronized_WSAGetLastError;  { V8.67 keep error }
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This function should be used with UDP IPv4 only. Use SendTB for TCP.        }
function TCustomWSocket.SendToTB(
                         Dest       : TSockAddr;
                         DestLen    : Integer;
                         const Data : TBytes;
                         Len: Integer = -1) : Integer;                        { V8.69 }
var
    ActualLen: Integer;
begin
    ActualLen := Length(Data);
    if (Len < 0) or (Len > ActualLen) then
        Len := ActualLen;
    if Len > 0 then
        Result := SendTo(Dest, DestLen, Pointer(Data), Len)
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This function should be used with UDP IPv6 only. Use SendTB for TCP.        }
function TCustomWSocket.SendToTB6(
                          Dest       : TSockAddrIn6;
                          DestLen    : Integer;
                          const Data : TBytes;
                          Len: Integer = -1) : Integer;                     { V8.69 }
var
    ActualLen: Integer;
begin
    ActualLen := Length(Data);
    if (Len < 0) or (Len > ActualLen) then
        Len := ActualLen;
    if Len > 0 then
        Result := SendTo6(Dest, DestLen, Pointer(Data), Len)
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.RealSend(var Data : TWSocketData; Len : Integer) : Integer;
begin
    if FType = SOCK_DGRAM then
        Result := WSocket_Synchronized_SendTo(FHSocket, Data, Len, FSendFlags, PSockAddr(@Fsin)^, IcsSizeOfAddr(Fsin))   { V9.5 }
    else
        Result := WSocket_Synchronized_Send(FHSocket, Data, Len, FSendFlags);
    if Result > 0 then begin
    {    FWriteCount := FWriteCount + Result;  V8.30 done in TryToSend to avoid SSL overhead }
        if Assigned(FCounter) then
            FCounter.FLastSendTick := IcsGetTickCount64;    { V8.71 }
        TriggerSendData(Result);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TryToSend;
var
    Len       : Integer;
    Count     : Integer;
    Data      : TWSocketData;
    LastError : Integer;
begin
    FBufHandler.Lock;
    try
        if (FHSocket = INVALID_SOCKET) or (FBufHandler.IsEmpty) then begin
            bAllSent := TRUE;
            Exit;
        end;

        while TRUE do begin
            Len := FBufHandler.Peek(Data);
            if Len <= 0 then begin
                // Buffer is empty, every thing has been sent
                bAllSent := TRUE;
                break;
            end;
            Count := RealSend(Data, Len);
            if Count > 0 then begin
                Dec(FBufferedByteCount, Count);
                if FBufferedByteCount < 0 then
                    FBufferedByteCount := 0;
                FWriteCount := FWriteCount + Count;  { V8.30 was in RealSend }
            end;
            if Count = 0 then
                break;  // Closed by remote

            if Count = SOCKET_ERROR then begin
                LastError := WSocket_Synchronized_WSAGetLastError;
                if (LastError = WSAECONNRESET) or (LastError = WSAENOTSOCK) or
                   (LastError = WSAENOTCONN)   or (LastError = WSAEINVAL)   or
                   (LastError = WSAECONNABORTED) { 07/05/99 } or
                   (LastError = WSAESHUTDOWN)    { V8.29 Can't send after socket shutdown }
                then begin
                  {$IFNDEF NO_DEBUG_LOG}
                    if CheckLogOptions(loWsockErr) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
                            DebugLog(loWsockErr, Name +
                                     ' Winsock Send failed - ' +
                                     GetWinsockErr(LastError));
                  {$ENDIF}
                    FCloseInvoked := TRUE;           { 23/07/98 }
                    Close;
                    TriggerSessionClosed(LastError); { 23/07/98 }
                end
                else if LastError <> WSAEWOULDBLOCK then begin
                    SocketError('TryToSend failed');
                    break;
                end;
                break;
            end;
            FBufHandler.Remove(Count);
            if Count < Len then
                break; // Could not write as much as we wanted. Stop sending
        end;
    finally
        FBufHandler.UnLock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.PutStringInSendBuffer(const Str : RawByteString): Integer;
begin
    Result := Length(Str);
    if Result > 0 then
        PutDataInSendBuffer(Pointer(Str), Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function TCustomWSocket.PutStringInSendBuffer(const Str : UnicodeString; ACodePage : Cardinal): Integer;
begin
    Result := PutStringInSendBuffer(UnicodeToAnsi(Str, ACodePage));  // Explicit cast
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.PutStringInSendBuffer(const Str : UnicodeString): Integer;
begin
    Result := PutStringInSendBuffer(AnsiString(Str));  // Explicit cast
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.PutDataInSendBuffer(
    Data : TWSocketData;
    Len  : Integer);
begin
    if (Len <= 0) or (Data = nil) then
        Exit;

    FBufHandler.Lock;
    try
        FBufHandler.Write(Data, Len);
        Inc(FBufferedByteCount, Len);
        bAllSent := FALSE;
    finally
        FBufHandler.UnLock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Return -1 if error, else return number of byte written                    }
function TCustomWSocket.Send(Data : TWSocketData; Len : Integer) : Integer;
begin
    if (FState <> wsConnected) and (FState <> wsSocksConnected) then begin
        WSocket_Synchronized_WSASetLastError(WSAENOTCONN);
      { V8.70 wsoNoSendException means BgException should not called for this error,
         let application gracefully handle error with -1, set for servers }
        if NOT HasOption(FComponentOptions, wsoNoSendException) then
            SocketError('Send');
        Result := -1;
        Exit;
    end;

    bAllSent := FALSE;
    if Len <= 0 then
        Result := 0
    else begin
        Result   := Len;
        PutDataInSendBuffer(Data, Len);
    end;

    if bAllSent then
        Exit;

    TryToSend;

    if bAllSent then begin
        { We post a message to fire the FD_WRITE message which in turn will}
        { fire the OnDataSent event. We cannot fire the event ourself      }
        { because the event handler will eventually call send again.       }
        { Sending the message prevent recursive call and stack overflow.   }
        { The PostMessage function posts (places) a message in a window's  }
        { message queue and then returns without waiting for the           }
        { corresponding window to process the message. The message will be }
        { seen and routed by Delphi a litle later, when we will be out of  }
        { the send function.                                               }
        PostMessage(Handle,
                    FMsg_WM_ASYNCSELECT,
                    WParam(FHSocket),            { V8.08 }
                    IcsMakeLong(FD_WRITE, 0));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.Send(DataByte : Byte) : Integer;
begin
    Result := Send(@DataByte, 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.SendTB(const Data: TBytes; Len: Integer = -1) : Integer;     { V8.69 }
var
    ActualLen: Integer;
begin
    ActualLen := Length(Data);
    if (Len < 0) or (Len > ActualLen) then
        Len := ActualLen;
    if Len > 0 then
        Result := Send(Pointer(Data), Len)
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Return -1 if error, else return number of bytes written }
{$IFDEF COMPILER12_UP}
function TCustomWSocket.SendStr(const Str : UnicodeString; ACodePage : Cardinal) : Integer;
begin
    Result := SendStr(UnicodeToAnsi(Str, ACodePage));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Converts UnicodeString to AnsiString using System.DefaultSystemCodePage   }
function TCustomWSocket.SendStr(const Str : UnicodeString) : Integer;
begin
    Result := SendStr(AnsiString(Str)); // RTL convert
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Return -1 if error, else return number of byte written                    }
function TCustomWSocket.SendStr(const Str : RawByteString) : Integer;
begin
    Result := Length(Str);
    if Result > 0 then
        Result := Send(PAnsiChar(Str), Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SendText(const Str : RawByteString);
begin
    SendStr(Str);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
procedure TCustomWSocket.SendText(const Str : UnicodeString);
begin
    SendStr(AnsiString(Str));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SendText(const Str : UnicodeString; ACodePage : Cardinal);
begin
    SendStr(Str, ACodePage);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function HasOption(
    OptSet : TWSocketOptions;
    Opt    : TWSocketOption): Boolean;
begin
    Result := Opt in OptSet;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function AddOptions(Opts: array of TWSocketOption): TWSocketOptions;
var
    I : Integer;
begin
    Result := [];
    for I := Low(Opts) to High(Opts) do
        //Result := Result + [Opts[I]];  { Anton Sviridov }
        Include(Result, Opts[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  RemoveOption(
    OptSet : TWSocketOptions;
    Opt    : TWSocketOption) : TWSocketOptions;
begin
    Result := OptSet - [Opt];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ASyncReceive(
    Error           : Word;
    MySocketOptions : TWSocketOptions);
var
    bMore        : Boolean;
    lCount       : {$IFDEF FPC} Cardinal; {$ELSE} u_long; {$ENDIF}
    TrashCanBuf  : array [0..1023] of AnsiChar;  { AG 1/12/08 }
    TrashCan     : TWSocketData;
    TrashCanSize : Integer;
begin
    bMore := TRUE;
    while bMore do begin
        FLastError := 0;

        try
            if not TriggerDataAvailable(Error) then begin
                { Nothing wants to receive, we will receive and throw away  23/07/98 }
                TrashCanSize := SizeOf(TrashCanBuf); { V7.75 }
                TrashCan     := @TrashCanBuf;
                if DoRecv(TrashCan, TrashCanSize, 0) = SOCKET_ERROR then begin
                    FLastError := WSocket_Synchronized_WSAGetLastError;
                    if FLastError = WSAEWOULDBLOCK then begin
                        FLastError := 0;
                        break;
                    end;
                end;
            end;

            { DLR Honor the socket options being passed as parameters }
            if HasOption({FComponentOptions}MySocketOptions, wsoNoReceiveLoop) then  { V6.03 }
                break;

            if FLastError <> 0 then begin
                bMore := FALSE;
                { -1 value is not a true error but is used to break the loop }
                if FLastError = -1 then
                    FLastError := 0;
            end
            { Check if we have something new arrived, if yes, process it }
            else if WSocket_Synchronized_ioctlsocket(FHSocket, FIONREAD,
                                                     lCount) = SOCKET_ERROR then begin
                FLastError := WSocket_Synchronized_WSAGetLastError;
                bMore      := FALSE;
            end
            else if lCount = 0 then
                bMore := FALSE;
        except
            on E:Exception do begin
                HandleBackGroundException(E, 'TCustomWSocket.ASyncReceive');  { V8.62 don't ignore user errors }
                bMore := FALSE;                                               { V8.63 and don't continue looping }
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.5 clear IP information for connected socket }
procedure TCustomWSocket.ClearSocketIpInfo;
begin
    {$IFDEF COMPILER16_UP}
        FSessIpInfo := Default(TIcsSessIpInfo);
    {$ELSE}
        FillChar(FSessIpInfo, SizeOf(TIcsSessIpInfo), #0);
        FSessIpInfo.LocalAddr := '';
        FSessIpInfo.RemoteAddr := '';
        FSessIpInfo.LocalPort := '';
        FSessIpInfo.RemotePort := '';
        FSessIpInfo.SocketType := '';
        FSessIpInfo.Protocol := '';
        FSessIpInfo.ISOA2 := '';
        FSessIpInfo.CountryName := '';
        FSessIpInfo.RemoteRDNS := '';
    {$ENDIF}
    FSessIpInfo.SocFamily := sfAny;  { shows no IP address load yet }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.5 get IP information for connected socket, local and remote IP addresses, type and protocol }
procedure TCustomWSocket.GetSocketIpInfo;
var
    OptLen: Integer;
    CSAddrInfo: TCSAddrInfo;
begin
    if FState <> wsConnected then
        Exit;
    if FHSocket = INVALID_SOCKET then
        Exit;
    if Win32MajorVersion < 6 then  { only Windows Vista, 2008 and later }
        Exit;
    OptLen := SizeOf(CSAddrInfo);
    FillChar(CSAddrInfo, OptLen, #0);
    if WSocket_getsockopt(FHSocket, SOL_SOCKET, SO_BSP_STATE, @CSAddrInfo, OptLen) = SOCKET_ERROR then
        Exit;
    Move(CSAddrInfo.LocalAddr.Sockaddr^, FSessIpInfo.SocLocalAddr, CSAddrInfo.LocalAddr.SockaddrLength);
    Move(CSAddrInfo.RemoteAddr.Sockaddr^, FSessIpInfo.SocRemoteAddr, CSAddrInfo.RemoteAddr.SockaddrLength);
    FSessIpInfo.LocalAddr := WSocketSockAddrToStr(FSessIpInfo.SocLocalAddr);
    FSessIpInfo.LocalPort := IntToStr(Swap(FSessIpInfo.SocLocalAddr.sin6_port));
    FSessIpInfo.RemoteAddr := WSocketSockAddrToStr(FSessIpInfo.SocRemoteAddr);
    FSessIpInfo.RemotePort := IntToStr(Swap(FSessIpInfo.SocRemoteAddr.sin6_port));
    FSessIpInfo.SocFamily := WSocketFamilyFromAF(FSessIpInfo.SocLocalAddr.sin6_family);
    FSessIpInfo.SocType := CSAddrInfo.iSocketType;   // SOCK_STREAM or SOCK_DGRAM
    FSessIpInfo.Proto := CSAddrInfo.iProtocol;       // IPPROTO_TCP or IPPROTO_UDP
//    FSessIpInfo.SocketType := IcsIPProtoName(CSAddrInfo.iSocketType);
    FSessIpInfo.StartTick := IcsGetTickCount64;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_CONNECT(var msg: TMessage);
begin
{$IFDEF MSWINDOWS}
    if FConnectThreadId <> GetCurrentThreadId() then begin  { V9.4 }
      while FState = wsOpened do begin
        Sleep(5);
      end;
    end;
{$ENDIF}

    if FState <> wsConnected then begin
        if (IcsHiWord(msg.LParam) = 0) then begin { V9.1 don't set connected if it failed }
            ChangeState(wsConnected);
            GetSocketIpInfo;       { V9.5 }
        end;
        TriggerSessionConnectedSpecial(IcsHiWord(msg.LParam));
        if (IcsHiWord(msg.LParam) <> 0) and (FState <> wsClosed) then
            Close;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_READ(var msg: TMessage);
begin
    if FState <> wsConnected then begin
      ChangeState(wsConnected);
      GetSocketIpInfo;       { V9.5 }
      TriggerSessionConnectedSpecial(IcsHiWord(msg.LParam));
    end;
    ASyncReceive(IcsHiWord(msg.LParam), FComponentOptions);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_WRITE(var msg: TMessage);
begin
    TryToSend;
{ If you wants to test background exception, uncomment the next 2 lines. }
{   if bAllSent then                                                }
{       raise Exception.Create('Test TWSocket exception');          }
    if bAllSent then
        TriggerDataSent(IcsHiWord(msg.LParam));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_CLOSE(var msg: TMessage);
begin
    { In some strange situations I found that we receive a FD_CLOSE  }
    { during the connection phase, breaking the connection early !   }
    { This occurs for example after a failed FTP transfert Probably  }
    { something related to bugged winsock. Doesn't hurt with good    }
    { winsock. So let the code there !                               }
    if (FState <> wsConnecting) and (FHSocket <> INVALID_SOCKET) then begin
        { Check if we have something arrived, if yes, process it     }
        { DLR, since we are closing MAKE SURE WE LOOP in the receive }
        { function to get ALL remaining data                         }

        if (FState <> wsListening) then // We should not receive with listening sockets {AG}
            ASyncReceive(0, RemoveOption(FComponentOptions, wsoNoReceiveLoop));

        if not FCloseInvoked then begin
            FCloseInvoked := TRUE;
            TriggerSessionClosed(IcsHiWord(msg.LParam));
        end;

        if FState <> wsClosed then
            Close;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_ACCEPT(var msg: TMessage);
begin
    TriggerSessionAvailable(IcsHiWord(msg.LParam));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_ROUTING_INTERFACE_CHANGE(var msg: TMessage);
begin
    TriggerRoutingInterfaceChanged(IcsHiWord(msg.LParam));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_ADDRESS_LIST_CHANGE(var msg: TMessage);
begin
    TriggerAddressListChanged(IcsHiWord(msg.LParam));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerAddressListChanged(ErrCode: Word);
begin
    if Assigned(FOnAddressListChanged) then
        FOnAddressListChanged(Self, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerRoutingInterfaceChanged(ErrCode: Word);
begin
    if Assigned(FOnRoutingInterfaceChanged) then
        FOnRoutingInterfaceChanged(Self, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetOnAddressListChanged(
    const Value: TNetChangeEvent);
begin
    FOnAddressListChanged := Value;
    if Assigned(FOnAddressListChanged) then
        Include(FComponentOptions, wsoNotifyAddressListChange)
    else
        Exclude(FComponentOptions, wsoNotifyAddressListChange);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetOnRoutingInterfaceChanged(
    const Value: TNetChangeEvent);
begin
    FOnRoutingInterfaceChanged := Value;
    if Assigned(FOnRoutingInterfaceChanged) then
        Include(FComponentOptions, wsoNotifyRoutingInterfaceChange)
    else
        Exclude(FComponentOptions, wsoNotifyRoutingInterfaceChange);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF NO_DEBUG_LOG}
function WinsockMsgToString(var msg: TMessage) : String;
begin
    Result := '';
    if (msg.lParam and FD_CONNECT) <> 0 then
        Result := Result + ' FD_CONNECT';
    if (msg.lParam and FD_READ) <> 0 then
        Result := Result + ' FD_READ';
    if (msg.lParam and FD_WRITE) <> 0 then
        Result := Result + ' FD_WRITE';
    if (msg.lParam and FD_CLOSE) <> 0 then
        Result := Result + ' FD_CLOSE';
    if (msg.lParam and FD_ACCEPT) <> 0 then
        Result := Result + ' FD_ACCEPT';
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.WMASyncSelect(var msg: TMessage);
var
    Check   : Word;
    ParamLo : Word;
const
    TTTCount : Integer = 0;
begin
{TriggerDisplay('AsyncSelect ' + IntToStr(msg.wParam) + ', ' + IntToStr(msg.LParamLo));}
    { Verify that the socket handle is ours handle }
    if msg.wParam <> WPARAM(FHSocket) then
        Exit;

    if FPaused then
        exit;

    ParamLo := LoWord(msg.lParam);

    Check := ParamLo and FD_CONNECT;
    if Check <> 0 then begin
        FSelectMessage := FD_CONNECT;
        Do_FD_CONNECT(msg);
    end;

    Check := ParamLo and FD_READ;
    if Check <> 0 then begin
        FSelectMessage := FD_READ;
        Do_FD_READ(msg);
    end;

    Check := ParamLo and FD_WRITE;
    if Check <> 0 then begin
        FSelectMessage := FD_WRITE;
        Do_FD_WRITE(msg);
    end;

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
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure GetIPList(phe  : PHostEnt; ToList : TStrings);
type
    TaPInAddr = array [0..255] of PInAddr;
    PaPInAddr = ^TaPInAddr;
var
    pptr : PaPInAddr;
    I    : Integer;
begin
    pptr := PaPInAddr(Phe^.h_addr_list);

    I := 0;
    while pptr^[I] <> nil do begin
        ToList.Add(String(WSocket_inet_ntoa(pptr^[I]^)));
        Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure GetAliasList(phe : PHostEnt; ToList : TStrings);
type
    TaPAnsiChar = array [0..255] of PAnsiChar;
    PaPAnsiChar = ^TaPAnsiChar;
var
    pptr : PaPAnsiChar;
    I    : Integer;
begin
    pptr := PaPAnsiChar(Phe^.h_aliases);
    I    := 0;
    while pptr^[I] <> nil do begin
        ToList.Add(String(pptr^[I]));
        Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.WMAsyncGetHostByName(var msg: TMessage);
var
    ErrCode : Word;
{$IFDEF MSWINDOWS}
    Phe     : Phostent;
{$ENDIF}
begin
    if FDnsLookupHandle = 0 then begin
        { We are still executing WSAAsyncGetHostByName and FDnsLookupHandle }
        { has not been assigned yet ! We should proceed later.              }
        FDnsLookupTempMsg  := msg;
        FDnsLookupCheckMsg := TRUE;
        Exit;
    end
    else if msg.wParam <> WPARAM(FDnsLookupHandle) then
        Exit;

    FDnsLookupHandle := 0;
    ErrCode := IcsHiWord(Msg.LParam);
    if ErrCode = 0 then begin
      {$IFDEF MSWINDOWS}
        if (FSocketFamily = sfIPv4) and (not HasOption(FComponentOptions, wsoIcsDnsLookup)) then begin  { V8.43, V8.70 }
            Phe := PHostent(@FDnsLookupBuffer);
            if phe <> nil then begin
                GetIpList(Phe, FDnsResultList);
                FDnsResult := FDnsResultList.Strings[0];
            end;
        end
        else begin
      {$ENDIF}
            FDnsResultList.Assign(TIcsAsyncDnsLookupRequest(msg.WParam).ResultList);
            if FDnsResultList.Count > 0 then
                FDnsResult := FDnsResultList[0];
      {$IFDEF MSWINDOWS}
        end;
      {$ENDIF}
    end;
    TriggerDnsLookupDone(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.WMAsyncGetHostByAddr(var msg: TMessage);
var
{$IFDEF MSWINDOWS}
    Phe     : Phostent;
{$ENDIF}
    ErrCode : Word;
begin
    if msg.wParam <> WPARAM(FDnsLookupHandle) then
        Exit;
    FDnsLookupHandle := 0;
    ErrCode          := IcsHiWord(Msg.LParam);
    if ErrCode = 0 then begin
        FDnsResultList.Clear;
      {$IFDEF MSWINDOWS}
        if (FSocketFamily = sfIPv4) and (not HasOption(FComponentOptions, wsoIcsDnsLookup)) then begin  { V8.44, V8.70 }
            Phe := PHostent(@FDnsLookupBuffer);
            if phe <> nil then begin
                FDnsResult := String(StrPas(Phe^.h_name));
                FDnsResult := IcsIDNAToUnicode(FDnsResult);   { V8.64 }
                FDnsResultList.Add(FDnsResult);
                GetAliasList(Phe, FDnsResultList);  {AG 03/03/06}
            end;
        end
        else begin
            FDnsResultList.Assign(TIcsAsyncDnsLookupRequest(msg.WParam).ResultList);
        end;
      {$ENDIF}
      {$IFDEF POSIX}
        FDnsResultList.Assign(TIcsAsyncDnsLookupRequest(msg.WParam).ResultList);
      {$ENDIF}
        if FDnsResultList.Count > 0 then
            FDnsResult := FDnsResultList[0];
    end;
    TriggerDnsLookupDone(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetProto(sProto : String);
begin
    if FProtoAssigned and (sProto = FProtoStr) then
        Exit;

    if FState <> wsClosed then begin
        RaiseException('Cannot change Proto if not closed');
        Exit;
    end;

    FProtoStr := IcsTrim(sProto);

    if Length(FProtoStr) = 0 then begin
        FProtoAssigned := FALSE;
        Exit;
    end;

    FProtoResolved := FALSE;
    FProtoAssigned := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetRemotePort(sPort : String);
begin
    if FPortAssigned and (FPortStr = sPort) then
        Exit;

    if FState <> wsClosed then begin
        RaiseException('Cannot change Port if not closed');
        Exit;
    end;

    FPortStr := IcsTrim(sPort);

    if Length(FPortStr) = 0 then begin
        FPortAssigned := FALSE;
        Exit;
    end;

    FPortResolved := FALSE;
    FPortAssigned := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetRemotePort : String;
begin
    Result := FPortStr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetLocalPort(const sLocalPort : String);
begin
    if FState <> wsClosed then begin
        RaiseException('Cannot change LocalPort if not closed');
        Exit;
    end;

    FLocalPortStr      := sLocalPort;
    FLocalPortResolved := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetLocalAddr(const sLocalAddr : String);
begin
    if FState <> wsClosed then begin
        RaiseException('Cannot change LocalAddr if not closed');
        Exit;
    end;
    FLocalAddr := IcsTrim(sLocalAddr);
    if FLocalAddr = '' then
      FLocalAddr := ICS_ANY_HOST_V4;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetLocalAddr6(const sLocalAddr6: String);
begin
    if FState <> wsClosed then begin
        RaiseException('Cannot change LocalAddr6 if not closed');
        Exit;
    end;
    FLocalAddr6 := IcsTrim(sLocalAddr6);
    if FLocalAddr6 = '' then
      FLocalAddr6 := ICS_ANY_HOST_V6;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetMultiCastAddrStr(const sMultiCastAddrStr: String);
begin
    FMultiCastAddrStr := IcsTrim(sMultiCastAddrStr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetXPort: String;
var
    saddr    : TSockAddrIn6;
    saddrlen : Integer;
    port     : Integer;
begin
    Result := 'error';
    if FState in [wsConnected, wsBound, wsListening] then begin
        saddrlen := sizeof(saddr);
        if WSocket_Synchronized_GetSockName(FHSocket, PSockAddr(@saddr)^,
                                            saddrlen) = 0 then begin
            port     := WSocket_Synchronized_ntohs(saddr.sin6_port);
            Result   := IntToStr(port);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetXAddr: String;
var
    saddr    : TSockAddrIn6;
    saddrlen : Integer;
begin
    Result := 'error';
    if FState in [wsConnected, wsBound, wsListening] then begin
        saddrlen := sizeof(saddr);
        if WSocket_Synchronized_GetSockName(FHSocket, PSockAddr(@saddr)^,
                                            saddrlen) = 0 then
        begin
            Result := WSocketSockAddrToStr(saddr);    { V8.71 }
         {   if saddr.sin6_family = AF_INET then
                Result := WSocketIPv4ToStr(PSockAddrIn(@saddr)^.sin_addr.S_addr)
            else
                Result := WSocketIPv6ToStr(@saddr);
            //Result := String(WSocket_Synchronized_inet_ntoa(saddr.sin_addr));  }
        end;
     end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetAddr(const InAddr : String);
var
    LSocketFamily: TSocketFamily;
begin
    if FAddrAssigned and (FAddrStr = InAddr) then
        Exit;

    if FState <> wsClosed then begin
        RaiseException('Cannot change Addr if not closed');
        Exit;
    end;

    FAddrStr := IcsTrim(InAddr);

    if Length(FAddrStr) = 0 then begin
        FAddrAssigned := FALSE;
        Exit;
    end;

    { If the address is either a valid IPv4 or IPv6 address }
    { change current SocketFamily.                          }
    if WSocketIsIP(FAddrStr, LSocketFamily) then
    begin
        if (LSocketFamily = sfIPv4) or (IsIPv6APIAvailable) then
            FSocketFamily := LSocketFamily
        else
            FSocketFamily := FOldSocketFamily;
    end
    else
        FSocketFamily := FOldSocketFamily;

    FAddrResolved       := FALSE;
    FAddrAssigned       := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.RegisterIcsAsyncDnsLookup;
var
    PP: PPointer;
begin
    if FAsyncLookupPtr = nil then begin
        GThreadLocalStore.Lock;
        try
            FLookupThreadID := IcsGetCurrentThreadID;
            PP := GThreadLocalStore.RegisterStore(FLookupThreadID);
            if (PP^ = nil) then
                PP^ := TIcsAsyncDnsLookup.Create(CpuCount);
            FAsyncLookupPtr := PP^;
        finally
            GThreadLocalStore.Unlock;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.UnRegisterIcsAsyncDnsLookup;
begin
    if FAsyncLookupPtr <> nil then begin
        GThreadLocalStore.Lock;
        try
            if GThreadLocalStore.UnregisterStore(FLookupThreadID) = FAsyncLookupPtr then
                FreeAndNil(TObject(FAsyncLookupPtr));
        finally
            GThreadLocalStore.Unlock;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetMinMaxIcsAsyncDnsLookupThreads(
  AMinThreads : Byte; AMaxThreads : Byte);
begin
    if FAsyncLookupPtr = nil then
        RegisterIcsAsyncDnsLookup;
    TIcsAsyncDnsLookup(FAsyncLookupPtr).SetMinMaxThreads(AMinThreads, AMaxThreads);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.IcsAsyncGetHostByName(AWnd: HWND; AMsgID: UINT;
  const ASocketFamily: TSocketFamily; const AName: String;
  const AProtocol: Integer): THandle;
begin
    if FAsyncLookupPtr = nil then
        RegisterIcsAsyncDnsLookup;
    Result := TIcsAsyncDnsLookup(FAsyncLookupPtr).ExecAsync(
                          AWnd, AMsgID, ASocketFamily, AName, FALSE, AProtocol);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.IcsAsyncGetHostByAddr(AWnd: HWND; AMsgID: UINT;
  const ASocketFamily: TSocketFamily; const AAddr: String;
  const AProtocol: Integer): THandle;
begin
    if FAsyncLookupPtr = nil then
        RegisterIcsAsyncDnsLookup;
    Result := TIcsAsyncDnsLookup(FAsyncLookupPtr).ExecAsync(
                           AWnd, AMsgID, ASocketFamily, AAddr, TRUE, AProtocol);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.IcsCancelAsyncRequest(
  const ARequest: THandle): Integer;
begin
    if FAsyncLookupPtr <> nil then
        Result := TIcsAsyncDnsLookup(FAsyncLookupPtr).CancelAsyncRequest(ARequest)
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocket_Synchronized_ResolveHost(
    const AHostName  : string;
    var ASockAddrIn6 : TSockAddrIn6;
    const AFamily    : TSocketFamily;
    const AProtocol: Integer); overload;
var
    Hints     : TAddrInfo;
    AddrInfo  : PAddrInfo;
    NextInfo  : PAddrInfo;
    RetVal    : Integer;
    IPv4Addr  : TIcsIPv4Address;  { V8.71 was Cardinal }
    Success   : Boolean;
    PInfo     : PAddrInfo;
begin
    if AHostName = '' then
        raise ESocketException.Create('WSocket Resolve Host: Invalid Hostname');
    if (AFamily <> sfIPv6) and WSocketIsIPv4(AHostName) then
    begin
        { Address is a dotted numeric IPv4 address like 192.161.124.32 }
        ASockAddrIn6.sin6_family := AF_INET;
        IPv4Addr := WSocketStrToIPv4(AHostName, Success);
        if Cardinal(IPv4Addr) = INADDR_NONE then      { V8.71 }
        begin
            if AHostName = ICS_BROADCAST_V4 then
            begin
                PSockAddrIn(@ASockAddrIn6)^.sin_addr.S_addr := INADDR_BROADCAST;   { V9.4 }
                Exit;
            end;
            raise ESocketException.Create('Winsock Resolve Host: ''' + AHostName + ''' Invalid IP address.');
        end;
        PSockAddrIn(@ASockAddrIn6)^.sin_addr.S_addr := IPv4Addr;
        Exit;
    end
    else begin
            FillChar(Hints, SizeOf(Hints), 0);
            if AFamily = sfIPv4 then
                Hints.ai_family := AF_INET
            else if AFamily = sfIPv6 then
                Hints.ai_family := AF_INET6;
            {else
                Hints.ai_family := AF_UNSPEC;} // = 0 anyway

            AddrInfo := nil;
            Hints.ai_protocol := AProtocol;
          {$IFDEF POSIX}
            RetVal   := WSocket_Synchronized_GetAddrInfo(PAnsiChar(UnicodeToAnsi(AHostName, CP_UTF8)),
                                                         nil, @Hints, AddrInfo);
          {$ELSE}
            RetVal   := WSocket_Synchronized_GetAddrInfo(PChar(AHostName),
                                                         nil, @Hints, AddrInfo);
          {$ENDIF}
            if RetVal <> 0 then
                raise ESocketException.Create(
                 'Winsock Resolve Host: Cannot convert host address ''' +
                 AHostName + ''' - ' + GetWinsockErr(RetVal));
            try
                PInfo:= nil;
                NextInfo := AddrInfo;
                while NextInfo <> nil do
                begin
                    if NextInfo.ai_family = AF_INET then
                    begin
                        if (AFamily = sfIPv4) or (AFamily = sfAnyIPv4) or
                           (AFamily = sfAny) then
                        begin
                            ASockAddrIn6.sin6_family := NextInfo.ai_family;
                            PSockAddrIn(@ASockAddrIn6)^.sin_addr := PSockAddrIn(NextInfo.ai_addr).sin_addr;
                            Exit;
                        end;
                        if PInfo = nil then
                            PInfo := NextInfo;
                    end
                    else if NextInfo.ai_family = AF_INET6 then
                    begin
                        if (AFamily = sfIPv6) or (AFamily = sfAnyIPv6) or
                           (AFamily = sfAny) then
                        begin
                            ASockAddrIn6.sin6_family := NextInfo.ai_family;
                            ASockAddrIn6.sin6_addr := PSockAddrIn6(NextInfo.ai_addr)^.sin6_addr;
                            ASockAddrIn6.sin6_scope_id := PSockAddrIn6(NextInfo.ai_addr)^.sin6_scope_id;
                            Exit;
                        end;
                        if PInfo = nil then
                            PInfo := NextInfo;
                    end;

                    NextInfo := NextInfo.ai_next;
                end;

                if ((AFamily = sfAnyIPv6) or (AFamily = sfAnyIPv4)) and
                   (PInfo <> nil) then
                begin
                    ASockAddrIn6.sin6_family := PInfo.ai_family;
                    if PInfo.ai_family = AF_INET6 then
                    begin
                        ASockAddrIn6.sin6_addr := PSockAddrIn6(PInfo.ai_addr)^.sin6_addr;
                        ASockAddrIn6.sin6_scope_id := PSockAddrIn6(PInfo.ai_addr)^.sin6_scope_id;
                    end
                    else
                        PSockAddrIn(@ASockAddrIn6)^.sin_addr := PSockAddrIn(PInfo.ai_addr).sin_addr;
                end
                else
                    raise ESocketException.Create(
                           'Winsock Resolve Host: Cannot convert host address ''' +
                            AHostName + '''');
            finally
                WSocket_Synchronized_FreeAddrInfo(AddrInfo);
            end;
    end;
end;


(* V9.3 moved to OverbyteIcsTypes
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsIPv6AddrFromAddrInfo(AddrInfo: PAddrInfo): TIcsIPv6Address; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := PIcsIPv6Address(@PSockAddrIn6(AddrInfo^.ai_addr)^.sin6_addr)^;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ find family for Windows APIs from TSocketFamily }
function  WSocketFamilyToAF(AFamily: TSocketFamily): Integer;         { V8.71 }
begin
   if AFamily in [sfIPv4, sfAnyIPv4] then
        Result := AF_INET
    else if AFamily in [sfIPv6, sfAnyIPv6] then
        Result := AF_INET6
    else
        Result := AF_UNSPEC;
end;

*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ResolveHost(InAddr : AnsiString) : TInAddr; overload;
var
    Phe     : Phostent;
    IPAddr  : u_long;
begin
    if InAddr = '' then
      {  raise ESocketException.Create('WSocketResolveHost: ''' + InAddr + ''' Invalid Hostname.'); }
        raise ESocketException.Create('Winsock Resolve Host: ''' + String(InAddr) + ''' Invalid Hostname.');   { V5.26 }

    if WSocketIsDottedIP(InAddr) then begin
        { Address is a dotted numeric address like 192.161.124.32 }
        IPAddr := WSocket_Synchronized_inet_addr(PAnsiChar(InAddr));
        if IPAddr = u_long(INADDR_NONE) then begin
            if InAddr = ICS_BROADCAST_V4 then begin
                Result.s_addr := u_long(INADDR_BROADCAST);
                Exit;
            end;
            raise ESocketException.Create('Winsock Resolve Host: ''' + String(InAddr) +
                                         ''' Invalid IP address.');   { V5.26 }
        end;
        Result.s_addr := IPAddr;
        Exit;
    end;
    { Address is a hostname }
    Phe := WSocket_Synchronized_GetHostByName(PAnsiChar(InAddr));
    if Phe = nil then
        raise ESocketException.Create(
                 'Winsock Resolve Host: Cannot convert host address ''' +
                 String(InAddr) + ''' - ' +
                 GetWinsockErr(WSocket_Synchronized_WSAGetLastError));
    Result.s_addr := PInAddr(Phe^.h_addr_list^)^.s_addr;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ResolveHost(InAddr : UnicodeString) : TInAddr; overload;    { V8.70 }
begin
     Result := WSocket_Synchronized_ResolveHost(AnsiString(InAddr));
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketResolveHost(InAddr : AnsiString) : TInAddr;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_ResolveHost(InAddr);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketResolveHost(InAddr : UnicodeString) : TInAddr;     { V8.70 }
begin
    Result := WSocketResolveHost(AnsiString(InAddr));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocketResolveHost(const AHostName: string; var AAddr: TSockAddrIn6;
                                                            const ASocketFamily: TSocketFamily; const AProtocol: Integer);
begin
    {$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        WSocket_Synchronized_ResolveHost(AHostName, AAddr, ASocketFamily, AProtocol);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ convert IPv4 or IPv6 address into TSockAddrIn6, result family AF_UNSPEC for failure }
function  WSocketIPAddrToSocAddr(const IPAddr: String): TSockAddrIn6;             { V8.71 }
var
    SocFamily: TSocketFamily;
begin
    FillChar(Result, SizeOf(TSockAddrIn6), 0);
    Result.sin6_family := AF_UNSPEC;
    if NOT WSocketIsIPEx(IPAddr, SocFamily) then
        Exit;
    WSocketResolveHost(IPAddr, Result, SocFamily, 0);
    Result.sin6_port := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Convert port name or number to number in host order (ftp -> 21)           }
function WSocket_Synchronized_ResolvePort(Port : AnsiString; Proto : AnsiString) : WORD; overload;
var
    Pse      : Pservent;
begin
    if Port = '' then
      { raise ESocketException.Create('WSocketResolvePort: Invalid Port.'); }
        raise ESocketException.Create('Winsock Resolve Port: Invalid Port.');

    if Proto = '' then
        raise ESocketException.Create('Winsock Resolve Port: Invalid Proto.');

    if IsDigit(Port[1]) then
        Result := atoi(Port)
    else begin
        Pse := WSocket_Synchronized_GetServByName(PAnsiChar(Port), PAnsiChar(Proto));
        if Pse = nil then
            raise ESocketException.Create(
                      'Winsock Resolve Port: Cannot convert port ''' +
                      String(Port) + ''' - ' +
                      GetWinsockErr(WSocket_Synchronized_WSAGetLastError)); { V5.26 }
        Result := WSocket_Synchronized_ntohs(Pse^.s_port);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ResolvePort(Port : UnicodeString; Proto : UnicodeString) : WORD;   overload;       { V8.70 }
begin
    Result := WSocket_Synchronized_ResolvePort(AnsiString(Port), AnsiString(Proto));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Convert port name or number to number in host order (ftp -> 21)           }
function WSocketResolvePort(Port : AnsiString; Proto : AnsiString) : Word;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_ResolvePort(Port, Proto);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  WSocketResolvePort(Port : UnicodeString; Proto : UnicodeString) : Word;   { V8.70 }
begin
    Result := WSocketResolvePort(AnsiString(Port), AnsiString(Proto));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ResolveProto(sProto : AnsiString) : Integer; overload;
var
    Ppe     : Pprotoent;
begin
    if sProto = '' then
        raise ESocketException.Create('Winsock Resolve Proto: Invalid Protocol.');  { V5.26 }

    if IsDigit(sProto[1]) then
        Result := atoi(sProto)
    else begin
        sProto := IcsLowerCase(IcsTrim(sProto));
        if sProto = 'tcp' then
            Result := IPPROTO_TCP
        else if sProto = 'udp' then
            Result := IPPROTO_UDP
        else if sProto = 'raw' then
            Result := IPPROTO_RAW
        else begin
            ppe := WSocket_Synchronized_getprotobyname(PAnsiChar(sProto));
            if Ppe = nil then
                raise ESocketException.Create(
                          'Winsock Resolve Proto: Cannot convert protocol ''' +
                          String(sProto) + ''' - ' +
                          GetWinsockErr(WSocket_Synchronized_WSAGetLastError));    { V5.26 }
            Result := ppe^.p_proto;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ResolveProto(sProto : UnicodeString) : Integer; overload;    { V8.70 }
begin
    Result := WSocket_Synchronized_ResolveProto(AnsiString(sProto));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketResolveProto(sProto : AnsiString) : Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_ResolveProto(sProto);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketResolveProto(sProto : UnicodeString) : Integer;               { V8.70 }
begin
    Result := WSocketResolveProto(AnsiString(sProto));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetSockName(var saddr : TSockAddrIn; var saddrlen : Integer) : Integer;
begin
    Result := WSocket_Synchronized_GetSockName(FHSocket, saddr, saddrlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetPeerAddr: String;
var
    saddr    : TSockAddrIn6;
    saddrlen : Integer;
begin
    Result := 'error';
    if FState = wsConnected then begin
        saddrlen := sizeof(saddr);
        if WSocket_Synchronized_GetPeerName(FHSocket,
            PSockAddrIn(@saddr)^, saddrlen) = 0 then
        begin
            Result := WSocketSockAddrToStr(saddr);    { V8.71 }
         {   if saddr.sin6_family = AF_INET then
                Result := WSocketIPv4ToStr(PSockAddrIn(@saddr)^.sin_addr.S_addr)
            else
                Result := WSocketIPv6ToStr(@saddr);  }
        end
        else begin
            SocketError('GetPeerAddr');  { V8.51 corrected literal }
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetPeerPort: String;
var
    saddr    : TSockAddrIn6;
    saddrlen : Integer;
begin
    Result := 'error';
    if FState = wsConnected then begin
        saddrlen := sizeof(saddr);
        if WSocket_Synchronized_GetPeerName(FHSocket, PSockAddrIn(@saddr)^,
                                            saddrlen) = 0 then
            Result := IntToStr(WSocket_Synchronized_ntohs(saddr.sin6_port))
        else begin
            SocketError('GetPeerPort');
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetPeerName(var Name : TSockAddrIn; NameLen : Integer) : Integer;
begin
    if FState = wsConnected then
        Result := WSocket_Synchronized_GetPeerName(FHSocket, Name, NameLen)
    else
        Result := SOCKET_ERROR;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.InternalCancelDnsLookup(IgnoreErrors: Boolean);
var
    RetVal: Integer;
begin
    if FDnsLookupHandle = 0 then
        Exit;
    FInternalDnsActive := FALSE;     { V8.43 }
  {$IFDEF MSWINDOWS}
    if (FSocketFamily = sfIPv4) and (not HasOption(FComponentOptions, wsoIcsDnsLookup)) then   { V8.43, V8.70 }
        RetVal := WSocket_Synchronized_WSACancelAsyncRequest(FDnsLookupHandle)
    else
  {$ENDIF}
        RetVal := IcsCancelAsyncRequest(FDnsLookupHandle);
    if (RetVal <> 0) and (not IgnoreErrors) then begin
        FDnsLookupHandle := 0;
        SocketError('WSACancelAsyncRequest');
        Exit;
    end;
    FDnsLookupHandle := 0;
    if not (csDestroying in ComponentState) then begin
        TriggerDnsLookupDone(WSAEINTR);
        if FState = wsDnsLookup then
            ChangeState(wsClosed);  { V8.48 }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.CancelDnsLookup;
begin
    InternalCancelDnsLookup(FALSE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DnsLookup(const AHostName : String);
begin
    DnsLookup(AHostName, IPPROTO_TCP);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DnsLookup(const AHostName : String;
  const AProtocol: Integer);
var
    IPAddr   : TInAddr;
    IPv6Addr : TIcsIPv6Address;
//  HostName    : AnsiString;
    Success  : Boolean;
    ScopeID  : Cardinal;
    Err      : integer;
    ErrFlag  : Boolean;   { V8.64 }
begin
    if AHostName = '' then begin
        try
            RaiseException('DNS lookup: invalid host name.');
        finally   { V8.36 }
            TriggerDnsLookupDone(WSAEINVAL);
        end;
        Exit;
    end;

    { Cancel any pending lookup }
    if FDnsLookupHandle <> 0 then begin
    {$IFDEF MSWINDOWS}
        if (FSocketFamily = sfIPv4) and (not HasOption(FComponentOptions, wsoIcsDnsLookup)) then   { V8.43, V8.70 }
            WSocket_Synchronized_WSACancelAsyncRequest(FDnsLookupHandle)
        else
    {$ENDIF}
            IcsCancelAsyncRequest(FDnsLookupHandle);
        FDnsLookupHandle := 0;
        FInternalDnsActive := FALSE;      { V8.43 }
    end;

    FDnsResult := '';
    FDnsResultList.Clear;

  { V8.64 see if passed an IPv6 address, IDN can not cope }
    if (FSocketFamily <> sfIPv4) then
    begin
        IPv6Addr := WSocketStrToIPv6(IcsTrim(AHostName), Success, ScopeID);
        if Success and (ScopeID = 0) then
        begin
            FPunycodeHost := AHostName;   { V8.64 }
            FDnsResult := WSocketIPv6ToStr(IPv6Addr);
            FDnsResultList.Add(FDnsResult);
            TriggerDnsLookupDone(0);
            Exit;
        end;
    end;

  { V8.64 convert Unicode International Domain Name into Punycode ASCII }
    if HasOption(FComponentOptions, wsoIgnoreIDNA) then                        { V8.70 }
        FPunycodeHost := IcsTrim(AHostName)  // convert to ANSI, backward compatible
    else begin
        FPunycodeHost := IcsIDNAToASCII(IcsTrim(AHostName), HasOption(FComponentOptions, wsoUseSTD3AsciiRules), ErrFlag);   { V8.70 }
        if ErrFlag then begin
            FPunycodeHost := '';
         // don't raise exception since previously this would be a host not found error
         //   RaiseException(String(AHostName) + ': can''t start DNS lookup - invalid host name');
            TriggerDnsLookupDone(WSAEINVAL);
            Exit;
        end;
    end;

    if (FSocketFamily <> sfIPv6) and
       WSocketIsDottedIP(FPunycodeHost) then begin                              { V8.70 }
        IPAddr.S_addr := WSocket_Synchronized_inet_addr(FPunycodeHost);          { V8.70 }
        if IPAddr.S_addr <> u_long(INADDR_NONE) then begin
            FDnsResult := String(WSocket_Synchronized_inet_ntoa(IPAddr));
            FDnsResultList.Add(FDnsResult);     { 28/09/2002 }{ 12/02/2003 }
            TriggerDnsLookupDone(0);
            Exit;
        end;
    end;

    if FWindowHandle = 0 then begin
        RaiseException('DnsLookup: Window not assigned');
        TriggerDnsLookupDone(WSAEINVAL);  { V8.64 }
        Exit;   { V8.36 }
    end;

    { John Goodwin found a case where winsock dispatch WM_ASYNCGETHOSTBYNAME }
    { message before returning from WSAAsyncGetHostByName call. Because of   }
    { that, FDnsLookupHandle is not yet assigned when execution comes in     }
    { WMAsyncGetHostByName. John use a flag to check this situation.         }
    FDnsLookupCheckMsg := FALSE;

  {$IFDEF MSWINDOWS}
   { V8.43 new option for IPv4 DNS lookups to be done using thread, previously
     only IPv6 used thread.  This avoids windows limitation of one lookup at a time }
//    HostName := AnsiString(FPunycodeHost);     { V8.64 }
    if (FSocketFamily = sfIPv4) and (not HasOption(FComponentOptions, wsoIcsDnsLookup)) then     { V8.70 }
        FDnsLookupHandle   := WSocket_Synchronized_WSAAsyncGetHostByName(
                                  FWindowHandle,
                                  FMsg_WM_ASYNCGETHOSTBYNAME,
                                  PAnsiChar(AnsiString(FPunycodeHost)),   { V8.64 }
                               //   @HostName[1],
                                  @FDnsLookupBuffer,
                                  SizeOf(FDnsLookupBuffer))
    else
  {$ENDIF}
        FDnsLookupHandle   := IcsAsyncGetHostByName(
                                  FWindowHandle,
                                  FMsg_WM_ASYNCGETHOSTBYNAME,
                                  FSocketFamily,
                                  FPunycodeHost,          { V8.64 }
                                  AProtocol);

    if FDnsLookupHandle = 0 then begin
        Err := WSocket_Synchronized_WSAGetLastError;
        RaiseException(FPunycodeHost + ': can''t start DNS lookup - ' +
                                                GetWinsockErr(Err), Err);  { V5.26, V8.36 }
        TriggerDnsLookupDone(WSAEINVAL);   { V8.64 }
        Exit;
    end;
    if FDnsLookupCheckMsg then begin
        FDnsLookupCheckMsg := FALSE;
        WMAsyncGetHostByName(FDnsLookupTempMsg);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ReverseDnsLookup(const HostAddr: String);
begin
    ReverseDnsLookup(HostAddr, IPPROTO_TCP);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ReverseDnsLookup(const HostAddr: String;
  const AProtocol: Integer);
{$IFDEF MSWINDOWS}
var
    lAddr  : u_long;
{$ENDIF}
begin
    if HostAddr = '' then begin
        try
            RaiseException('Reverse DNS Lookup: Invalid host name.'); { V5.26 }
        finally    { V8.36 }
            TriggerDnsLookupDone(WSAEINVAL);
        end;
        Exit;
    end;
    { Cancel any pending lookup }
    if FDnsLookupHandle <> 0 then begin
      {$IFDEF MSWINDOWS}
        if (FSocketFamily = sfIPv4) and (not HasOption(FComponentOptions, wsoIcsDnsLookup)) then  { V8.44, V8.70 }
            WSocket_Synchronized_WSACancelAsyncRequest(FDnsLookupHandle)
        else
      {$ENDIF}
            IcsCancelAsyncRequest(FDnsLookupHandle);
        FDnsLookupHandle := 0;
        FInternalDnsActive := FALSE;      { V8.43 }
    end;

    FDnsResult := '';
    FDnsResultList.Clear;
  {$IFDEF MSWINDOWS}
    if (FSocketFamily = sfIPv4) and (not HasOption(FComponentOptions, wsoIcsDnsLookup)) then  { V8.44, V8.70 }
        lAddr := WSocket_Synchronized_inet_addr(HostAddr);                                    { V8.70 }
  {$ENDIF}
    if FWindowHandle = 0 then begin
        RaiseException('Reverse DNS Lookup: Window not assigned');  { V5.26 }
        exit;  { V8.36 }
    end;
  {$IFDEF MSWINDOWS}
   { V8.44 new option for IPv4 DNS lookups to be done using thread, previously
     only IPv6 used thread.  This avoids windows limitation of one lookup at a time }
    if (FSocketFamily = sfIPv4) and (not HasOption(FComponentOptions, wsoIcsDnsLookup)) then    { V8.70 }
        FDnsLookupHandle := WSocket_Synchronized_WSAAsyncGetHostByAddr(
                            FWindowHandle,
                            FMsg_WM_ASYNCGETHOSTBYADDR,
                            PAnsiChar(@lAddr), 4, PF_INET,
                            @FDnsLookupBuffer,
                            SizeOf(FDnsLookupBuffer))
    else
  {$ENDIF}
        FDnsLookupHandle := IcsAsyncGetHostByAddr(
                            FWindowHandle,
                            FMsg_WM_ASYNCGETHOSTBYADDR,
                            FSocketFamily,
                            IcsTrim(HostAddr),
                            AProtocol);

    if FDnsLookupHandle = 0 then
        RaiseException(HostAddr + ': can''t start reverse DNS lookup - ' +
                       GetWinsockErr(WSocket_Synchronized_WSAGetLastError));  { V5.26 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ReverseDnsLookupSync(const HostAddr: String);
begin
    ReverseDnsLookupSync(HostAddr, IPPROTO_TCP);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ReverseDnsLookupSync(const HostAddr: String;
  const AProtocol: Integer); {AG 03/03/06}
var
    szAddr : array [0..256] of AnsiChar;
    lAddr  : u_long;
    Phe    : Phostent;
begin
    if (Length(HostAddr) = 0) or (Length(HostAddr) >= SizeOf(szAddr)) then begin
        try
            RaiseException('Reverse DNS Lookup: Invalid host name.');   { V5.26 }
        finally   { V8.36 }
            TriggerDnsLookupDone(WSAEINVAL);
        end;
        Exit;
    end;
    { Cancel any pending lookup }
    if FDnsLookupHandle <> 0 then
    begin
      {$IFDEF MSWINDOWS}
        if (FSocketFamily = sfIPv4) and (not HasOption(FComponentOptions, wsoIcsDnsLookup)) then  { V8.44, V8.70 }
            WSocket_Synchronized_WSACancelAsyncRequest(FDnsLookupHandle)
        else
      {$ENDIF}
            IcsCancelAsyncRequest(FDnsLookupHandle);
      FDnsLookupHandle := 0;
      FInternalDnsActive := FALSE;        { V8.43 }
    end;
    FDnsResult := '';
    if FSocketFamily = sfIPv4 then
    begin
        FDnsResultList.Clear;
        StrPCopy(szAddr, AnsiString(HostAddr)); { Length already checked above }

        lAddr := WSocket_Synchronized_inet_addr(szAddr);

        Phe := WSocket_Synchronized_gethostbyaddr(PAnsiChar(@lAddr), 4, AF_INET);
        if Phe = nil then
            TriggerDnsLookupDone(WSocket_Synchronized_WSAGetLastError)
        else begin
          {$IFDEF POSIX}
            {$IFDEF DELPHI23_UP}          { V8.21 }
            FDnsResult := String(StrPas(Phe^.h_name));
            {$ELSE}
            FDnsResult := String(StrPas(Phe^.hname)); // Typo in Posix header
            {$ENDIF}
          {$ELSE}
            FDnsResult := String(StrPas(Phe^.h_name));
          {$ENDIF}
          { V8.64 if result has ACE xn--. convert it to Unicode, ignore errors }
            FDnsResult := IcsIDNAToUnicode(FDnsResult);
            FDnsResultList.Add(FDnsResult);
            GetAliasList(Phe, FDnsResultList);
            TriggerDnsLookupDone(0);
        end;
    end
    else begin
        lAddr := WSocket_Synchronized_ResolveName(HostAddr, TRUE, FSocketFamily,
                                                  FDnsResultList, AProtocol);
        if lAddr <> 0 then
            TriggerDnsLookupDone(lAddr)
        else begin
            if FDnsResultList.Count > 0 then begin
                FDnsResult := FDnsResultList[0];
            end;
            TriggerDnsLookupDone(0);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.BindSocket;
var
    SockName      : TSockAddrIn6;
    SockNamelen   : Integer;
    LocalSockName : TSockAddrIn6;
    LLocalAddr    : String;
    LSocketFamily : TSocketFamily;
    ErrorCode     : integer;
    FriendlyMsg   : String;
begin
    if FAddrFormat = AF_INET6 then // requires Addr being resolved
    begin
        LSocketFamily := sfIPv6;
        LLocalAddr := FLocalAddr6;
    end
    else begin
        LSocketFamily := sfIPv4;
        LLocalAddr := FLocalAddr;
    end;
    FillChar(LocalSockName, Sizeof(LocalSockName), 0);
    if FSocketFamily = sfIPv4 then begin
        LocalSockName.sin6_family      := AF_INET;
        LocalSockName.sin6_port        := WSocket_Synchronized_htons(FLocalPortNum);
        PSockAddrIn(@LocalSockName)^.sin_addr.S_addr := WSocket_Synchronized_ResolveHost(LLocalAddr).s_addr;  { V8.70 }
    end
    else begin
        WSocket_Synchronized_ResolveHost(LLocalAddr, LocalSockName, LSocketFamily, FProto);
        LocalSockName.sin6_port := WSocket_Synchronized_htons(FLocalPortNum);
    end;
    SockNamelen := IcsSizeOfAddr(LocalSockName);    { V9.5 }
    if WSocket_Synchronized_bind(HSocket, PSockAddrIn(@LocalSockName)^, SockNamelen) <> 0 then begin
        ErrorCode := WSocket_Synchronized_WSAGetLastError;
        if (ErrorCode = WSAEADDRINUSE) or (ErrorCode = WSAEACCES)  then   { V8.36 more friendly message for common error }
            FriendlyMsg := 'Another client is using address ' +
                                LLocalAddr + ':' + IntToStr(FLocalPortNum) ;
        RaiseException('Bind socket failed - ' + GetWinsockErr(ErrorCode),
                   ErrorCode, '', FriendlyMsg, 'Bind Socket',
                            LLocalAddr, IntToStr(FLocalPortNum), FProtoStr);  { V8.36 }
        Exit;
    end;
    if WSocket_Synchronized_GetSockName(FHSocket, PSockAddrIn(@SockName)^, SockNamelen) <> 0 then begin
        RaiseException('Winsock get socket name failed - ' +
                       GetWinsockErr(WSocket_Synchronized_WSAGetLastError));  { V5.26 }
        Exit;
    end;
    FLocalPortNum := WSocket_Synchronized_ntohs(SockName.sin6_port);
    FLocalPortStr := IntToStr(FLocalPortNum);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetKeepAliveOption;
var
    OptVal        : Integer;
    Status        : Integer;
  {$IFDEF MSWINDOWS}
    KeepAliveIn   : TTcpKeepAlive;
    KeepAliveOut  : TTcpKeepAlive;
    BytesReturned : Cardinal;
  {$ENDIF}
begin
    if FKeepAliveOnOff = wsKeepAliveOff then
        Exit;
    Assert(FHSocket <> INVALID_SOCKET); { V7.27 }
    if FKeepAliveOnOff = wsKeepAliveOnSystem then begin
        OptVal := 1;
        Status := WSocket_Synchronized_setsockopt(FHSocket, SOL_SOCKET,
                                                  SO_KEEPALIVE, @OptVal,
                                                  SizeOf(OptVal));

        if Status <> 0 then
            SocketError('setsockopt(SO_KEEPALIVE)');
        Exit;
    end;
  {$IFDEF MSWINDOWS}
    FillChar(KeepAliveIn, SizeOf(KeepAliveIn), 0);
    FillChar(KeepAliveOut, SizeOf(KeepAliveOut), 0);
    BytesReturned := 0;

    KeepAliveIn.OnOff             := 1;
    KeepAliveIn.KeepAliveInterval := FKeepAliveInterval;
    KeepAliveIn.KeepAliveTime     := FKeepAliveTime;
    Status := WSocket_WSAIoctl(FHSocket,      SIO_KEEPALIVE_VALS,
                               @KeepAliveIn,  SizeOf(KeepAliveIn),
                               @KeepAliveOut, SizeOf(KeepAliveOut),
                               BytesReturned, nil, nil);
    if Status <> 0 then begin
        SocketError('WSocket_WSAIoctl(SIO_KEEPALIVE_VALS)');
        Exit;
    end;
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetLingerOption;
var
    iStatus : Integer;
    li      : TLinger;
begin
    if FLingerOnOff = wsLingerNoSet then
        Exit;                            { Option set is disabled, ignore }

    if FHSocket = INVALID_SOCKET then begin
        RaiseException('Cannot set linger option at this time');
        Exit;
    end;

    li.l_onoff  := Ord(FLingerOnOff);    { 0/1 = disable/enable linger }
    li.l_linger := FLingerTimeout;       { timeout in seconds          }
    iStatus     := WSocket_Synchronized_setsockopt(FHSocket, SOL_SOCKET,
                                      SO_LINGER, @li, SizeOf(li));

    if iStatus <> 0 then begin
        SocketError('setsockopt(SO_LINGER)');
        Exit;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.SetTcpNoDelayOption: Boolean; { V7.27 }
var
    optval  : Integer;
begin
    Assert(FHSocket <> INVALID_SOCKET);
    if HasOption(FComponentOptions, wsoTcpNoDelay) then
        optval := -1 { true }
    else
        optval := 0; { false }
    Result := WSocket_Synchronized_setsockopt(FHSocket, IPPROTO_TCP,
                                              TCP_NODELAY,
                                              @optval, SizeOf(optval)) = 0;
    if not Result then
        SocketError('setsockopt(IPPROTO_TCP, TCP_NODELAY)');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.SetAddressListChangeNotification: Boolean;
{$IFDEF MSWINDOWS}
var
    LBytesRcvd : Cardinal;
begin
    if FHSocket <> INVALID_SOCKET then begin
        if FSelectEvent and FD_ADDRESS_LIST_CHANGE = 0 then begin
            FSelectEvent := FSelectEvent or FD_ADDRESS_LIST_CHANGE;
            Result := WSocket_Synchronized_WSAASyncSelect(FHSocket,
                                                          Handle,
                                                          FMsg_WM_ASYNCSELECT,
                                                          FSelectEvent) <> SOCKET_ERROR;
        end
        else
            Result := True;
        if Result then
            Result := (WSocket_WSAIoctl(FHSocket, SIO_ADDRESS_LIST_CHANGE, nil, 0,
                                        nil, 0, LBytesRcvd, nil, nil) <> SOCKET_ERROR) or
                      (WSocket_Synchronized_WSAGetLastError = WSAEWOULDBLOCK);
    end
    else
        Result := True;
{$ENDIF}
{$IFDEF POSIX}
begin
    Result := False;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.SetRoutingInterfaceChangeNotification: Boolean;
{$IFDEF MSWINDOWS}
var
    LBytesRcvd : Cardinal;
begin
    if FHSocket <> INVALID_SOCKET then begin
        if FSelectEvent and FD_ROUTING_INTERFACE_CHANGE = 0 then begin
            FSelectEvent := FSelectEvent or FD_ROUTING_INTERFACE_CHANGE;
            Result := WSocket_Synchronized_WSAASyncSelect(FHSocket,
                                                          Handle,
                                                          FMsg_WM_ASYNCSELECT,
                                                          FSelectEvent) <> SOCKET_ERROR;
        end
        else
            Result := True;
        if Result then
            Result := (WSocket_WSAIoctl(FHSocket, SIO_ROUTING_INTERFACE_CHANGE,
                                        @Fsin, IcsSizeOfAddr(Fsin), nil, 0, LBytesRcvd,      { V9.5 }
                                        nil, nil) <> SOCKET_ERROR) or
                      (WSocket_Synchronized_WSAGetLastError = WSAEWOULDBLOCK);
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
procedure TCustomWSocket.Connect;
var
    iStatus : Integer;
    optval  : Integer;
    optlen  : Integer;
    lAddr   : TSockAddrIn6;
    TmpOnError: TNotifyEvent;
    ErrFlag  : boolean;  { V8.64 }
begin
{$IFDEF MSWINDOWS}
    FConnectThreadId := GetCurrentThreadId(); { V9.4 }
{$ENDIF}
    FLastError := 0;            { V8.68 reset to avoid problems later }
    if ((FHSocket <> INVALID_SOCKET) and (NOT (FState in [wsClosed, wsDnsLookup]))) or FInternalDnsActive then begin    { V8.48 }
        RaiseException('Connect: Socket already in use');
        Exit;
    end;

    if  not FPortAssigned then begin
        RaiseException('Connect: No Port Specified');
        Exit;
    end;

    if not FAddrAssigned then begin
        RaiseException('Connect: No IP Address Specified');
        Exit;
    end;

    if not FProtoAssigned then begin
        RaiseException('Connect: No Protocol Specified');
        Exit;
    end;

{ V9.5 clear session connection IP information set after connection }
    ClearSocketIpInfo;

    try
        if not FProtoResolved then begin
            { The next line will trigger an exception in case of failure }
            FProto := WSocket_Synchronized_ResolveProto(FProtoStr);                         { V8.70 }
            case FProto of
                IPPROTO_UDP: FType := SOCK_DGRAM;
                IPPROTO_TCP: FType := SOCK_STREAM;
                IPPROTO_RAW: FType := SOCK_RAW;
            else
                FType := SOCK_RAW;
            end;
            FProtoResolved := TRUE;
        end;

        if not FPortResolved then begin
            { The next line will trigger an exception in case of failure }
            FPortNum       := WSocket_Synchronized_ResolvePort(FPortStr, FProtoStr);         { V8.70 }
            Fsin.sin6_port := WSocket_Synchronized_htons(FPortNum);
            FPortResolved  := TRUE;
        end;

        if not FLocalPortResolved then begin
            { The next line will trigger an exception in case of failure }
            FLocalPortNum      := WSocket_Synchronized_ResolvePort(FLocalPortStr, FProtoStr);    { V8.70 }
            FLocalPortResolved := TRUE;
        end;

       { V8.43 see if doing an async DNS lookup instead of blocking sync DNS lookup }
        if not FAddrResolved then begin
            if HasOption(FComponentOptions, wsoAsyncDnsLookup) and (not WSocketIsDottedIP(FAddrStr)) then begin    { V8.70 }
                  { If Socket.OnError is assigned, any raised exception will be transferred to }
                  { the handler silently. So clear the handler temporarily to catch exception. }
                  TmpOnError := OnError;
                  OnError := nil;
                  { Trigger OnChangeState event }
                  ChangeState(wsDnsLookup);     { V8.48 }
                  try
                      DnsLookup(FAddrStr);
                      FInternalDnsActive := TRUE;
                      Exit; { Actual connect will happen on DNS lookup done }
                  finally
                      OnError := TmpOnError;
                  end;
            end
            else begin

           { V8.64 convert Unicode International Domain Name into ASCII Punycode }
               if HasOption(FComponentOptions, wsoIgnoreIDNA) then                    { V8.70 }
                    FPunycodeHost := String(AnsiString(IcsTrim(FAddrStr)))  // convert to ANSI, backward compatible
               else begin
                    FPunycodeHost := IcsIDNAToASCII(IcsTrim(FAddrStr), HasOption(FComponentOptions, wsoUseSTD3AsciiRules), ErrFlag);   { V8.70 }
                    if ErrFlag then begin
                        FPunycodeHost := '';
                        RaiseException('Connect: Invalid Host Name Specified');
                        Exit;
                    end;
               end;

           { The next line will trigger an exception in case of failure }
              if FSocketFamily = sfIPv4 then
              begin
                  Fsin.sin6_family := AF_INET;
                  PSockAddrIn(@Fsin).sin_addr.S_addr := WSocket_Synchronized_ResolveHost(FPunycodeHost).s_addr;   { V8.64 } { V8.70 }
              end
              else
                  WSocket_Synchronized_ResolveHost(FPunycodeHost, Fsin, FSocketFamily, FProto);   { V8.64 }
            end;
            FAddrResolved := TRUE;
            FAddrFormat := Fsin.sin6_family;
        end;

    except
        on E:Exception do begin
            RaiseException('connect: ' + E.Message);
            Exit;
        end;
    end;

    { V8.60 keep resolved IP address as string, code from debug log later }
    FAddrResolvedStr := WSocketSockAddrToStr(Fsin);    { V8.71 }
    FAddrFormat := Fsin.sin6_family;                   { V9.1 not set if FAddrResolved by DnsLookup }

    { Remove any data from the internal output buffer }
    { (should already be empty !)                     }
    DeleteBufferedData;

    { Open the socket }
    FHSocket := WSocket_Synchronized_socket(FAddrFormat, FType, FProto);
    if FHSocket = INVALID_SOCKET then begin
        SocketError('Connect (socket)');
        Exit;
    end;

  {$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loWsockInfo) then
        DebugLog(loWsockInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) + ' ' + 'Socket handle created handle=' + IntToStr(FHSocket));
  {$ENDIF}

  {$IFDEF MACOS}
    { No SIGPIPE on writes but EPIPE in errno }
    optlen  := SizeOf(Integer);
    optval  := 1;
    iStatus := WSocket_setsockopt(FHSocket, SOL_SOCKET, SO_NOSIGPIPE, PAnsiChar(@optval), optlen);
    if iStatus <> 0 then begin
        SocketError('setsockopt(SO_NOSIGPIPE)');
        Exit;
    end;
  {$ENDIF}

    { Get winsock send buffer size }
    optlen  := SizeOf(FSocketSndBufSize);
    iStatus := WSocket_getsockopt(FHSocket, SOL_SOCKET, SO_SNDBUF, PAnsiChar(@FSocketSndBufSize), optlen);
    if iStatus <> 0 then begin
        SocketError('getsockopt(SO_SNDBUF)');
        Exit;
    end;

    { Get winsock receive buffer size }
    optlen  := SizeOf(FSocketRcvBufSize);
    iStatus := WSocket_getsockopt(FHSocket, SOL_SOCKET, SO_RCVBUF, PAnsiChar(@FSocketRcvBufSize), optlen);
    if iStatus <> 0 then begin
        SocketError('getsockopt(SO_RCVBUF)');
        Exit;
    end;

    { Trigger OnChangeState event }
    ChangeState(wsOpened);

    if FState <> wsOpened then begin  { 07/07/02 }
        { Socket has been closed in the OnChangeState event ! }
        WSocket_Synchronized_WSASetLastError(WSAEINVAL);
        SocketError('Connect (Invalid operation in OnChangeState)');
        Exit;
    end;
    if FType = SOCK_DGRAM then begin
        BindSocket;
        if FMultiCast then begin
            if FMultiCastIpTTL <> IP_DEFAULT_MULTICAST_TTL then begin
                optval  := FMultiCastIpTTL; { set time-to-live for multicast }
                if FAddrFormat = PF_INET then
                    iStatus := WSocket_Synchronized_SetSockOpt(FHSocket, IPPROTO_IP, IP_MULTICAST_TTL, @optval, SizeOf(optval))
                else if FAddrFormat = PF_INET6 then
                    iStatus := WSocket_Synchronized_SetSockOpt(FHSocket, IPPROTO_IPV6, IPV6_MULTICAST_HOPS, @optval, SizeOf(optval))
                else begin
                    SocketError('setsockopt(IP_MULTICAST_TTL) Invalid address format');
                    Exit;
                end;
                if iStatus <> 0 then begin
                    SocketError('setsockopt(IP_MULTICAST_TTL)');
                    Exit;
                end;
            end;
            if ((FAddrFormat = AF_INET) and (FLocalAddr <> ICS_ANY_HOST_V4)) or
                                           ((FAddrFormat = AF_INET6) and (FLocalAddr6 <> ICS_ANY_HOST_V6)) then begin
                if FAddrFormat = PF_INET then
                    PSockAddrIn(@laddr)^.sin_addr.S_addr := WSocket_Synchronized_ResolveHost(FLocalAddr).S_addr          { V8.70 }
                else
                    WSocket_Synchronized_ResolveHost(FLocalAddr6, laddr, sfIPv6, FProto);

                if FAddrFormat = PF_INET then
                    iStatus := WSocket_Synchronized_SetSockOpt(FHSocket, IPPROTO_IP,  IP_MULTICAST_IF,
                                              @PSockAddr(@laddr)^.sin_addr.S_addr, SizeOf(PSockAddr(@laddr)^.sin_addr.S_addr))
                else if FAddrFormat = PF_INET6 then
                begin
                    OptVal := 0; // Default IPv6 interface, fix me!
                    iStatus := WSocket_Synchronized_SetSockOpt(FHSocket, IPPROTO_IPV6, IPV6_MULTICAST_IF,
                                                                                          PAnsiChar(@OptVal), SizeOf(OptVal));
                end
                else begin
                    SocketError('setsockopt(IP_MULTICAST_TTL) Invalid address format');
                        Exit;
                end;

                if iStatus <> 0 then begin
                    SocketError('setsockopt(IP_MULTICAST_IF)');
                    Exit;
                end;
            end;                                                       { /RK }
        end;
        //if sin.sin_addr.S_addr = u_long(INADDR_BROADCAST) then begin
        if ((Fsin.sin6_family = AF_INET) and (PSockAddrIn(@FSin).sin_addr.S_addr = u_long(INADDR_BROADCAST))) or
                    ((Fsin.sin6_family = AF_INET6) and IN6_IS_ADDR_MULTICAST({$IFNDEF POSIX}@{$ENDIF}Fsin.sin6_addr)) then
        begin
            OptVal  := 1;
            iStatus := WSocket_Synchronized_setsockopt(FHSocket, SOL_SOCKET, SO_BROADCAST, @OptVal, SizeOf(OptVal));
            if iStatus <> 0 then begin
                SocketError('setsockopt(SO_BROADCAST)');
                Exit;
            end;
        end;

        FSelectEvent := FD_READ or FD_WRITE;
    end
    else begin
        { Socket type is SOCK_STREAM }
        optval  := -1;
        iStatus := WSocket_Synchronized_setsockopt(FHSocket, SOL_SOCKET, SO_REUSEADDR, @optval, SizeOf(optval));

        if iStatus <> 0 then begin
            SocketError('setsockopt(SO_REUSEADDR)');
            Exit;
        end;

        if HasOption(FComponentOptions, wsoTcpNoDelay) and (not SetTcpNoDelayOption) then
            Exit;
        SetLingerOption;
        SetKeepAliveOption;

        if (FLocalPortNum <> 0) or ((FAddrFormat = AF_INET) and (FLocalAddr <> ICS_ANY_HOST_V4)) or
                                                  ((FAddrFormat = AF_INET6) and (FLocalAddr6 <> ICS_ANY_HOST_V6)) then
            BindSocket;
        FSelectEvent := FD_READ or FD_WRITE or FD_CLOSE or FD_CONNECT;
    end;

  {$IFDEF MSWINDOWS}
    if HasOption(FComponentOptions, wsoNotifyAddressListChange) then            { V8.70 }
        FSelectEvent := FSelectEvent or FD_ADDRESS_LIST_CHANGE;
    if HasOption(FComponentOptions, wsoNotifyRoutingInterfaceChange) then       { V8.70 }
        FSelectEvent := FSelectEvent or FD_ROUTING_INTERFACE_CHANGE;
  {$ENDIF}

    iStatus := WSocket_Synchronized_WSAASyncSelect({$IFDEF POSIX}Self,{$ENDIF} FHSocket, Handle, FMsg_WM_ASYNCSELECT, FSelectEvent);
    if iStatus <> 0 then begin
        SocketError('WSAAsyncSelect');
        Exit;
    end;

  {$IFDEF MSWINDOWS}
    if HasOption(FComponentOptions, wsoNotifyAddressListChange) and (not SetAddressListChangeNotification) then begin    { V8.70 }
        SocketError('Connect: SetAddressListChangeNotification');
        Exit;
    end;

    if HasOption(FComponentOptions, wsoNotifyRoutingInterfaceChange) and (not SetRoutingInterfaceChangeNotification) then begin  { V8.70 }
        SocketError('Connect: SetRoutingInterfaceChangeNotification');
        Exit;
    end;
  {$ENDIF}

    if FType = SOCK_DGRAM then begin
        ChangeState(wsConnected);
        GetSocketIpInfo;       { V9.5 }
        TriggerSessionConnectedSpecial(0);
    end
    else begin
      {$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loWsockInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }

            if Fsin.sin6_family = AF_INET then  { V8.60 kept the string earlier }
                DebugLog(loWsockInfo, 'TWSocket will connect to ' + IcsFmtIpv6AddrPort(FAddrResolvedStr,
                                                              IntToStr(WSocket_Synchronized_ntohs(PSockAddrIn(@Fsin).sin_port))))
            else
                DebugLog(loWsockInfo, 'TWSocket will connect to ' + IcsFmtIpv6AddrPort(FAddrResolvedStr,
                                                                        IntToStr(WSocket_Synchronized_ntohs(Fsin.sin6_port))));
      {$ENDIF}
        ChangeState(wsConnecting);  { V9.4 change state before connect, in case event is called too quickly  !!! TESTING }
        iStatus := WSocket_Synchronized_Connect(FHSocket, PSockAddrIn(@Fsin)^, IcsSizeOfAddr(Fsin));      { V9.5 }
        if iStatus = 0 then begin
          { V9.4 beware Connect may trigger a connected event before the next line is called, so special
               handling in Do_FD_CONNECT waiting for wsOpening to change to wsConnecting }
        //    ChangeState(wsConnecting);
        end
        else begin
            iStatus := WSocket_Synchronized_WSAGetLastError;
            if (iStatus = WSAEWOULDBLOCK) {$IFDEF POSIX} or (iStatus = WSAEINPROGRESS) {$ENDIF} then
                ChangeState(wsConnecting)
            else begin
                ChangeState(wsOpened);  { V9.4 SocketError closes socket }
                FLastError := WSocket_Synchronized_WSAGetLastError;
                SocketError('Connect');
                Exit;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Listen;
var
    iStatus        : Integer;
    optval         : Integer;
    optlen         : Integer;
    mreq           : ip_mreq;
    mreqv6         : TIpv6MReq;
    Success        : Boolean;
    FriendlyMsg    : String;
    ErrorCode      : Integer;
{$IFDEF MSWINDOWS}
    dwBufferInLen  : DWORD;
    dwBufferOutLen : DWORD;
    dwDummy        : DWORD;
{$ENDIF}
begin
    FLastError := 0;            { V8.68 reset to avoid problems later }
    FriendlyMsg := '';
    if not FPortAssigned then begin
        WSocket_Synchronized_WSASetLastError(WSAEINVAL);
        SocketError('listen: port not assigned');
        Exit;
    end;

    if not FProtoAssigned then begin
        WSocket_Synchronized_WSASetLastError(WSAEINVAL);
        SocketError('listen: protocol not assigned');
        Exit;
    end;

    if not FAddrAssigned then begin
        WSocket_Synchronized_WSASetLastError(WSAEINVAL);
        SocketError('listen: address not assigned');
        Exit;
    end;

    try
        if not FProtoResolved then begin
            { The next line will trigger an exception in case of failure }
            if IcsCompareText(Copy(FProtoStr, 1, 4), 'raw_') = 0 then begin
                FType  := SOCK_RAW;
                FProto := WSocket_Synchronized_ResolveProto(Copy(FProtoStr, 5, 10));     { V8.70 }
            end
            else begin
                FProto := WSocket_Synchronized_ResolveProto(FProtoStr);                  { V8.70 }
                if FProto = IPPROTO_UDP then
                    FType := SOCK_DGRAM
                else
                    FType := SOCK_STREAM;
            end;
            FProtoResolved := TRUE;
        end;

        if not FPortResolved then begin
            { The next line will trigger an exception in case of failure }
            FPortNum       := WSocket_Synchronized_ResolvePort(FPortStr, FProtoStr);     { V8.70 }
            Fsin.sin6_port := WSocket_Synchronized_htons(FPortNum);
            FPortResolved  := TRUE;
        end;

        if not FAddrResolved then begin
            { The next line will trigger an exception in case of failure }
            if FSocketFamily = sfIPv4 then
            begin
                Fsin.sin6_family := AF_INET;
                PSockAddrIn(@Fsin).sin_addr.s_addr := WSocket_Synchronized_ResolveHost(FAddrStr).s_addr;   { V8.70 }
            end
            else
                WSocket_Synchronized_ResolveHost(FAddrStr, Fsin, FSocketFamily, FProto);

            FAddrResolved := TRUE;
            FAddrFormat := Fsin.sin6_family;
        end;
    except
        on E:Exception do begin
            RaiseException('listen: ' + E.Message);
            Exit;
        end;
    end;

    { Remove any data from the internal output buffer }
    { (should already be empty !)                     }
    DeleteBufferedData;

    FHSocket := WSocket_Synchronized_socket(FAddrFormat, FType, FProto);

    if FHSocket = INVALID_SOCKET then begin
        SocketError('socket');
        exit;
    end;

    if FType = SOCK_DGRAM then begin  { UDP }
        if FReuseAddr then begin
        { Enable multiple tasks to listen on duplicate address and port }
            optval  := -1;
            iStatus := WSocket_Synchronized_SetSockOpt(FHSocket, SOL_SOCKET, SO_REUSEADDR, @optval, SizeOf(optval));

            if iStatus <> 0 then begin
                SocketError('setsockopt(SO_REUSEADDR)');
                Close;
                Exit;
            end;
        end;
    end;

{$IFDEF MSWINDOWS}
    if FExclusiveAddr then begin
    { V8.36 Prevent other applications accessing this address and port }
    { V8.42 this is windows specific }
        optval  := -1;
        iStatus := WSocket_Synchronized_SetSockOpt(FHSocket, SOL_SOCKET, SO_EXCLUSIVEADDRUSE, @optval, SizeOf(optval));

        if iStatus <> 0 then begin
            SocketError('setsockopt(SO_EXCLUSIVEADDRUSE)');
            Close;
            Exit;
        end;
    end;
{$ENDIF}

    iStatus := WSocket_Synchronized_bind(FHSocket, PSockAddr(@Fsin)^, IcsSizeOfAddr(Fsin));   { V9.5 }
    if iStatus = 0 then
        ChangeState(wsBound)
    else begin
        try
            ErrorCode := WSocket_Synchronized_WSAGetLastError;
            if (ErrorCode = WSAEADDRINUSE) or (ErrorCode = WSAEACCES)  then   { V8.36 more friendly message for common error }
                FriendlyMsg := 'Another server is already listening on ' + FAddrStr + ':' + FPortStr ;
            SocketError('listen: Bind', ErrorCode, FriendlyMsg);
        finally
            WSocket_Synchronized_closesocket(FHSocket);
            FHSocket := INVALID_SOCKET;
        end;
        Exit;
    end;

    case FType of
{$IFDEF MSWINDOWS}
    SOCK_RAW :
        begin
            if HasOption(FComponentOptions, wsoSIO_RCVALL) then begin
                dwBufferInLen  := 1;     { RCVALL_ON=1, RCVALL_IPLEVEL=3 no promiscious }
                dwBufferOutLen := 0;
                iStatus := WSocket_Synchronized_WSAIoctl(FHSocket, SIO_RCVALL, @dwBufferInLen,  SizeOf(dwBufferInLen),
                                                                     @dwBufferOutLen, SizeOf(dwBufferOutLen), dwDummy, nil, nil);

                if iStatus = SOCKET_ERROR then begin
                    SocketError('WSAIoctl(SIO_RCVALL)');
                    Close;
                    Exit;
                end;
            end;

            ChangeState(wsListening);
            ChangeState(wsConnected);
            GetSocketIpInfo;       { V9.5 }
            TriggerSessionConnectedSpecial(0);
        end;
{$ENDIF}
    SOCK_DGRAM :  { UDP }
        begin
            if FMultiCast then begin
                if FAddrFormat = AF_INET then begin
                    { Use setsockopt() to join a multicast group }
                    { mreq.imr_multiaddr.s_addr := WSocket_inet_addr('225.0.0.37');}
                    { mreq.imr_multiaddr.s_addr := sin.sin_addr.s_addr;}
                    { mreq.imr_multiaddr.s_addr := WSocket_inet_addr(FAddrStr);}
                    mreq.imr_multiaddr.s_addr := WSocket_Synchronized_inet_addr(FMultiCastAddrStr);    { V8.70 }
                    { mreq.imr_interface.s_addr := htonl(INADDR_ANY);} { RK}
                    mreq.imr_interface.s_addr := WSocket_Synchronized_ResolveHost(FAddrStr).s_addr;  { V8.70 }
                    iStatus := WSocket_Synchronized_SetSockOpt(FHSocket, IPPROTO_IP, IP_ADD_MEMBERSHIP, @mreq, SizeOf(mreq));

                    if iStatus <> 0 then begin
                        SocketError('setsockopt(IP_ADD_MEMBERSHIP)');
                        Exit;
                    end;
                end
                else if FAddrFormat = AF_INET6 then begin
                    PIcsIPv6Address(@mreqv6.ipv6mr_multiaddr)^ := WSocketStrToIPv6(FMultiCastAddrStr, Success);
                    if not Success then begin
                        SocketError('setsockopt(IPV6_ADD_MEMBERSHIP) Invalid multicast address');
                        Exit;
                    end;
                    mreqv6.ipv6mr_interface := 0; // IPv6 default interface, fix me!
                    iStatus := WSocket_Synchronized_SetSockOpt(FHSocket, IPPROTO_IPV6, IPV6_ADD_MEMBERSHIP, PAnsiChar(@mreqv6), SizeOf(mreqv6));

                    if iStatus <> 0 then begin
                        SocketError('setsockopt(IPV6_ADD_MEMBERSHIP)');
                        Exit;
                    end;
                end
                else begin
                    SocketError('setsockopt(IP_ADD_MEMBERSHIP) Invalid address format');
                        Exit;
                end;
            end;
            ChangeState(wsListening);
            ChangeState(wsConnected);
            GetSocketIpInfo;       { V9.5 }
            TriggerSessionConnectedSpecial(0);
        end;
    SOCK_STREAM : { TCP }
        begin
            iStatus := WSocket_Synchronized_Listen(FHSocket, FListenBacklog);
            if iStatus = 0 then
                ChangeState(wsListening)
            else begin
                SocketError('Listen');
                Exit;
            end;
        end;
    else
        SocketError('Listen: unexpected protocol.');
        Exit;
    end;

    { Get winsock send buffer size - V7.84 }
    optlen  := SizeOf(FSocketSndBufSize);
    iStatus := WSocket_getsockopt(FHSocket, SOL_SOCKET, SO_SNDBUF, PAnsiChar(@FSocketSndBufSize), optlen);

    if iStatus <> 0 then begin
        SocketError('getsockopt(SO_SNDBUF)');
        Exit;
    end;

    { Get winsock receive buffer size }
    optlen  := SizeOf(FSocketRcvBufSize);
    iStatus := WSocket_getsockopt(FHSocket, SOL_SOCKET, SO_RCVBUF, PAnsiChar(@FSocketRcvBufSize), optlen);

    if iStatus <> 0 then begin
        SocketError('getsockopt(SO_RCVBUF)');
        Exit;
    end;

    { FP:26/09/06 Are FD_READ and FD_WRITE really necessary ? Probably not ! }
    { Lodewijk Ellen reported a problem with W2K3SP1 triggering an AV in     }
    { accept. Keeping only FD_ACCEPT and FD_CLOSE solved the problem.        }
    { Anyway, a listening socket doesn't send nor receive any data so those  }
    { notification are useless.                                              }
    if FType = SOCK_STREAM then
        FSelectEvent := FD_ACCEPT or FD_CLOSE
    else
        FSelectEvent := FD_READ or FD_WRITE; // works in both Win and Posix

  {$IFDEF MSWINDOWS}
    if HasOption(FComponentOptions, wsoNotifyAddressListChange) then         { V8.70 }
        FSelectEvent := FSelectEvent or FD_ADDRESS_LIST_CHANGE;
    if HasOption(FComponentOptions, wsoNotifyRoutingInterfaceChange) then    { V8.70 }
        FSelectEvent := FSelectEvent or FD_ROUTING_INTERFACE_CHANGE;
  {$ENDIF}

    iStatus := WSocket_Synchronized_WSAASyncSelect({$IFDEF POSIX}Self,{$ENDIF} FHSocket, Handle, FMsg_WM_ASYNCSELECT, FSelectEvent);
    if iStatus <> 0 then begin
        SocketError('WSAASyncSelect');
        exit;
    end;

  {$IFDEF MSWINDOWS}
    if HasOption(FComponentOptions, wsoNotifyAddressListChange) and     { V8.70 }
       (not SetAddressListChangeNotification) then begin
        SocketError('Listen: SetAddressListChangeNotification');
        Exit;
    end;

    if HasOption(FComponentOptions, wsoNotifyRoutingInterfaceChange) and      { V8.70 }
       (not SetRoutingInterfaceChangeNotification) then begin
        SocketError('Listen: SetRoutingInterfaceChangeNotification');
        Exit;
    end;
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.Accept: TSocket;
var
    len : Integer;
  {$IFDEF POSIX}
    LastErr: Integer;
  {$ENDIF}
begin
  {$IFDEF POSIX}
    try
  {$ENDIF}
        if FState <> wsListening then begin
            WSocket_Synchronized_WSASetLastError(WSAEINVAL);
            SocketError('not a listening socket');
            Result := INVALID_SOCKET;
            Exit;
        end;
        len := IcsSizeOfAddr(sin6);     { V9.5 }
        FASocket := WSocket_Synchronized_Accept(FHSocket, @sin6, @len);
        Result := FASocket;

        if (FASocket = INVALID_SOCKET) then begin
          {$IFDEF MSWINDOWS}
            SocketError('Accept');
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
        DebugLog(loWsockInfo,
                IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) + ' ' +
                'Socket accepted ' + IntToStr(FASocket));
{$ENDIF}

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Pause;
begin
    FPaused := TRUE;
    WSocket_Synchronized_WSAASyncSelect(
                                      {$IFDEF POSIX}
                                        Self,
                                      {$ENDIF}
                                        FHSocket,
                                        Handle, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Resume;
begin
    FPaused := FALSE;
    WSocket_Synchronized_WSAASyncSelect(
                                      {$IFDEF POSIX}
                                        Self,
                                      {$ENDIF}
                                        FHSocket,
                                        Handle,
                                        FMsg_WM_ASYNCSELECT, FSelectEvent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Shutdown(How : Integer);
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loWsockInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
      DebugLog(loWsockInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) + ' ' +
               'TCustomWSocket.Shutdown ' + IntToStr(How) + ' handle=' + IntToStr(FHSocket));
{$ENDIF}
    if FHSocket <> INVALID_SOCKET then begin
      {$IFDEF POSIX}
        WSocketSynchronizedSetShutdownCalled(Self, How);
      {$ENDIF}
        WSocket_Synchronized_Shutdown(FHSocket, How);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DeleteBufferedData;
begin
    if Assigned(FBufHandler) then begin
        FBufHandler.Lock;
        try
            FBufHandler.DeleteAllData;
            FBufferedByteCount := 0;
        finally
            FBufHandler.UnLock;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Abort;
begin
    InternalAbort(0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.InternalAbort(ErrCode : Word);
begin
    InternalCancelDnsLookup(TRUE);
    DeleteBufferedData;
    { Be sure to close as fast as possible (abortive close) }
    if (State = wsConnected) and (FProto = IPPROTO_TCP) then begin
        LingerOnOff := wsLingerOff;
        SetLingerOption;
    end;
    InternalClose(FALSE, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Close;
begin
    InternalClose(TRUE, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.CloseDelayed;
begin
    PostMessage(Handle, FMsg_WM_CLOSE_DELAYED, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.WMCloseDelayed(var msg: TMessage);
begin
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Flush;
begin
    while (FHSocket <> INVALID_SOCKET) and     { No more socket   }
          (not bAllSent) do begin              { Nothing to send  }
            { Break; }
        TryToSend;
        MessagePump;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.InternalClose(bShut : Boolean; Error : Word);
var
    iStatus : Integer;
begin
    InternalCancelDnsLookup(TRUE);   { V8.48 }
    if FHSocket = INVALID_SOCKET then begin
        if FState <> wsClosed then begin
            ChangeState(wsClosed);
            AssignDefaultValue;
        end;
        Exit;
    end;

    if FState = wsClosed then
        Exit;

{ 11/10/98 called shutdown(1) instead of shutdown(2). This disables only    }
{ sends. Disabling receives as well produced data lost is some cases.       }
{ Manifest constants for Shutdown                                           }
{  SD_RECEIVE = 0;   disables receives                                      }
{  SD_SEND    = 1;   disables sends, *Use this one for graceful close*      }
{  SD_BOTH    = 2;   disables both sends and receives                       }

    if bShut then
        ShutDown(1);

    if FHSocket <> INVALID_SOCKET then begin
        repeat
          {$IFDEF MSWINDOWS}
            { Disable winsock notification otherwise notifications may be }
            { posted even after the call to closesocket()                 }
            WSocket_Synchronized_WSAASyncSelect(FHSocket, Handle, 0, 0); { V8.05 }
          {$ENDIF}
            { Close the socket }
            iStatus := WSocket_Synchronized_closesocket(FHSocket);
            if iStatus <> 0 then begin
                FLastError := WSocket_Synchronized_WSAGetLastError;
                if FLastError <> WSAEWOULDBLOCK then begin
                  {$IFDEF POSIX}
                    WSocketSynchronizedRemoveEvents(Self, False);
                    IcsClearMessages(Handle, FMsg_WM_ASYNCSELECT, WPARAM(FHSocket));
                  {$ENDIF}
                    FHSocket := INVALID_SOCKET;
                  {$IFDEF MSWINDOWS}
                    { Ignore the error occuring when winsock DLL not initialized (occurs when using TWSocket from a DLL) }
                    if FLastError = WSANOTINITIALISED then
                        break;
                  {$ENDIF}
                    SocketError('Disconnect (closesocket)');
                    Exit;
                end;
              {$IFDEF MSWINDOWS}
                { Next line is untested, however I think we have to reenable socket notification here.  (AG)  }
                WSocket_Synchronized_WSAASyncSelect(FHSocket, Handle, FMsg_WM_ASYNCSELECT, FSelectEvent);    { V8.05 }
              {$ENDIF}
                MessagePump;
            end;
        until iStatus = 0;
      {$IFDEF POSIX}
        WSocketSynchronizedRemoveEvents(Self, True);
        IcsClearMessages(Handle, FMsg_WM_ASYNCSELECT, WPARAM(FHSocket));
      {$ENDIF}
        FHSocket := INVALID_SOCKET;
    end;

    ChangeState(wsClosed);
    if (not (csDestroying in ComponentState)) and (not FCloseInvoked) then begin
        FCloseInvoked := TRUE;
       TriggerSessionClosed(Error);
    end;
    { 29/09/98 Protect AssignDefaultValue because SessionClosed event handler }
    { may have destroyed the component.                                       }
    try
        AssignDefaultValue;
    except
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.WaitForClose;
var
    lCount    : {$IFDEF FPC} Cardinal; {$ELSE} u_long; {$ENDIF}
    Status    : Integer;
    DataBuf   : TWSocketData;
    Ch        : AnsiChar;
begin
    while (FHSocket <> INVALID_SOCKET) and (FState <> wsClosed) do begin
        MessagePump;

        if WSocket_Synchronized_ioctlsocket(FHSocket, FIONREAD, lCount) = SOCKET_ERROR then
            break;
        if lCount > 0 then
            TriggerDataAvailable(0);
        DataBuf := @Ch;
        Status := DoRecv(DataBuf, 1, 0);
        if Status <= 0 then begin
            FLastError := WSocket_Synchronized_WSAGetLastError;
            if FLastError <> WSAEWOULDBLOCK then
                break;
        end;
        MessagePump;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketGetHostByAddr(Addr : AnsiString) : PHostEnt;
var
    szAddr : array[0..256] of AnsiChar;
    lAddr  : u_long;
begin
    if (Length(Addr) = 0) or (Length(Addr) >= SizeOf(szAddr)) then
        raise ESocketException.Create('Winsock Get Host Addr: Invalid address.');   { V5.26 }

    StrPCopy(szAddr, Addr); { Length already checked above }
    lAddr  := WSocket_inet_addr(szAddr);
    Result := WSocket_gethostbyaddr(PAnsiChar(@lAddr), 4, PF_INET);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  WSocketGetHostByAddr(Addr : UnicodeString) : PHostEnt;    { V8.70 }
begin
    Result :=  WSocketGetHostByAddr(AnsiString(Addr));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketResolveIp(
    const IpAddr        : AnsiString;
    const ASocketFamily : TSocketFamily = DefaultSocketFamily;
    const AProtocol     : Integer  = IPPROTO_TCP) : AnsiString;
var
    Phe     : PHostEnt;
    ResList : TStringList;
begin
    if ASocketFamily = sfIPv4 then begin
        phe := WSocketGetHostByAddr(IpAddr);
        if Phe = nil then
            Result := ''
        else begin
          {$IFDEF MSWINDOWS}
            SetLength(Result, StrLen(Phe^.h_name));
            StrCopy(@Result[1], Phe^.h_name);
          {$ELSE}
            {$IFDEF DELPHI23_UP}           { V8.21 }
            SetLength(Result, StrLen(Phe^.h_name));
            StrCopy(@Result[1], Phe^.h_name);
            {$ELSE}
            SetLength(Result, StrLen(Phe^.hname));
            StrCopy(@Result[1], Phe^.hname);
            {$ENDIF}
          {$ENDIF}
        end;
    end
    else begin
        ResList := TStringList.Create;
        try
            if (WSocket_ResolveName(string(IpAddr), TRUE, ASocketFamily, ResList, AProtocol) <> 0) or
               (ResList.Count = 0) then
            begin
                Result := '';
            end
            else
                Result := AnsiString(ResList[0]);
        finally
            ResList.Free;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  WSocketResolveIp(const IpAddr : UnicodeString;
    const ASocketFamily: TSocketFamily = DefaultSocketFamily;
    const AProtocol: Integer = IPPROTO_TCP) : UnicodeString;          { V8.70 }
begin
    Result := UnicodeString(WSocketResolveIp(AnsiString(IpAddr), ASocketFamily, AProtocol));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketGetHostByName(Name : AnsiString) : PHostEnt;
var
    szName : array[0..256] of AnsiChar;
begin
    if (Length(Name) = 0) or (Length(Name) >= SizeOf(szName)) then
        raise ESocketException.Create('Winsock Get Host Name: Invalid Hostname.');   { V5.26 }

    StrPCopy(szName, Name);
    Result := WSocket_gethostbyname(szName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  WSocketGetHostByName(Name : UnicodeString) : PHostEnt;    { V8.70 }
begin
    Result := WSocketGetHostByName(AnsiString(Name));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure GetLocalIPList(AIPList: TStrings;
  const ASocketFamily: TSocketFamily = DefaultSocketFamily;
  const AProtocol: Integer = IPPROTO_TCP);
begin
{$IFNDEF NO_ADV_MT}
    CritSecIpList.Enter;
    try
{$ENDIF}
        AIPList.Assign(LocalIPList(ASocketFamily, AProtocol));
{$IFNDEF NO_ADV_MT}
    finally
        CritSecIpList.Leave;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LocalIPList(const ASocketFamily: TSocketFamily = DefaultSocketFamily;
  const AProtocol: Integer  = IPPROTO_TCP) : TStrings;
var
{$IFDEF MSWINDOWS}
    phe           : PHostEnt;
{$ENDIF}
    Hints         : TAddrInfo;
    AddrInfo      : PAddrInfo;
    AddrInfoNext  : PAddrInfo;
    RetVal        : Integer;
    LHostName     : string;
    Idx           : Integer;
begin
{$IFNDEF NO_ADV_MT}
    CritSecIpList.Enter;
    try
{$ENDIF}
        IPList.Clear;
        Result := IPList;

      { V8.52 only use GetHostByName for Windows XP, 2003 and earlier }
{$IFDEF MSWINDOWS}
        if (ASocketFamily = sfIPv4) and (Win32MajorVersion <= 5) then begin
            phe  := WSocketGetHostByName(LocalHostName);   { deprecated since 2003 and Vista }
            if phe <> nil then
                GetIpList(Phe, IPList);
        end
        else begin
{$ENDIF}
            LHostName := string(LocalHostName);
            FillChar(Hints, SizeOf(Hints), 0);
            if ASocketFamily = sfIPv6 then
                Hints.ai_family := AF_INET6
            else if ASocketFamily = sfIPv4 then
                Hints.ai_family := AF_INET;
            AddrInfo := nil;
            Hints.ai_protocol := AProtocol;
{$IFDEF MSWINDOWS}
            RetVal := WSocket_GetAddrInfo(PChar(LHostName), nil, @Hints, AddrInfo);  { only 2003 and Vista and later }
{$ELSE}
            RetVal := WSocket_GetAddrInfo(PAnsiChar(UnicodeToAnsi(LHostName, CP_UTF8)), nil, @Hints, AddrInfo);
{$ENDIF}
            if RetVal <> 0 then
                raise ESocketException.Create(
                    'Winsock GetAddrInfo: Cannot convert host address ''' +
                    LHostName + ''' - ' + GetWinsockErr(RetVal));
            try
                AddrInfoNext := AddrInfo;
                IDX := 0;
                while AddrInfoNext <> nil do
                begin
                    if AddrInfoNext.ai_family = AF_INET then
                    begin
                        if ASocketFamily = sfAnyIPv4 then
                        begin
                            IPList.Insert(IDX,
{$IFDEF MSWINDOWS}
                            WSocketIPv4ToStr(AddrInfoNext^.ai_addr^.sin_addr.S_addr));
{$ELSE}
                            WSocketIPv4ToStr(PSockAddrIn(AddrInfoNext^.ai_addr)^.sin_addr.S_addr));
{$ENDIF}
                            Inc(IDX);
                        end
                        else
                            IPList.Add(
{$IFDEF MSWINDOWS}
                            WSocketIPv4ToStr(AddrInfoNext^.ai_addr^.sin_addr.S_addr)
{$ELSE}
                            WSocketIPv4ToStr(PSockAddrIn(AddrInfoNext^.ai_addr)^.sin_addr.S_addr)
{$ENDIF}
                            );
                    end
                    else if AddrInfoNext.ai_family = AF_INET6 then
                    begin
                        if ASocketFamily = sfAnyIPv6 then
                        begin
                            IPList.Insert(IDX, WSocketIPv6ToStr(PSockAddrIn6(AddrInfoNext.ai_addr)));
                            Inc(IDX);
                        end
                        else
                            IPList.Add(WSocketIPv6ToStr(PSockAddrIn6(AddrInfoNext.ai_addr)));
                    end;
                    AddrInfoNext := AddrInfoNext.ai_next;
                end;
            finally
                WSocket_FreeAddrInfo(AddrInfo);
            end;
{$IFDEF MSWINDOWS}
        end;
{$ENDIF}
{$IFNDEF NO_ADV_MT}
    finally
        CritSecIpList.Leave;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LocalHostName : AnsiString;
begin
    if WSocket_gethostname(Result) <> 0 then
        raise ESocketException.Create('Winsock Get Host Name failed');

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetLocalHostName : String;                                 { V8.70 }
begin
    Result := String(LocalHostName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.TimerIsSet(var tvp : TTimeVal) : Boolean;
begin
    Result := (tvp.tv_sec <> 0) or (tvp.tv_usec <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.TimerCmp(var tvp : TTimeVal; var uvp : TTimeVal; IsEqual : Boolean) : Boolean;
begin
    Result := (tvp.tv_sec = uvp.tv_sec) and (tvp.tv_usec = uvp.tv_usec);
    if not IsEqual then
        Result := not Result;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TimerClear(var tvp : TTimeVal);
begin
   tvp.tv_sec  := 0;
   tvp.tv_usec := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetSendFlags(newValue : TSocketSendFlags);
begin
    case newValue of
    wsSendNormal: FSendFlags := 0;
    wsSendUrgent: FSendFlags := MSG_OOB;
    else
        RaiseException('Invalid SendFlags');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetSendFlags : TSocketSendFlags;
begin
    case FSendFlags of
    0       : Result := wsSendNormal;
    MSG_OOB : Result := wsSendUrgent;
    else
        RaiseException('Invalid internal SendFlags');
        Result := wsSendNormal;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerDebugDisplay(Msg : String);
begin
    if Assigned(FOnDebugDisplay) then
        FOnDebugDisplay(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerSendData(BytesSent : Integer);
begin
    if Assigned(FOnSendData) then
        FOnSendData(Self, BytesSent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerSessionAvailable(Error : Word);
begin
    if Assigned(FOnSessionAvailable) then
        FOnSessionAvailable(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerSessionConnectedSpecial(Error : Word);
begin
    if Assigned(FCounter) and (FType = SOCK_STREAM) and (Error = 0) then
        FCounter.SetConnected;
    TriggerSessionConnected(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerSessionConnected(Error : Word);
begin
    FReadCount  := 0;  { 7.24 }
    FWriteCount := 0;  { 7.24 }
    if Assigned(FOnSessionConnected) then
        FOnSessionConnected(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerSessionClosed(Error : Word);
begin
    if Assigned(FOnSessionClosed) then
        FOnSessionClosed(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.TriggerDataAvailable(Error : Word) : Boolean;
begin
    Result := Assigned(FOnDataAvailable);
    if not Result then
        Exit;
    FOnDataAvailable(Self, Error);  { 23/01/99 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerDataSent(Error : Word);
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loWsockDump) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loWsockDump, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) + ' ' + 'TriggerDataSent handle=' + IntToStr(FHSocket));
{$ENDIF}
    if Assigned(FOnDataSent) then
        FOnDataSent(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerError;
begin
    if Assigned(FOnError) then
        FOnError(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerException (E: ESocketException);   { V8.36 }
begin
    if Assigned(FOnException) then
        FOnException(Self, E);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerDNSLookupDone(Error : Word);
var
    TmpOnError: TNotifyEvent;
begin
    { Actions if it was internal DNS call.           }
    { In case of error call TriggerSessionConnected  }

   { PENDING - round robin DNS for multiple IP addresses on failure }

   { V8.60 moved before Connect attempt so user can change DnsResult }
    if Assigned(FOnDNSLookupDone) then
        FOnDNSLookupDone(Self, Error);

   { V8.43 finished an async lookup, now trigger connect }
    if FInternalDnsActive then
    begin
        FInternalDnsActive := FALSE;
        if Error = 0 then
            try
                { The next line will trigger an exception in case of failure }
                if FSocketFamily = sfIPv4 then
                begin
                    Fsin.sin6_family := AF_INET;
                    PSockAddrIn(@Fsin).sin_addr.S_addr := WSocket_Synchronized_ResolveHost(DnsResult).s_addr;    { V8.70 }
                end
                else
                    WSocket_Synchronized_ResolveHost(DnsResult, Fsin, FSocketFamily, FProto);
                FAddrResolved := TRUE;
                { If Socket.OnError is assigned, any raised exception will be transferred to }
                { the handler silently. So clear the handler temporarily to catch exception. }
                TmpOnError := OnError;
                OnError := nil;
                try
                    Connect;
                finally
                    OnError := TmpOnError;
                end;
            except on E: Exception do
                HandleBackGroundException(E, 'TCustomWSocket.TriggerDNSLookupDone');
            end
        else
            TriggerSessionConnected(Error);
        Exit;
    end;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerChangeState(OldState, NewState : TSocketState);
begin
    if Assigned(FOnChangeState) then
        FOnChangeState(Self, OldState, NewState);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SocketError(sockfunc: String; LastError: Integer = 0;
                                                      FriendlyMsg: String = '');   { V8.36 added FriendlyMsg }
var
    Error  : Integer;
    Line   : String;
begin
    if LastError = 0 then
        Error := WSocket_Synchronized_WSAGetLastError
    else
        Error := LastError;
    Line  := WSocketErrorDesc(Error) ;
    if (FSocketErrs = wsErrFriendly) then
        Line := Line + ' in ' + sockfunc    { V8.36 }
    else
        Line := Line + ' (#' + IntToStr(Error) + ' in ' + sockfunc + ')' ;   { V5.26 }
    if (Error = WSAECONNRESET) or
       (Error = WSAENOTCONN) then begin
        WSocket_Synchronized_closesocket(FHSocket);
        FHSocket := INVALID_SOCKET;
        if FState <> wsClosed then
           TriggerSessionClosed(Error);
        ChangeState(wsClosed);
    end;
    FLastError := Error;
    RaiseException(Line, Error, WSocketErrorDesc(Error), FriendlyMsg,
                                   sockfunc, FAddrStr, FPortStr, FProtoStr);  { V8.42 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *} { V5.21 }
{$IFNDEF NO_DEBUG_LOG}
function TCustomWSocket.CheckLogOptions(const LogOption: TLogOption): Boolean;
begin
    Result := Assigned(FIcsLogger) and (LogOption in FIcsLogger.LogOptions);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DebugLog(LogOption: TLogOption; const Msg: String);  { V5.21 }
begin
    if Assigned(FIcsLogger) then
        FIcsLogger.DoDebugLog(Self, LogOption, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *} { V5.21 }
procedure TCustomWSocket.SetIcsLogger(const Value: TIcsLogger);
begin
    if Value <> FIcsLogger then begin
        if FIcsLogger <> nil then
            FIcsLogger.RemoveFreeNotification(Self);
        if Value <> nil then
            Value.FreeNotification(Self);
        FIcsLogger := Value;
    end;
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetSocketFamily(const Value: TSocketFamily);
begin
    if (FState <> wsClosed) or (FDnsLookupHandle <> 0) then begin
        RaiseException('Cannot change SocketFamily when not closed or in DNS lookup');
        Exit;
    end;

    if Value <> FSocketFamily then begin
        if Value <> sfIPv4 then begin
            try
                if not IsIPv6APIAvailable then
                    raise ESocketException.Create(
                     'SetSocketFamily: New API requires winsock 2.2 ' +
                     'and Windows XP, property "SocketFamily" reset to "sfIPv4"');
            except
                FSocketFamily := sfIPv4;
                FOldSocketFamily := FSocketFamily;
                Exit;
            end;
        end;
        FSocketFamily := Value;
        FOldSocketFamily := FSocketFamily;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetSocketRcvBufSize(BufSize : Integer);
var
    iStatus : Integer;
    optlen  : Integer;
begin
    optlen  := SizeOf(BufSize);
    iStatus := WSocket_setsockopt(FHSocket, SOL_SOCKET, SO_RCVBUF,
                                  PAnsiChar(@BufSize), optlen);
    if iStatus = 0 then
        FSocketRcvBufSize := BufSize
    else
        SocketError('setsockopt(SO_RCVBUF)');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetSocketSndBufSize(BufSize : Integer);
var
    iStatus : Integer;
    optlen  : Integer;
begin
    optlen  := SizeOf(BufSize);
    iStatus := WSocket_setsockopt(FHSocket, SOL_SOCKET, SO_SNDBUF,
                                  PAnsiChar(@BufSize), optlen);
    if iStatus = 0 then
        FSocketSndBufSize := BufSize
    else
        SocketError('setsockopt(SO_SNDBUF)');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetSin(const Value: TSockAddrIn);
begin
    PSockAddrIn(@Fsin)^ := Value;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetSin: TSockAddrIn;
begin
    Result := PSockAddrIn(@Fsin)^;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetCurrentSocketFamily: TSocketFamily;
var
    saddr    : TSockAddrIn6;
    saddrlen : Integer;
begin
    if FCurrentAddrFamily <> AF_UNSPEC then
    begin
        if FCurrentAddrFamily = AF_INET6 then
            Result := sfIPv6
        else
            Result := sfIPv4;
    end
    else if FState in [wsConnected, wsBound, wsListening] then
    begin
        Result := FSocketFamily; // Dummy
        saddrlen := sizeof(saddr);
        if WSocket_Synchronized_GetSockName(FHSocket, PSockAddrIn(@saddr)^,
                                            saddrlen) = 0 then
        begin
            FCurrentAddrFamily := saddr.sin6_family;
            if FCurrentAddrFamily = AF_INET then
                Result := sfIPv4
            else if FCurrentAddrFamily = AF_INET6 then
                Result := sfIPv6
            else
                raise ESocketException.Create('Unknown socket family');
        end
        else
            SocketError('GetSockName');
    end
    else
        Result := FSocketFamily;
end;


(* V9.3 moved to OverbyteIcsTypes

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketSocksErrorDesc(ErrCode : Integer) : String;
begin
    case ErrCode of
        socksNoError              : Result := 'No Error';
        socksProtocolError        : Result := 'Protocol Error';
        socksVersionError         : Result := 'Version Error';
        socksAuthMethodError      : Result := 'Authentication Method Error';
        socksGeneralFailure       : Result := 'General Failure';
        socksConnectionNotAllowed : Result := 'Connection Not Allowed';
        socksNetworkUnreachable   : Result := 'Network Unreachable';
        socksHostUnreachable      : Result := 'Host Unreachable';
        socksConnectionRefused    : Result := 'Connection Refused';
        socksTtlExpired           : Result := 'TTL Expired';
        socksUnknownCommand       : Result := 'Unknown Command';
        socksUnknownAddressType   : Result := 'Unknown Address Type';
        socksUnassignedError      : Result := 'Unassigned Error';
        socksInternalError        : Result := 'Internal Error';
        socksDataReceiveError     : Result := 'Data Receive Error';
        socksAuthenticationFailed : Result := 'Authentication Failed';
        socksRejectedOrFailed     : Result := 'Rejected Or Failed';
        socksHostResolutionFailed : Result := 'Host Resolution Failed';
        else
            Result := 'Not a SOCKS error';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketHttpStatusCodeDesc(HttpStatusCode : Integer) : String;
const
    sHttpStatusCode = 'HTTP status code';
begin
    { Rather lengthy, I know, anyway RAM is cheap today. }
    if (HttpStatusCode >= 100) and (HttpStatusCode < 600) then
    begin
        case HttpStatusCode of
            100       : Result := 'Continue';
            101       : Result := 'Switching Protocols';
            102       : Result := 'Processing';

            200       : Result := 'OK';
            201       : Result := 'Created';
            202       : Result := 'Accepted';
            203       : Result := 'Non-Authoritative Information';
            204       : Result := 'No Content';
            205       : Result := 'Reset Content';
            206       : Result := 'Partial Content';
            207       : Result := 'Multi-Status (WebDAV)';

            300       : Result := 'Multiple Choices';
            301       : Result := 'Moved Permanently';
            302       : Result := 'Found';
            303       : Result := 'See Other';
            304       : Result := 'Not Modified';
            305       : Result := 'Use Proxy';
            306       : Result := 'Switch Proxy';
            307       : Result := 'Temporary Redirect';

            400       : Result := 'Bad Request';
            401       : Result := 'Unauthorized';
            402       : Result := 'Payment Required';
            403       : Result := 'Forbidden';
            404       : Result := 'Not Found';
            405       : Result := 'Method Not Allowed';
            406       : Result := 'Not Acceptable';
            407       : Result := 'Proxy Authentication Required';
            408       : Result := 'Request Timeout';
            409       : Result := 'Conflict';
            410       : Result := 'Gone';
            411       : Result := 'Length Required';
            412       : Result := 'Precondition Failed';
            413       : Result := 'Request Entity Too Large';
            414       : Result := 'Request-URI Too Long';
            415       : Result := 'Unsupported Media Type';
            416       : Result := 'Requested Range Not Satisfiable';
            417       : Result := 'Expectation Failed';
            418       : Result := 'I''m a teapot';
            422       : Result := 'Unprocessable Entity (WebDAV)';
            423       : Result := 'Locked (WebDAV)';
            424       : Result := 'Failed Dependency (WebDAV)';
            425       : Result := 'Unordered Collection';
            444       : Result := 'No Response';
            426       : Result := 'Upgrade Required';
            449       : Result := 'Retry With';
            450       : Result := 'Blocked by Windows Parental Controls';
            499       : Result := 'Client Closed Request';

            500       : Result := 'Internal Server Error';
            501       : Result := 'Not Implemented';
            502       : Result := 'Bad Gateway';
            503       : Result := 'Service Unavailable';
            504       : Result := 'Gateway Timeout';
            505       : Result := 'HTTP Version Not Supported';
            506       : Result := 'Variant Also Negotiates';
            507       : Result := 'Insufficient Storage (WebDAV)';
            509       : Result := 'Bandwidth Limit Exceeded';
            510       : Result := 'Not Extended';
            else
                Result := sHttpStatusCode + ' ' + IntToStr(HttpStatusCode);
        end;
    end
    else
        Result := 'Not a ' + sHttpStatusCode;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

function WSocketHttpTunnelErrorDesc(ErrCode : Integer) : String;
const
    sNotAHttpTunnelError = 'Not a HTTP tunnel error';
var
    LErr : Integer;
begin
    if (ErrCode >= ICS_HTTP_TUNNEL_BASEERR) and
       (ErrCode <= ICS_HTTP_TUNNEL_MAXERR) then
    begin
        LErr := ErrCode - ICS_HTTP_TUNNEL_BASEERR;
        if (LErr >= 100) and (LErr < 600) then
        begin
            if LErr = 200 then
                Result := 'No Error'
            else
                Result := WSocketHttpStatusCodeDesc(LErr);
        end
        else begin
            case ErrCode of
                ICS_HTTP_TUNNEL_PROTERR :
                    Result := 'Protocol Error';
                ICS_HTTP_TUNNEL_GENERR  :
                    Result := 'General Failure';
                ICS_HTTP_TUNNEL_VERSIONERR :
                    Result := sHttpVersionError;
                else
                    Result := sNotAHttpTunnelError;
            end;
        end;
    end
    else
        Result := sNotAHttpTunnelError;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketProxyErrorDesc(ErrCode : Integer) : String;
begin
    if (ErrCode >= ICS_HTTP_TUNNEL_BASEERR) and (ErrCode <= ICS_HTTP_TUNNEL_MAXERR) then
        Result := 'HTTP Proxy - ' + WSocketHttpTunnelErrorDesc(ErrCode)
    else if (ErrCode >= ICS_SOCKS_BASEERR) and ((ErrCode <= ICS_SOCKS_MAXERR)) then
        Result := 'SOCKS - ' + WSocketSocksErrorDesc(ErrCode)
    else
        Result := 'Not a proxy error';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketIsProxyErrorCode(ErrCode: Integer): Boolean;
begin
    Result := ((ErrCode >= ICS_SOCKS_BASEERR) and
               (ErrCode <= ICS_SOCKS_MAXERR)) or
              ((ErrCode >= ICS_HTTP_TUNNEL_BASEERR) and
               (ErrCode <= ICS_HTTP_TUNNEL_MAXERR));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketErrorMsgFromErrorCode(ErrCode: Integer) : String;
begin
    if WSocketIsProxyErrorCode(ErrCode) then
        Result := WSocketProxyErrorDesc(ErrCode)
    else
        Result := {$IFDEF MSWINDOWS} 'Winsock - ' {$ELSE} 'Socket - ' {$ENDIF} +
                  WSocketErrorDesc(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketGetErrorMsgFromErrorCode(ErrCode : Integer) : String;
begin
    Result := WSocketErrorMsgFromErrorCode(ErrCode) + ' (#' + IntToStr(ErrCode) + ')';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SocketErrorDesc(ErrCode : Integer) : String;
begin
    case ErrCode of
    0:
      Result := 'No Error';
    WSAEINTR:
      Result := 'Interrupted system call';
    WSAEBADF:
      Result := 'Bad file number';
    WSAEACCES:
      Result := 'Permission denied';
    WSAEFAULT:
      Result := 'Bad address';
    WSAEINVAL:
      Result := 'Invalid argument';
    WSAEMFILE:
      Result := 'Too many open files';
    WSAEWOULDBLOCK:
      Result := 'Operation would block';
    WSAEINPROGRESS:
      Result := 'Operation now in progress';
    WSAEALREADY:
      Result := 'Operation already in progress';
    WSAENOTSOCK:
      Result := 'Socket operation on non-socket';
    WSAEDESTADDRREQ:
      Result := 'Destination address required';
    WSAEMSGSIZE:
      Result := 'Message too long';
    WSAEPROTOTYPE:
      Result := 'Protocol wrong type for socket';
    WSAENOPROTOOPT:
      Result := 'Protocol not available';
    WSAEPROTONOSUPPORT:
      Result := 'Protocol not supported';
    WSAESOCKTNOSUPPORT:
      Result := 'Socket type not supported';
    WSAEOPNOTSUPP:
      Result := 'Operation not supported on socket';
    WSAEPFNOSUPPORT:
      Result := 'Protocol family not supported';
    WSAEAFNOSUPPORT:
      Result := 'Address family not supported by protocol family';
    WSAEADDRINUSE:
      Result := 'Address already in use';
    WSAEADDRNOTAVAIL:
      Result := 'Address not available';
    WSAENETDOWN:
      Result := 'Network is down';
    WSAENETUNREACH:
      Result := 'Network is unreachable';
    WSAENETRESET:
      Result := 'Network dropped connection on reset';
    WSAECONNABORTED:
      Result := 'Connection aborted';
    WSAECONNRESET:
      Result := 'Connection reset by peer';
    WSAENOBUFS:
      Result := 'No buffer space available';
    WSAEISCONN:
      Result := 'Socket is already connected';
    WSAENOTCONN:
      Result := 'Socket is not connected';
    WSAESHUTDOWN:
      Result := 'Can''t send after socket shutdown';
    WSAETOOMANYREFS:
      Result := 'Too many references: can''t splice';
    WSAETIMEDOUT:
      Result := 'Connection timed out';
    WSAECONNREFUSED:
      Result := 'Connection refused';
    WSAELOOP:
      Result := 'Too many levels of symbolic links';
    WSAENAMETOOLONG:
      Result := 'File name too long';
    WSAEHOSTDOWN:
      Result := 'Host is down';
    WSAEHOSTUNREACH:
      Result := 'No route to host';
    WSAENOTEMPTY:
      Result := 'Directory not empty';
    WSAEPROCLIM:
      Result := 'Too many processes';
    WSAEUSERS:
      Result := 'Too many users';
    WSAEDQUOT:
      Result := 'Disc quota exceeded';
    WSAESTALE:
      Result := 'Stale NFS file handle';
    WSAEREMOTE:
      Result := 'Too many levels of remote in path';
  {$IFDEF MSWINDOWS}
    WSASYSNOTREADY:
      Result := 'Network sub-system is unusable';
    WSAVERNOTSUPPORTED:
      Result := 'WinSock DLL cannot support this application';
    WSANOTINITIALISED:
      Result := 'WinSock not initialized';
  {$ENDIF}
    WSAHOST_NOT_FOUND:
      Result := 'Host not found';
    WSATRY_AGAIN:
      Result := 'Non-authoritative host not found';
    WSANO_RECOVERY:
      Result := 'Non-recoverable error';
    WSANO_DATA:
      Result := 'No Data';
    WSASERVICE_NOT_FOUND:
      Result := 'Service not found'; // Name resolution
    else
  {$IFDEF MSWINDOWS}
      Result := 'Not a Winsocket error';
  {$ELSE}
      Result := 'Not a socket error';
  {$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketErrorDesc(ErrCode : Integer) : String;
begin
    Result := SocketErrorDesc(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetWinsockErr(ErrCode: Integer): String ;    { V5.26 }
begin
    Result := SocketErrorDesc(ErrCode) + ' (#' + IntToStr(ErrCode) + ')' ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetWindowsErr(ErrCode: Integer): String ;    { V5.26 }
    {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := SysErrorMessage(ErrCode) + ' (#' + IntToStr(ErrCode) + ')' ;
end;

*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

         X X        X X        X X       X      X      X X      X X X X
       X     X    X     X    X     X     X     X     X     X    X
       X          X     X    X           X   X       X          X
         X X      X     X    X           X X           X X        X X
             X    X     X    X           X   X             X          X
       X     X    X     X    X     X     X     X     X     X    X     X
         X X        X X        X X       X      X      X  X       X X

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomSocksWSocket.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FSocksUsercode := '';
    FSocksPassword := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.AssignDefaultValue;
begin
    inherited AssignDefaultValue;
    FSocksState          := socksData;
    FSocksServer         := '';
    FSocksPort           := '';
    FSocksLevel          := '5';
    FSocksRcvdCnt        := 0;
    FSocksPortAssigned   := FALSE;
    FSocksServerAssigned := FALSE;
    FProxyURL            := '';              { V8.65 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.SetSocksLevel(newValue : String);
begin
    if State <> wsClosed then begin
        RaiseException('Can''t change socks level if not closed');
        Exit;
    end;
    if (newValue <> '4')  and (newValue <> '5') and
       (newValue <> '4A') and (newValue <> '4a') then begin
        RaiseException('Invalid socks level. Must be 4, 4A or 5.');
        Exit;
    end;
    FSocksLevel := IcsUpperCase(newValue);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSocksWSocket.GetSocksPort: String;
begin
    Result := FSocksPort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.SetSocksPort(sPort : String);
begin
    if State <> wsClosed then begin
        RaiseException('Can''t change socks port if not closed');
        Exit;
    end;

    FSocksPort := IcsTrim(sPort);

    if Length(FSocksPort) = 0 then begin
        FSocksPortAssigned := FALSE;
        Exit;
    end;
    FSocksPortAssigned := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Set socks requisites by single function. If Host is empty, cleanup everything
procedure TCustomSocksWSocket.SetSocks(const Host, Port, User, Pass: string);  { V8.66 }
begin
  // Cleanup everything
    if Host = '' then begin
        SocksServer := '';
        SocksPort := '';
        SocksAuthentication := socksNoAuthentication;
        SocksUsercode := '';
        SocksPassword := '';
        Exit;
    end;

    SocksServer := Host;
    SocksPort := Port;

    if (User = '') then
        SocksAuthentication := socksNoAuthentication
    else
        SocksAuthentication := socksAuthenticateUsercode;
    SocksUsercode := User;
    SocksPassword := Pass;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Set http proxy requisites by single function. If Host is empty, cleanup everything
procedure TCustomSocksWSocket.SetHTTPTunnel(const Host, Port, User, Pass: string);    { V8.66 }
begin
  // Cleanup everything
    if Host = '' then begin
        HttpTunnelServer := '';
        HttpTunnelPort := '';
        HttpTunnelAuthType := htatNone;
        HttpTunnelUsercode := '';
        HttpTunnelPassword := '';
        Exit;
    end;

    HttpTunnelServer := Host;
    HttpTunnelPort := Port;

    if (User = '') then
        HttpTunnelAuthType := htatNone
    else
        HttpTunnelAuthType := htatBasic;
    HttpTunnelUsercode := User;
    HttpTunnelPassword := Pass;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Set HTTP or Socks proxy URL: 'proto://[user:password@]host:port' where "proto" = 'socks5' or 'http'
procedure TCustomSocksWSocket.SetProxyURL(const Value: string);            { V8.66 }
var
    Proto, User, Pass, Host, Port, Path: String;
begin
    if Value = FProxyURL then Exit;

  // Cleanup everything
    if Value = '' then begin
        FProxyURL := Value;
        SetSocks('');
        SetHTTPTunnel('');
        Exit;
    end;
    ParseURL(Value, Proto, User, Pass, Host, Port, Path);

  // check that Proto and Host are assigned
    if (Proto = '') or (Host = '') then begin
        RaiseException('Error with proxy URL: ' + Value + ' - it must contain proto and host');
        Exit;
    end;

  // check that Path is not assigned, except /
    if Length (Path) > 1 then begin
        RaiseException('Error with proxy URL:' + Value + ' - path not allowed');
        Exit;
    end;

  // switch proto
    if Proto = PROXY_PROTO_HTTP then
        SetHTTPTunnel(Host, Port, User, Pass)
    else if Proto = PROXY_PROTO_SOCKS5 then
        SetSocks(Host, Port, User, Pass)
    else
        RaiseException('Error with proxy URL "' + Value + ' - Unknown proxy protocol');
    FProxyURL := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSocksWSocket.GetSocksServer: String;
begin
    Result := FSocksServer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.SetSocksServer(sServer : String);
begin
    if State <> wsClosed then begin
        RaiseException('Can''t change socks server if not closed');
        Exit;
    end;

    FSocksServer := IcsTrim(sServer);
    FSocksServerAssigned := Length(FSocksServer) > 0;

    if FHttpTunnelServerAssigned and FSocksServerAssigned then
    begin
        FSocksServer         := '';
        FSocksServerAssigned := FALSE;
        raise Exception.Create('Can''t use Socks when HTTP proxy is used as well');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.Listen;
begin
    { Check if we really wants to use socks server }
    if not FSocksServerAssigned then begin
        { No socks server assigned, Listen as usual }
        inherited Listen;
        Exit;
    end;
    RaiseException('Listening is not supported thru socks server');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.Connect;
var
    LSocketFamily: TSocketFamily;
begin
    { Check if we really wants to use socks server }
    if not FSocksServerAssigned then begin
        { No socks server assigned, connect as usual }
        inherited Connect;
        Exit;
    end;

    if (IcsLowerCase(FProtoStr) <> 'tcp') and (IcsTrim(FProtoStr) <> '6') then begin
        RaiseException('TCP is the only protocol supported thru socks server'); { V5.26 }
        Exit;
    end;

 { V8.56 IPv6 support from Max Terentiev, V8.66 corrected by Fr0sT.Brutal to allow host names }
    if WSocketIsIP(FSocksServer, LSocketFamily) then begin // is socks proxy IP is IPv6 ?
        if (LSocketFamily = sfIPv6) and not IsIPv6APIAvailable then
            raise ESocketException.Create('IPv6 is not available');
        FSocketFamily := LSocketFamily;
    end;

    try
        if not FPortResolved then begin
            { The next line will trigger an exception in case of failure }
            Fsin.sin6_port := WSocket_Synchronized_htons(WSocket_Synchronized_ResolvePort(FSocksPort, FProtoStr));    { V8.70 }
            FPortResolved  := TRUE;
        end;

        if not FAddrResolved then begin
            { The next line will trigger an exception in case of failure }
            if FSocketFamily = sfIPv4 then begin
                Fsin.sin6_family := AF_INET;
                PSockAddrIn(@Fsin).sin_addr.s_addr := WSocket_Synchronized_ResolveHost(FSocksServer).s_addr;        { V8.70 }
            end
            else begin
                WSocket_Synchronized_ResolveHost(FSocksServer, Fsin, FSocketFamily, IPPROTO_TCP);
                if (Fsin.sin6_family <> AF_INET) and (FSocksLevel[1] <> '5') then
                    raise ESocketException.Create('IPv6 not supported with current socks version');
            end;
            FAddrResolved := TRUE;
        { V8.56 IPv6 support from Max Terentiev }
            FAddrFormat := Fsin.sin6_family;
        end;
        { The next line will trigger an exception in case of failure }
        FPortNum := WSocket_Synchronized_ResolvePort(FPortStr, FProtoStr);     { V8.70 }
    except
        on E:Exception do begin
            RaiseException('Connect: ' + E.Message);  { V5.26 }
            Exit;
        end;
    end;

    FSocksState := socksNegociateMethods;
    FRcvCnt     := 0;
    inherited Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.TriggerSessionConnectedSpecial(Error : Word);
var
    Buf : array [0..2] of AnsiChar;
begin
    if FSocksState = socksNegociateMethods then begin
        {ChangeState(wsSocksConnected);}
        TriggerSocksConnected(Error);
        if Error <> 0 then begin
            FSocksState := socksData;
            inherited TriggerSessionConnectedSpecial(Error);
            Exit;
        end;
        if FSocksLevel[1] = '4' then
            SocksDoConnect
        else begin
            if FSocksAuthentication = socksNoAuthentication then
                FSocksAuthNumber := #$00        { No authentification }
            else
                FSocksAuthNumber := #$02;       { Usercode/Password   }
            Buf[0] := #$05;                     { Version number      }
            Buf[1] := #$01;                     { Number of methods   }
            Buf[2] := FSocksAuthNumber;         { Method identifier   }
            Send(@Buf, 3);

        end;
    end
    else
        inherited TriggerSessionConnectedSpecial(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.TriggerSessionClosed(Error : Word);
begin
    if FSocksState = socksAuthenticate then
        TriggerSocksAuthState(socksAuthFailure);
    if FSocksState <> socksData then
        DataAvailableError(socksGeneralFailure,
                           WSocketErrorMsgFromErrorCode(socksGeneralFailure));
    inherited TriggerSessionClosed(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.TriggerSocksConnected(Error : Word);
begin
    if Assigned(FOnSocksConnected) then
        FOnSocksConnected(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.TriggerSocksError(Error : Integer; Msg : String);
begin
    if Assigned(FOnSocksError) then
        FOnSocksError(Self, Error, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.TriggerSocksAuthState(AuthState : TSocksAuthState);
begin
    if Assigned(FOnSocksAuthState) then
        FOnSocksAuthState(Self, AuthState);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Rfc1929  Username/Password Autentication protocol.
The UNAME field contains the username as known to the source operating system.
The PLEN field contains the length of the PASSWD field that follows.
The PASSWD field contains the password association with the given UNAME.

Rfc1929 does not mention anything about character sets allowed so currently
the Win32 code below converts the user name and password to ANSI using the
default system code page.                                                   }

procedure TCustomSocksWSocket.SocksDoAuthenticate;
var
    Buf     : array [0..127] of AnsiChar;
    I       : Integer;
    TempS   : AnsiString;
begin
    FSocksState := socksAuthenticate;
    TriggerSocksAuthState(socksAuthStart);
    Buf[0] := #$01; {06/03/99}           { Socks version }
    I      := 1;
    TempS  := AnsiString(FSocksUsercode);
    Buf[I] := AnsiChar(Length(TempS));
    Move(TempS[1], Buf[I + 1], Length(TempS));
    I := I + 1 + Length(TempS);

    TempS  := AnsiString(FSocksPassword);
    Buf[I] := AnsiChar(Length(TempS));
    Move(TempS[1], Buf[I + 1], Length(TempS));
    I := I + 1 + Length(TempS);
    try
{TriggerDisplay('Send = ''' + BufToStr(Buf, I) + '''');}
        Send(@Buf, I);
    except
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.SocksDoConnect;
type
    pu_long = ^u_long;
var
    Buf     : array [0..127] of AnsiChar;
    I       : Integer;
    ErrCode : Integer;
{$IFDEF COMPILER12_UP}
    S : AnsiString;
{$ENDIF}
    LSocketFamily : TSocketFamily;
    ConvOk : Boolean;
    v4Addr : TIcsIPv4Address;
    v6Addr : TIcsIPv6Address;
begin
    FSocksState := socksConnect;
    if FSocksLevel[1] = '4' then begin
        Buf[0] := #4;                                 { Version number  }
        Buf[1] := #1;                                 { Connect command }
        PWORD(@Buf[2])^  := WSocket_Synchronized_ntohs(FPortNum);  { Dest port       }
        if FSocksLevel = '4A' then
            { Conventional IP saying we can't convert the destination   }
            { host's domain name to find its IP address                 }
            { The destination must then follow the user ID              }
            pu_long(@Buf[4])^ := WSocket_Synchronized_inet_addr('0.0.0.1')
        else begin
            { With original SOCKS4, we have to supply the dest address  }
            try
                pu_long(@Buf[4])^ := WSocket_Synchronized_ResolveHost(FAddrStr).s_addr;     { V8.70 }
            except
                on E:Exception do begin
                     ErrCode := socksHostResolutionFailed;
                     TriggerSocksError(ErrCode, E.ClassName + ' ' + E.Message);
                     InternalClose(TRUE, ErrCode);
                     Exit;
                end;
            end;
        end;
        I := 8;
        if Length(FSocksUsercode) > 0 then begin
            { I'm not sure it has to be like that ! Should I also use the }
            { password or not ?                                           }
        {$IFDEF COMPILER12_UP}
            S := AnsiString(FSocksUsercode);
            if Length(S) > 0 then begin
                Move(Pointer(S)^, Buf[I], Length(S));
                I := I + Length(S);
            end;
        {$ELSE}
            Move(FSocksUsercode[1], Buf[I], Length(FSocksUsercode));
            I := I + Length(FSocksUsercode);
        {$ENDIF}
        end;
        Buf[I] := #0;
        Inc(I);
        if FSocksLevel = '4A' then begin
            { We have to supply the destination host name                 }
            Move(AnsiString(FAddrStr)[1], Buf[I], Length(FAddrStr));  // No length change expected (ASCII)
            I := I + Length(FAddrStr);
            Buf[I] := #0;  { Alon Gingold }
            Inc(I);        { Alon Gingold }
        end;
        { Buf[I] := #0;      Alon Gingold }
        { Inc(I);            Alon Gingold }
    end
    else begin
        Buf[0] := #$05;            { Socks version }
        Buf[1] := #$01;            { Connect command }
        Buf[2] := #$00;            { Reserved, must be $00 }

     (* Buf[3] := #$03;            { Address type is domain name }
        Buf[4] := AnsiChar((Length(FAddrStr)));
        { Should check buffer overflow }
        Move(AnsiString(FAddrStr)[1], Buf[5], Length(FAddrStr)); // No length change expected (ASCII)
        I := 5 + Length(FAddrStr);
        PWord(@Buf[I])^ := WSocket_Synchronized_htons(FPortNum);
        I := I + 2;  *)

    { V8.56 IPv6 support from Max Terentiev }
        if not WSocketIsIP(FAddrStr,LSocketFamily) then begin
            Buf[3] := #$03;            { Address type is domain name }
            Buf[4] := AnsiChar((Length(FAddrStr)));
            { Should check buffer overflow }
            Move(AnsiString(FAddrStr)[1], Buf[5], Length(FAddrStr)); // No length change expected (ASCII)
            I := 5 + Length(FAddrStr);
            PWord(@Buf[I])^ := WSocket_Synchronized_htons(FPortNum);
            I := I + 2;
        end
        else begin
            if LSocketFamily=sfIPv4 then begin
                Buf[3] := #$01; // IPv4
                v4Addr := WSocketStrToIPv4(FAddrStr,ConvOk);
                Move(v4Addr,Buf[4],4);
                PWord(@Buf[8])^ := WSocket_Synchronized_htons(FPortNum);
                I := 10;
            end
            else begin
                Buf[3] := #$04; // IPv6
                v6Addr := WSocketStrToIPv6(FAddrStr,ConvOk);
                Move(v6Addr,Buf[4],16);
                PWord(@Buf[20])^ := WSocket_Synchronized_htons(FPortNum);
                I := 22;
            end;
        end;
    end;

    try
        Send(@Buf, I);
    except
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.DataAvailableError(
    ErrCode : Integer;
    Msg     : String);
begin
{   TriggerSocksError(ErrCode, Msg); }
{   inherited TriggerSessionConnectedSpecial(ErrCode); }
{   InternalClose(TRUE, ErrCode); }
    TriggerSocksError(ErrCode, Msg);
    FSocksState := socksData;
    {**ALON** Added, so TriggerSessionConnectedSpecial will only call inherited}
    {inherited} TriggerSessionConnectedSpecial(ErrCode);
    {**ALON** removed 'inherited' now calls top level}
    InternalClose(TRUE, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSocksWSocket.TriggerDataAvailable(Error : Word) : Boolean;
var
    Len     : Integer;
    I       : Integer;
    ErrCode : Word;
    ErrMsg  : String;
    InAddr  : TInAddr;
    AnsLen  : Integer;
begin
    if FSocksState = socksData then begin
        Result := inherited TriggerDataAvailable(Error);
        Exit;
    end;

    if Error <> 0 then begin
        DataAvailableError(Error, 'data receive error');
        Result := FALSE;
        Exit;
    end;


    if FSocksState = socksNegociateMethods then begin
        Result := TRUE;
        Len := Receive(@FRcvBuf[FRcvCnt], Sizeof(FRcvBuf) - FRcvCnt - 1);
        if Len < 0 then
            Exit;
        FRcvCnt := FRcvCnt + Len;

        if FSocksLevel[1] = '4' then begin
            { We should never comes here }
            DataAvailableError(socksProtocolError, 'TWSocket logic error');
            Exit;
        end
        else begin  { SOCKS5 }
            { We are waiting only two bytes }
            if FRcvCnt < 2 then
                Exit;
            FRcvCnt := 0; { Clear receive counter }
            if FRcvBuf[0] <> $05 then begin
                DataAvailableError(socksVersionError, 'socks version error');
                Exit;
            end;
            if FRcvBuf[1] = $00 then begin
                { No authentication required }
                if FSocksAuthNumber <> #$00 then
                    { We asked for authentification, so complains... }
                    TriggerSocksAuthState(socksAuthNotRequired);
            end
            else if FRcvBuf[1] = $02 then begin
                { Usercode/Password authentication required }
                SocksDoAuthenticate;
                Exit;
            end
            else begin
                DataAvailableError(socksAuthMethodError, 'authentification method not acceptable');
                Exit;
            end;
            SocksDoConnect;
        end;
    end
    else if FSocksState = socksConnect then begin
        Result := TRUE;
        if FSocksLevel[1] = '4' then begin
            { We want at most 8 characters }
            Len := Receive(@FRcvBuf[FRcvCnt], 8 - FRcvCnt);
            if Len < 0 then
                Exit;
            FRcvCnt := FRcvCnt + Len;

            { We are waiting for 8 bytes }
            if FRcvCnt < 8 then
                Exit;
            FRcvCnt := 0; { Clear receive counter }
            if FRcvBuf[0] <> 0 then begin
                DataAvailableError(socksVersionError, 'socks version error');
                Exit;
            end;
            if FRcvBuf[1] <> 90 then begin  { david.brock }
                case FRcvBuf[1] of
                91: ErrCode := socksRejectedOrFailed;
                92: ErrCode := socksConnectionRefused;
                93: ErrCode := socksAuthenticationFailed;
                else
                   ErrCode := socksUnassignedError;
                end;
                case ErrCode of
                socksRejectedOrFailed :
                    ErrMsg := 'request rejected or failed';
                socksConnectionRefused :
                    ErrMsg := 'connection refused';
                socksAuthenticationFailed :
                    ErrMsg := 'authentification failed';
                else
                    ErrMsg := 'unassigned error #' + IntToStr(Ord(FRcvBuf[1]));
                end;
                DataAvailableError(ErrCode, ErrMsg);
                Exit;
            end;
            FSocksState := socksData;
            TriggerSessionConnectedSpecial(0);
            Result := TriggerDataAvailable(0);
            {**ALON** removed 'inherited' now calls top level}
        end
        else begin { SOCKS5 }
            Len := Receive(@FRcvBuf[FRcvCnt], Sizeof(FRcvBuf) - FRcvCnt - 1);
            if Len < 0 then
                Exit;
            FRcvCnt := FRcvCnt + Len;
            if FRcvCnt >= 1 then begin
                { First byte is version, we expect version 5 }
                if FRcvBuf[0] <> $05 then begin
                    DataAvailableError(socksVersionError, 'socks version error');
                    Exit;
                end;
            end;
            if FRcvCnt >= 2 then begin
                if FRcvBuf[1] <> $00 then begin
                    case FRcvBuf[1] of
                    1: ErrCode := socksGeneralFailure;
                    2: ErrCode := socksConnectionNotAllowed;
                    3: ErrCode := socksNetworkUnreachable;
                    4: ErrCode := socksHostUnreachable;
                    5: ErrCode := socksConnectionRefused;
                    6: ErrCode := socksTtlExpired;
                    7: ErrCode := socksUnknownCommand;
                    8: ErrCode := socksUnknownAddressType;
                    else
                       ErrCode := socksUnassignedError;
                    end;
                    case ErrCode of
                    socksGeneralFailure :
                        ErrMsg := 'general SOCKS server failure';
                    socksConnectionNotAllowed :
                        ErrMsg := 'connection not allowed by ruleset';
                    socksNetworkUnreachable :
                        ErrMsg := 'network unreachable';
                    socksHostUnreachable :
                        ErrMsg := 'host unreachable';
                    socksConnectionRefused :
                        ErrMsg := 'connection refused';
                    socksTtlExpired :
                        ErrMsg := 'time to live expired';
                    socksUnknownCommand :
                        ErrMsg := 'command not supported';
                    socksUnknownAddressType :
                        ErrMsg := 'address type not supported';
                    else
                        ErrMsg := 'unassigned error #' + IntToStr(Ord(FRcvBuf[1]));
                    end;
                    DataAvailableError(ErrCode, ErrMsg);
                    Exit;
                end;
            end;
            if FRcvCnt < 5 then
                Exit;

            { We have enough data to learn the answer length }
            if FRcvBuf[3] = $01 then
                AnsLen := 10                     { IP V4 address }
            else if FRcvBuf[3] = $03 then
                AnsLen := 7 + Ord(FRcvBuf[4])    { Domain name   }
            else
                AnsLen := 5;                     { Other unsupported }

            if FRcvCnt < AnsLen then
                Exit;

            if FRcvBuf[3] = $01 then begin
                { IP V4 address }
                InAddr.S_addr := FRcvBuf[4] or
                                 (FRcvBuf[5] shl 8) or
                                 (FRcvBuf[6] shl 16) or
                                 (FRcvBuf[7] shl 24);
                FBoundAddr := WSocket_Synchronized_inet_ntoa(InAddr);
                I := 4 + 4;
            end
            else if FRcvBuf[3] = $03 then begin
                { Domain name }
                SetLength(FBoundAddr, Ord(FRcvBuf[4]));
                Move(FRcvBuf[5], FBoundAddr[1], Length(FBoundAddr)); { david.brock }
                I := 4 + Ord(FRcvBuf[4]) + 1;
            end
           { V8.56 IPv6 support from Max Terentiev }
            else if FRcvBuf[3] = $04 then begin
                I := 16 + 4 // IPv6
            end
            else begin
                { Unsupported address type }
                DataAvailableError(socksUnknownAddressType, 'address type not supported');
                Exit;
            end;

            FBoundPort  := IcsIntToStrA(WSocket_Synchronized_ntohs(
                                        FRcvBuf[I] or (FRcvBuf[I + 1] shl 8)));
            I           := I + 2;
            FSocksState := socksData;
{           inherited TriggerSessionConnectedSpecial(0); }
{ if IsConsole then WriteLn('SOCKS5 NEGOCIATION OK');}
            {inherited} TriggerSessionConnectedSpecial(0);
            {**ALON** removed 'inherited' now calls top level}
            FSocksRcvdCnt := FRcvCnt - I;
            if FSocksRcvdCnt < 0 then
                FSocksRcvdCnt := 0
            else
                FSocksRcvdPtr := I; //@FRcvBuf[I];
{           Result := inherited TriggerDataAvailable(0);}
            Result := {inherited} TriggerDataAvailable(0);
            {**ALON** removed 'inherited' now calls top level}
        end;
    end
    else if FSocksState = socksAuthenticate then begin
        Result := TRUE;
        Len := Receive(@FRcvBuf[FRcvCnt], Sizeof(FRcvBuf) - FRcvCnt - 1);
        if Len < 0 then
            Exit;
        FRcvCnt := FRcvCnt + Len;
        { We expect 2 bytes }
        if FRcvCnt < 2 then  {AG 15/02/11}
            Exit;            {AG 15/02/11}
        { First byte is version of the subnegotiation proto (rfc1929), }
        { we expect version 1. However i.e. WinGate returns socks      }
        { version 5 here if user credentials are wrong, so don't       }
        { trigger a version error but socksAuthenticationFailed. AG 15/02/11 }
        if FRcvBuf[0] <> $01 then begin { 06/03/99 }
{                TriggerSocksAuthState(socksAuthFailure); Burlakov 12/11/99 }
{                DataAvailableError(socksVersionError, 'socks version error'); AG 15/02/11 }
            DataAvailableError(socksAuthenticationFailed, 'socks authentication failed'); {AG 15/02/11}
            Exit;
        end;
        if FRcvCnt = 2 then begin
            { Second byte is status }
            if FRcvBuf[1] <> $00 then begin
{                TriggerSocksAuthState(socksAuthFailure); Burlakov 12/11/99 }
                DataAvailableError(socksAuthenticationFailed, 'socks authentication failed');
                Exit;
            end;
        end
        else if FRcvCnt > 2 then begin
{            TriggerSocksAuthState(socksAuthFailure); Burlakov 12/11/99 }
            DataAvailableError(socksProtocolError, 'too much data availaible');
            Exit;
        end;
        FRcvCnt := 0; { 06/03/99 }
        TriggerSocksAuthState(socksAuthSuccess);
        SocksDoConnect;
    end
    else begin
        { We should never comes here ! }
        DataAvailableError(socksInternalError, 'internal error');
        Result := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSocksWSocket.GetRcvdCount : Integer;
begin
    if FSocksRcvdCnt <= 0 then
        Result := inherited GetRcvdCount
    else
        Result := FSocksRcvdCnt;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSocksWSocket.DoRecv(
    var Buffer : TWSocketData;
    BufferSize : Integer;
    Flags      : Integer) : Integer;
begin
    if FSocksRcvdCnt <= 0 then begin
        Result := inherited DoRecv(Buffer, BufferSize, Flags);
        Exit;
    end;
    { We already have received data into our internal buffer }
    if FSocksRcvdCnt <= BufferSize then begin
        { User buffer is greater than received data, copy all and clear }
        Move(FRcvBuf[FSocksRcvdPtr], Buffer^, FSocksRcvdCnt); { V7.33 }
        Result        := FSocksRcvdCnt;
        FSocksRcvdCnt := 0;
        Exit;
    end;
    { User buffer is smaller, copy as much as possible }
    Move(FRcvBuf[FSocksRcvdPtr], Buffer^, BufferSize); { V7.33 }
    Result        := BufferSize;
    FSocksRcvdPtr := FSocksRcvdPtr + BufferSize;
    FSocksRcvdCnt := FSocksRcvdCnt - BufferSize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

              X          X     X       X      X X X X
              X          X     X X     X      X
              X          X     X   X   X      X
              X          X     X     X X      X X X
              X          X     X       X      X
              X          X     X       X      X
              X X X X    X     X       X      X X X X

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomLineWSocket.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FLineEnd   := #13#10;
    FLineMode  := FALSE;
    FLineEdit  := FALSE;  { AG 2/12/07}
    FLineLimit := 65536;  { Arbitrary line limit }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomLineWSocket.Destroy;
begin
    try          { V8.71 JK }
        if FRcvdPtr <> nil then begin
            FreeMem(FRcvdPtr, FRcvBufSize);
            FRcvdPtr     := nil;
            FRcvBufSize := 0;
        end;
    finally
        inherited Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.WndProc(var MsgRec: TMessage);
begin
    with MsgRec do begin
        if Msg = FMsg_WM_TRIGGER_DATA_AVAILABLE then begin
            { We *MUST* handle all exception to avoid application shutdown }
            try
                WMTriggerDataAvailable(MsgRec)
            except
                on E:Exception do
                    HandleBackGroundException(E, 'WMTriggerDataAvailable');    { V8.68 was WndProc }
            end;
        end
        else
            inherited WndProc(MsgRec);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.WMTriggerDataAvailable(var msg: TMessage);
var
    Count : Integer;
begin
{$IFDEF OLD_20040117}
    while FRcvdCnt > 0 do
        TriggerDataAvailable(0);
{$ELSE}
    Count := 0;
    while FRcvdCnt > 0 do begin
        Inc(Count);
        FLineFound := FALSE;
        TriggerDataAvailable(0);
        if (FRcvdCnt <= 0) or
           (FLineMode and (Count > 3) and (not FLineFound)) then
            Break;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.SetLineMode(newValue : Boolean);
begin
    if FLineMode = newValue then
        Exit;
    FLineMode := newValue;
    if (FRcvdCnt > 0) or (FLineLength > 0) then
        PostMessage(Handle, FMsg_WM_TRIGGER_DATA_AVAILABLE, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
{ Returns -1 on error only if event OnError is assigned, otherwise an       }
{ ESocketException may be raised. Returns the number of bytes written on    }
{ success. LineEnd is treated as a raw sequence of bytes, hence it's not    }
{ converted but sent as is.                                                 }
function TCustomLineWSocket.SendLine(
    const Str : UnicodeString;
    ACodePage : Cardinal) : Integer;
begin
    Result := PutStringInSendBuffer(Str, ACodePage);
    if Result > 0 then begin
        if SendStr(LineEnd) > -1 then
            Inc(Result, Length(LineEnd))
        else
            Result := -1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomLineWSocket.SendLine(const Str : UnicodeString) : Integer;
begin
    Result := PutStringInSendBuffer(Str);
    if Result > 0 then begin
        if SendStr(LineEnd) > -1 then
            Inc(Result, Length(LineEnd))
        else
            Result := -1;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Returns -1 on error only if event OnError is assigned, otherwise an       }
{ ESocketException may be raised. Returns the number of bytes written on    }
{ success.                                                                  }
function TCustomLineWSocket.SendLine(const Str : RawByteString) : Integer;
begin
    Result := PutStringInSendBuffer(Str);
    if Result > 0 then begin
        if SendStr(LineEnd) > -1 then
            Inc(Result, Length(LineEnd))
        else
            Result := -1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomLineWSocket.GetRcvdCount : Integer;
begin
    if not FLineMode then
        Result := inherited GetRcvdCount
    else
        Result := FLineLength;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomLineWSocket.DoRecv(
    var Buffer : TWSocketData;
    BufferSize : Integer;
    Flags      : Integer) : Integer;
begin
    if FLineMode and (FLineLength > 0) then begin
        { We are in line mode and a line is received }
        if FLineLength <= BufferSize then begin
            { User buffer is greater than received data, copy all and clear }
            Move(FRcvdPtr^, Buffer^, FLineLength);
            Result      := FLineLength;
            FLineLength := 0;
            Exit;
        end;
        { User buffer is smaller, copy as much as possible }
        Move(FRcvdPtr^, Buffer^, BufferSize);
        { Move the end of line to beginning of buffer to be read the next time }
        Move(PAnsiChar(FRcvdPtr)[BufferSize], FRcvdPtr^, FLineLength - BufferSize);
        Result      := BufferSize;
        FLineLength := FLineLength - BufferSize;
        Exit;
    end;

    if FLineMode or (FRcvdCnt <= 0) then begin
        { There is nothing in our internal buffer }
        Result := inherited DoRecv(Buffer, BufferSize, Flags);
        Exit;
    end;

    { We already have received data into our internal buffer }
    if FRcvdCnt <= BufferSize then begin
        { User buffer is greater than received data, copy all and clear }
        Move(FRcvdPtr^, Buffer^, FRcvdCnt);
        Result   := FRcvdCnt;
        FRcvdCnt := 0;
        Exit;
    end;
    { User buffer is smaller, copy as much as possible }
    Move(FRcvdPtr^, Buffer^, BufferSize);
    { Then move remaining data to front og buffer  16/10/99 }
    Move(PAnsiChar(FRcvdPtr)[BufferSize], FRcvdPtr^, FRcvdCnt - BufferSize + 1);
    Result   := BufferSize;
    FRcvdCnt := FRcvdCnt - BufferSize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Edit received data. Handle TAB and BACKSPACE characters.                  }
{ A data packet has been received into FRcvPtr buffer, starting from        }
{ FRcvdCnt offset. Packet size if passed as the Len argument.               }
procedure TCustomLineWSocket.EditLine(var Len : Integer);
var
    Buf     : PAnsiChar;
    BufSize : Integer;
    I       : Integer;
    J       : Integer;
    Edited  : Boolean;
    NewCnt  : Integer;
    NewSize : Integer;
const
    BackString : String = #8 + ' ' + #8;
begin
    BufSize := 0;
    try
        Edited := FALSE;
        I      := FRcvdCnt;
        J      := FRcvdCnt;
        NewCnt := FRcvdCnt;
        { Loop to process all received char }
        while I < (FRcvdCnt + Len) do begin
            if PAnsiChar(FRcvdPtr)[I] = #8 then begin   { BACKSPACE character }
                if FLineEcho and (J > 0) then
                    SendStr(BackString);
                if not Edited then begin
                    { Not edited yet, so we allocate a buffer to store }
                    { edited data and we remember we edited data.      }
                    Edited := TRUE;
                    { Computer buffer size as a multiple of 256 bytes  }
                    BufSize := ((FRcvdCnt + Len + 256) shr 8) shl 8;
                    GetMem(Buf, BufSize);
                    { Copy data already processed }
                    Move(FRcvdPtr^, Buf^, I);
                end;
                if J > 0 then begin
                    Dec(J);
                    if J < NewCnt then
                        NewCnt := J;
                end;
                Inc(I);
            end
            else if PAnsiChar(FRcvdPtr)[I] = #9 then begin  { TAB character }
                if not Edited then begin
                    { Not edited yet, so we allocate a buffer to store }
                    { edited data and we remember we edited data.      }
                    Edited := TRUE;
                    { Computer buffer size as a multiple of 256 bytes  }
                    BufSize := ((FRcvdCnt + Len + 256) shr 8) shl 8;
                    GetMem(Buf, BufSize);
                    { Copy data already processed }
                    Move(FRcvdPtr^, Buf^, I);
                end;
                repeat
                    if FLineEcho then
                        SendStr(' ');
                    Buf[J] := ' ';
                    Inc(J);
                until (J and 7) = 0;
                Inc(I);
            end
            else begin
                if FLineEcho then
                    Send(@PAnsiChar(FRcvdPtr)[I], 1);
                if Edited then begin
                    if J >= BufSize then begin
                        { Need to allocate more buffer space }
                        NewSize := BufSize + 256;
                        ReallocMem(Buf, NewSize);
                        BufSize := NewSize;
                    end;
                    Buf[J] := PAnsiChar(FRcvdPtr)[I];
                end;
                Inc(I);
                Inc(J);
            end;
        end;
        if Edited then begin
            if J >= FRcvBufSize then begin
                { Current buffer is too small, allocate larger }
                NewSize := J + 1;
                ReallocMem(FRcvdPtr, NewSize);
                FRcvBufSize := NewSize;
            end;
            { Move edited data back to original buffer }
            Move(Buf^, FRcvdPtr^, J);
            PAnsiChar(FRcvdPtr)[J] := #0;
            FRcvdCnt := NewCnt;
            Len      := J - FRcvdCnt;
        end;
    finally
        if BufSize > 0 then
            FreeMem(Buf, BufSize);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.TriggerLineLimitExceeded(
    Cnt           : Integer;
    var ClearData : Boolean);
begin
    if Assigned(FOnLineLimitExceeded) then
        FOnLineLimitExceeded(Self, Cnt, ClearData);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomLineWSocket.TriggerDataAvailable(ErrCode : Word) : Boolean;
var
    Cnt        : Integer;
    Len        : Integer;
    NewSize    : Integer;
    SearchFrom : Integer;
    I          : Integer;
    Found      : Boolean;
begin
{  if (not FLineMode) or (Length(FLineEnd) = 0) then begin }
    if (not FLineMode) or (Length(FLineEnd) = 0) or
       (FSocksState <> socksData) or (FHttpTunnelState <> htsData)
    {**ALON** added check so, if data is received while still handshaking }
    { with the socks server, we ask the TCustomSocksWSocket to handle it  }
    then begin
        { We are not in line mode }
        Result := inherited TriggerDataAvailable(ErrCode);
        Exit;
    end;

    { We are in line mode. We receive data ourself }

    Result := TRUE;
    Cnt    := inherited GetRcvdCount;
    { if Cnt <= 0 then }
    {    Exit;         }
    if Cnt < 0 then
        Exit;
    if Cnt = 0 then
        Cnt := 255;

    if (FRcvdCnt + Cnt + 1) > FRcvBufSize then begin
        { Current buffer is too small, allocate larger }
        NewSize := FRcvdCnt + Cnt + 1;
        ReallocMem(FRcvdPtr, NewSize);
        FRcvBufSize := NewSize;
    end;

    Len := Receive(IncPtr(FRcvdPtr, FRcvdCnt), Cnt);
{$IFDEF OLD_20040117}
    if Len <= 0 then
        Exit;
    FRcvdPtr[FRcvdCnt + Len] := #0;
{$ELSE}
    if Len <= 0 then begin
        if FRcvdCnt <= 0 then
            Exit;
        Len := 0;
    end;
{$ENDIF}

    if Len > 0 then begin
        if FLineEdit then
            EditLine(Len)
        else if FLineEcho then
            Send(IncPtr(FRcvdPtr, FRcvdCnt), Len);
    end;

    SearchFrom := FRcvdCnt - Length(FLineEnd);
    if SearchFrom < 0 then
        SearchFrom := 0;
    FRcvdCnt := FRcvdCnt + Len;
    while FLineMode do begin
        Found := FALSE;
        I := SearchFrom;
        while I < (FRcvdCnt - Length(FLineEnd) + 1) do begin
            if PAnsiChar(FRcvdPtr)[I] = AnsiChar(FLineEnd[1]) then begin
                Found := StrLComp(PAnsiChar(@(PAnsiChar(FRcvdPtr)[I])),
                                  PAnsiChar(FLineEnd), Length(FLineEnd)) = 0;
                if Found then
                    break;    { Found the end of line marker }
            end;
            Inc(I);
        end;
        if not Found then begin
            if ((FLineLimit > 0) and (FRcvdCnt > FLineLimit)) then begin
                FLineClearData := TRUE;
                TriggerLineLimitExceeded(FRcvdCnt, FLineClearData);
                if FLineClearData then begin
                    FLineLength        := 0;
                    FRcvdCnt           := 0;
                    FLineClearData     := FALSE;
                end;
            end;
            break;
        end;
        FLineLength       := I + Length(FLineEnd);
        FLineReceivedFlag := TRUE;
        FLineFound        := TRUE;
        { We received a complete line. We need to signal it to application }
        { The application may not have a large buffer so we may need       }
        { several events to read the entire line. In the meanwhile, the    }
        { application may turn line mode off.                              }
        while FLineMode and (FLineLength > 0) do begin
            if not inherited TriggerDataAvailable(0) then
                { There is no handler installed }
                FLineLength := 0;
        end;
        { Move remaining data in front of buffer }
        if FLineLength > 0 then begin
            { Line mode was turned off in the middle of a line read. }
            { We preserve unread line and other received data.       }
            Move(PAnsiChar(FRcvdPtr)[I], PAnsiChar(FRcvdPtr)[FLineLength],
                 FRcvdCnt - I);
            FRcvdCnt := FRcvdCnt - I + FLineLength;
        end
        else begin
            Move(PAnsiChar(FRcvdPtr)[I + Length(FLineEnd)], PAnsiChar(FRcvdPtr)[0],
                 FRcvdCnt - I - Length(FLineEnd));
            FRcvdCnt := FRcvdCnt - I - Length(FLineEnd);
        end;
        if FRcvdCnt >= 0 then
            PAnsiChar(FRcvdPtr)[FRcvdCnt] := #0;
        SearchFrom := 0;
        { It is possible the user has turned line mode to off. If data is }
        { still available in the receive buffer, we will deliver it.      }
        while (not FLineMode) and (FRcvdCnt > 0) do            { 26/01/04 }
            inherited TriggerDataAvailable(0);                 { 26/01/04 }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.InternalAbort(ErrCode : Word);   { V7.49 }
begin
    { Abort as soon as possible, see TriggerSessionClosed below.      }
    FLineClearData := TRUE; { Skip subsequent calls to DataAvailable. }
    inherited InternalAbort(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.TriggerSessionClosed(Error : Word);
begin
    FLineReceivedFlag := TRUE;
    if FRcvdPtr <> nil then begin
        if FLineMode and (FRcvdCnt > 0) and (not FLineClearData) then begin
            FLineLength       := FRcvdCnt;
            while FLineMode and (FLineLength > 0) do
                inherited TriggerDataAvailable(0);
        end;
        FreeMem(FRcvdPtr, FRcvBufSize);
        FRcvdPtr    := nil;
        FRcvBufSize := 0;
        FRcvdCnt    := 0;
    end;
    inherited TriggerSessionClosed(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

                 X X      X     X    X       X     X X X
               X     X      X   X    X X     X   X      X
               X              X X    X   X   X   X
                 X X            X    X     X X   X
                     X          X    X       X   X
               X     X    X     X    X       X   X      X
                 X X        X X      X       X     X X X

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSyncWSocket.InternalDataAvailable(
    Sender : TObject;
    Error  : Word);
var
    Len : Integer;
begin
    SetLength(FLinePointer^, FLineLength);
    Len := Receive(@FLinePointer^[1], FLineLength);
    if Len <= 0 then
        FLinePointer^ := ''
    else
        SetLength(FLinePointer^, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSyncWSocket.WaitUntilReady(var DoneFlag : Boolean) : Integer;
begin
    Result := 0;           { Suppose success }
    FTimeStop := IcsGetTrgMSecs64(FTimeout);      { V8.71 }
    while not DoneFlag do begin
        if ((FTimeout > 0) and (IcsTestTrgTick64(FTimeStop))) or Terminated then begin  { V8.71 }
            { Application is terminated or timeout occured }
            Result := WSA_WSOCKET_TIMEOUT;
            break;
        end;
        MessagePump;
        { Do not use 100% CPU, but slow down transfers on high speed LAN }
        Sleep(0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ DEPRECATED: DO NOT USE Synchronize procedure for a new application.       }
{ Instead, use pure event-driven design.                                    }
function TCustomSyncWSocket.Synchronize(
    Proc : TWSocketSyncNextProc;
    var DoneFlag : Boolean) : Integer;
begin
    DoneFlag := FALSE;
    if Assigned(Proc) then
        Proc;
    Result := WaitUntilReady(DoneFlag);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ DEPRECATED: DO NOT USE ReadLine procedure for a new application.          }
{ Instead, use pure event-driven design using OnDataAvailable event.        }
procedure TCustomSyncWSocket.ReadLine(
    Timeout    : Integer;  { seconds if positive, milli-seconds if negative }
    var Buffer : AnsiString);
var
    OldDataAvailable : TDataAvailable;
    OldLineMode      : Boolean;
    Status           : Integer;
begin
    Buffer            := '';
    if FState <> wsConnected then begin
        RaiseException('ReadLine failed: not connected');
        Exit;
    end;

    { Positive timeout means seconds. Negative means milli-seconds }
    { Null means 60 seconds.                                       }
    if TimeOut = 0 then
        FTimeOut      := 60000
    else if TimeOut > 0 then
        FTimeOut      := Timeout * 1000
    else
        FTimeOut      := -Timeout;

    FLineReceivedFlag := FALSE;
    FLinePointer      := @Buffer;
    { Save existing OnDataAvailable handler and install our own }
    OldDataAvailable  := FOnDataAvailable;
    FOnDataAvailable  := InternalDataAvailable;
    { Save existing line mode and turn it on }
    OldLineMode       := FLineMode;
    FLineMode         := TRUE;
    try
        Status := Synchronize(nil, FLineReceivedFlag);
        if Status = WSA_WSOCKET_TIMEOUT then begin
             { Sender didn't send line end within allowed time. Get all }
             { data available so far.                                   }
             if FRcvdCnt > 0 then begin
                 SetLength(Buffer, FRcvdCnt);
                 Move(FRcvdPtr^, Buffer[1], FRcvdCnt);
                 FRcvdCnt := 0;
             end;
        end;
        { Should I raise an exception to tell the application that       }
        { some error occured ?                                           }
    finally
        FOnDataAvailable := OldDataAvailable;
        FLineMode        := OldLineMode;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF BUILTIN_TIMEOUT}

{ TCustomTimeoutWSocket }

const
    MIN_TIMEOUT_SAMPLING_INTERVAL = 1000;

constructor TCustomTimeoutWSocket.Create(AOwner: TComponent);
begin
    inherited;
    FTimeoutKeepThreadAlive := TRUE;
    FTimeoutSampling := 5000;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.TimeoutHandleTimer(
    Sender: TObject);
begin
    if (FTimeoutConnect > 0) and (FState <> wsConnected) then begin
        if IcsElapsedMsecs64(FTimeoutConnectStartTick) > FTimeoutConnect then begin   { V8.71 }
            TimeoutStopSampling;
            TriggerTimeout(torConnect);
        end;
    end
    else if (FTimeoutIdle > 0) then begin
        if IcsElapsedMsecs64(FCounter.GetLastAliveTick) > FTimeoutIdle then begin     { V8.71 }
            TimeoutStopSampling;
            TriggerTimeout(torIdle);
        end;
    end
    else
        TimeoutStopSampling;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.Connect;
begin
    if FTimeoutConnect > 0 then begin
        TimeoutStartSampling;
        FTimeoutConnectStartTick := IcsGetTickCount64;    { V8.71 }
    end
    else if FTimeoutIdle > 0 then
        TimeoutStartSampling;
    inherited Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.SetTimeoutKeepThreadAlive(const Value: Boolean);
begin
    FTimeoutKeepThreadAlive := Value;
    if FTimeoutTimer <> nil then
        FTimeoutTimer.KeepThreadAlive := FTimeoutKeepThreadAlive;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.SetTimeoutSampling(const Value: Cardinal);
begin
    if (Value > 0) and (Value < MIN_TIMEOUT_SAMPLING_INTERVAL) then
       FTimeoutSampling := MIN_TIMEOUT_SAMPLING_INTERVAL
    else
       FTimeoutSampling := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.TimeoutStartSampling;
begin
    if not Assigned(FTimeoutTimer) then begin
        FTimeoutTimer := TIcsThreadTimer.Create(Self);
        FTimeoutTimer.KeepThreadAlive := FTimeoutKeepThreadAlive;
        FTimeoutTimer.OnTimer := TimeoutHandleTimer;
    end;
    if not Assigned(FCounter) then
        CreateCounter
    else
        FCounter.LastSendTick := IcsGetTickCount64;    { V8.71 } // Init
    if FTimeoutTimer.Interval <> FTimeoutSampling then
        FTimeoutTimer.Interval := FTimeoutSampling;
    if not FTimeoutTimer.Enabled then
        FTimeoutTimer.Enabled := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.TimeoutStopSampling;
begin
    if Assigned(FTimeoutTimer) then
        FTimeoutTimer.Enabled := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.DupConnected;
begin
    if FTimeoutIdle > 0 then
        TimeoutStartSampling
    else
        TimeoutStopSampling;
    inherited DupConnected;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.ThreadAttach;
begin
    inherited ThreadAttach;
    if Assigned(FTimeoutTimer) then
        FTimeoutTimer.Enabled := FTimeoutOldTimerEnabled;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.ThreadDetach;
begin
    if Assigned(FTimeoutTimer) and
      (IcsGetCurrentThreadID = FThreadID) then begin
        FTimeoutOldTimerEnabled := FTimeoutTimer.Enabled;
        if FTimeoutOldTimerEnabled then
            FTimeoutTimer.Enabled := FALSE;
    end;
    inherited ThreadDetach;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.TriggerSessionClosed(Error: Word);
begin
    TimeoutStopSampling;
    inherited TriggerSessionClosed(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.TriggerSessionConnectedSpecial(
  Error: Word);
begin
    if (Error = 0) and (FTimeoutIdle > 0) then
        TimeoutStartSampling
    else
        TimeoutStopSampling;
    inherited TriggerSessionConnectedSpecial(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.TriggerTimeout(Reason: TTimeoutReason);
begin
    if Assigned(FOnTimeout) then
        FOnTimeout(Self, Reason);
end;
{$ENDIF BUILTIN_TIMEOUT}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF BUILTIN_THROTTLE}

{ TCustomThrottledWSocket }

constructor TCustomThrottledWSocket.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FBandwidthKeepThreadAlive := TRUE;
    FBandwidthSampling := 1000; { Msec sampling interval }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomThrottledWSocket.ThreadAttach;
begin
    inherited ThreadAttach;
    if Assigned(FBandwidthTimer) then
        FBandwidthTimer.Enabled := FBandwidthOldTimerEnabled;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomThrottledWSocket.ThreadDetach;
begin
    if Assigned(FBandwidthTimer) and
      (IcsGetCurrentThreadID = FThreadID) then begin
        FBandwidthOldTimerEnabled := FBandwidthTimer.Enabled;
        if FBandwidthOldTimerEnabled then
            FBandwidthTimer.Enabled := FALSE;
    end;
    inherited ThreadDetach;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomThrottledWSocket.DupConnected;
begin
    inherited DupConnected;
    SetBandwidthControl;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomThrottledWSocket.SetBandwidthControl;
var
    I : Int64;
begin
    FBandwidthCount := 0;
    if FBandwidthLimit > 0 then
    begin
        if not Assigned(FBandwidthTimer) then begin
            FBandwidthTimer := TIcsThreadTimer.Create(Self);
            FBandwidthTimer.KeepThreadAlive := FBandwidthKeepThreadAlive;
            FBandwidthTimer.OnTimer := BandwidthHandleTimer;
        end;
        FBandwidthTimer.Interval := FBandwidthSampling;
        if not FBandwidthTimer.Enabled then
            FBandwidthTimer.Enabled := TRUE;
        // Number of bytes we allow during a sampling period, max integer max.
        I := Int64(FBandwidthLimit) * FBandwidthSampling div 1000;
        if I < MaxInt then
            FBandwidthMaxCount := I
        else
            FBandwidthMaxCount := MaxInt;
        FBandwidthPaused := FALSE;
        Include(FComponentOptions, wsoNoReceiveLoop);
        FBandwidthEnabled := TRUE;
    {$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loWsockInfo) then
            DebugLog(loWsockInfo,
                     IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                     ' Bandwidth ON handle=' + IntToStr(FHSocket));
    {$ENDIF}
    end
    else begin
        if Assigned(FBandwidthTimer) then begin
            if FBandwidthTimer.Enabled then
                FBandwidthTimer.Enabled := FALSE;
            if FBandwidthEnabled then begin
                FBandwidthEnabled := FALSE;
            {$IFNDEF NO_DEBUG_LOG}
                if CheckLogOptions(loWsockInfo) then
                    DebugLog(loWsockInfo,
                            IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                            ' Bandwidth OFF handle=' + IntToStr(FHSocket));
            {$ENDIF}
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomThrottledWSocket.SetBandwidthKeepThreadAlive(
  const Value: Boolean);
begin
    FBandwidthKeepThreadAlive := Value;
    if FBandwidthTimer <> nil then
        FBandwidthTimer.KeepThreadAlive := FBandwidthKeepThreadAlive;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomThrottledWSocket.SetBandwidthLimit(const Value: Cardinal);   { V7.55 }
begin
    if Value <> FBandwidthLimit then begin
        FBandwidthLimit := Value;
        if Assigned(FBandwidthTimer) then SetBandwidthControl;  { only if done this already, to change it }
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomThrottledWSocket.SetBandwidthSampling(const Value: Cardinal);
begin
    if FBandwidthSampling <> Value then begin   { V7.55 }
        if Value < 500 then
            FBandwidthSampling := 500
        else
            FBandwidthSampling := Value;
        if Assigned(FBandwidthTimer) then SetBandwidthControl;  { only if done this already, to change it }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomThrottledWSocket.RealSend(var Data: TWSocketData;
  Len: Integer): Integer;
begin
    if not FBandwidthEnabled then
        Result := inherited RealSend(Data, Len)
    else begin
        { Try to adjust amount of data actually passed to winsock }
        if (Len > 0) and (FBandwidthCount < FBandwidthMaxCount) and
           (FBandwidthCount + Cardinal(Len) > FBandwidthMaxCount) then
            Len := (FBandwidthMaxCount - FBandwidthCount) + 1;

        Result := inherited RealSend(Data, Len);

        if (Result > 0) then begin
            Inc(FBandwidthCount, Result);
            if (FBandwidthCount > FBandwidthMaxCount) and
               (not FBandwidthPaused) then begin
                FBandwidthPaused := TRUE;
                Pause;
           {$IFNDEF NO_DEBUG_LOG}
                if CheckLogOptions(loWsockInfo) then
                    DebugLog(loWsockInfo,
                            IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                            ' Bandwidth Paused on send handle=' + IntToStr(FHSocket));
           {$ENDIF}
           end;
       end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomThrottledWSocket.Receive(Buffer: TWSocketData;
  BufferSize: Integer): Integer;
begin
    { The Receive throttle does not work if FD_CLOSE message has been received }
    { yet since handler Do_FD_CLOSE removes option wsoNoReceiveLoop.           }
    if (not FBandwidthEnabled) or not HasOption(FComponentOptions, wsoNoReceiveLoop) then    { V8.70 }
        Result := inherited Receive(Buffer, BufferSize)
    else begin
        { Try to adjust amount of data to be received from winsock }
        if (BufferSize > 0) and (FBandwidthCount < FBandwidthMaxCount) and
           (FBandwidthCount + Cardinal(BufferSize) > FBandwidthMaxCount) then
            BufferSize := (FBandwidthMaxCount - FBandwidthCount) + 1;

        Result := inherited Receive(Buffer, BufferSize);

        if (Result > 0) then begin
            Inc(FBandwidthCount, Result);
            if (FBandwidthCount > FBandwidthMaxCount) and
               (not FBandwidthPaused) then begin
                FBandwidthPaused := TRUE;
                Pause;
            {$IFNDEF NO_DEBUG_LOG}
                if CheckLogOptions(loWsockInfo) then
                    DebugLog(loWsockInfo,
                             IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                             ' Bandwidth Paused on receive handle=' + IntToStr(FHSocket));
            {$ENDIF}
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomThrottledWSocket.BandwidthHandleTimer(
    Sender: TObject);
begin
    if FBandwidthPaused then begin
        FBandwidthPaused := FALSE;
        Dec(FBandwidthCount, FBandwidthMaxCount);
        if FBandwidthCount > FBandwidthMaxCount then
            FBandwidthCount := FBandwidthMaxCount;
        if (FHSocket <> INVALID_SOCKET) then begin
        {$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loWsockInfo) then
                DebugLog(loWsockInfo,
                         IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                         ' Bandwidth Resume ' + IntToStr(FHSocket));
        {$ENDIF}
            Resume;
        end;
    end
    else
        FBandwidthCount := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomThrottledWSocket.TriggerSessionClosed(Error: Word);
begin
    if Assigned(FBandwidthTimer) then begin
        FBandwidthTimer.Enabled := FALSE;
        FBandwidthEnabled       := FALSE;
    end;
    inherited TriggerSessionClosed(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomThrottledWSocket.TriggerSessionConnectedSpecial(Error: Word);
begin
    { Turn on the throttle early, inherited TriggerSessionConnectedSpecial }
    { might already process the first data chunk.                          }
    if (Error = 0) then
        SetBandwidthControl;
    inherited TriggerSessionConnectedSpecial(Error);
    if (Error <> 0) and Assigned(FBandwidthTimer) then begin
        FBandwidthTimer.Enabled := FALSE;
        FBandwidthEnabled       := FALSE;
    end;
end;
{$ENDIF BUILTIN_THROTTLE}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ You must define USE_SSL so that SSL code is included in the component.    }
{ Either in OverbyteIcsDefs.inc or in the project/package options.          }
{$IFDEF USE_SSL}
var
//    GSslRegisterAllCompleted  : Boolean = FALSE;    { V9.1 engine gone }
    TraceCount : Integer = 0;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure LoadSsl;
begin
    IcsLoadSsl;  { V8.69 moved function to OverbyteIcsLIBEAY }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

procedure UnloadSsl;
begin
    IcsUnloadSsl;  { V8.69 moved function to OverbyteIcsLIBEAY }
end;

{ V9.1 callbacks used by SslContext to interact with WSocket }

 {* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function PeerVerifyCallback(Ok: Integer; StoreCtx : PX509_STORE_CTX) : Integer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
var
    MySsl   : PSSL;
    Obj     : TCustomSslWSocket;
    Cert    : PX509;
    CurCert : TX509Base;
begin
{$IFNDEF NO_SSL_MT}
    LockVerifyCB.Enter;
    try
{$ENDIF}
        // Retrieve the pointer to the SSL of the current connection
        MySsl := X509_STORE_CTX_get_ex_data(StoreCtx, SSL_get_ex_data_X509_STORE_CTX_idx);
        // Retrieve the object reference we stored at index 0
        Obj := TCustomSslWSocket(SSL_get_ex_data(MySsl, 0));
        if Assigned(Obj) then begin
            Obj.Pause;
            Obj.FSsl_In_CB := TRUE;
            try
                Cert := X509_STORE_CTX_get_current_cert(StoreCtx);
                { Lookup this cert in our custom list (chain) }
                CurCert := Obj.SslCertChain.Find(Cert);
                { Add it to our list }
                if not Assigned(CurCert) then begin
                    CurCert := Obj.SslCertChain.Add(Cert);
                    CurCert.VerifyResult := X509_STORE_CTX_get_error(StoreCtx);
                    CurCert.FirstVerifyResult := CurCert.VerifyResult;
                end
                else { Unfortunately me must overwrite here }
                    CurCert.VerifyResult := X509_STORE_CTX_get_error(StoreCtx);
                CurCert.VerifyDepth := X509_STORE_CTX_get_error_depth(StoreCtx);
                Obj.SslCertChain.LastVerifyResult := CurCert.VerifyResult;
{$IFNDEF NO_DEBUG_LOG}
                if Obj.CheckLogOptions(loSslInfo) then begin  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
                    Obj.DebugLog(loSslInfo,'VCB> VerifyPeer: Subject = '  + CurCert.SubjectOneLine);
                    Obj.DebugLog(loSslInfo,'VCB> VerifyPeer: Serial  = $' + CurCert.SerialNumHex);  { V8.40 }
                    Obj.DebugLog(loSslInfo,'VCB> VerifyPeer: Error   = '  + CurCert.VerifyErrMsg);
                end;
{$ENDIF}
                // Save verify result, V8.66 only once
                if Obj.FSslVerifyResult = -1 then
                    Obj.FSslVerifyResult := CurCert.VerifyResult;
                Obj.TriggerSslVerifyPeer(Ok, CurCert);
             {   if Ok <> 0 then
                    Obj.FSslVerifyResult := X509_V_OK;    V8.66 was never used }
            finally
                Obj.Resume;
                Obj.FSsl_In_CB := FALSE;
                if Obj.FHSocket = INVALID_SOCKET then
                    PostMessage(Obj.FWindowHandle, Obj.FMsg_WM_RESET_SSL, 0, 0);
            end;
        end;
        Result := Ok;
{$IFNDEF NO_SSL_MT}
    finally
        LockVerifyCB.Leave;
    end;
{$ENDIF}
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ClientCertCallback(Ssl: PSSL; X509: PPX509; PKEY: PPEVP_PKEY): Integer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
var
    Obj  : TCustomSslWSocket;
    Cert : TX509Base;
    X, P : Pointer;
begin
    { It's called when a client certificate is requested by a server and no    }
    { certificate was yet set for the SSL object. client_cert_cb() is the      }
    { application defined callback. If it wants to set a certificate, a        }
    { certificate/private key combination must be set using the x509 and pkey  }
    { arguments and ``1'' must be returned. The certificate will be installed  }
    { into ssl, see the NOTES and BUGS sections. If no certificate should be   }
    { set, ``0'' has to be returned and no certificate will be sent.           }
    { A negative return value will suspend the handshake and the handshake     }
    { function will return immediatly. SSL_get_error(3) will return            }
    { SSL_ERROR_WANT_X509_LOOKUP to indicate, that the handshake was suspended.}
    { The next call to the handshake function will again lead to the call of   }
    { client_cert_cb(). It is the job of the client_cert_cb() to store         }
    { information about the state of the last call, if required to continue.   }

    { Called when a client certificate is requested but there is not one set   }
    { against the SSL_CTX or the SSL.  If the callback returns 1, x509 and     }
    { pkey need to point to valid data.  The library will free these when      }
    { required so if the application wants to keep these around, increment     }
    { their reference counts.  If 0 is returned, no client cert is             }
    { available.  If -1 is returned, it is assumed that the callback needs     }
    { to be called again at a later point in time.  SSL_connect will return    }
    { -1 and SSL_want_x509_lookup(ssl) returns TRUE.  Remember that            }
    { application data can be attached to an SSL structure via the             }

{$IFNDEF NO_SSL_MT}
    LockClientCertCB.Enter;
    try
{$ENDIF}
        Result := 0;
        Obj := TCustomSslWSocket(SSL_get_ex_data(Ssl, 0));
        if Assigned(Obj) then begin
            Obj.FSsl_In_CB := TRUE;
            Obj.Pause;
            try
                if Assigned(Obj.FOnSslCliCertRequest) then begin
                    Cert := nil;
                    try
                        Obj.FOnSslCliCertRequest(Obj, Cert);
                        if (Cert <> nil) and (Cert.X509 <> nil) and
                           (Cert.PrivateKey <> nil) then begin
                            X     := X509_dup(Cert.X509);
                            P     := Ics_EVP_PKEY_dup(Cert.PrivateKey);
                            X509^  := X;
                            PKEY^  := P;
                            Result := 1;
                        end
                        else begin
                            //X509  := nil;
                            //PKEY  := nil;
                        end;
                    except
                        // psst
                    end;
                end;
            finally
                Obj.Resume;
                Obj.FSsl_In_CB := FALSE;
                if Obj.FHSocket = INVALID_SOCKET then
                    PostMessage(Obj.FWindowHandle, Obj.FMsg_WM_RESET_SSL, 0, 0);
            end;
        end;
{$IFNDEF NO_SSL_MT}
    finally
        LockClientCertCB.Leave;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function NewSessionCallback(const SSL: PSSL; Sess: PSSL_SESSION): Integer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
var
    Obj                : TCustomSslWSocket;
    AddToInternalCache : Boolean;
    SessID             : Pointer;
    IdLen              : Integer;
    CurrentSession     : Pointer;
    IncRefCount        : Boolean;
begin
   { If this callback is not null, it will be called each                  }
   { time a session id is added to the cache.  If this function            }
   { returns 1, it means that the callback will do a                       }
   { SSL_SESSION_free() when it has finished using it. Otherwise,          }
   { on 0, it means the callback has finished with it.                     }
   { Also: If this function returns 0,  the session object will not be     }
   { cached. A nonzero return allows the session to be cached              }

{$IFNDEF NO_SSL_MT}
    LockNewSessCB.Enter;
    try
{$ENDIF}
{xIFNDEF DELPHI25_UP}
        Result := 0;
{xENDIF}
        Obj := TCustomSslWSocket(SSL_get_ex_data(SSL, 0));
        if not Assigned(Obj) then
            raise Exception.Create('NewSessionCallback Obj not assigned');
        Obj.FSsl_In_CB := TRUE;
        try
{$IFNDEF NO_DEBUG_LOG}
            if Obj.CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
                Obj.DebugLog(loSslInfo, 'NSCB> New session created');
{$ENDIF}
      { V8.55 client now handled here as well }
            if (Obj.SslMode = sslModeClient) then begin
                if Assigned(Obj.FOnSslCliNewSession) then begin
                    CurrentSession := SSL_get_Session(Obj.FSsl);
                    IncRefCount := FALSE;
            {$IFNDEF NO_DEBUG_LOG}
                    if Obj.CheckLogOptions(loSslInfo) then
                        Obj.DebugLog(loSslInfo, IntToHex(INT_PTR(Obj), SizeOf(Pointer) * 2) +
                                 ' CliNewSessionCB [' +
                                 IntToHex(INT_PTR(CurrentSession), SizeOf(CurrentSession) * 2) + '] ' +
                                 'Reused: ' + BoolToStr(Obj.SslSessionReused, TRUE));
            {$ENDIF}
                    Obj.FOnSslCliNewSession(Obj, CurrentSession, Obj.SslSessionReused, IncRefCount);
                    if IncRefCount and (CurrentSession <> nil) then begin   // external cache sets this false
                       SSL_get1_Session(Obj.FSsl); // inc reference counter
                       Result := 1;
                    end;
                end;
            end
        { sever only }
            else begin
                SessID := SSL_SESSION_get_id(Sess, @IdLen); { 03/02/07 AG }
                AddToInternalCache := FALSE; // not sure about the default value
                if Assigned(Obj.FOnSslSvrNewSession) then
                    Obj.FOnSslSvrNewSession(Obj, Sess, SessID, IdLen, AddToInternalCache);
                if AddToInternalCache then   // external cache sets this false
                    Result := 1
                else
                    Result := 0;
            end;
        finally
            Obj.FSsl_In_CB := FALSE;
            if Obj.FHSocket = INVALID_SOCKET then
                PostMessage(Obj.FWindowHandle, Obj.FMsg_WM_RESET_SSL, 0, 0);
        end;
{$IFNDEF NO_SSL_MT}
    finally
        LockNewSessCB.Leave;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetSessionCallback(const SSL: PSSL; SessId: Pointer; IdLen: Integer; Ref: PInteger) : PSSL_SESSION; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
var
    Obj         : TCustomSslWSocket;
    Sess        : Pointer;
    IncRefCount : Boolean;
begin
    { SessId = Session ID that's being requested by the peer.             }
    { The Session ID is distinctly different from the session ID context  }
    { Ref = An output from the callback. It is used to allow the          }
    { callback to specify whether the reference count on the returned     }
    { session object should be incremented or not. It returns as          }
    { nonzero if the object's reference count should be incremented;      }
    { otherwise, zero is returned                                         }

{$IFNDEF NO_SSL_MT}
    LockGetSessCB.Enter;
    try
{$ENDIF}
{$IFNDEF COMPILER25_UP}
        Result := nil;
{$ENDIF}
        Obj := TCustomSslWSocket(SSL_get_ex_data(SSL, 0));
        if not Assigned(Obj) then
            raise Exception.Create('GetSessionCallback Obj not assigned');
        Obj.FSsl_In_CB := TRUE;
        try
{$IFNDEF NO_DEBUG_LOG}
            if Obj.CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
                Obj.DebugLog(loSslInfo, 'GSCB> Get session');
{$ENDIF}
            Sess := nil;
            IncRefCount := (Ref^ <> 0);
            if Assigned(Obj.FOnSslSvrGetSession) then
                Obj.FOnSslSvrGetSession(Obj, Sess, SessId, IdLen, IncRefCount);
            if IncRefCount then
                Ref^ := 1
            else
                Ref^ := 0;
            Result := Sess;
        finally
            Obj.FSsl_In_CB := FALSE;
            if Obj.FHSocket = INVALID_SOCKET then
                PostMessage(Obj.FWindowHandle, Obj.FMsg_WM_RESET_SSL, 0, 0);
        end;
{$IFNDEF NO_SSL_MT}
    finally
        LockGetSessCB.Leave;
    end;
{$ENDIF}
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure InfoCallBack(const ssl: PSSL; Where: Integer; Ret: Integer); {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
var
{$IFNDEF NO_DEBUG_LOG}
    Str : String;
    Pre : String;
    W   : Integer;
    Err : Integer;
{$ENDIF}
    Obj : TCustomSslWSocket;
begin
{$IFNDEF NO_SSL_MT}
    LockInfoCB.Enter;
    try
{$ENDIF}
        // TSslDebugLevel = (ssldbgNone, ssldbgError, ssldbgInfo, ssldbgDump);
        Obj := TCustomSslWSocket(SSL_get_ex_data(Ssl, 0));
        if not Assigned(Obj) then
            raise Exception.Create('ICB> Extended data not assigned fatal error!');
        Obj.FSsl_In_CB := TRUE;
        try
{$IFNDEF NO_DEBUG_LOG}

        { all this stuff is debug logging, not necessary for normal operation }
            if Obj.CheckLogOptions(loSslErr) or
               Obj.CheckLogOptions(loSslInfo) then begin

                Pre := IntToHex(INT_PTR(Obj), SizeOf(Pointer) * 2) + ' ICB> ';

                W := Where and (not SSL_ST_MASK);
                if (W and SSL_ST_CONNECT) <> 0 then
                    Str := 'SSL_connect: '
                else if (w and SSL_ST_ACCEPT) <> 0 then
                    Str := 'SSL_accept: '
                else if (Where and SSL_CB_HANDSHAKE_START) <> 0 then
                    Str := 'SSL_handshake_start: '                { V8.53 }
                else if (Where and SSL_CB_HANDSHAKE_DONE) <> 0 then
                    Str := 'SSL_handshake_done: '                 { V8.53 }
                else if (Where and SSL_CB_ALERT) <> 0 then
                    Str := 'SSL_alert: '                          { V8.55 }
                else
                    Str := 'undefined: ';

                if ((Where and SSL_CB_LOOP) <> 0) then begin
                    if Obj.CheckLogOptions(loSslInfo) then    { V8.55 was SslDevel, really Errs }
                        Obj.DebugLog(loSslInfo, Pre + Str +   { V8.59 really meant Info }
                                        String(SSL_state_string_long(ssl)));
                end
                else if ((Where and SSL_CB_ALERT) <> 0) and
                        Obj.CheckLogOptions(loSslInfo) then begin    { V8.59 was SslDevel, really meant Info }
                    if (Where and SSL_CB_READ) <> 0 then
                        Str := 'read '
                    else
                        Str := 'write ';
                    Obj.DebugLog(loSslInfo, Pre + 'SSL3 alert ' + Str +
                                 String(SSL_alert_type_string_long(ret)) + ' ' +
                                 String(SSL_alert_desc_string_long(ret)));
                end
                else if (Where and SSL_CB_EXIT) <> 0 then begin
                    if Ret = 0 then begin
             //         if Obj.CheckLogOptions(loSslErr) then   { V8.59 was Devel, Err and Info }
                            Obj.DebugLog(loSslErr, Pre + Str + 'failed in ' +
                                            String(SSL_state_string_long(ssl)));
                    end
                    else if Ret < 0 then begin
                        Err := SSL_get_error(ssl, Ret);
                        if NOT ((Err = SSL_ERROR_WANT_READ) or        { V8.14 only want real errors }
                                 (Err = SSL_ERROR_WANT_WRITE)) then begin
                              {  if Err = SSL_ERROR_SSL then  { V8.14 report proper error }
                              {      Obj.HandleSslError  V8.55 clears error, don't use for debug purposes }
                              {  else   }
                            Obj.DebugLog(loSslErr, Pre + Str + 'error ' + IntToStr (Err) + { V8.14 actual error }
                                                    ' in ' + String(SSL_state_string_long(ssl)));
                        end;
                    end;
                end
                else begin
                    if Obj.CheckLogOptions(loSslInfo) then     { V8.59 really meant Info }
                        Obj.DebugLog(loSslDevel, Pre + Str + 'where=' + IntToHex(where, 8) +
                              ', state=' + String(SSL_state_string_long(ssl))); { V8.53 added state }
                end;
            end;
{$ENDIF}
         { OpenSSL InfoCallback is when state changes }
            if (Where and SSL_CB_HANDSHAKE_START) <> 0 then begin
             {   Obj.FInHandshake   := TRUE;   V8.55 does not seem to be used anywhere ???? }
                Inc(Obj.FHandShakeCount);

           { V8.53 TLSv1.3 does not have renegotiation so skip these checks }
           { V8.66 no longer supporting renegotiation }
            end
          { V8.55 TLSv1.3 comes here too often due to tickets, FHandshakeEventDone is
            used to make sure the event is only called once }
            else if ((Where and SSL_CB_HANDSHAKE_DONE) > 0) and (NOT Obj.FHandshakeEventDone) then begin
                Obj.FHandshakeDone   := TRUE;  { triggers event once then reset }
{$IFNDEF NO_DEBUG_LOG}
                if Obj.CheckLogOptions(loSslErr) or Obj.CheckLogOptions(loSslInfo) then begin
                    Err := SSL_get_verify_result(Ssl);
                    if Obj.CheckLogOptions(loSslInfo) or (Err <> X509_V_OK) then
                       Obj.DebugLog(loSslErr, Pre + 'SSL_CB_HANDSHAKE_DONE, Error ' +
                            IcsX509VerifyErrorToStr (Err));   { V8.14 real literal, V8.39 better function }
                end;
{$ENDIF}
            end
        finally
            Obj.FSsl_In_CB := FALSE;
            if Obj.FHSocket = INVALID_SOCKET then
                PostMessage(Obj.FWindowHandle, Obj.FMsg_WM_RESET_SSL, 0, 0);
        end;
{$IFNDEF NO_SSL_MT}
    finally
        LockInfoCB.Leave;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ this callback not really used with OpenSSL 1.1.1 and later, uses ClientHello instead,
  but still need to send SslTlsExtErr saved from ClientHello from here }
function ServerNameCallback(SSL: PSSL; var ad: Integer; arg: Pointer): Integer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
var
    Ws : TCustomSslWSocket;
    PServerName : PAnsiChar; // Pointer to A-Label string
    Ctx : TSslContext;
    Err : TTlsExtError;
begin
    Ws := TCustomSslWSocket(SSL_get_ex_data(SSL, 0));
    if NOT Assigned(Ws) then begin   { V8.45 }
        Result := SSL_TLSEXT_ERR_OK;
        exit;
    end;

 { V8.64 we already have SNI from ClientHello callback, just need to tell client }
    if (Ws.FSslServerName <> '') then begin
        Result := Ord(Ws.FCliHelloData.SslTlsExtErr);
        Exit;
    end;

{$IFNDEF NO_SSL_MT}
    LockServerNameCB.Enter;  { V8.15 }
    try
{$ENDIF}
    PServerName := SSL_get_servername(SSL, TLSEXT_NAMETYPE_host_name);
    if Assigned(PServerName) then
    begin
        Ws.FSsl_In_CB := TRUE;
        try
         { V8.64 if result has ACE xn--. convert it to Unicode, ignore errors }
            Ws.FSslServerName := IcsIDNAToUnicode(String(PServerName));
            Ctx := nil;
         {   Err := teeAlertWarning; //SSL_TLSEXT_ERR_ALERT_WARNING  }
            Err := teeOk;  { V8.26 warning stop Java clients connecting }
            Ws.TriggerSslServerName(Ctx, Err);     { V8.45 }
            { Do not switch context if not initialized }
            if Assigned(Ctx) and Assigned(Ctx.SslCtxPtr) then
            begin
                if Ws.SslContext <> Ctx then
                begin
                    { Clear the options inherited from current Ctx.    }
                    { Not sure whether it is required, shouldn't hurt. }
                    SSL_clear_options(SSL, SSL_CTX_get_options(Ws.SslContext.SslCtxPtr));     { V8.51 }
                    Ws.SslContext := Ctx;
                    SSL_set_SSL_CTX(SSL, Ctx.SslCtxPtr);
                    SSL_set_options(SSL, SSL_CTX_get_options(ctx.SslCtxPtr));    { V8.51 }
                    SSL_CTX_set_tlsext_servername_callback(Ctx.SslCtxPtr, @ServerNameCallBack);
                {$IFNDEF NO_DEBUG_LOG}
                    if Ws.CheckLogOptions(loSslInfo) then
                        Ws.DebugLog(loSslInfo,  'SNICB> Switching context server_name "'  + Ws.FSslServerName + '"');
                {$ENDIF}
                end;
                Result := SSL_TLSEXT_ERR_OK;
            end
            else begin
                if Err = teeAlertFatal then ad := 112; // AD_UNRECOGNIZED_NAME;
                if Err = teeAlertWarning then ad := 112; // AD_UNRECOGNIZED_NAME;
                Result := Ord(Err);  { may return warning or error if no SNI }
            end;
        finally
            Ws.FSsl_In_CB := FALSE;
            if Ws.FHSocket = INVALID_SOCKET then
                PostMessage(Ws.FWindowHandle, Ws.FMsg_WM_RESET_SSL, 0, 0);
        end;
    end
    else
        Result := SSL_TLSEXT_ERR_OK;
{$IFNDEF NO_SSL_MT}
    finally
        LockServerNameCB.Leave;  { V8.15 }
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.56 application layer protocol negotiation, servers only }
{ client sends us a list of protocols it supports, and we can select one or
 ignore them all, ie spdy/1, http/1.1, h2 (http/2). pop3, acme-tls/1, etc }
function AlpnSelectCallBack(SSL: PSSL; var output: Pointer; var outlen: Integer;
                              input: Pointer; inlen: Integer; arg: Pointer): Integer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
var
    Ws: TCustomSslWSocket;
    Err: TTlsExtError;
    ProtoList: TStringList;
    Count: Integer;
    SelProto: String;
begin
    Result := SSL_TLSEXT_ERR_NOACK;
    outlen := 0;
    Ws := TCustomSslWSocket(SSL_get_ex_data(SSL, 0));
    if Assigned(Ws) then begin
        ProtoList := TStringList.Create;
        try
            try
                Count := IcsWireFmtToStrList(TBytes(input), inlen, ProtoList);
                if Count > 0 then begin

                    {$IFNDEF NO_DEBUG_LOG}
                        if Ws.CheckLogOptions(loSslInfo) then begin
                            Ws.DebugLog(loSslInfo, 'AlpnCB> inlen: ' + IntToStr(inlen) + ' - ' + IcsBufferToHex(input^, inlen));  { V8.64 }
                            Ws.DebugLog(loSslInfo, 'AlpnCB> Protocols: ' + ProtoList.CommaText);
                        end;
                    {$ENDIF}

                 { ask user if they want to select a single protocol to use from the list }
                    SelProto := '';
                    Err := teeNoAck;
                    Ws.TriggerSslAlpnSelect(ProtoList, SelProto, Err);
                    outlen := Length(SelProto);
                    if (Err = teeOk) and (outlen > 0) then begin
                        WS.FAlpnProtoAnsi := AnsiString(SelProto);   { V8.62 made static }
                        output := @WS.FAlpnProtoAnsi[1];
                        Result := SSL_TLSEXT_ERR_OK;  { V8.62 }
                    end
                    else if Err = teeAlertWarning then
                        Result := SSL_TLSEXT_ERR_ALERT_WARNING    { V8.62 }
                    else if Err = teeAlertFatal then
                        Result := SSL_TLSEXT_ERR_ALERT_FATAL;     { V8.62 }
                end;
            except   { V8.64 ignore any error here }
            end;
        finally
            ProtoList.Free;
        end;
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.64 new callback in OpenSSL 1.1.1 that replaces ServerNameCallback and ALPN callback and has more info }
function ClientHelloCallback(SSL: PSSL; var al: Integer; arg: Pointer): Integer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
var
    Ws: TCustomSslWSocket;
    Ctx: TSslContext;
    DataPtr: PAnsiChar;
    DataLen: size_t;
    DataExt: TBytes;
    I, Slen: Integer;
    ATemp: AnsiString;
{$IFNDEF NO_DEBUG_LOG}
    S: String;
{$ENDIF}

    function GetExtension(EType: Integer): Integer;
    begin
        Result := 0;
        DataLen := 0;
        if (SSL_client_hello_get0_ext(SSL, EType, @DataPtr, @DataLen) <> 1) then Exit;     { V8.66 var gone }
        if DataLen = 0 then Exit;
        Result := DataLen;
        SetLength(DataExt, DataLen);
        Move(DataPtr^, DataExt[0], DataLen);
    end;

    function GetByteDataPtr: TBytes;
    begin
        SetLength(Result, DataLen);
        if DataLen > 0 then
            Move(DataPtr^, Result[0], DataLen);
    end;

    function GetWordExt(Base: Integer): TIcsWordArray;
    var
        WLen, J: Integer;
    begin
        SetLength(Result, 0);
        if (Length(DataExt) <= (Base + 1)) then Exit;
        Wlen := DataExt[Base] div 2;
        SetLength(Result, WLen);
        if WLen > 0 then begin
            Move(DataExt[Base + 1], Result[0], WLen * 2);
            for J := 0 to Wlen - 1 do
                Result[J] := Swap(Result[J]);  // change endian
        end;
    end;

    function GetByteExt: TBytes;
    var
        Len: Integer;
    begin
        SetLength(Result, 0);
        if (Length(DataExt) <= 2) then Exit;
        len := DataExt[1];
        SetLength(Result, Len);
        if Len > 0 then
            Move(DataExt[2], Result[0], Len);
    end;

begin
    Result := SSL_CLIENT_HELLO_SUCCESS;
    Ws := TCustomSslWSocket(SSL_get_ex_data(SSL, 0));
    if NOT Assigned(Ws) then begin   // can not do anything useful
        Exit;
    end;

{ decode client hello data, ignore if badly formatted or corrupted }
    try

    { client hello is SSLv2 format, nothing more ICS can do, not supported }
        Ws.FCliHelloData.Sslv2 := (SSL_client_hello_isv2(SSL) = 1);
        if Ws.FCliHelloData.Sslv2 then begin
            {$IFNDEF NO_DEBUG_LOG}
                 Ws.DebugLog(loSslErr, 'CliHello> SSLv2 protocol not supported');
            {$ENDIF}
            al := 70; // TLS1_AD_PROTOCOL_VERSION
            Result := SSL_CLIENT_HELLO_ERROR;
            Exit;
        end;

     { client version }
        Ws.FCliHelloData.LegacyVersion := SSL_client_hello_get0_legacy_version(SSL);

     { get random bytes for keys }
        DataLen := SSL_client_hello_get0_random(SSL, @DataPtr);        { V8.66 var gone }
        Ws.FCliHelloData.Random := GetByteDataPtr;

     { get session ID, server may use old session }
        DataLen := SSL_client_hello_get0_session_id(SSL, @DataPtr);     { V8.66 var gone }
        Ws.FCliHelloData.SessionId := GetByteDataPtr;

     { get ciphers client can accept from us }
        DataLen := SSL_client_hello_get0_ciphers(SSL, @DataPtr);         { V8.66 var gone }
        Ws.FCliHelloData.CipherSuites := GetByteDataPtr;

     { get list of extensions available in hello }
        if (SSL_client_hello_get1_extensions_present(SSL, @DataPtr, @DataLen) = 1) then begin       { V8.66 var gone }
            SetLength(Ws.FCliHelloData.ExtnList, DataLen);
            Move(DataPtr^, Ws.FCliHelloData.ExtnList[0], (DataLen*SizeOf(Integer)));
            Ws.FCliHelloData.ExtnTotal := DataLen;
            OPENSSL_free(DataPtr);
        end;

     { check extensions }
        if Ws.FCliHelloData.ExtnTotal > 0 then begin
            for I := 0 to Ws.FCliHelloData.ExtnTotal - 1 do begin
                if (GetExtension(Ws.FCliHelloData.ExtnList[I]) > 0) then begin
                    case Ws.FCliHelloData.ExtnList[I] of
            // SNI "000C 00 0009 6C6F63616C686F7374" received
            //       len dns len l o c a l h o s t
                        TLSEXT_TYPE_server_name: begin
                            if (DataExt[0] = 0) and (DataExt[1] = DataLen - 2) then begin
                                SLen := DataExt[4]; // skip entry type
                                SetLength(ATemp, SLen);
                                Move(DataExt[5], Atemp[1], Slen);
                                Ws.FCliHelloData.PunyServerName := String(ATemp);
                            { V8.64 if result has ACE xn--. convert it to Unicode, ignore errors }
                                Ws.FCliHelloData.ServerName := IcsIDNAToUnicode(Ws.FCliHelloData.PunyServerName);
                                Ws.FSslServerName := Ws.FCliHelloData.ServerName;
                            end;
                        end;
                        TLSEXT_TYPE_application_layer_protocol_negotiation: begin
                            Ws.FCliHelloData.AlpnRaw := GetByteExt;
                            Ws.FCliHelloData.AlpnList := IcsWireFmtToCSV(Ws.FCliHelloData.AlpnRaw, Length(Ws.FCliHelloData.AlpnRaw));
                        end;
                        TLSEXT_TYPE_elliptic_curves: begin
                            Ws.FCliHelloData.EllipCurves := GetWordExt(1);
                        end;
                        TLSEXT_TYPE_signature_algorithms: begin
                            Ws.FCliHelloData.SigAlgos := GetWordExt(1);
                        end;
                        TLSEXT_TYPE_ec_point_formats: begin
                            Ws.FCliHelloData.ECPoints := GetWordExt(1);
                        end;
                        TLSEXT_TYPE_status_request: begin
                           Ws.FCliHelloData.StatusRequest := GetByteExt;   { OCSP stapling }
                        end;
                        TLSEXT_TYPE_renegotiate: begin
                           Ws.FCliHelloData.Renegotiate := GetByteExt;
                        end;
                        TLSEXT_TYPE_key_share: begin
                           Ws.FCliHelloData.KeyShare := GetByteExt;
                        end;
                        TLSEXT_TYPE_psk_kex_modes: begin
                           Ws.FCliHelloData.PSKExchMode := GetByteExt;
                        end;
                        TLSEXT_TYPE_psk: begin
                           Ws.FCliHelloData.PSKData := GetByteExt;
                        end;
                        TLSEXT_TYPE_supported_versions: begin
                            Ws.FCliHelloData.SuppVersions := GetWordExt(0);
                        end;
                        TLSEXT_TYPE_client_cert_type: begin   { V9.5 }
                            Ws.FCliHelloData.CliCertType := GetByteExt;
                        end;
                        TLSEXT_TYPE_server_cert_type: begin   { V9.5 }
                            Ws.FCliHelloData.SrvCertType := GetByteExt;
                        end;
                    end;
                end;
            end;
        end;
    except
       // ignore badly formatted extensions
    end;

{$IFNDEF NO_DEBUG_LOG}
    if Ws.CheckLogOptions(loSslInfo) then begin
        S := WSocketGetSslVerStr(Ws.FCliHelloData.LegacyVersion);
        if Length(Ws.FCliHelloData.SuppVersions) > 0 then begin
            for I := 0 to Length(Ws.FCliHelloData.SuppVersions) - 1 do
                S := S + ', ' + WSocketGetSslVerStr(Ws.FCliHelloData.SuppVersions[I]);
        end;
        Ws.DebugLog(loSslInfo,  'CliHello> Server Name: ' + Ws.FCliHelloData.PunyServerName + ', ' +
                                               'ALPN: ' + Ws.FCliHelloData.AlpnList + ', ' + 'CliHello> Versions: ' + S);
       { application can log ciphers, algos, etc in ServerName event, if it's called }
    end;
{$ENDIF}

 { SNI server name indication extension }
    try
        if (Ws.FSslServerName <> '') then begin
            Ctx := nil;
            Ws.FSsl_In_CB := TRUE;
            try
                Ws.FCliHelloData.SslTlsExtErr := teeOk;  { V8.26 ExtWarning stopped Java clients connecting }
                Ws.TriggerSslServerName(Ctx, Ws.FCliHelloData.SslTlsExtErr);
                if Ws.FCliHelloData.SslTlsExtErr = teeAlertFatal then begin     { reject SSL connection }
                    Result := SSL_CLIENT_HELLO_ERROR;
                    al := 112; // AD_UNRECOGNIZED_NAME;
                    Exit;
                end;

            { Do not switch context if not initialized }
                if Assigned(Ctx) and Assigned(Ctx.SslCtxPtr) then begin
                    if Ws.SslContext <> Ctx then begin
                    {$IFNDEF NO_SSL_MT}
                        LockServerNameCB.Enter;  { V8.15 }
                        try
                    {$ENDIF}
                            { Clear the options inherited from current Ctx.    }
                            { Not sure whether it is required, shouldn't hurt. }
                            SSL_clear_options(SSL, SSL_CTX_get_options(Ws.SslContext.SslCtxPtr));     { V8.51 }
                            Ws.SslContext := Ctx;
                            SSL_set_SSL_CTX(SSL, Ctx.SslCtxPtr);
                            SSL_set_options(SSL, SSL_CTX_get_options(ctx.SslCtxPtr));    { V8.51 }
                        {$IFNDEF NO_DEBUG_LOG}
                            if Ws.CheckLogOptions(loSslInfo) then
                                Ws.DebugLog(loSslInfo, 'CliHello> Switching context server_name "' + Ws.FSslServerName + '"');
                        {$ENDIF}
                    {$IFNDEF NO_SSL_MT}
                        finally
                            LockServerNameCB.Leave;  { V8.15 }
                        end;
                    {$ENDIF}
                    end;
                end;
            finally
                Ws.FSsl_In_CB := FALSE;
                if Ws.FHSocket = INVALID_SOCKET then begin
                    PostMessage(Ws.FWindowHandle, Ws.FMsg_WM_RESET_SSL, 0, 0);
                    Result := SSL_CLIENT_HELLO_ERROR;
                end;
            end;
        end;
    except
       // ignore SNI/ALPN event exceptions
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.69 OCSP status callback for OCSP stapling response from server during handshake }
function TlsStatusCallback(SSL: PSSL; arg: Pointer): Integer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
var
    Ws: TCustomSslWSocket;
    RawPtr: PAnsiChar;
    RawLen: Integer;
    MyOcspResponse: POCSP_RESPONSE;
begin
    Result := 1;  // OK
    Ws := TCustomSslWSocket(SSL_get_ex_data(SSL, 0));
    if NOT Assigned(Ws) then Exit;     // fatal error

 // client we receive OCSP response status from server and check it
    if SSL_is_server(SSL) = 0 then begin
        Ws.FOcspStapleStatus := -1;
        RawLen := SSL_get_tlsext_status_ocsp_resp(SSL, @RawPtr);
    {$IFNDEF NO_DEBUG_LOG}
        if Ws.CheckLogOptions(loSslInfo) then
             Ws.DebugLog(loSslInfo, 'OCSP staple status response from server, len=' + IntToStr(RawLen));
    {$ENDIF}
        SetLength(Ws.FOcspStapleRaw, RawLen);
        try
            if RawLen > 0 then begin
                Move(RawPtr^, Ws.FOcspStapleRaw[1], RawLen);   { keep OCSP to be processed by TOcspHttp }
                MyOcspResponse := d2i_OCSP_RESPONSE(Nil, @RawPtr, RawLen);
                if MyOcspResponse <> Nil then begin
                    Ws.FOcspStapleStatus := OCSP_response_status(MyOcspResponse);
                    if Ws.FOcspStapleStatus <> OCSP_RESPONSE_STATUS_SUCCESSFUL then
                        Ws.FOcspStapleRaw := '';
                    OCSP_RESPONSE_free(MyOcspResponse);
                end;
            end;
        except
           // ignore errors with bad data
        end;
    end

// server see if got stapled OCSP response to send from cache
    else begin
        RawLen := Length(Ws.FOcspStapleRaw);
        if RawLen > 0 then begin
            RawPtr := @Ws.FOcspStapleRaw[1];
            SSL_set_tlsext_status_ocsp_resp(SSL, RawPtr, RawLen);
            Result := SSL_TLSEXT_ERR_OK;
        end
        else
            Result := SSL_TLSEXT_ERR_NOACK;
    end;
 end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.TriggerSslServerName(var Ctx: TSslContext; var ErrCode: TTlsExtError);  { V8.45 }
begin
    if Assigned(FOnSslServerName) then
        FOnSslServerName(Self, Ctx, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.62 get application layer protocol selected by server, clients only }
procedure TCustomSslWSocket.SslGetAlpnProtocol;
var
    plen: integer;
    pdata: Pointer;
    temp: AnsiString;
begin
    FSslAlpnProtoRcvd := '';
    if NOT Assigned(FSsl) then Exit;
    plen := 0;
    pdata := Nil;
    SSL_get0_alpn_selected(FSsl, @pdata, @plen);  // not null terminated     { V8.66 var gone }
    if (plen > 0) and Assigned(pdata) then begin
        SetLength(temp, plen);
        Move(pdata^, temp[1], plen);
        FSslAlpnProtoRcvd := String(temp);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.1 keep application layer protocols supported, clients only }
{ must be set before connection opened }
procedure TCustomSslWSocket.SetSslAlpnProtocols(ProtoList: TStrings);
begin
    if NOT Assigned(ProtoList) then
        Exit;
    if NOT Assigned(FSslAlpnProtoList) then
        FSslAlpnProtoList := TStringList.Create;
    if FSslAlpnProtoList.Text <> ProtoList.Text then
        FSslAlpnProtoList.Assign(ProtoList);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.56 application layer protocol negotiation, servers only }
procedure TCustomSslWSocket.TriggerSslAlpnSelect(ProtoList: TStrings; var SelProto: String; var ErrCode: TTlsExtError);
begin
    if Assigned(FOnSslAlpnSelect) then
        FOnSslAlpnSelect(Self, ProtoList, SelProto, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF NO_DEBUG_LOG}
function TCustomSslWSocket.GetMyBioName(B: PBIO) : String;
begin
         if (b = Fibio)   then Result := 'ibio'
    else if (b = Fnbio)   then Result := 'nbio'
    else if (b = Fsslbio) then Result := 'sslbio'
    else                       Result := 'bio';
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.my_BIO_ctrl_pending(B: PBIO) : integer;
begin
    HandleSslError;
    if b = nil then begin
        Result := 0;
        Exit;
    end;
    Result := BIO_ctrl_pending(B);
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslDevel) then begin  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
    Inc(TraceCount);
        DebugLog(loSslDevel, Format('%s BIO_ctrl_pending(%s) = %d   [%d]',
                 [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                 GetMyBioName(b), Result, TraceCount]));
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DataToString(Buf : Pointer; Len : Integer) : String;
var
    P : PAnsiChar;    { V9.4 never updated for unicode }
begin
    P      := PAnsiChar(Buf);
    Result := '';
    while Len > 0 do begin
        if Word(P^) in [Ord(#32)..Ord(#126)] then
            Result := Result + Char(P^)
        else
            Result := Result + '$' + IntToHex(Ord(P^), 2) + ' ';
        Inc(P);
        Dec(Len);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.my_BIO_read(B: PBIO; Buf: Pointer; Len: Integer): Integer;
begin
    HandleSslError;
    if b = nil then begin
        Result := 0;
        Exit;
    end;
    Result := BIO_read(B, Buf, Len);
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslDevel) then begin  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
    Inc(TraceCount);
        DebugLog(loSslDevel, Format('%s BIO_read(%s, 0x%x, %d) = %d   [%d]',
                       [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                       GetMyBioName(b), INT_PTR(Buf), Len, Result, TraceCount]));
    end
    else if CheckLogOptions(loSslDump) then begin
        Inc(TraceCount);
        DebugLog(loSslDump, Format('%s BIO_read(%s, 0x%x, %d) = %d   [%d] Data:%s',
                       [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                       GetMyBioName(b), INT_PTR(Buf), Len, Result,
                       TraceCount, DataToString(Buf, Result)]));
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.my_BIO_ctrl(bp: PBIO; Cmd: Integer; LArg: Integer; PArg: Pointer): Integer;
{$IFNDEF NO_DEBUG_LOG}
var
    CmdName  : String;
    LArgName : String;
{$ENDIF}
begin
    if bp = nil then begin
        Result := 0;
        Exit;
    end;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslDevel) then begin  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        LArgName := IntToStr(LArg);
        case (cmd) of
        BIO_CTRL_FLUSH:         CmdName := 'BIO_CTRL_FLUSH';
        BIO_CTRL_PENDING_:      CmdName := 'BIO_CTRL_PENDING';           { V8.66 literal changed }
        BIO_C_DO_STATE_MACHINE: CmdName := 'BIO_C_DO_STATE_MACHINE'; // <= 02/01/06 AG
        BIO_C_SET_SSL:
            begin
                CmdName := 'BIO_C_SET_SSL';
                case (larg) of
                BIO_NOCLOSE: LArgName := 'BIO_NOCLOSE';
                end;
            end;
        else
            CmdName := IntToStr(Cmd);
        end;
    end;
{$ENDIF}
    Result := BIO_ctrl(bp, Cmd, LArg, PArg);
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslDevel) then begin  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
    Inc(TraceCount);
        DebugLog(loSslDevel, Format('%s BIO_ctrl(%s, %s, %s, 0x%x) = %d   [%d]',
                             [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                             GetMyBioName(bp), CmdName, LArgName, INT_PTR(PArg),
                             Result, TraceCount]));
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.my_BIO_ctrl_get_write_guarantee(b: PBIO): size_t;    { V8.66 corrected result }
begin
    HandleSslError;
    if b = nil then begin
        Result := 0;
        Exit;
    end;
    Result := BIO_ctrl_get_write_guarantee(b);
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslDevel) then begin  { V5.21 }
    Inc(TraceCount);
        DebugLog(loSslDevel,
                 Format('%s BIO_ctrl_get_write_guarantee(%s) = %d   [%d]',
                 [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2), GetMyBioName(b),
                 Result, TraceCount]));
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{#$IFDEF SSL_NEVER}
function TCustomSslWSocket.my_BIO_ctrl_get_read_request(b: PBIO): size_t;    { V8.66 corrected result }
begin
    HandleSslError;
    if b = nil then begin
        Result := 0;
        Exit;
    end;
    Result := BIO_ctrl_get_read_request(b);
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslDevel) then begin  { V5.21 }
        Inc(TraceCount);
        DebugLog(loSslDevel, Format('%s BIO_ctrl_get_read_request(%s) = %d   [%d]',
                                   [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                                   GetMyBioName(b), Result, TraceCount]));
    end;
{$ENDIF}
end;
{#$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.my_BIO_write(B: PBIO; Buf: Pointer; Len: Integer): Integer;
begin
    HandleSslError;
    if b = nil then begin
        Result := 0;
        Exit;
    end;
    Result := BIO_write(B, Buf, Len);
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslDevel) then begin  { V5.21 }
        Inc(TraceCount);
        DebugLog(loSslDevel, Format('%s BIO_write(%s, 0x%x, %d) = %d   [%d]',
                                   [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                                   GetMyBioName(b),
                                   INT_PTR(Buf),  Len, Result, TraceCount]));
    end
    else if CheckLogOptions(loSslDump) then begin
        Inc(TraceCount);
        DebugLog(loSslDump, Format('%s BIO_write(%s, 0x%x, %d) = %d   [%d] Data:%s',
                                   [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                                   GetMyBioName(b),
                                   INT_PTR(Buf), Len, Result,
                                   TraceCount, DataToString(Buf, Result)]));
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.HandleSslError;
begin
    FLastSslError := ERR_peek_error;
    if FLastSslError = 0 then
        Exit;
    FSslHandshakeErr := FLastSslError; { V8.14 }
    FSslHandshakeRespMsg := String(LastOpenSslErrMsg(TRUE));  { V8.14 }
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslErr) or CheckLogOptions(loSslInfo) then begin     { V8.55 }
        Inc(TraceCount);
        DebugLog(loSslErr, Format('%s HandleSslError handle=%d  [%d] %s',     { V8.55 }
                                  [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                                   FHSocket, TraceCount,
                                   FSslHandshakeRespMsg]));
    end
    else
        ERR_clear_error;
{$ELSE}
    ERR_clear_error;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.my_WSocket_recv(s: TSocket; var Buf: TWSocketData;
    len, flags: Integer): Integer;
begin
    Result := WSocket_recv(s, Buf, Len, Flags);
{$IFDEF POSIX}
    if (not FPaused) and ((Result > -1) or (errno = WSAEWOULDBLOCK)) then
        WSocketSynchronizedEnableReadEvent(Self);
{$ENDIF}
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslDevel) then begin  { V5.21 }
        Inc(TraceCount);
        DebugLog(loSslDevel, Format('%s Winsock recv( %d, 0x%x, %d, %d) = %d   [%d]',
                                   [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                                   s, INT_PTR(Buf), Len, Flags, Result, TraceCount]));
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.my_RealSend(Buf : TWSocketData; Len : Integer) : Integer;
begin
    Result := RealSend(Buf, Len);
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslDevel) then begin  { V5.21 }
        Inc(TraceCount);
        DebugLog(loSslDevel, Format('%s my_RealSend (0x%x, %d, %d) = %d   [%d]',
                                   [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                                   FHSocket,
                                   INT_PTR(Buf), Len, Result, TraceCount]));
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.my_BIO_should_retry(b: PBIO): Boolean;
begin
    if b = nil then begin
        Result := FALSE;
        Exit;
    end;
    Result := (BIO_should_retry(b) <> 0);   { V8.66 was boolean }
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslDevel) then begin  { V5.21 }
        Inc(TraceCount);
        DebugLog(loSslDevel, Format('%s BIO_should_retry(%s) = %d   [%d]',
                               [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                               GetMyBioName(b), Ord(Result), TraceCount]))
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomSslWSocket.Create(AOwner: TComponent);
begin
    FSslEnable              := FALSE;
    FSslContext             := nil;
    FSslAcceptableHosts     := TStringList.Create;
    FSslCertChain           := TX509List.Create(nil);
    FX509Class              := TX509Base;
    FMayTriggerFD_Read      := TRUE;
    FMayTriggerFD_Write     := TRUE;
    FMayTriggerDoRecv       := TRUE;
    FMayTriggerSslTryToSend := TRUE;
    inherited Create(AOwner);
    FSslBufList := TIcsBufferHandler.Create(nil);
    FSslBufList.BufSize := GSSL_BUFFER_SIZE;  // 4096  { V8.27 size now configurable }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomSslWSocket.Destroy;
begin
    try          { V8.71 JK }
        FreeAndNil(FSslPeerCert);
        { Removes TSslContext's free notification in a thread-safe way }
        SetSslContext(nil);
    finally
        try          { V8.71 JK }
            inherited Destroy;
        except
            // ignore execpton
        end;
    end;
    try      { V8.71 }
        FreeAndNil(FSslAcceptableHosts);
        DeleteBufferedSslData;
        FreeAndNil(FSslBufList);
        FreeAndNil(FSslCertChain);
        FreeAndNil(FSslAlpnProtoList);  { V9.1 }
    finally
        FinalizeSSL;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.InitializeSsl;
begin
    if FSslInitialized then
        Exit;
    LoadSsl;
    FSslInitialized := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.FinalizeSsl;
begin
    if not FSslInitialized then
        Exit;
    ResetSsl;
    UnloadSsl;
    FSslInitialized := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.Accept: TSocket;
begin
    Result := inherited Accept;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.SslOK: Boolean;               { V8.66 is TLS/SSL negotiated OK }
begin
    Result := False;
    if NOT Assigned(FSsl) then Exit;
    Result := (SSL_get_state(FSsl) = TLS_ST_OK);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Do_FD_ACCEPT(var Msg: TMessage);
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslDevel) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslDevel, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' Do_FD_ACCEPT handle=' + IntToStr(FHSocket));
{$ENDIF}
    inherited Do_FD_ACCEPT(msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.SocketDataPending : Boolean;
var
    Count : u_long;
begin
    FLastError := WSocket_Synchronized_ioctlsocket(FHSocket, FIONREAD, Count);
    Result := Count > 0;
    if FLastError = SOCKET_ERROR then begin
        FLastError := WSocket_WSAGetLastError;
        if (FLastError > WSABASEERR) and (FLastError <> WSAEWOULDBLOCK) and
           (FLastError <> WSAENOTCONN) then
        else
            FLastError := 0;
    end;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslDevel) then
        DebugLog(loSslDevel, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                 ' Socket data pending: ' + IntToStr(Count) + ' Err: ' +
                 IntToStr(FLastError) + ' handle=' + IntToStr(FHSocket));
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Do_FD_CLOSE(var Msg: TMessage);
var
    SslStOk : Boolean;
begin
    if (not FSslEnable) or (FSocksState <> socksData) or
       (FHttpTunnelState <> htsData) or
       (not Assigned(FSsl)) then begin
        inherited Do_FD_CLOSE(msg);
        Exit;
    end;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) + ' TCustomSslWSocket.Do_FD_CLOSE error #' +
          IntToStr(msg.LParamHi) + ', handle=' + IntToStr(FHSocket) + ', SslIntShutDown=' + IntToStr(FSslIntShutDown) +
          ', SocketState=' + SocketStateNames[FState] + ', DataPending=' + IntToStr(Ord(SocketDataPending)) +
          ', CloseInvoked=' + BoolToStr(FCloseInvoked, True) + ', BIOPending=' + IntToStr(my_BIO_ctrl_pending(FSslbio)) +
          ', SslState=' + GetEnumName(TypeInfo(OSSL_HANDSHAKE_STATE), Ord(SSL_get_state(FSsl)))  );   { V9.4 more }
{$ENDIF}
    if (FState = wsConnecting) or (FHSocket = INVALID_SOCKET) then
        Exit;

    SslStOk := SSL_get_state(FSsl) = TLS_ST_OK;  { V8.27, V8.66 }

    if not FCloseCalled then begin
        FCloseCalled := TRUE;
{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then
            DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                     ' TCustomSslWSocket.Do_FD_CLOSE CloseCalled handle=' + IntToStr(FHSocket) +    { V9.4 }
                        ', State=' + String(SSL_state_string_long(Fssl)) +  { V8.27 }
                        ' (' + GetEnumName(TypeInfo(OSSL_HANDSHAKE_STATE), Ord(SSL_get_state(FSsl))) + { V8.54 } { V8.66 }
                        '), Err=' + OpenSslErrMsg(FLastSslError));            { V8.54 }
{$ENDIF}
    end;
    if FNetworkError = 0 then begin
        { The connection was closed , we need to read as much as we can }
        { as well as process data pending in the SslBio }
        if (SslStOk and (my_BIO_ctrl_pending(FSslbio) > 0)) and (FSslIntShutDown < 2) then begin  { V9.4 not if stutdown started }
            TriggerEvents;
            Exit;
        end
        else if SocketDataPending and (FLastError = 0) then begin
          {$IFDEF POSIX}
            { We'll receive a FD_READ message in Windows only!    }
            { in POSIX we have to trigger a read event explicitly }
            TriggerEvent(sslFdRead, 0);
          {$ENDIF}
            Exit;
        end
        else if (FLastError > WSABASEERR) then
            FNetworkError := FLastError;

        if SslStOk and (FSslIntShutDown < 2) then begin
            FSslBiShutDownFlag := FALSE;
            InternalShutDown(1);
            Exit;
        end
    end;

    if FNetworkError > 0 then begin
        if (msg.LParamHi = 0) then
            msg.LParamHi := FNetworkError;
    end;

    if (not SslStOk) and (not (csDestroying in ComponentState)) then begin         // AG 03/03/06
        if NOT FHandshakeEventDone then  { V8.55 }
            TriggerSslHandshakeDone(1);  // error !!!
        FHandshakeEventDone := TRUE;  { V8.55 }
        if (FState = wsConnected) and (FSslIntShutDown < 2) and  // AG 03/03/06
           (msg.LParamHi = 0) then begin                         // AG 03/03/06
            inherited ShutDown(1);                               // AG 03/03/06
        end;
    end;

{$IFNDEF NO_DEBUG_LOG}
 {   if CheckLogOptions(loSslInfo) then
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                 ' FCloseInvoked=' + IntToStr(Ord(FCloseInvoked)) + ' handle=' + IntToStr(FHSocket) +
                 ', State=' + String(SSL_state_string_long(Fssl))); { V8.27 }   { V9.4 done earlier }
{$ENDIF}
    if (FHSocket <> INVALID_SOCKET) and (not FCloseInvoked) and {AG 12/30/07}
       (not (csDestroying in ComponentState)) then begin   // AG 03/03/06
        FCloseInvoked := TRUE;
        TriggerSessionClosed(msg.LParamHi);
    end;

    FSslEnable := FALSE;
    ResetSsl;

    if FState <> wsClosed then begin
        inherited InternalClose(FALSE, msg.LParamHi);  // close the socket
    //   FHSocket := INVALID_SOCKET;                                            { V9.4 ensure no more processing }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Do_FD_CONNECT(var Msg: TMessage);
begin
    FCloseCalled    := FALSE;
    FSslIntShutDown := 0;
    inherited Do_FD_CONNECT(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.TriggerEvent(Event: TSslEvent; ErrCode: Word): Boolean;
{$IFNDEF NO_DEBUG_LOG}
var
    S : String;
{$ENDIF}
begin
    Result := FALSE;
    if (not FSslEnable) or FPaused then { AG V7.26 FPause condition added }
        Exit;
    { Returns TRUE if a message was posted successfully and the socket isn't paused }
    if not (Event in FPendingSslEvents) then begin
        case Event of
            sslFdRead  :  Result := PostMessage(Handle, FMsg_WM_SSL_ASYNCSELECT,
                                        WParam(FHSocket), IcsMakeLong(FD_READ, ErrCode));  { V8.08 }
            sslFdWrite :  Result := PostMessage(Handle, FMsg_WM_SSL_ASYNCSELECT,
                                        WParam(FHSocket), IcsMakeLong(FD_WRITE, ErrCode)); { V8.08 }
            sslFdClose :  Result := PostMessage(Handle, FMsg_WM_SSL_ASYNCSELECT,
                                        WParam(FHSocket), IcsMakeLong(FD_CLOSE, ErrCode)); { V8.08 }
        end;
        if Result then
            FPendingSslEvents := FPendingSslEvents + [Event];
    end;
{$IFNDEF NO_DEBUG_LOG}
    if Result and CheckLogOptions(loSslDevel) then begin
        case Event of
            sslFdRead  : S := 'sslFdRead ';
            sslFdWrite : S := 'sslFdWrite ';
            sslFdClose : S := 'sslFdClose ';
            else
                S := 'Unknown';
        end;
        DebugLog(loSslDevel, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                 ' TriggerEvent handle=' + S + IntToStr(FHSocket));
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.ThreadAttach;
begin
    inherited ThreadAttach;
    { Re-post pending events to the new window }
    if sslFdRead in FPendingSslEvents then begin
        FPendingSslEvents := FPendingSslEvents - [sslFdRead];
        TriggerEvent(sslFdRead, 0);
    end;
    if sslFdWrite in FPendingSslEvents then begin
        FPendingSslEvents := FPendingSslEvents - [sslFdWrite];
        TriggerEvent(sslFdWrite, 0);
    end;
    if sslFdClose in FPendingSslEvents then begin
        FPendingSslEvents := FPendingSslEvents - [sslFdClose];
        TriggerEvent(sslFdClose, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Do_FD_READ(var Msg: TMessage);
var
    Len        : Integer; // How much to receive
    Buffer     : array{ [0..(SSL_BUFFER_SIZE * 2) -1]} of AnsiChar;  { V8.27 size now configurable }
    BuffSize   : Integer;  { V8.27 }
    NumRead    : Integer;
    nError     : Integer;
    Res        : Integer;
    PBuf       : TWSocketData;
    Dummy      : Byte;
begin
    if (not FSslEnable) or (FSocksState <> socksData) or  { V8.42 don't stop reading for non-SSL }
       (FHttpTunnelState <> htsData) then begin
        inherited Do_FD_READ(msg);
        Exit;
    end;

    BuffSize := (GSSL_BUFFER_SIZE * 2)-1;  { V8.27 size now configurable }
    SetLength(Buffer, BuffSize);

 { V8.22 moved here from Do_SSL_FD_READ  }
 { stop read windows events until we've processed this block }
    WSocket_Synchronized_WSAASyncSelect({$IFDEF POSIX}Self,{$ENDIF} FHSocket, Handle, FMsg_WM_ASYNCSELECT, FD_WRITE or FD_CLOSE or FD_CONNECT);
    try
      {$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslDevel) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
            DebugLog(loSslDevel, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) + ' TCustomSslWSocket.Do_FD_READ handle=' + IntToStr(FHSocket));
      {$ENDIF}

        if (FNetworkError > 0) then
            Exit;

        if (SSL_get_state(FSsl) = TLS_ST_OK){ V8.27 } and (FSslBioWritePendingBytes < 0) and // V8.66
           (my_BIO_ctrl_pending(FSslbio) > 0) then begin
          {$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loSslDevel) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
                DebugLog(loSslDevel, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) + ' TriggerDataAvailable (Do_FD_READ_1) handle=' + IntToStr(FHSocket));
          {$ENDIF}
            TriggerDataAvailable(0);
            Exit;
        end;

        FMayTriggerFD_Read := FALSE;

        { Get number of bytes we can receive and store in the network input bio.  }
        { New call to BIO_ctrl_get_read_request in order to read only the amount  }
        { of data from the socket that is needed to satisfy a read request, if    }
        { any. Without that call I had random errors on bi-directional shutdowns. }
        Len := my_BIO_ctrl_get_read_request(FNBio);
        if Len = 0 then
            Len := my_BIO_ctrl_get_write_guarantee(FNBio);
        if Len > BuffSize then   { V8.27 was sizeof(Buffer) }
            Len := BuffSize
        else if Len = 0 then begin
            FMayTriggerFD_Read := TRUE;
            TriggerEvents;
            Exit;
        end;
        // Receive data
        PBuf := @Buffer[0];
        NumRead := my_WSocket_recv(FHSocket, PBuf, Len, 0);
        if (NumRead > 0) then begin
            // Store it in the network input bio and process data
            my_BIO_write(FNBio, PBuf, NumRead);     { V8.27 was @Buffer }
            my_BIO_ctrl(FNBio, BIO_CTRL_FLUSH, 0, nil);
            // Look if input data was valid.
            // We may not call BIO_read if a write operation is pending !!
            if (FSslBioWritePendingBytes < 0) then begin
                Res := my_BIO_read(FSslBio, @Dummy, 0); //Pointer(1)
                if Res < 0 then begin
                    if not my_BIO_should_retry(FSslBio) then begin
                        HandleSslError;
                        if (not FExplizitSsl) or (SSL_get_state(FSsl) <> TLS_ST_OK)  { V8.27 } then begin   { V8.66 }
                            WSocket_WSASetLastError(WSAECONNABORTED);
                            FNetworkError := WSAECONNABORTED;
                            FLastError    := WSAECONNABORTED; //XX
                            TriggerEvent(sslFdClose, 0);
                        end
                        else begin
                            WSocket_WSASetLastError(WSAEWOULDBLOCK);
                            FLastError := WSAEWOULDBLOCK; //XX
                            FSslEnable := False;
                            ResetSsl;
                            if FSslIntShutDown < 2 then
                                TriggerSslShutDownComplete(FLastSslError);
                        end;
                      {$IFNDEF NO_DEBUG_LOG}
                        if CheckLogOptions(loSslErr) then  { V5.21 }
                            DebugLog(loSslErr, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                              ' NetworkError #' + IntToStr(FNetworkError) + ' TCustomSslWSocket.Do_FD_READ handle=' + IntToStr(FHSocket));  { V8.70 added more }
                      {$ENDIF}
                        Exit;
                    end
                    else begin
                        FMayTriggerDoRecv := TRUE;
                        WSocket_WSASetLastError(WSAEWOULDBLOCK);
                        FLastError := WSAEWOULDBLOCK; //XX
                    end;
                end
                else if (FSslVersNum >= SSL3_VERSION) then begin // Doesn't work in SSLv2 - 12/06/05
                    if SSL_get_Error(FSSL, Res) = SSL_ERROR_ZERO_RETURN then begin
                    { SSL closure alert received }
                        if FSslState < sslInShutdown then begin
                            FSslState := sslInShutdown;
                            { V7.80 }
                            if (not FSslBiShutDownFlag) then // May be set later in the message handler if a SSL bi-shutdown message is pending
                            begin
                                SslShutDownAsync(1); // If a SSL bi-shutdown is pending this will be ignored in the message handler
                                WSocket_WSASetLastError(WSAEWOULDBLOCK);
                                FLastError := WSAEWOULDBLOCK;
                                Exit;
                            end;
                            { / V7.80 }
                        end;
                        if (not FSslBiShutDownFlag) and (FSslIntShutDown = 2) then
                            TriggerEvent(sslFdClose, FNetWorkError);
                    end;
                    WSocket_WSASetLastError(WSAEWOULDBLOCK);
                    FLastError := WSAEWOULDBLOCK; //XX
                    FMayTriggerDoRecv := TRUE;
                end;
            end
            else begin
                FMayTriggerDoRecv := TRUE;
                WSocket_WSASetLastError(WSAEWOULDBLOCK);
                FLastError := WSAEWOULDBLOCK; //XX
              {$IFNDEF NO_DEBUG_LOG}
                if CheckLogOptions(loSslDevel) then  { V5.21 }
                   DebugLog(loSslDevel, 'SslBio write operation pending: ' + IntToStr(FSslBioWritePendingBytes));
              {$ENDIF}
            end;
        end else
        if Numread = 0 then begin
            if FState = wsconnected then begin
                TriggerEvent(sslFdClose, msg.LParamHi);
            end;
        end
        else if Numread = SOCKET_ERROR then begin
            nError := WSocket_WSAGetLastError;
            if (nError > WSABASEERR) and (nError <> WSAEWOULDBLOCK) and (nError <> WSAENOTCONN) and (nError <> WSAECONNABORTED) then begin   { V8.70 ignore Aborted error }
                FNetworkError := nError;
                FLastError    := FNetworkError;
                TriggerEvent(sslFdClose, 0);
              {$IFNDEF NO_DEBUG_LOG}
                if CheckLogOptions(loSslErr) then  { V5.21 }
                    DebugLog(loSslErr, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' NetworkError #' + IntToStr(FNetworkError) + ' TCustomSslWSocket.Do_FD_READ_2 handle=' + IntToStr(FHSocket));  { V8.70 added more }
              {$ENDIF}
                Exit;
            end;
        end;

        if (SSL_get_state(FSsl) = TLS_ST_OK) and  { V8.27, V8.66 }
           (FSslBioWritePendingBytes < 0) and // <= 12/08/05
           (my_BIO_ctrl_pending(FSslbio) > 0) then begin
          {$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loSslDevel) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
                DebugLog(loSslDevel, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) + ' TriggerDataAvailable (Do_FD_READ_2) handle=' + IntToStr(FHSocket));
          {$ENDIF}
            TriggerDataAvailable(0);
        end;

        if (FSslIntShutDown = 1) and SslShutDownCompleted(FShutDownHow) then begin
            if not FSslBiShutDownFlag then begin
                TriggerEvent(sslFdClose, 0);
                Exit;
            end;
        end;

        TriggerEvents;
    finally
       { V8.22 moved here from Do_SSL_FD_READ }
        WSocket_Synchronized_WSAASyncSelect({$IFDEF POSIX}Self,{$ENDIF} FHSocket, Handle, FMsg_WM_ASYNCSELECT, FD_READ or FD_WRITE or FD_CLOSE or FD_CONNECT);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Do_FD_WRITE(var Msg: TMessage);
var
    Len        : Integer;    // How much to send
    Buffer     : array { [0..16383] } of AnsiChar;  { V8.27 size now configurable }
    BuffSize   : integer;
    NumRead    : Integer;
    NumSent    : Integer;
    Err        : Cardinal;
begin
    if (not FSslEnable) or (FSocksState <> socksData) or
       (FHttpTunnelState <> htsData) then begin
        inherited Do_FD_WRITE(msg);
        Exit;
    end;

{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslDevel) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslDevel, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' TCustomSslWSocket.Do_FD_WRITE handle=' + IntToStr(FHSocket));
{$ENDIF}
    if (FNetworkError > 0) then
        Exit;

    FMayTriggerFD_Write := FALSE;
    BuffSize := (GSSL_BUFFER_SIZE * 2)-1;  { V8.27 size now configurable }
    SetLength(Buffer, BuffSize);

    // Send encrypted data in the send buffer
    inherited TryToSend;
    // May have closed the connection
    if (FHSocket = INVALID_SOCKET) then begin
{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslDevel) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
            DebugLog(loSslDevel, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                                          ' INVALID_SOCKET');
{$ENDIF}
        Exit;
    end
    else if not bAllSent then begin
{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslDevel) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
            DebugLog(loSslDevel, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                               ' * Not bAllSent handle=' + IntToStr(FHSocket));
{$ENDIF}
        FMayTriggerFD_Write := TRUE;
        { AG 01/10/07 - Outcommented next line in order to avoid too many  }
        { socket errors WSAEWOULDBLOCK, this is experimental and needs to  }
        { be tested heavily! If you experience strange hangs uncomment it  }
        { again.                                                           }
        //TriggerEvents;
        Exit;  // We have not sent everything
    end;

    // Send the data waiting in the network bio
    Len     := my_BIO_ctrl_pending(FNBio);
    if Len > BuffSize then Len := BuffSize;            { V8.27 sanity check }
    NumRead := my_BIO_read(FNBio, @Buffer[0], Len);    { V8.27 }
    if NumRead <= 0 then
        FMayTriggerFD_Write := TRUE;

    while (NumRead > 0) do begin
        NumSent := my_RealSend(@Buffer[0], NumRead);  { V8.27 }
        if NumSent = 0 then begin
            if FState = wsconnected then
                TriggerEvent(sslFdClose, 0);
        end;
        if (NumSent = SOCKET_ERROR) or (NumSent < NumRead) then begin
            if NumSent = SOCKET_ERROR then begin
                Err := WSocket_WSAGetLastError;
                if (Err > WSABASEERR) and
                   (Err <> WSAEWOULDBLOCK) and
                   (Err <> WSAENOTCONN) then begin
                    FNetworkError := Err;
                    FLastError    := Err; //XX
                    TriggerEvent(sslFdClose, 0);
{$IFNDEF NO_DEBUG_LOG}
                    if CheckLogOptions(loSslErr) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
                        DebugLog(loSslErr,
                            IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                            ' Winsock Error ' + WSocketErrorDesc(FNetworkError) +
                            ' handle=' + IntToStr(FHSocket));
{$ENDIF}
                    Exit;
                end
                else
                    NumSent := 0;
            end;
            bAllSent := FALSE;
            inherited PutDataInSendBuffer(@Buffer[NumSent], NumRead - NumSent);
        end;
        if NumSent = 0 then
            break;

        Len := my_BIO_ctrl_pending(FNBio);
        if Len = 0 then begin
            FMayTriggerFD_Write := TRUE;
            break;
        end;
        if Len > BuffSize then Len := BuffSize;            { V8.27 sanity check }
        NumRead := my_BIO_read(FNBio, @Buffer[0], Len);    { V8.27 }
        if Numread <= 0 then
            FMayTriggerFD_Write := TRUE;
    end;

    if (SSL_get_state(FSsl) = TLS_ST_OK) then begin { V8.27, V8.66 }
        if not bSslAllSent then
            TryToSend
        (* else if {bSslAllSent and} bAllSent and (my_BIO_ctrl_pending(FNBio)= 0) and
            (FSslState = sslEstablished) {FSslEstablished} then begin *)
        else if bAllSent and // condition replaced, note check in front 12/08/05
            (my_BIO_ctrl_pending(FNBio)= 0) and FSendPending then begin
            //Inc(FTriggerCount); //test
            FSendPending := FALSE;
            TriggerDataSent(0);
        end;
    end;

    if (FSslIntShutDown = 1) and SslShutDownCompleted(FShutDownHow) then begin
        if not FSslBiShutDownFlag then begin
            TriggerEvent(sslFdClose, 0);
            Exit;
        end;
    end;

    TriggerEvents;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.DoRecv(
    var Buffer : TWSocketData;
    BufferSize : Integer;
    Flags      : Integer): Integer;
var
    Numread : Integer;
begin
    if (not FSslEnable) or (FSocksState <> socksData) or
       (FHttpTunnelState <> htsData) then begin
        Result := inherited DoRecv(Buffer, BufferSize, Flags);
        Exit;
    end;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslDevel) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslDevel, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' TCustomSslWSocket.DoRecv handle=' + IntToStr(FHSocket));
{$ENDIF}
    if FNetworkError > 0 then begin
        if (my_BIO_ctrl_pending(FSslbio) > 0) and
           (FSslIntShutDown = 0) then begin
            Result := my_BIO_read(FSslbio, @Buffer, BufferSize);
            Exit;
        end;
        WSocket_WSASetLastError(FNetworkError);
        FLastError := FNetworkError; //XX
        Result     := SOCKET_ERROR;
        Exit;
    end;
    if FSslIntShutDown = 1 then begin
        WSocket_WSASetLastError(WSAESHUTDOWN);
        FLastError := WSAESHUTDOWN; //XX
        Result := SOCKET_ERROR;
        Exit;
    end;
    if (BufferSize = 0) then begin
        Result := 0;
        Exit;
    end;
    if (SSL_get_state(FSsl) <> TLS_ST_OK) or { V8.27, V8.66 }            //<= 01/01/06 AG
       (my_BIO_ctrl_pending(FSslbio) = 0) then begin
        if FState = wsclosed then begin
            Result := 0;
            Exit;
        end;
        if FCloseCalled then begin
            TriggerEvent(sslFdClose, 0);
            Result := 0;
            Exit;
        end
        else if FState <> wsconnected then begin
            WSocket_WSASetLastError(FNetworkError);
            FLastError := FNetworkError; //XX
            Result := SOCKET_ERROR;
            Exit;
        end;
        FMayTriggerDoRecv := TRUE;
        TriggerEvents;
        WSocket_WSASetLastError(WSAEWOULDBLOCK);
        FLastError := WSAEWOULDBLOCK; //XX
        Result := SOCKET_ERROR;
        Exit;
    end;

    Numread := my_BIO_read(FSslbio, Buffer, BufferSize);

    if Numread = 0 then begin
        if (FSslVersNum >= SSL3_VERSION) and { V7.80 }
           (SSL_get_error(FSsl, Numread) = SSL_ERROR_ZERO_RETURN) then begin
            { SSL closure alert received }
            if FSslState < sslInShutdown then begin
                FSslState := sslInShutdown;
                { V7.80 }
                if (not FSslBiShutDownFlag) then // May be set later in the message handler if a SSL bi-shutdown message is pending
                begin
                    SslShutDownAsync(1); // If SSL bi-shutdown is pending this will be ignored in the message handler
                    WSocket_WSASetLastError(WSAEWOULDBLOCK);
                    FLastError := WSAEWOULDBLOCK;
                    Result := SOCKET_ERROR;
                    Exit;
                end;
                { / V7.80 }
            end;
        end;
        FMayTriggerDoRecv := TRUE;
        TriggerEvents;
        WSocket_WSASetLastError(WSAEWOULDBLOCK);
        FLastError := WSAEWOULDBLOCK; //XX
        Result := SOCKET_ERROR;
        Exit;
    end;

    if Numread < 0 then begin
        if not my_BIO_should_retry(FSslbio) then begin
            HandleSslError;
            if not FExplizitSsl then begin
                FNetworkError := WSAECONNABORTED;
                WSocket_WSASetLastError(WSAECONNABORTED);
                FLastError := WSAECONNABORTED; //XX
                TriggerEvent(sslFdClose, 0);
                Result := SOCKET_ERROR;
            end
            else begin
                WSocket_WSASetLastError(WSAEWOULDBLOCK);
                FLastError := WSAEWOULDBLOCK; //XX
                Result     := SOCKET_ERROR;
                FSslEnable := FALSE;
                ResetSsl;
                if FSslIntShutDown < 2 then
                    TriggerSslShutDownComplete(FLastSslError);
            end;
{$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loSslErr) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
                DebugLog(loSslErr, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                                 ' NetworkError #' + IntToStr(FNetworkError) +
                                 ' TCustomSslWSocket.DoRecv handle=' + IntToStr(FHSocket));  { V8.70 added more }
{$ENDIF}
            Exit;
        end
        else begin
            FMayTriggerDoRecv := TRUE;
            // FMayTriggerFD_READ := TRUE;     // <= 12/14/05 ???
            TriggerEvents;
            WSocket_WSASetLastError(WSAEWOULDBLOCK);
            FLastError := WSAEWOULDBLOCK; //XX
            Result     := SOCKET_ERROR;
            Exit;
        end;
    end;
    FMayTriggerDoRecv := TRUE;
    TriggerEvents;
    Result := Numread;
    if (Result > 0) then begin
        FReadCount := FReadCount + Result;             { V8.30 was in Receive }
        if Assigned(FCounter) then
            FCounter.FLastRecvTick := IcsGetTickCount64;    { V8.71 } { V8.30 was missing }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.GetRcvdCount : Integer;
begin
    if csDesigning in ComponentState then begin
        Result := -1;
        Exit;
    end;

    if (not FSslEnable) or (FSocksState <> socksData) or
       (FHttpTunnelState <> htsData) then
        Result := inherited GetRcvdCount
    else
        Result := my_BIO_ctrl_pending(FSslbio);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.GetSslPeerCert: TX509Base;
begin
    if not Assigned(FSslPeerCert) then
        FSslPeerCert := FX509Class.Create(nil);
    Result := FSslPeerCert;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Close;
begin
    if not FCloseCalled then begin
        FCloseCalled       := TRUE;
        FSslBiShutDownFlag := FALSE;
{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then
            DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                     ' SocketCloseCalled handle=' + IntToStr(FHSocket));
{$ENDIF}
    end;

    if FSslEnable and Assigned(FSsl) and
      (SSL_get_state(FSsl) = TLS_ST_OK) and (my_BIO_ctrl_pending(FSslbio) > 0) then  { V8.27, V8.66 }
        TriggerEvents
    else begin
        inherited Close;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Shutdown(How : Integer);
begin
    if (FHSocket = INVALID_SOCKET) then
        Exit;
    if (not FSslEnable) or (not Assigned(FSsl)) then begin
        inherited ShutDown(How);
        Exit;
    end;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                 ' TCustomSslWSocket.ShutDown ' + IntToStr(How) + ' handle=' + IntToStr(FHSocket));
{$ENDIF}
    FShutDownHow       := How;
    FSslBiShutDownFlag := FALSE;
    InternalShutDown(How);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.SslShutdownCompleted(How: Integer) : Boolean;
var
    Buffer  : array [0..1023] of Char;
    NumRead : Integer;
begin
    if (not FSslEnable) or (not Assigned(FSsl)) then begin
        Result := TRUE;
        if FSslIntShutDown < 2 then begin
            FSslBiShutDownFlag := FALSE;
            FSslIntShutDown    := 2;
            if Assigned(FSsl) then
                ResetSsl;
            TriggerSslShutDownComplete(FNetworkError);
        end;
        if FState <> wsClosed then begin
            inherited ShutDown(How);
            inherited InternalClose(FALSE, 0);
        end;
{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then
            DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                     ' SslShutdownCompleted Repeated, handle=' + IntToStr(FHSocket));         { V9.4 }
{$ENDIF}
        Exit;
    end;
    Result := FALSE;
{   Manifest constants for Shutdown
    SD_RECEIVE                = 0;  // disables receives
    SD_SEND                   = 1;  // disables sends, Use this one for graceful close
    SD_BOTH                   = 2;  // disables both sends and receives
}
    try
        if (FNetworkError = 0) and
           ((FSslIntShutDown = 0) or (not bAllSent) {or (not bSslAllSent)}) then
            Exit;

        { A bi-directional SSL shutdown w/o socket  close. We need to       }
        { receive peer's shutdown notification before the SSL can be killed }
        { and communication may continue in plaintext data.                 }
        if FSslBiShutDownFlag and (FNetworkError = 0) then begin
            NumRead := SSL_get_shutdown(FSsl);
            if (NumRead and SSL_RECEIVED_SHUTDOWN = 0) or
               (NumRead and SSL_SENT_SHUTDOWN = 0) then
                Exit;
        end;

        // Empty read buffer
        repeat
            Numread := BIO_read(FSslbio, @Buffer, SizeOf(Buffer));
        until numread <= 0;

        if (my_BIO_ctrl_pending(FNbio) > 0) and (FNetworkError = 0) then
            Exit
        else begin  // SSL ShutDown is finished
            Result := TRUE;
            if FSslIntShutDown < 2 then begin
                FSslIntShutDown := 2;
                FSslState := sslShutdownComplete;
                if FSslBiShutDownFlag then begin
                    FSslEnable := FALSE;
                    ResetSsl;
                end;
                TriggerSslShutDownComplete(FNetworkError);
                if not FSslBiShutDownFlag then begin
                    inherited ShutDown(FShutDownHow);
                    //if not FCloseCalled then
                        TriggerEvent(sslFdClose, 0);
                end;
            end;
        end;
    finally
{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then
            DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                     ' SslShutdownCompleted *'+ IntToStr(Ord(Result)) + '* handle=' + IntToStr(FHSocket));
{$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.InternalShutdown(How: Integer);
var
    Res, Err : Integer;
begin
    // Apache server www.dfn-pca.de:443
{   Manifest constants for Shutdown
    SD_RECEIVE                = 0;  // disables receives
    SD_SEND                   = 1;  // disables sends, Use this one for graceful close
    SD_BOTH                   = 2;   //disables both sends and receives
}
    if (FHSocket = INVALID_SOCKET) or
       (not FSslEnable) or (not Assigned(FSsl) or
       (FSslState = sslShutdownComplete)) then begin
        Exit;
    end;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                 ' SslInternalShutdown handle=' + IntToStr(FHSocket));
{$ENDIF}

    if FSslIntShutDown = 0 then
        FSslIntShutDown := 1
    else begin
        if not SslShutDownCompleted(How) then begin
            TriggerEvents;
            WSocket_Synchronized_WSASetLastError(WSAEWOULDBLOCK);
            FLastError := WSAEWOULDBLOCK;
        end;
        Exit;
    end;

    if not FSslBiShutDownFlag then begin
        Res := SSL_get_shutdown(FSsl);
        if (Res and SSL_RECEIVED_SHUTDOWN = 0) then // not yet received a notify
            Res := SSL_shutdown(FSsl)             // send our notify
        else
            Res := 1;
    end
    else
        Res := SSL_shutdown(FSsl);             // send our notify

    if Res >= 0 then begin
        if Res = 0 then begin  // we have not yet received a notify from the peer
            FSslState := sslInShutdown;
            SSL_shutdown(FSsl);
        end;
        if not SslShutDownCompleted(How) then begin
            FMayTriggerFD_Write := TRUE;
            TriggerEvents;
            WSocket_Synchronized_WSASetLastError(WSAEWOULDBLOCK);
            FLastError := WSAEWOULDBLOCK {WSAECONNABORTED}; //XX  AG //03/03/06
        end;
    end
    else begin
        Err := SSL_get_error(FSsl, -1);
        if (Err = SSL_ERROR_WANT_READ) or
           (Err = SSL_ERROR_WANT_WRITE) then begin
            TriggerEvents;
            WSocket_Synchronized_WSASetLastError(WSAEWOULDBLOCK);
            FLastError := WSAEWOULDBLOCK {WSAECONNABORTED}; //XX  AG //03/03/06
        end
        else if not SslShutDownCompleted(How) then begin
            TriggerEvents;
            WSocket_Synchronized_WSASetLastError(WSAEWOULDBLOCK);
            FLastError := WSAEWOULDBLOCK {WSAECONNABORTED}; //XX  AG //03/03/06
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.WMTriggerSslShutDownComplete(var msg: TMessage);
begin
    TriggerSslShutDownComplete(msg.LParam);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.TriggerSslShutDownComplete(ErrCode: Integer);
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then
        DebugLog(loSslInfo,
                 Format('%s TriggerSslShutDownComplete(%d) %d',
                        [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                        ErrCode, FHSocket]));
{$ENDIF}
    if Assigned(FOnSslShutdownComplete) then
        FOnSslShutdownComplete(Self, FSslBiShutDownFlag, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.SetSslAcceptableHosts(Value : TStrings);
begin
    FSslAcceptableHosts.Assign(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.SetSslContext(const Value: TSslContext);
begin
    if Value <> FSslContext then begin
        if FSslContext <> nil then
            FSslContext.RemoveFreeNotification(Self);
        if Value <> nil then
            Value.FreeNotification(Self);
        FSslContext := Value;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Dup(NewHSocket: TSocket);
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' Dup accepting accepted socket = ' +
                      IntToStr(NewHSocket));
{$ENDIF}
    inherited Dup(NewHSocket);
    ChangeState(wsConnected);
    if FSslEnable then begin
        try
            case FSslMode of
                sslModeServer : AcceptSslHandshake;
                sslModeClient : StartSslHandshake;
            else
                raise Exception.Create('Invalid SslMode');
            end;
        except
            on E : Exception do begin
                FSslEnable := FALSE;
                ResetSSL;
                inherited InternalClose(FALSE, WSAECONNABORTED);
                HandleBackGroundException(E, 'TCustomSslWSocket.Dup');
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsSslRenegotiationDisallowed(Obj: TCustomSslWSocket): Boolean;
begin
    Result := True;  { V8.66 not recommended, easy to attack }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 handshake protocol message callback }
{$IFNDEF NO_DEBUG_LOG}

function FindSslVersions(Lit: integer): string;
var
    I: integer;
begin
    result := '' ;
    for I := 0 to High (LitsSslVersions) do begin
        if LitsSslVersions[I].L = Lit then begin
            result := LitsSslVersions[I].S;
            exit;
        end;
    end;
end ;

function FindAlertTypes(Lit: integer): string;
var
    I: integer;
begin
    result := '' ;
    for I := 0 to High (LitsAlertTypes) do begin
        if LitsAlertTypes[I].L = Lit then begin
            result := LitsAlertTypes[I].S;
            exit;
        end;
    end;
end ;

function FindHandshake(Lit: integer): string;
var
    I: integer;
begin
    result := '' ;
    for I := 0 to High (LitsHandshake) do begin
        if LitsHandshake[I].L = Lit then begin
            result := LitsHandshake[I].S;
            exit;
        end;
    end;
end ;

procedure SslProtoMsgCallback(write_p, version, content_type: integer;
                      buf: PAnsiChar; len: size_t; ssl: PSSL; arg: Pointer); {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}  { V8.51 corrected len, V8.64 not a function }
var
    Ws : TCustomSslWSocket;
    info: string;
    arg0, arg1: integer;
begin
    Ws := TCustomSslWSocket(SSL_get_ex_data(ssl, 0));
    arg0 := Ord(buf [0]);
    arg1 := Ord(buf [1]);
    if version <> 0 then begin
        info := FindSslVersions(version);
        case content_type of
            SSL3_RT_CHANGE_CIPHER_SPEC: info := info + ' Change Cipher Spec';
            SSL3_RT_ALERT: begin
                info := info + ' Alert, ';
                if arg0 = 1 then info := info + 'Warning: '
                else  if arg0 = 2 then info := info + 'Fatal: ';
                info := info + FindAlertTypes (arg1);
            end;
            SSL3_RT_HANDSHAKE: begin
                info := info + ' Handshake: ';
                info := info + FindHandshake (arg0);
                // pending look at handshake packets, cipher lists, certificates, etc
            end;
            SSL3_RT_APPLICATION_DATA: begin
                info := info + ' Application Data';
            end;
            DTLS1_RT_HEARTBEAT: begin
                info := info + ' Heartbeat: ';
                if arg0 = 1 then info := info + 'Request'
                else  if arg0 = 2 then info := info + 'Response';
            end;
            else
               info := info + ' Unknown Content Type: ' + IntToStr(content_type);  { V9.4 }
        end
    end
    else
        info := 'None';

//    info := info + ', State: ' + String(SSL_state_string(ssl));  // four letters
    info := info + ', State: ' + String(SSL_state_string_long(ssl));  // longer
    if write_p = 1 then
        info := info + ', Send'
    else
        info := info + ', Recv';

    if Ws.CheckLogOptions(loSslInfo) then begin
        Ws.DebugLog(loSslInfo, Format('ProtoMsg: %s, DataLen: %d, Data= %s',
                           [info, len, IcsBufferToHex (buf[0], len)]));
    end;

    if Assigned (Ws.FOnSslProtoMsg) then
        Ws.FOnSslProtoMsg(Ws, info, write_p, version, content_type, buf, len);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF NO_DEBUG_LOG}
{ V8.69 client only, report extesion the server sends, may be calle several times }

procedure TlsExtension_cb(SSL: PSSL; client_server: Integer; type_: Integer;
  data: PAnsiChar; len: Integer; arg: Pointer); cdecl;
var
    ExtName : String;
    CS : String;
    Ws : TCustomSslWSocket;
begin
    Ws := TCustomSslWSocket(SSL_get_ex_data(Ssl, 0));
    case type_ of
      TLSEXT_TYPE_server_name :
          ExtName := 'server name';
      TLSEXT_TYPE_max_fragment_length :
          ExtName := 'max fragment length';
      TLSEXT_TYPE_client_certificate_url :
          ExtName := 'client certificate URL';
      TLSEXT_TYPE_trusted_ca_keys :
          ExtName := 'trusted CA keys';
      TLSEXT_TYPE_truncated_hmac :
          ExtName := 'truncated HMAC';
      TLSEXT_TYPE_status_request :
          ExtName := 'status request (OCSP stapling)';
      TLSEXT_TYPE_elliptic_curves :
          ExtName := 'elliptic curves';
      TLSEXT_TYPE_ec_point_formats :
          ExtName := 'EC point formats';
      TLSEXT_TYPE_session_ticket :
          ExtName := 'server ticket';
      TLSEXT_TYPE_signature_algorithms :
          ExtName := 'signature algorithms';    { V8.56 }
      TLSEXT_TYPE_use_srtp :
          ExtName := 'use srtp';    { V8.56 }
      TLSEXT_TYPE_heartbeat :
          ExtName := 'heartbeat';    { V8.56 }
      TLSEXT_TYPE_application_layer_protocol_negotiation :
          ExtName := 'application layer protocol negotiation';    { V8.56 }
      TLSEXT_TYPE_signed_certificate_timestamp :
          ExtName := 'signed certificate timestamp';    { V8.56 }
      TLSEXT_TYPE_padding :
          ExtName := 'padding';    { V8.56 }
      TLSEXT_TYPE_encrypt_then_mac :
          ExtName := 'encrypt then mac';    { V8.56 }
      TLSEXT_TYPE_extended_master_secret :
          ExtName := 'extended master secret';    { V8.56 }
      TLSEXT_TYPE_psk :
          ExtName := 'psk';    { V8.56 }
      TLSEXT_TYPE_early_data :
          ExtName := 'early_data';    { V8.56 }
      TLSEXT_TYPE_supported_versions :
          ExtName := 'supported_versions';    { V8.56 }
      TLSEXT_TYPE_cookie :
          ExtName := 'cookie';    { V8.56 }
      TLSEXT_TYPE_psk_kex_modes :
          ExtName := 'psk_kex_modes';    { V8.56 }
      TLSEXT_TYPE_certificate_authorities :
          ExtName := 'certificate_authorities';    { V8.56 }
      TLSEXT_TYPE_post_handshake_auth :
          ExtName := 'post_handshake_auth';    { V8.56 }
      TLSEXT_TYPE_signature_algorithms_cert :
          ExtName := 'signature_algorithms_cert';    { V8.56 }
      TLSEXT_TYPE_key_share :
          ExtName := 'key_share';    { V8.56 }
      TLSEXT_TYPE_renegotiate :
          ExtName := 'renegotiate';    { V8.56 }
    else
        ExtName := 'unknown';
    end;
    if client_server = 0 then
        CS := 'client'
    else
        CS := 'server';
    if Ws.CheckLogOptions(loSslInfo) then
         Ws.DebugLog(loSslInfo, Format('TLSExtCB> TLS %s extension "%s" (id=%d), len=%d',
                                        [CS, ExtName, Type_, len]));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.27 list all ciphers supported for current SSL context which must be initialised }
{ Supported=True returns only ciphers acceptable according to protocol and settings,
  otherwise it's complete list available
  Remote=True for server client connections gets list sent by remote client,
  otherwise it's the list the client or servers supports according to protocol settings }
function TCustomSslWSocket.SslGetSupportedCiphers (Supported, Remote: boolean): String;
var
    I, Total: Integer;
    NewSSL: Boolean;
    MySsl: PSSL;
    MyStack: PSTACK_OF_SSL_CIPHER;
    MyCipher: PSSL_CIPHER; { PAnsiChar;  V8.66 }
begin
    Result := '';
    NewSSL := false;
    if (NOT Assigned(FSsl)) then begin
        if NOT Assigned(FSslContext.SslCtxPtr) then Exit;
      //Create temporary SSL Object
        MySsl := SSL_new(FSslContext.SslCtxPtr);
        if not Assigned(MySsl) then
            RaiseLastOpenSslError(Exception, TRUE,
                                  'Error on creating the Ssl object');
       NewSSL := true;
    end
    else
       MySsl := FSsl;

    if Supported then begin
       if Remote then
            MyStack := SSL_get_client_ciphers(MySsl)   // list received by server from remote client
       else
            MyStack := SSL_get1_supported_ciphers(MySsl)  // list supported by client or server
    end
    else
        MyStack := SSL_get_ciphers(MySsl);   // all ciphers
    Total := OPENSSL_sk_num(MyStack);
    if Total <= 0 then Exit;
    for I := 0 to Total - 1 do begin
        MyCipher := PSSL_CIPHER(OPENSSL_sk_value(MyStack, I));
        if Assigned (MyCipher) then
            Result := Result + String(SSL_CIPHER_get_name(MyCipher)) + #13#10;
    end;
    if Supported and (NOT Remote) then OPENSSL_sk_free(MyStack);
    if NewSSL then SSL_free(MySsl);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.64 convert list of ciphers from Client Hello to strings, 1.1.1 and later }
{ Warning - API always returns empty stacks }
function TCustomSslWSocket.SslBytesToCiphers(const CList: TBytes): String;
var
    I, Total: Integer;
    MyStack1, MyStack2: PSTACK_OF_SSL_CIPHER;
    MyCipher: PSSL_CIPHER; { PAnsiChar;  V8.66 }
begin
    Result := '';
    if (NOT Assigned(FSsl)) then Exit;
    if Length(CList) = 0 then Exit;
//    if NOT Assigned(SSL_bytes_to_cipher_list) then Exit;
    MyStack1 := OPENSSL_sk_new_null;
    MyStack2 := OPENSSL_sk_new_null;
    if SSL_bytes_to_cipher_list(FSsl, @CList, Length(CList), 0, @MyStack1, @MyStack2)<> 1 then begin    { V8.66 0 not false }
        RaiseLastOpenSslError(Exception, TRUE, 'Error converting cipher list');
        Exit;
    end;
    Total := OPENSSL_sk_num(MyStack1);
    if (Total <= 0) or (Total > 50) then Exit;
    for I := 0 to Total - 1 do begin
        MyCipher := PSSL_CIPHER(OPENSSL_sk_value(MyStack1, I));
        if Assigned (MyCipher) then
            Result := Result + String(SSL_CIPHER_get_name(MyCipher)) + #13#10;
    end;
    OPENSSL_sk_free(MyStack1);
    OPENSSL_sk_free(MyStack2);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.SslSessionReused : Boolean;
begin
    Result := FSslEnable and Assigned(FSsl) and
                (SSL_session_reused(FSsl) = 1);   { V8.51, V8.66 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Server mode checks whether a renegotiation request is pending             }
function TCustomSslWSocket.SslRenegotiatePending : Boolean;
begin
    Result := FALSE;
 // no longer supported
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Forces a SSL re-negotiation, app. data can still be received.             }
function TCustomSslWSocket.SslStartRenegotiation : Boolean;
begin
    Result := FALSE;
 // no longer supported
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.StartSslHandshake;
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) + ' StartSslHandshake handle=' + IntToStr(FHSocket));
{$ENDIF}
    if not FSslEnable then
        Exit;
    if not Assigned(FSslContext) then
        raise ESslContextException.Create('SSL requires a context object');
    if Assigned(FSsl) then
        ResetSsl;
    FSslState := sslHandshakeInit;
    FSslMode  := sslModeClient;
    try
        if (FSslContext.SslCtxPtr = nil) then
            FSslContext.InitContext;
        InitSslConnection(TRUE, FSslContext.SslCtxPtr);
    except
        on E : Exception do begin
            FSslState := sslNone;
{$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loSslErr) then  { V8.57 was loSslInfo }
                DebugLog(loSslErr, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                          ' Fatal error StartSslHandshake handle=' + IntToStr(FHSocket) +
                          ' ' + E.Classname + ' ' + E.Message);
{$ENDIF}
            if NOT FHandshakeEventDone then
                TriggerSslHandshakeDone(1);  { V8.55 handshake failed during negotiation }
            FHandshakeEventDone := TRUE;  { V8.55 }
            raise
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.AcceptSslHandshake;
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' AcceptSslHandshake handle=' + IntToStr(FHSocket));
{$ENDIF}
    if not FSslEnable then
        Exit;
    if not Assigned(FSslContext) then
        raise ESslContextException.Create('SSL requires a context object');
    if Assigned(FSsl) then
        ResetSsl;
    FSslState := sslHandshakeInit;
    FSslMode  := sslModeServer;
    FHandshakeEventDone := FALSE;  { V8.55 }
    try
        if (FSslContext.SslCtxPtr = nil) then
            FSslContext.InitContext;
        InitSslConnection(FALSE, FSslContext.SslCtxPtr);
    except
        on E : Exception do begin
            FSslState  := sslNone;
{$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loSslErr) then  { V8.57 was loSslInfo }
                DebugLog(loSslErr, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                          ' Fatal error AcceptSslHandshake handle=' + IntToStr(FHSocket) +
                          ' ' + E.Classname + ': ' + E.Message);
{$ENDIF}
            if NOT FHandshakeEventDone then
                TriggerSslHandshakeDone(1);  { V8.55 handshake failed during negotiation }
            FHandshakeEventDone := TRUE;  { V8.55 }
            raise
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.DupConnected;
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' DupConnected');
{$ENDIF}
    inherited DupConnected;     { V9.5 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSSLWSocket.InternalAbort(ErrCode : Word);
begin
    FSslEnable := FALSE;
    ResetSsl;
    InternalCancelDnsLookup(TRUE);
    DeleteBufferedData;
    { Be sure to close as fast as possible (abortive close) }
    if (State = wsConnected) and (FProto = IPPROTO_TCP) then begin
        LingerOnOff := wsLingerOff;
        SetLingerOption;
    end;
    //inherited ShutDown(2);
    inherited InternalClose(FALSE, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.InternalClose(bShut: Boolean; Error: Word);
begin
    if (not FSslEnable) or (not Assigned(FSsl)) then begin
        inherited InternalClose(bShut, Error);
        Exit;
    end;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' SslInternalClose handle=' + IntToStr(FHSocket));
{$ENDIF}
    if FHSocket = INVALID_SOCKET then begin
        if FState <> wsClosed then begin
            ChangeState(wsClosed);
            AssignDefaultValue;
        end;
        Exit;
    end;
    if FState = wsClosed then
        Exit;


    if bShut then begin
        if (not (csDestroying in Componentstate)) and (FSslIntShutDown = 0) then // AG 03/03/06
            ShutDown(1) // sends a SSL shutdown notify, then calls inherited ShutDown(1);
        else begin
            if FSslIntShutDown = 0 then
                inherited ShutDown(1);
            inherited InternalClose(FALSE, Error); // close the socket
        end;
    end
    else
        inherited InternalClose(FALSE, Error); // close the socket
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Listen;
begin
    inherited Listen;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' Listening');
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.TriggerSslVerifyPeer(
    var Ok     : Integer;
    Cert       : TX509Base);
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslInfo, 'TriggerSslVerifyPeer');
{$ENDIF}
    if Assigned(FOnSslVerifyPeer) then
        FOnSslVerifyPeer(Self, Ok, Cert);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.1 set SslContext callbacks, previously in InitContext }
{ must be called before an SslContext is used for a connection }
procedure TCustomSslWSocket.SetSslCallbacks(pSSLContext: PSSL_CTX);
begin
 { set verify callback and mode }
    FSslContext.VerifyCallbackPtr := @PeerVerifyCallback;  { keep so verify can be changed later }
    if FSslContext.SslVerifyPeer then begin
        SSL_CTX_set_verify(pSSLContext, FSslContext.SslVerifyPeerModesValue, @PeerVerifyCallback);
        SSL_CTX_set_verify_depth(pSSLContext, FSslContext.SslVerifyDepth);
    end
    else begin
        SSL_CTX_set_verify(pSSLContext, 0, Nil);
        SSL_CTX_set_verify_depth(pSSLContext, 0);
    end;

 { Set session callbacks, ssl server mode only }
    SSL_CTX_sess_set_new_cb(pSSLContext, @NewSessionCallback);
    SSL_CTX_sess_set_get_cb(pSSLContext, @GetSessionCallback);

 { V8.64 OpenSSL 1.1.1 and later server uses client_hello callback instead of ServerName }
    SSL_CTX_set_client_hello_cb(pSSLContext, @ClientHelloCallBack, Self);

 { V9.1 was later, servers only }
    SSL_CTX_set_tlsext_servername_callback(pSSLContext,  @ServerNameCallBack);

  { V8.56 set application layer protocol select callback, servers only }
    SSL_CTX_set_alpn_select_cb(pSSLContext, @AlpnSelectCallBack, Self);

  { V8.69 OCSP stapling status requested, check certificate still valid }
  { on client we receive status in callback, on server we sent cached status }
    if FSslContext.SslOcspStatus then begin                                   { V9.1 }
        SSL_CTX_set_tlsext_status_type(pSSLContext, TLSEXT_STATUSTYPE_ocsp);
        if SSL_CTX_set_tlsext_status_cb(pSSLContext, @TlsStatusCallback) = 0 then
            RaiseLastOpenSslError(ESslContextException, TRUE, 'SSL_CTX_set_tlsext_status_cb failed');
        SSL_CTX_set_tlsext_status_arg(pSSLContext, Self);
    end;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.InitSSLConnection(ClientMode: Boolean; pSSLContext: PSSL_CTX = nil);
var
    Count             : Integer;
    ReadBytes         : size_t;  { V8.51 }
    SessionIDContext  : TSslSessionIdContext; // for session caching in ssl server mode
    SslCachedSession  : Pointer;
    FreeSession       : Boolean;
    SIdCtxLen         : Integer;
    Dummy             : Byte;
    VerifyParam       : PX509_VERIFY_PARAM;  { V8.39 }
    AHost             : AnsiString;  { V8.64 }
    AlpnBuffer        : TBytes;    { V9.1 }
    AlpnBufflen       : Integer;
    SocFamily         : TSocketFamily;  { V9.5 }
begin
    if not FSslEnable then
        Exit;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) + ' InitSSLConnection handle=' + IntToStr(FHSocket));
{$ENDIF}
    FSslCertChain.Clear;
    FSslCertChain.X509Class := FX509Class;
    FSslCertChain.LastVerifyResult := 0;
    if not Assigned(FSslPeerCert) then
        FSslPeerCert := FX509Class.Create(nil);
    FSslPeerCert.AssignDefaults;
    FSslPeerCert.FreeAndNilX509;
    //FTriggerCount            := 0; //Test
    FShutDownHow             := 1;
    FSslIntShutDown          := 0;
    FNetworkError            := 0;
    FLastSslError            := 0;
    FSslBiShutDownFlag       := FALSE;
    FCloseCalled             := FALSE;
    FSslHandshakeRespMsg     := '';  { V8.14 set with success or failure message once handshake completes }
    FSslHandshakeErr         := 0;   { V8.14 }
    FSslCipherDesc           := '';  { V8.14  }
    FSslEncryption           := '';  { V8.14  }
    FSslKeyExchange          := '';  { V8.14  }
    FSslMessAuth             := '';  { V8.14  }
    FSslCertPeerName         := '';  { V8.39  }
    FHandshakeEventDone      := FALSE;  { V8.55 }
    FSslAlpnProtoRcvd        := '';  { V8.62  }
    FSslVerifyResult         := -1;  { V8.66  }
    FOcspStapleStatus        := -1;  { V8.69  }
    FOcspStapleRaw           := '';  { V8.69  }
    FPendingSslEvents        := [];
    FMayTriggerFD_Read       := TRUE;  // <= 01/06/2006 AG
    FMayTriggerFD_Write      := TRUE;  // <= 01/06/2006 AG
    FMayTriggerDoRecv        := TRUE;  // <= 01/06/2006 AG
    FMayTriggerSslTryToSend  := TRUE;  // <= 01/06/2006 AG
    InitializeSsl; // Load libraries
    try
        SslCritSect.Enter;
        try
         { V9.1 callbacks now set here instead ofInitContext, callbacks must be before SSL_new(ctx) !! }
            SetSslCallbacks(pSSLContext);

          // Create new SSL Object
            FSsl := SSL_new(pSSLContext); // FSsl inherits all options
            if not Assigned(FSsl) then
                RaiseLastOpenSslError(Exception, TRUE, 'Error on creating the Ssl object');

          // Create filter BIO for SSL I/O
            FSslBio := BIO_new(BIO_f_ssl);
            if (FSslBio = nil) then
                RaiseLastOpenSslError(Exception, TRUE, 'Creating SslBIO failed');

          // create two more BIOs for reading and writing
            BIO_new_bio_pair(@FIBio, GSSL_BUFFER_SIZE, @FNBio, GSSL_BUFFER_SIZE);  { V8.27 size now configurable }
            if (FNBio = nil) or (FIBio = nil) then
                RaiseLastOpenSslError(Exception, TRUE, 'Creating Read/Write BIOs failed');

          // set Self object pointer for callbacks
            if SSL_set_ex_data(FSsl, 0, Self) <> 1 then
                RaiseLastOpenSslError(Exception, TRUE, 'SSL_set_ex_data failed');

            {if FCount mod 2 = 0 then  // Test
                raise Exception.Create('Test exception');}

          // Init SSL connection
            if ClientMode then begin
                // If we want send client certificates different from default
                if (FSslContext.SslCertFile = '') and Assigned(FOnSslCliCertRequest) and
                                                           (not Assigned(SSL_CTX_get_client_cert_cb(pSSLContext))) then
                    SSL_CTX_set_client_cert_cb(pSSLContext, @ClientCertCallBack);
                // Get a cached session from the application
                SslCachedSession := nil;
                FreeSession      := TRUE;

                if Assigned(FOnSslCliGetSession) then
                    FOnSslCliGetSession(Self, SslCachedSession, FreeSession);
                if Assigned(SslCachedSession) and (SSL_set_session(FSsl, SslCachedSession) = 1) then begin  // 01/14/06 AG
                {$IFNDEF NO_DEBUG_LOG}
                    if CheckLogOptions(loSslInfo) then  { V5.21 }
                        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) + ' Reuse session [' +
                                                 IntToHex(INT_PTR(SslCachedSession), SizeOf(SslCachedSession) * 2) +']');
                {$ENDIF}
                    if FreeSession then
                        SSL_SESSION_Free(SslCachedSession);
                    SslCachedSession := nil;
                end
                else begin
                    if SSL_set_session(FSsl, nil) = 0 then
                        RaiseLastOpenSslError(EOpenSslError, TRUE, 'Unable to set session');
                end;

               { V8.39 get pointer to verify parameters, which we may alter in a moment }
                VerifyParam := SSL_get0_param(FSsl);    { do not free it! }

                { FSslServerName is the servername to be sent in client helo. }
                { If not empty, enables SNI in SSL client mode.               }
                if (FSslServerName <> '') then begin
                 { V9.5 server name extension not allowed raw IP addresses, convert then to domain names }
                    if WSocketIsIP(FSslServerName, SocFamily) then
                        AHost := IcsReverseIPArpa(FSslServerName)    { V9.5 }
                    else
                    { V8.64 needs A-Label punycode, not UTF8 }
                        AHost := AnsiString(IcsIDNAToASCII(FSslServerName));
                    if (SSL_set_tlsext_host_name(FSsl, AHost) = 0) then    { V9.5 AnsiString version }
                        RaiseLastOpenSslError(EOpenSslError, TRUE, 'Unable to set TLS servername extension');

                 { V8.39 set host so certificate common name or domains can be validated }
                    if (FSslContext.SslCheckHostFlagsValue <> -1) then begin
                        X509_VERIFY_PARAM_set_flags(VerifyParam, FSslContext.SslVerifyFlagsValue);
                        X509_VERIFY_PARAM_set_depth(VerifyParam, FSslContext.SslVerifyDepth);
                        X509_VERIFY_PARAM_set_hostflags(VerifyParam, FSslContext.SslCheckHostFlagsValue);
                        if (X509_VERIFY_PARAM_set1_host(VerifyParam, Pointer(AHost), Length (AHost)) = 0) then  { V8.64 }
                                 RaiseLastOpenSslError(EOpenSslError, TRUE, 'Unable to set host varify param');
                    end;
                end
                else
                    X509_VERIFY_PARAM_set1_host(VerifyParam, Nil, 0);  { clear old host }

             { V9.1 set ALPN protocols supported to send to server, if set }
                if Assigned(FSslAlpnProtoList) and (FSslAlpnProtoList.Count > 0) then begin
                    IcsStrListToWireFmt(FSslAlpnProtoList, AlpnBuffer);
                    AlpnBufflen := Length(AlpnBuffer);
                    if AlpnBufflen > 0 then begin
                        if SSL_set_alpn_protos(FSsl, @AlpnBuffer[0], AlpnBufflen) <> 0 then
                            RaiseLastOpenSslError(Exception, TRUE, 'Error setting alpn protos');
                    end;
                end;

{$IFNDEF NO_DEBUG_LOG}
           { V8.69 report TLS extensions sent by server to client  }
                if CheckLogOptions(loSslInfo) then
                     SSL_set_tlsext_debug_callback(FSsl, @TlsExtension_CB);
{$ENDIF}
                SSL_set_connect_state(FSsl);   { sets and starts client handshake, eventually }
            end
            else begin // Server mode
                if Assigned(FOnSslSetSessionIDContext) then begin
                    SessionIDContext := '';
                    FOnSslSetSessionIDContext(Self, SessionIDContext);
                    { This is so bad. We should consider a breaking change }
                    { and use AnsiString, same for session keys :(         }
                    SIdCtxLen := Length(SessionIDContext) * SizeOf(Char);
                    if SIdCtxLen > 0 then begin
                        if SIdCtxLen > SSL_MAX_SSL_SESSION_ID_LENGTH then
                        { Should trigger an exception rather than silently }
                        { truncate the data..                              }
                            SIdCtxLen := SSL_MAX_SSL_SESSION_ID_LENGTH;
                        { So with Unicode there are only 16 items left.    }
                        if ssl_set_session_id_context(FSsl,  @SessionIDContext[1], SIdCtxLen) = 0 then begin
                    {$IFNDEF NO_DEBUG_LOG}
                            if CheckLogOptions(loSslErr) then { V5.21 }
                                DebugLog(loSslErr, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                                                      ' ssl_set_session_id_context failed handle=' + IntToStr(FHSocket));
                        end
                        else begin
                            if CheckLogOptions(loSslInfo) then
                                DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                                                  ' SessionIDContext: ' + SessionIDContext + ' handle=' + IntToStr(FHSocket));
                    {$ENDIF}
                        end;
                    end;
                end;

                { FSslServerName receives the servername from client helo if }
                { FOnSslServerName was assigned in SSL server mode.          }
                FSslServerName := '';
//              if { Assigned(FOnSslServerName) and }
//                    (FSslContext.FSslVersionMethod >= sslV3) then begin   { V8.24 not SSLv2, V8.56 always enabled }

            //    if (SSL_CTX_set_tlsext_servername_callback(pSSLContext,  @ServerNameCallBack) = 0) then  { V9.1 moved earlier }
            //        RaiseLastOpenSslError(EOpenSslError, TRUE, 'Unable to initialize servername callback for SNI');

              { V9.1 try again if failed earlier, servers only }
                SSL_CTX_set_alpn_select_cb(pSSLContext, @AlpnSelectCallBack, Self);

                SSL_set_accept_state(FSsl);     { sets and starts server handshake, eventually }
            end;

         // connect our read and write BIOs to the SSL object
            SSL_set_bio(FSsl, FIBio, FIBio);

         // callback to receive state information
            SSL_set_info_callback(FSsl, @InfoCallBack);   { V8.66 }

{$IFNDEF NO_DEBUG_LOG}
          { V8.40 protocol message callback for handshake debugging }
            if (CheckLogOptions(loSslInfo) or Assigned (FOnSslProtoMsg)) then
                SSL_set_msg_callback(FSsl, @SslProtoMsgCallback);   { V8.66 }
{$ENDIF}

            ERR_clear_error;

         // sets the internal SSL pointer to our filter BIO
            if my_BIO_ctrl(FSslBio, BIO_C_SET_SSL, BIO_NOCLOSE, FSsl) = 0 then     { BIO_set_ssl macro }
                RaiseLastOpenSslError(EOpenSslError, TRUE, 'Unable to set BIO as SSL');

         // try and read 0 to check filter BIO is set
{$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loSslInfo) then  { V8.51 }
                DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) + ' Start Ssl ReadBIO');
{$ENDIF}
            Count := BIO_read_ex(FSslbio, @Dummy, 0, @ReadBytes);     { V8.51 new in 1.1.1 V8.66 var gone }
            if Count <= 0 then begin
                if not my_BIO_should_retry(FSslbio) then begin
                    { Usually happens with an invalid context option set }
                    { V8.55 such as no ciphers available, tell someone!! }
                    Count := BIO_get_retry_reason(FSslbio);
                    RaiseLastOpenSslError(EOpenSslError, TRUE, 'InitSSLConnection: ReadBIO, Retry Reason ' + IntToStr(Count));
                end;
            end;

            //Initialize SSL negotiation
            if (FState = wsConnected) then begin
                TriggerEvent(sslFdRead, 0);
                TriggerEvent(sslFdWrite, 0);
            end;

            // Not a real error. Used to break the loop in TCustomWSocket.ASyncReceive
            FLastError := -1;
            FSslState  := sslHandshakeStarted;
        finally
            SslCritSect.Leave;
        end;
    except
        SslCachedSession := nil;                                 // 01/14/06 AG
        FSslState := sslHandshakeFailed; // just to allow the Reset
        ResetSsl;
        FSslState := sslHandshakeFailed;
        raise
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.AssignDefaultValue;
begin
    ResetSsl;
    inherited AssignDefaultValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.DeleteBufferedSslData;
begin
    if not Assigned(FSslBufList) then
        Exit;
    { Delete all data buffer }
    FSslBufList.Lock;
    try
        FSslBufList.DeleteAllData;
    finally
        FSslBufList.UnLock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.PutDataInSslBuffer(Data : Pointer; Len : Integer);
begin
    FSendPending := TRUE;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslDevel) then begin  { V5.21 }
        Inc(TraceCount);
        DebugLog(loSslDevel,
                 Format('%s PutDataInSslBuffer handle= %s len %d  [%d] ',
                       [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                       IntToStr(FHSocket), Len, TraceCount]));
    end
    else if CheckLogOptions(loSslDump) then begin  { V5.21 }
        Inc(TraceCount);
        DebugLog(loSslDump,
                 Format('%s PutDataInSslBuffer handle=%s [%d] Data:%s',
                        [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                        IntToStr(FHSocket), TraceCount, DataToString(Data, Len)]));
    end;
{$ENDIF}
    if (Len <= 0) or (Data = nil) then
        Exit;

    FSslBufList.Lock;
    try
        FSslBufList.Write(Data, Len);
        bSslAllSent := FALSE;
    finally
        FSslBufList.UnLock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.ResetSslDelayed;
begin
    PostMessage(FWindowHandle, FMsg_WM_RESET_SSL, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Resume;
begin
    inherited;
    TriggerEvents;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.ResetSSL;
begin
    if FSsl_In_CB or (FSslState = sslHandshakeInit) then
        Exit;

{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) {and Assigned(FSsl)} then  { V5.21 }    { V8.70 even if no FSsl }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                 ' ResetSslSession starting handle=' + IntToStr(FHSocket));
{$ENDIF}

    try
        DeleteBufferedSslData;
        FSslVersion              := 'Unknown';
        FSslCipher               := FSslVersion;
        FSslTotalBits            := 0;
        FSslSecretBits           := 0;
        FSslVersNum              := 0;
        FSslCipherDesc           :='';     { V8.14 }

      {  FInHandshake             := FALSE;  V8.55 }
        FHandshakeEventDone      := FALSE;  { V8.55 }
        FHandshakeDone           := FALSE;
    // V8.55 reset is called during error handling, don't clear error reasons
    //    FSslHandshakeRespMsg     := '';  { V8.14 set with success or failure message once handshake completes }
    //    FSslHandshakeErr         := 0;   { V8.14 }
        FSslCipherDesc           := '';  { V8.14  }
        FSslEncryption           := '';  { V8.14  }
        FSslKeyExchange          := '';  { V8.14  }
        FSslMessAuth             := '';  { V8.14  }
        FSslBioWritePendingBytes := -1;
        FSslInRenegotiation      := FALSE;
        FNetworkError            := 0;
        FSslState                := sslNone;
        FHandShakeCount          := 0;
        FSslSupportsSecureRenegotiation := FALSE;

{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then
            DebugLog(loSslInfo, 'ResetSslSession SetShutDown FSsl handle=' + IntToStr(Integer(FSsl)));       { V8.70 !!!! }
{$ENDIF}
        if Assigned(FSsl) then // Avoids sending a shutdown notify on freeing the SSL
            SSL_set_shutdown(FSsl, SSL_SENT_SHUTDOWN);

{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then
            DebugLog(loSslInfo, 'ResetSslSession FSslBio handle=' + IntToStr(Integer(FSslbio)));       { V8.70 !!!! }
{$ENDIF}
        if Assigned(FSslbio) then
            BIO_free(FSslbio);
        FSslbio := nil;

{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then
            DebugLog(loSslInfo, 'ResetSslSession FNBio handle=' + IntToStr(Integer(FNBio)));       { V8.70 !!!! }
{$ENDIF}
        if Assigned(FNbio) then
            BIO_free(FNbio);
        FNbio := nil;

{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then
            DebugLog(loSslInfo, 'ResetSslSession Fibio handle=' + IntToStr(Integer(Fibio)));       { V8.70 !!!! }
{$ENDIF}
        if Assigned(Fibio) then
            BIO_free(FIbio);
        FIbio := nil;

{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then
            DebugLog(loSslInfo, 'ResetSslSession Close FSsl handle=' + IntToStr(Integer(FSsl)));       { V8.70 !!!! }
{$ENDIF}
        if Assigned(FSsl) then
            SSL_free(FSsl);
        FSsl := nil;
    except
        on E:Exception do
            HandleBackGroundException(E, 'Fatal exception in ResetSSL');  { V8.70 }
    end;

{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) + ' ResetSslSession Done');      { V8.70 }
{$ENDIF}
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.RaiseLastOpenSslError(
    EClass          : ExceptClass;
    Dump            : Boolean = FALSE;
    const CustomMsg : String  = '');
begin
    FLastSslError := ERR_peek_error;
    FSslHandshakeRespMsg := String(LastOpenSslErrMsg(TRUE));  { V8.55 }
    if Length(CustomMsg) > 0 then
        FSslHandshakeRespMsg := CustomMsg + ' - ' + FSslHandshakeRespMsg;
    raise EClass.Create(#13#10 + FSslHandshakeRespMsg + #13#10);  { V8.55 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Writes unencrypted data from the buffer to the SslBio                     }
procedure TCustomSslWSocket.TryToSend;
var
    Len       : Integer;
    Count     : Integer;
    Data      : TWSocketData;
begin
    if (not FSslEnable) or (FSocksState <> socksData) or
       (FHttpTunnelState <> htsData) then begin
{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslDevel) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
            DebugLog(loSslDevel, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' TryToSend handle=' + IntToStr(FHSocket));
{$ENDIF}
        inherited TryToSend;
        Exit;
    end;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslDevel) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslDevel, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' SslTryToSend handle=' + IntToStr(FHSocket));
{$ENDIF}

{$IFDEF MSWINDOWS}                                                                            { TIN }
  if FThreadId <> GetCurrentThreadId then begin                                               { TIN }
      TriggerEvent(sslFdWrite, 0);                                                            { TIN }
      Exit;                                                                                   { TIN }
    end;                                                                                      { TIN }
{$ENDIF}                                                                                      { TIN }

    FSslBufList.Lock;
    try
        if (FHSocket = INVALID_SOCKET) or (FSslBufList.IsEmpty) then begin
            bSslAllSent := TRUE;
            Exit;
        end;

        while TRUE do begin
            Len := FSslBufList.Peek(Data);
            if Len <= 0 then begin
                // Buffer is empty, every thing has been sent
                bSslAllSent := TRUE;
                break;
            end;
            if (FHSocket = INVALID_SOCKET) or                { No more socket  }
               (FSslBufList.IsEmpty) then                    { Nothing to send }
                Exit;
            if FNetworkError > 0 then begin
                WSocket_WSASetLastError(FNetworkError);
                FLastError := FNetworkError; //XX
                TriggerEvent(sslFdClose, 0);  // AG 03/03/06
                Exit;
            end;
            if FCloseCalled then begin      // AG 03/03/06  moved here
            { We don't trigger any error so far, just ignoring user data }
            { to be sent. We could close at once with WSAESHUTDOWN ?     }
                //FLastError := WSAESHUTDOWN;
                TriggerEvent(sslFdClose, 0);
                Exit;
            end;
            if (FSslIntShutDown > 0) then begin
                WSocket_WSASetLastError(WSAESHUTDOWN);
                FLastError := WSAESHUTDOWN; //XX
                if not FSslBiShutDownFlag then
                    TriggerEvent(sslFdClose, 0);
                Exit;
            end;
            if (not Assigned(FSsl)) or (SSL_get_state(FSsl) <> TLS_ST_OK) or   { V8.27, V8.66 }
               (SSL_renegotiate_pending(FSsl) = 1) then begin    // <= 12/31/05 AG
               { Don't write app. data while in handshake }        // <= 12/31/05 AG
                FMayTriggerSslTryToSend := TRUE;
                WSocket_WSASetLastError(WSAEWOULDBLOCK);
                FLastError := WSAEWOULDBLOCK; //XX
                Exit;
            end;

            {if FCloseCalled then begin      // AG 03/03/06 moved up
                TriggerEvent(sslFdClose, 0);
                Exit;
            end;}

            if FSslBioWritePendingBytes >= 0 then
                Len := FSslBioWritePendingBytes;

            Count := my_BIO_write(FSslbio, Data, Len);
            if Count = 0 then begin
                FSslBioWritePendingBytes := -1;
                my_BIO_ctrl(FSslbio, BIO_CTRL_FLUSH, 0, nil);
                WSocket_WSASetLastError(WSAECONNABORTED);
                FLastError := WSAECONNABORTED; //XX
                break;
            end;
            if Count < 0 then begin
                if my_BIO_should_retry(FSslbio) then begin
                    FSslBioWritePendingBytes := Len;
                    if FState = wsClosed then
                        Exit;
                    if FState <> wsConnected then begin
                        WSocket_WSASetLastError(FNetworkError);
                        FLastError := FNetworkError; //XX
                        Exit;
                    end;
                    FMayTriggerSslTryToSend := TRUE;
                    TriggerEvents;
                    WSocket_WSASetLastError(WSAEWOULDBLOCK);
                    FLastError := WSAEWOULDBLOCK; //XX
                    Exit;
                end;
                { Fatal error if BIO_should_retry = FALSE }
                HandleSslError;
                if FState = wsClosed then
                    Exit
                else if FState <> wsConnected then begin
                    WSocket_WSASetLastError(FNetworkError);
                    FLastError := FNetworkError; //XX
                    Exit;
                end;
                WSocket_WSASetLastError(WSAECONNABORTED);
                FLastError := WSAECONNABORTED; //XX
                TriggerEvent(sslFdClose, 0);
                Exit;
            end;
            my_BIO_ctrl(FSslbio, BIO_CTRL_FLUSH, 0, nil);
            FSslBioWritePendingBytes := -1;
            FSslBufList.Remove(Count);
            FWriteCount := FWriteCount + Count;  { V8.30 was in RealSend }
            if Count < Len then
                { Could not write as much as we wanted. Stop sending }
                break;
        end; //while
    finally
        FSslBufList.Unlock;
    end;
    FMayTriggerSslTryToSend := TRUE;
    TriggerEvents;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.SslBiShutDownAsync;
begin
    PostMessage(FWindowHandle, FMsg_WM_BI_SSL_SHUTDOWN, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.SslShutDownAsync(How: Integer);         { V7.80 }
begin
    PostMessage(FWindowHandle, FMsg_WM_BI_SSL_SHUTDOWN, How, 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.WMSslBiShutDown(var msg: TMessage);
begin
    if FSslEnable and (FSslIntShutDown = 0) then begin
        FSslBiShutDownFlag := (msg.LParam = 0);                     { V7.80 }
        InternalShutdown(msg.WParam);                               { V7.80 }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.WMSslASyncSelect(var msg: TMessage);
begin
{ Select messages not posted by the socket but from the component }
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslDevel) then begin { V5.21 }
        if __DataSocket = Self then
            DebugLog(loSslDevel,
                     IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                     ' SslAsyncSelect DataSocket ' +
                     IntToStr(msg.wParam) +  ', ' +
                     IntToStr(msg.LParamLo) + WinsockMsgToString(Msg))
        else
            DebugLog(loSslDevel,
                     IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                     ' SslAsyncSelect ' +
                     IntToStr(msg.wParam) + ', ' +
                     IntToStr(msg.LParamLo) + WinsockMsgToString(Msg));
    end;
{$ENDIF}
    if (msg.wParam <> WPARAM(FHSocket)) then
        Exit;
    {  ?
    if FPaused then
        exit;
    }
    if msg.lParamLo and FD_READ <> 0 then begin
        FPendingSslEvents := FPendingSslEvents - [sslFdRead];
        Do_FD_READ(Msg);  { V8.22 }
       {   Do_Ssl_FD_READ(Msg);  }
    end
    else if msg.lParamLo and FD_WRITE <> 0 then begin
        FPendingSslEvents := FPendingSslEvents - [sslFdWrite];
        Do_FD_WRITE(Msg)
    end
    else if msg.lParamLo and FD_CLOSE <> 0 then begin
        FPendingSslEvents := FPendingSslEvents - [sslFdClose];
        Do_FD_CLOSE(Msg)
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.WndProc(var MsgRec: TMessage);
begin
    try                                                          // <= 12/12/05
        if MsgRec.Msg = FMsg_WM_SSL_ASyncSelect then
            WMSslASyncSelect(MsgRec)
        else if MsgRec.Msg = FMsg_WM_TRIGGER_DATASENT then
            TriggerDataSent(0)
        else if MsgRec.Msg = FMsg_WM_RESET_SSL then
            ResetSsl
        else if MsgRec.Msg = FMsg_WM_BI_SSL_SHUTDOWN then
            WMSslBiShutDown(MsgRec)
        else if MsgRec.Msg = FMsg_WM_TRIGGER_SSL_SHUTDOWN_COMPLETED then
            WMTriggerSslShutDownComplete(MsgRec)
        else
            inherited WndProc(MsgRec);
    except                                                       // <= 12/12/05
        on E:Exception do
            HandleBackGroundException(E, 'TCustomSslWSocket.WndProc: Msg ' + IntToStr(MsgRec.Msg));  { V8.62 } { V8.68 added msg }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Notification(AComponent: TComponent; operation: TOperation);
begin
    inherited Notification(AComponent, operation);
    if operation = opRemove then begin
        if AComponent = FSslContext then
            FSslContext := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.TriggerEvents;
var
    State   :  OSSL_HANDSHAKE_STATE;  { V8.66 was TSslHandshakeState  }
{$IFNDEF NO_DEBUG_LOG}
    Str   : String;
{$ENDIF}
begin
    if not Assigned(FSsl) or (not FSslEnable) then
        Exit;
    State := SSL_get_state(FSsl);  { V8.27, V8.66 }
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslDevel) then begin { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        Str := IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
               ' TriggerEvents handle=' + IntToStr(FHSocket) + ' State=' +
               String(SSL_state_string_long(FSsl));  { V8.27  }

        DebugLog(loSslDevel, Str +
              ' // MayFD_Read='      + BoolToStr(FMayTriggerFD_Read, True) +
              ' MayDoRecv='       + BoolToStr(FMayTriggerDoRecv, True)  +
              ' MayFD_Write='     + BoolToStr(FMayTriggerFD_Write, True) +
              ' MaySslTryToSend=' + BoolToStr(FMayTriggerSslTryToSend, True) +
              ' bSslAllSent='     + BoolToStr(bSslAllSent, True) +
              ' bAllSent='        + BoolToStr(bAllSent, True));   { V8.22 display words }
    end;
{$ENDIF}

    if FHandshakeDone = TRUE then begin
        FHandshakeDone := FALSE;
        FHandshakeEventDone := TRUE;  { V8.55 TLS1.3 stop multiple events }
        TriggerSslHandshakeDone(0);  // OK
        if FSslInRenegotiation then
            FSslInRenegotiation := FALSE;
    end;

    if (my_BIO_ctrl_pending(FNbio) > 0) then begin
        if FMayTriggerFD_Write then begin
            if TriggerEvent(sslFdWrite, 0) then
                FMayTriggerFD_Write := FALSE;
        end;
    end
    else if (not bAllSent) and (State = TLS_ST_OK) and    { V8.27 }
            (my_BIO_ctrl_get_write_guarantee(FSslbio) > 0) and
             FMayTriggerFD_Write then begin  // AG 03/03/06
        if TriggerEvent(sslFdWrite, 0) then
            FMayTriggerFD_Write := FALSE;    // AG 03/03/06
    end
    else if (not bSslAllSent) and (State = TLS_ST_OK) and  { V8.27 }
             FMayTriggerSslTryToSend then begin
        FMayTriggerSslTryToSend := FALSE;
        TryToSend;
    end
    else if bAllSent and bSslAllSent and FSendPending and
       (State = TLS_ST_OK) then begin   { V8.27 }
        FSendPending := FALSE;
        TriggerDataSent(0);
    end;

    if (State = TLS_ST_OK) and (my_BIO_ctrl_pending(FSslbio) > 0) then begin   { V8.27 }
        if FMayTriggerDoRecv  then begin
            if TriggerEvent(sslFdRead, 0) then
                FMayTriggerDoRecv := FALSE;
        end;
    end
    else if (my_BIO_ctrl_get_write_guarantee(FNbio) > 0) and
             FMayTriggerFD_Read then begin
        if TriggerEvent(sslFdRead, 0) then
            FMayTriggerFD_Read := FALSE;
    end;

    if ((FCloseCalled and (FSslIntShutDown = 0)) or
       ((FSslIntShutDown = 2) and not FSslBiShutdownFlag)) and   // AG 03/03/06
       (State = TLS_ST_OK) and (my_BIO_ctrl_pending(FSslbio) = 0) then    { V8.27 }
        TriggerEvent(sslFdClose, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.TriggerSessionConnected(ErrCode: Word);
begin
    inherited TriggerSessionConnected(ErrCode);
    { An upper layer may have started the SSL! So we check FSsl=nil as well }
    if FSslEnable and (ErrCode = 0) and (FSsl = nil) then begin
        try
            { Both procedures may raise an exception! }
            if FSslMode = sslModeClient then
                StartSslHandshake
            else
                AcceptSslHandshake;
                //raise Exception.Create('******** TEST ************');
        except
            on E : Exception do begin
                FSslEnable := FALSE;
                ResetSsl;
                inherited InternalClose(FALSE, WSAECONNABORTED);
                HandleBackGroundException(E, 'TCustomSslWSocket.TriggerSessionConnected');
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Just to make UI easier: parse a semi-colon delimited texte string with
// a list of hosts and build the FSslAcceptableHosts list.
procedure TCustomSslWSocket.SetAcceptableHostsList(
    const SemiColonSeparatedList : String);
var
    Host : String;
    Buf  : String;
    I    : Integer;
begin
    FSslAcceptableHosts.Clear;
    Buf := SemiColonSeparatedList;
    while TRUE do begin
        I := Pos(';', Buf);
        if I > 0 then begin
            Host := IcsTrim(Copy(Buf, 1, I - 1));
            if Host > '' then
                FSslAcceptableHosts.Add(Host);
            Delete(Buf, 1, I);
        end
        else begin
            Host := IcsTrim(Buf);
            if Host > '' then
                FSslAcceptableHosts.Add(Host);
            break;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// code moved to callback V8.55
procedure TCustomSslWSocket.TriggerSslCliNewSession;
begin
//
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.TriggerSslHandshakeDone(ErrCode : Word);
var
    Cipher       : Pointer;
    PeerX        : PX509;
    Disconnect   : Boolean;
    RefCert      : TX509Base;
    Buffer       : array [0..128] of AnsiChar; { V8.14 }
    VerifyParam  : PX509_VERIFY_PARAM;  { V8.39 }

  { examples of SSL_CIPHER_description():
   <ciphername> <first protocol version> <key exchange> <authentication> <symmetric encryption method> <message authentication code>
      TLS13-CHACHA20-POLY1305-SHA256 TLSv1.3 Kx=any      Au=any  Enc=CHACHA20/POLY1305(256) Mac=AEAD
      ECDHE-RSA-AES256-GCM-SHA256    TLSv1.2 Kx=ECDH   Au=RSA   Enc=AESGCM(256) Mac=AEAD
      ECDHE-ECDSA-AES256-GCM-SHA384  TLSv1.2 Kx=ECDH   Au=ECDSA Enc=AESGCM(256) Mac=AEAD
      ECDHE-RSA-CHACHA20-POLY1305    TLSv1.2 Kx=ECDH   Au=RSA   Enc=CHACHA20/POLY1305(256) Mac=AEAD
      AES256-SHA256                  TLSv1.2 Kx=RSA    Au=RSA   Enc=AES(256)    Mac=SHA256
      RSA-PSK-AES256-CBC-SHA384      TLSv1.0 Kx=RSAPSK Au=RSA   Enc=AES(256)    Mac=SHA384 }

    function FindCiphArg (const key: string): string;
    var
        I: integer;
    begin
        I := Pos(key, FSslCipherDesc);
        if I <= 0 then
            Result := ''
        else begin
            Result := Copy(FSslCipherDesc, I + Length (key), 99);
            I := Pos(' ', Result);
            if I <= 0 then I := Pos(#10, Result);
            if I > 0 then SetLength(Result, I - 1);
        end;
    end;

begin
    PeerX := nil;
    if (FHSocket = INVALID_SOCKET) then
        ErrCode := 1;
    if (FSslHandshakeErr > 0) then begin
        if FSslVerifyResult = X509_V_OK then begin   { V8.66 ignore OpenSSL errors if certificate chain verified OK }
            FSslHandshakeErr := 0;
            FSslHandshakeRespMsg := '';
        end
        else begin
            ErrCode := Ics_Ssl_ERR_GET_REASON(FSslHandshakeErr);  { V8.14 keep error reason only  }
            case ErrCode of
                SSL_R_HTTPS_PROXY_REQUEST:  FSslHandshakeRespMsg := 'Error, HTTPS proxy request, no SSL handshake';
                SSL_R_HTTP_REQUEST:         FSslHandshakeRespMsg := 'Error, HTTP request, no SSL handshake';
                SSL_R_WRONG_VERSION_NUMBER: FSslHandshakeRespMsg := 'Error, wrong SSL version';
                SSL_R_UNKNOWN_PROTOCOL:     FSslHandshakeRespMsg := 'Error, unknown SSL protocol';
            end;
        end;
    end;
    SslGetAlpnProtocol;  { V8.62, V9.1 even if failed }
    if (ErrCode = 0) and Assigned(FSsl) then
        FSslState := sslEstablished
    else
        FSslState := sslHandshakeFailed;

    if FSslState = sslEstablished then begin
        FSslSupportsSecureRenegotiation :=
            SSL_get_secure_renegotiation_support(FSsl) = 1;
        FSslVersion := String(SSL_get_version(FSsl));
        FSslVersNum := SSL_version(FSsl);
        Cipher      := SSL_get_current_cipher(FSsl);
        if Assigned(Cipher) then begin
            FSslCipher     := String(SSL_CIPHER_get_name(Cipher));
            FSslSecretBits := SSL_CIPHER_get_bits(Cipher, @FSslTotalBits);
            FSslCipherDesc := string(SSL_CIPHER_description (Cipher, Buffer, 128));     { V8.14 }
            FSslEncryption := FindCiphArg ('Enc=');       { V8.14  }
            FSslKeyExchange := FindCiphArg ('Kx=');       { V8.14  }
            FSslMessAuth   := FindCiphArg ('Mac=');       { V8.14  }
            FSslKeyAuth    := FindCiphArg ('Au=');        { V8.41  }
        end;

    { V9.3 find cipher curve EC group name }
{..IF declared(OPENSSL_VERSION_MINOR_)}
{$IFDEF YuOpenSSL}
        {.IF (OPENSSL_VERSION_MINOR_ >= Integer(2))}
        if ICS_OPENSSL_VERSION_NUMBER >=  OSSL_VER_3200 then
            FSslGroupName := String(SSL_get0_group_name(FSsl));
        {.IFEND}
{$ELSE}
        if ICS_OPENSSL_VERSION_NUMBER >=  OSSL_VER_3200 then
            FSslGroupName := String(SSL_get0_group_name(FSsl));
{$ENDIF YuOpenSSL}  { V9.4 }

        if FSslContext.SslVerifyPeer then begin
        { Get the peer cert from OSSL. Note that servers always send their }
        { certificates, clients on server request only. This gets the peer }
        { cert also when a session was reused.                             }
            PeerX := SSL_get_peer_certificate(FSsl);

          { V8.39 get pointer to verify parameters, which we may alter in a moment }
            VerifyParam := SSL_get0_param(FSsl);    { do not free it! }
          { V8.64 if domain has ACE xn--. convert it to Unicode, ignore errors }
            FSslCertPeerName := IcsIDNAToUnicode(String(X509_VERIFY_PARAM_get0_peername (VerifyParam)));
        end;

     { V8.14 set with success or failure message once handshake completes }
        if FSslKeyAuth = 'any' then   { V8.52 TLSv1.3 does not have all params }
            FSslHandshakeRespMsg := Format('SSL Connected OK with %s, cipher %s, encryption %s, message auth %s, group %s',
                [SslVersion, SslCipher, FSslEncryption, FSslMessAuth, FSslGroupName])
        else
            FSslHandshakeRespMsg := Format('SSL Connected OK with %s, cipher %s, key auth %s, key exchange %s, encryption %s, message auth %s, group %s',
                [SslVersion, SslCipher, FSslKeyAuth, FSslKeyExchange, FSslEncryption, FSslMessAuth, FSslGroupName])
    end  // FSslState = sslEstablished
    else begin
        if (FSslHandshakeRespMsg = '') then begin  { V8.14  }
       { V8.64 always show real error, add state if possible }
            if FSslHandshakeErr = 0 then
                FSslHandshakeRespMsg := String(LastOpenSslErrMsg(true))  { get all queued errors, if any }
            else
                FSslHandshakeRespMsg := OpenSslErrMsg(FSslHandshakeErr);
        end;
        if Fssl <>  Nil then
            FSslHandshakeRespMsg := FSslHandshakeRespMsg +
                                ', State: ' + String(SSL_state_string_long(Fssl));   { V8.54 }
        if (ErrCode = 1) then
            FSslHandshakeRespMsg := FSslHandshakeRespMsg + ', connection closed unexpectedly';
    end;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslInfo, Format('%s SslHandshakeDone(%d) Handle=%d. %s, ' +
                             'VerifyResult=%s',
                             [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                             ErrCode, FHSocket, FSslHandshakeRespMsg,    { 8.54 simplified }
                             IcsX509VerifyErrorToStr(FSslVerifyResult)])); { V8.66 }
{$ENDIF}
    if Assigned(FSslPeerCert) then begin                                { V8.65 }
        FSslPeerCert.X509 := PeerX;
        if Assigned(PeerX) then begin
            { Do we have the peer cert in our chain?                          }
            RefCert := FSslCertChain.Find(PeerX);
            X509_free(PeerX);
            { If we have a chain with the peer certificate (new session)      }
            { copy some values.                                               }
            if RefCert <> nil then
            begin
                FSslPeerCert.Sha1Digest         := RefCert.Sha1Digest;
                FSslPeerCert.Sha1Hex            := RefCert.Sha1Hex;
                FSslPeerCert.Sha256Digest       := RefCert.Sha256Digest;    { V8.63 }
                FSslPeerCert.Sha256Hex          := RefCert.Sha256Hex;
                FSslPeerCert.VerifyResult       := RefCert.VerifyResult;
                FSslPeerCert.CustomVerifyResult := RefCert.CustomVerifyResult;
                FSslPeerCert.FirstVerifyResult  := RefCert.FirstVerifyResult;
            end
            { We don't have a chain with peer certificate (reused session )   }
            { get the session verify result from OSSL and assign it. This     }
            { verify result is the original one which is i.e. not changed if  }
            { we set "Ok := 1;" in OnSslVerifyPeer event.                     }
            else begin
                FSslPeerCert.VerifyResult := SSL_get_verify_result(FSsl);
                //FSslPeerCert.FirstVerifyResult := FSslPeerCert.VerifyResult; ?
            end;
        end;
    end;

    Disconnect := FALSE;
    if Assigned(FOnSslHandshakeDone) then
        FOnSslHandshakeDone(Self, ErrCode, FSslPeerCert, Disconnect);
    if Disconnect and (ErrCode = 0) then
        Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.PutDataInSendBuffer(Data : TWSocketData; Len : Integer);
begin
    if (not FSslEnable) or (FSocksState <> socksData) or
       (FHttpTunnelState <> htsData) then begin
{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslDevel) then begin { V5.21 }
            Inc(TraceCount);
            DebugLog(loSslDump, Format('%s PutDataInSendBuffer handle=%s  len %d [%d]',
                             [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                             IntToStr(FHSocket), Len, TraceCount]));
        end
        else if CheckLogOptions(loSslDump) then begin { V5.21 }
            Inc(TraceCount);
            DebugLog(loSslDump, Format('%s PutDataInSendBuffer handle=%s [%d] Data:%s',
                             [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                             IntToStr(FHSocket), TraceCount,
                             DataToString(Data, Len)]));
        end;
{$ENDIF}
        inherited PutDataInSendBuffer(Data, Len);
        Exit;
    end;
    if Len <= 0 then
        Exit;
    bSslAllSent := FALSE;
    PutDataInSslBuffer(Data, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.MsgHandlersCount : Integer;
begin
    Result := 5 + inherited MsgHandlersCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.AllocateMsgHandlers;
begin
    inherited AllocateMsgHandlers;
    FMsg_WM_TRIGGER_DATASENT    := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_SSL_ASYNCSELECT     := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_RESET_SSL           := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_BI_SSL_SHUTDOWN     := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_TRIGGER_SSL_SHUTDOWN_COMPLETED := FWndHandler.AllocateMsgHandler(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.FreeMsgHandlers;
begin
    if Assigned(FWndHandler) then begin
        FWndHandler.UnregisterMessage(FMsg_WM_TRIGGER_DATASENT);
        FWndHandler.UnregisterMessage(FMsg_WM_SSL_ASYNCSELECT);
        FWndHandler.UnregisterMessage(FMsg_WM_RESET_SSL);
        FWndHandler.UnregisterMessage(FMsg_WM_BI_SSL_SHUTDOWN);
        FWndHandler.UnregisterMessage(FMsg_WM_TRIGGER_SSL_SHUTDOWN_COMPLETED);
    end;
    inherited FreeMsgHandlers;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF} // USE_SSL

{ TCustomHttpTunnelWSocket }

const
  sHttpProxyConnect         = 'CONNECT';
  sHttpProxyKeepAlive       = 'Proxy-Connection: Keep-Alive';
  sHttpProxyAuthorization   = 'Proxy-Authorization: ';
  sCrLf                     = #13#10;
  sCrLfCrLf                 = sCrLf + sCrLf;
  sHttpProto                = 'HTTP/1.1';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.AllocateMsgHandlers;
begin
    inherited AllocateMsgHandlers;
    FMsg_WM_TUNNEL_RECONNECT := FWndHandler.AllocateMsgHandler(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.AssignDefaultValue;
begin
    if FHttpTunnelReconnectRequest = htrrNone then
        inherited AssignDefaultValue
    else begin
        { We will reconnect so do not clear all }
        FHSocket            := INVALID_SOCKET;
        FSelectEvent        := 0;
        FState              := wsClosed;
        bAllSent            := TRUE;
        FPaused             := FALSE;
        FCloseInvoked       := FALSE;
    end;
    FHttpTunnelState         := htsData;
    FHttpTunnelRcvdCnt       := 0;
    FHttpTunnelRcvdIdx       := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.Connect;
var
  LSocketFamily:TSocketFamily;
begin
    if not FHttpTunnelServerAssigned then
        inherited Connect

    else if (IcsLowerCase(FProtoStr) <> 'tcp') and (IcsTrim(FProtoStr) <> '6') then
        RaiseException('TCP is the only protocol supported by HTTP proxies')
    else begin
     { V8.56 IPv6 support from Max Terentiev, V8.66 corrected by Fr0sT.Brutal to allow host names }
        if WSocketIsIP(String(FHttpTunnelServer), LSocketFamily) then begin
  {          if (LSocketFamily = sfIPv4) or (IsIPv6APIAvailable) then
                FSocketFamily := LSocketFamily
            else
                FSocketFamily := DefaultSocketFamily;
        end
        else
          raise ESocketException.Create('Unsupported http proxy address format');
    }
            if (LSocketFamily = sfIPv6) and not IsIPv6APIAvailable then
              raise ESocketException.Create('IPv6 is not available');
            FSocketFamily := LSocketFamily;
        end;
        if WSocketIsIP(FAddrStr, LSocketFamily) then begin
            if LSocketFamily=sfIPv6 then
                FAddrStr:='['+FAddrStr+']';  // IPv6 must be in [ ]
        end;

        try
            if not FPortResolved then begin
                { The next line will trigger an exception in case of failure }
                Fsin.sin6_port  := WSocket_Synchronized_htons(WSocket_Synchronized_ResolvePort(FHttpTunnelPort, AnsiString(FProtoStr)));
                FPortResolved := TRUE;
            end;

            if not FAddrResolved then begin
                { The next line will trigger an exception in case of failure }
                if FSocketFamily = sfIPv4 then
                begin
                    Fsin.sin6_family := AF_INET;
                    PSockAddrIn(@Fsin).sin_addr.S_addr := WSocket_Synchronized_ResolveHost(FHttpTunnelServer).s_addr;
                end
                else
                    WSocket_Synchronized_ResolveHost(HttpTunnelServer, Fsin, FSocketFamily, IPPROTO_TCP);
                FAddrResolved := TRUE;
            { V8.56 IPv6 support from Max Terentiev }
                FAddrFormat := Fsin.sin6_family; // IPv6 support
            end;
            { The next line will trigger an exception in case of failure }
            FPortNum := WSocket_Synchronized_ResolvePort(FPortStr, FProtoStr);         { V8.70 }
        except
            on E: Exception do begin
                RaiseException('Connect: ' + E.Message);
                Exit;
            end;
        end;
        FHttpTunnelCloseNotified   := FALSE;
        FHttpTunnelRcvdCnt         := 0;
        FHttpTunnelRcvdIdx         := 0;
        FHttpTunnelServerAuthTypes := [];
        FHttpTunnelLastResponse    := '';
        FHttpTunnelStatusCode      := ICS_HTTP_TUNNEL_PROTERR;
        if Length(FHttpTunnelBuf) <> FHttpTunnelBufSize then
            SetLength(FHttpTunnelBuf, FHttpTunnelBufSize);
        FHttpTunnelState := htsConnecting;
        inherited Connect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomHttpTunnelWSocket.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FHttpTunnelBufSize  := 1024;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.FreeMsgHandlers;
begin
    if Assigned(FWndHandler) then
        FWndHandler.UnregisterMessage(FMsg_WM_TUNNEL_RECONNECT);
    inherited FreeMsgHandlers;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomHttpTunnelWSocket.GetRcvdCount : Integer;
begin
    if FHttpTunnelRcvdCnt <= 0 then
        Result := inherited GetRcvdCount
    else
        Result := FHttpTunnelRcvdCnt;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.Do_FD_CLOSE(var msg: TMessage);
begin
    FHttpTunnelCloseNotified := TRUE;
    inherited Do_FD_CLOSE(msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomHttpTunnelWSocket.DoRecv(var Buffer: TWSocketData; BufferSize,
  Flags: Integer): Integer;
begin
    if (FHttpTunnelRcvdCnt > 0) and (FHttpTunnelState = htsData) then
    begin
        { We still have previously received data in our internal buffer }
        if FHttpTunnelRcvdCnt <= BufferSize then begin
            { User buffer is greater or equal buffered data, copy all and clear }
            Move(FHttpTunnelBuf[FHttpTunnelRcvdIdx], Buffer^, FHttpTunnelRcvdCnt);
            Result             := FHttpTunnelRcvdCnt;
            FHttpTunnelRcvdCnt := 0;
            FHttpTunnelRcvdIdx := 0;
        end
        else begin
            { User buffer is smaller, copy as much as possible }
            Move(FHttpTunnelBuf[FHttpTunnelRcvdIdx], Buffer^, BufferSize);
            Result             := BufferSize;
            FHttpTunnelRcvdIdx := FHttpTunnelRcvdIdx + BufferSize;
            FHttpTunnelRcvdCnt := FHttpTunnelRcvdCnt - BufferSize;
            { We've still buffered data, we MUST ensure it's read!   }
            { Otherwise the application might wait for it infinitely }
            if (FState = wsConnected) then
                PostMessage(Handle, FMsg_WM_ASYNCSELECT,
                             WPARAM(FHSocket), LPARAM(FD_READ));
        end;
    end
    else
        Result := inherited DoRecv(Buffer, BufferSize, Flags)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomHttpTunnelWSocket.GetHttpTunnelLastResponse: String;
begin
    Result := String(FHttpTunnelLastResponse);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomHttpTunnelWSocket.GetHttpTunnelServer: String;
begin
    Result := String(FHttpTunnelServer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomHttpTunnelWSocket.GetHttpTunnelPort: String;
begin
    Result := String(FHttpTunnelPort);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomHttpTunnelWSocket.HttpTunnelGetNtlmMessage3: String;
var
    Hostname : String;
    NtlmInfo : TNTLM_Msg2_Info;
    LDomain, LUser: String;
begin
    { get local hostname }
    try
        Hostname := String(LocalHostName);
    except
        Hostname := '';
    end;
    NtlmInfo := NtlmGetMessage2(String(FHttpTunnelAuthChallenge));
    NtlmParseUserCode(HttpTunnelUsercode, LDomain, LUser, FALSE);
    { With Squid proxy LDomain may not be empty (at the time of writing), }
    { a user code of <DOMAIN>\<UserName> works with Squid. Fix below      }
    { works with Squid, however I'm not 100% sure whether to include it   }
    { by default.                                                         }
    if (LDomain = '') and (Pos('@', LUser) = 0) then
        LDomain := NtlmInfo.Domain;

    { hostname is the local hostname }
    Result := NtlmGetMessage3(LDomain,
                              Hostname,
                              LUser,
                              FHttpTunnelPassword,
                              NtlmInfo,                  { V7.86 }
                              CP_ACP,
                              FHttpTunnelLmCompatLevel); { V7.86 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.HttpTunnelClear;
begin
    { Only called before request }
    FHttpTunnelStatusCode       := ICS_HTTP_TUNNEL_PROTERR;
    FHttpTunnelKeepsAlive       := TRUE;
    FHttpTunnelContentLength    := 0;
    FHttpTunnelProto            := htp11;
    FHttpTunnelRcvdCnt          := 0;
    FHttpTunnelRcvdIdx          := 0;
    FHttpTunnelWaitingBody      := FALSE;
    FHttpTunnelLastResponse     := '';
    FHttpTunnelReconnectRequest := htrrNone;
{$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
    FHttpTunnelAuthDigestCached := FALSE;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.HttpTunnelSendAuthBasic;
var
    LAuthHdr : String;
begin
    HttpTunnelClear;
    FHttpTunnelCurAuthType := htatBasic;
    FHttpTunnelState       := htsWaitResp1;
    LAuthHdr := sCrLf + sHttpProxyAuthorization + 'Basic ' + IcsBase64Encode(FHttpTunnelUsercode + ':' +  FHttpTunnelPassword);  { V9.4 }
    SendStr(sHttpProxyConnect + ' ' + FAddrStr + ':' + IntToStr(FPortNum) + ' ' + sHttpProto + LAuthHdr + sCrLfCrLf);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
procedure TCustomHttpTunnelWSocket.HttpTunnelSendAuthDigest;
var
    LAuthHdr, Uri : String;
begin
    HttpTunnelClear;
    FHttpTunnelCurAuthType := htatDigest;
    FHttpTunnelState       := htsWaitResp2;
    Inc(FHttpTunnelAuthDigestInfo.Nc);
    Uri := '/';  // Required for Squid!
    LAuthHdr := sCrLf + sHttpProxyAuthorization + 'Digest ' +
                AuthDigestGenerateRequest(FHttpTunnelUsercode,
                                          FHttpTunnelPassword,
                                          sHttpProxyConnect,
                                          Uri,
                                          FHttpTunnelAuthDigestHash,
                                          FHttpTunnelAuthDigestInfo);
    SendStr(sHttpProxyConnect + ' ' + FAddrStr + ':' + IntToStr(FPortNum) + ' ' + sHttpProto + LAuthHdr + sCrLfCrLf);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.HttpTunnelSendAuthNtlm_1;
var
    LAuthHdr : String;
begin
    HttpTunnelClear;
    FHttpTunnelCurAuthType := htatNtlm;
    FHttpTunnelState       := htsWaitResp1;
    LAuthHdr := sCrLf + sHttpProxyAuthorization + 'NTLM ' +
                NtlmGetMessage1('', '', FHttpTunnelLmCompatLevel); { V7.86 }
    if not HasOption(FComponentOptions, wsoNoHttp10Tunnel) then                 { V8.70 }
        { Make some HTTP/1.0 proxies happy, i.e MSP 2.0 }
        LAuthHdr := LAuthHdr + sCrLf + sHttpProxyKeepAlive;
    SendStr(sHttpProxyConnect + ' ' + FAddrStr + ':' + IntToStr(FPortNum) +
            ' ' + sHttpProto + LAuthHdr + sCrLfCrLf);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.HttpTunnelSendAuthNtlm_3;
var
    LAuthHdr : String;
begin
    HttpTunnelClear;
    FHttpTunnelState := htsWaitResp2;
    LAuthHdr := sCrLf + sHttpProxyAuthorization + 'NTLM ' +
                HttpTunnelGetNtlmMessage3;
    SendStr(sHttpProxyConnect + ' ' + FAddrStr + ':' + IntToStr(FPortNum) +
            ' ' + sHttpProto + LAuthHdr + sCrLfCrLf);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.HttpTunnelSendPlainConnect;
begin
    HttpTunnelClear;
    FHttpTunnelState := htsWaitResp0;
    { We send the Keep-Alive header only since there are proxies i.e. the      }
    { CSM 4.2 that require at least two header lines in order to send a reply. }
    SendStr(sHttpProxyConnect + ' ' + FAddrStr + ':' + IntToStr(FPortNum) +
            ' ' + sHttpProto + sCrLf + sHttpProxyKeepAlive + sCrLfCrLf);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomHttpTunnelWSocket.HttpTunnelTriggerResultOrContinue: Boolean;
begin
    Result := TRUE;
    if FHttpTunnelStatusCode = 200 then begin
        FHttpTunnelState := htsData;
    {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
        FHttpTunnelAuthDigestCached := (FHttpTunnelCurAuthType = htatDigest);
    {$ENDIF}
        TriggerSessionConnected(0);
    end
    else if FHttpTunnelCurAuthType in [htatNone, htatBasic] then begin
        TriggerHttpTunnelError(FHttpTunnelStatusCode);
        Result := FALSE;
    end
    else if (FHttpTunnelStatusCode <> 407) then begin
        TriggerHttpTunnelError(FHttpTunnelStatusCode);
        Result := FALSE;
    end
    { ------------- 407 starts here ------------------ }
    else if not FHttpTunnelKeepsAlive then begin
        { Connection has to be closed, happens with both HTTP 1.0 and 1.1 }
        { check if we have to reconnect asynchronously.                   }
        if (FHttpTunnelState = htsWaitResp0) then begin
            { It's a response to our plain CONNECT request }
            if FHttpTunnelCurAuthType = htatDetect then begin
                if htsatBasic in FHttpTunnelServerAuthTypes then
                    FHttpTunnelReconnectRequest := htrrBasic
            {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
                else if (htsatDigest in FHttpTunnelServerAuthTypes) and
                    FHttpTunnelAuthDigestValid then
                  FHttpTunnelReconnectRequest := htrrDigest
            {$ENDIF}
                else if htsatNtlm in FHttpTunnelServerAuthTypes then
                    FHttpTunnelReconnectRequest := htrrNtlm1;
            end
        {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
            else if FHttpTunnelCurAuthType = htatDigest then begin
                if (htsatDigest in FHttpTunnelServerAuthTypes) and
                    FHttpTunnelAuthDigestValid then
                    FHttpTunnelReconnectRequest := htrrDigest;
            end
        {$ENDIF};
        end;
        if FHttpTunnelReconnectRequest = htrrNone then
            TriggerHttpTunnelError(FHttpTunnelStatusCode)
        else begin
            { Close the connection and start a new one asynchronously in }
            { in TriggerSessionClosed.                                   }
            FHttpTunnelState := htsData;
            FHttpTunnelStatusCode := ICS_HTTP_TUNNEL_PROTERR;
            if not FHttpTunnelCloseNotified then
                inherited InternalClose(TRUE, 0);
        end;
        Result := FALSE;
    end
{$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
    else if FHttpTunnelCurAuthType = htatDigest then begin
        { Digest }
        if (htsatDigest in FHttpTunnelServerAuthTypes) and
            FHttpTunnelAuthDigestValid and
           (FHttpTunnelState in [htsWaitResp0, htsWaitResp1]) then
            HttpTunnelSendAuthDigest
        else begin
            TriggerHttpTunnelError(FHttpTunnelStatusCode);
            Result := FALSE;
        end;
    end
{$ENDIF}
    else if FHttpTunnelCurAuthType = htatNtlm then begin
        { NTLM }
        if (htsatNtlm in FHttpTunnelServerAuthTypes) and
           (FHttpTunnelState = htsWaitResp1) then
            HttpTunnelSendAuthNtlm_3
        else begin
            TriggerHttpTunnelError(FHttpTunnelStatusCode);
            Result := FALSE;
        end;
    end
    else if FHttpTunnelCurAuthType = htatDetect then begin
        { Detect AuthType supported by the proxy }
        if htsatBasic in FHttpTunnelServerAuthTypes then
            HttpTunnelSendAuthBasic
    {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
        else if (htsatDigest in FHttpTunnelServerAuthTypes) and
                FHttpTunnelAuthDigestValid then
            HttpTunnelSendAuthDigest
    {$ENDIF}
        else if htsatNtlm in FHttpTunnelServerAuthTypes then
            HttpTunnelSendAuthNtlm_1
        else begin
            TriggerHttpTunnelError(FHttpTunnelStatusCode);
            Result := FALSE;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomHttpTunnelWSocket.HttpTunnelProcessHdrLine(
    Data  : PAnsiChar;
    Cnt   : Integer): Boolean;
var
    I : Integer;
    LStatusCode : Integer;
    LStr : AnsiString;
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loWsockDump) then begin
        if Cnt > 0 then begin
            SetLength(LStr, Cnt);
            Move(Data^, Pointer(LStr)^, Cnt);
        end;
        DebugLog(loWsockDump, String(LStr));
    end;
{$ENDIF}

    Result := TRUE;
    if Cnt = 0 then begin
        { Empty line, end of header }
        FHttpTunnelWaitingBody := (FHttpTunnelContentLength > 0) or FHttpTunnelChunked;
        if not FHttpTunnelWaitingBody then
            Result := HttpTunnelTriggerResultOrContinue
        else if FHttpTunnelChunked then begin
            FHttpTunnelChunkSize  := 0;
            FHttpTunnelChunkRcvd  := 0;
            FHttpTunnelChunkState := htcsGetSize;
        end;
    end
    else if FHttpTunnelStatusCode = ICS_HTTP_TUNNEL_PROTERR then
    begin
        { HTTP/1.x <status code> }
        if (Cnt >= 12) and (StrLIComp(Data, PAnsiChar('HTTP/1.'), 7) = 0) then begin
            FHttpTunnelChunked := FALSE;
            if Data[7] = '0' then begin
                FHttpTunnelProto := htp10;
                if HasOption(FComponentOptions, wsoNoHttp10Tunnel) then begin  { V8.70 }
                    FHttpTunnelLastResponse := sHttpVersionError;
                    TriggerHttpTunnelError(ICS_HTTP_TUNNEL_VERSIONERR);
                    Result := FALSE;
                    Exit; //***
                end;
            end;
            FHttpTunnelKeepsAlive := (FHttpTunnelProto = htp11);
            SetLength(FHttpTunnelLastResponse, Cnt);
                Move(Data^, Pointer(FHttpTunnelLastResponse)^, Cnt);

            LStatusCode := 0;
            I           := 8;
            while (I < Cnt) and (Data[I] = #$20) do Inc(I);
            while (I < Cnt) and (Data[I] in ['0'..'9']) do begin
                LStatusCode := LStatusCode * 10 + Byte(Data[I]) - Byte('0');
                Inc(I);
            end;
            if LStatusCode >= 100 then
                FHttpTunnelStatusCode := LStatusCode;
        end;
        if FHttpTunnelStatusCode = ICS_HTTP_TUNNEL_PROTERR then begin
            TriggerHttpTunnelError(FHttpTunnelStatusCode);
            Result := FALSE;
        end;
    end
    else if (Cnt > 25) and (StrLIComp(Data, 'Transfer-Encoding:', 18) = 0) then begin
        I := 18;
        while (I < Cnt) and (Data[I] = #$20) do Inc(I);
        Inc(Data, I);
        FHttpTunnelChunked := (Cnt >= I + 7) and
                              (StrLIComp(Data, PAnsiChar('chunked'), 7) = 0);
    end
    else if (Cnt > 22) and (StrLIComp(Data, 'Proxy-Connection:', 17) = 0) then begin
        I := 17;
        while (I < Cnt) and (Data[I] = #$20) do Inc(I);
        Inc(Data, I);
        if (Cnt >= I + 10) and (StrLIComp(Data, PAnsiChar('keep-alive'), 10) = 0) then
            FHttpTunnelKeepsAlive := TRUE
        else if (Cnt >= I + 5) and (StrLIComp(Data, PAnsiChar('close'), 5) = 0) then
            FHttpTunnelKeepsAlive := FALSE;
    end
    else if (Cnt > 15) and (StrLIComp(Data, 'Content-Length:', 15) = 0) then begin
        I := 15;
        while (I < Cnt) and (Data[I] = #$20) do Inc(I);
        while (I < Cnt) and (Data[I] in ['0'..'9']) do begin
            FHttpTunnelContentLength := FHttpTunnelContentLength * 10 +
                                        Byte(Data[I]) - Byte('0');
            Inc(I);
        end;
    end
    else if (FHttpTunnelStatusCode = 407) then begin
        { Auth required.
          Get the NTLM challenge and collect FHttpTunnelServerAuthTypes. }
        if (Cnt >= 24) and (StrLIComp(Data,
                            PAnsiChar('Proxy-Authenticate:'), 19) = 0) then begin
            I := 19;
            while (I < Cnt) and (Data[I] = #$20) do Inc(I);
            Inc(Data, I);
            if (Cnt >= I + 20) and (StrLIComp(Data, PAnsiChar('Digest'), 6) = 0) then begin
            { Digest challenge }
            {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
                Inc(I, 6);
                Inc(Data, 6);
                while (I < Cnt) and (Data^ = #$20) do begin
                    Inc(I);
                    Inc(Data);
                end;
                SetLength(LStr, Cnt - I);
                Move(Data^, Pointer(LStr)^, Cnt - I);
                AuthDigestParseChallenge(String(LStr), FHttpTunnelAuthDigestInfo);
                FHttpTunnelAuthDigestValid :=
                      AuthDigestValidateResponse(FHttpTunnelAuthDigestInfo);
            {$ENDIF}
                Include(FHttpTunnelServerAuthTypes, htsatDigest);
            end
            else if (Cnt >= I + 5) and (StrLIComp(Data, PAnsiChar('Basic'), 5) = 0) then
                Include(FHttpTunnelServerAuthTypes, htsatBasic)
            else if (Cnt >= I + 4) and (StrLIComp(Data, PAnsiChar('NTLM'), 4) = 0) then begin
                if (FHttpTunnelState = htsWaitResp1) then begin
                    if (Cnt > 100) then begin
                        { NTLM challenge }
                        Inc(I, 5);
                        Inc(Data, 5);
                        while (I < Cnt) and (Data^ = #$20) do begin
                            Inc(I);
                            Inc(Data);
                        end;
                        SetLength(FHttpTunnelAuthChallenge, Cnt - I);
                        Move(Data^, Pointer(FHttpTunnelAuthChallenge)^, Cnt - I);
                    end;
                    Include(FHttpTunnelServerAuthTypes, htsatNtlm);
                end
                else if (FHttpTunnelState = htsWaitResp0) then
                    Include(FHttpTunnelServerAuthTypes, htsatNtlm);
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.Listen;
begin
    if not FHttpTunnelServerAssigned then
        inherited Listen
    else
        RaiseException('Listening is not supported thru HTTP proxy servers');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomHttpTunnelWSocket.MsgHandlersCount : Integer;
begin
    Result := 1 + inherited MsgHandlersCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.RaiseException(const Msg : String);
begin
    HttpTunnelClear;
    inherited RaiseException(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.SetHttpTunnelAuthType(
  const Value: THttpTunnelAuthType);
begin
    if State <> wsClosed then
        RaiseException('Can''t change HTTP proxy authentication if not closed')
    else begin
        if FHttpTunnelAuthType <> Value then
        begin
            FHttpTunnelAuthType    := Value;
            FHttpTunnelCurAuthType := Value;
        {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
            FHttpTunnelAuthDigestCached := FALSE;
            FHttpTunnelAuthDigestValid  := FALSE;
        {$ENDIF}
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.SetHttpTunnelBufferSize(BufSize: Integer);
begin
    FHttpTunnelBufSize := BufSize;
    if FHttpTunnelBufSize < 0 then
        FHttpTunnelBufSize := 0;
    SetLength(FHttpTunnelBuf, FHttpTunnelBufSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.SetHttpTunnelPassword(const Value: String);
begin
    if State <> wsClosed then
        RaiseException('Can''t change HTTP proxy password if not closed')
    else begin
        if FHttpTunnelPassword <> Value then begin
            FHttpTunnelCurAuthType := FHttpTunnelAuthType;
            FHttpTunnelPassword := Value;
        {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
            FHttpTunnelAuthDigestCached := FALSE;
            FHttpTunnelAuthDigestValid  := FALSE;
        {$ENDIF}
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.SetHttpTunnelPort(const Value: String);
var
    NewValue : AnsiString;
begin
    if State <> wsClosed then
        RaiseException('Can''t change HTTP proxy port if not closed')
    else begin
        NewValue := AnsiString(IcsTrim(Value));
        if NewValue <> FHttpTunnelPort then begin
            FHttpTunnelCurAuthType      := FHttpTunnelAuthType;
        {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
            FHttpTunnelAuthDigestCached := FALSE;
            FHttpTunnelAuthDigestValid  := FALSE;
        {$ENDIF}
        end;
        FHttpTunnelPort := NewValue;
        FHttpTunnelPortAssigned := FHttpTunnelPort <> '';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.SetHttpTunnelServer(const Value: String);
var
    NewValue : AnsiString;
begin
    if State <> wsClosed then
        RaiseException('Can''t change HTTP proxy if not closed')
    else begin
        NewValue := AnsiString(IcsTrim(Value));
        if NewValue <> FHttpTunnelServer then begin
            FHttpTunnelCurAuthType      := FHttpTunnelAuthType;
            FHttpTunnelServer           := NewValue;
        {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
            FHttpTunnelAuthDigestCached := FALSE;
            FHttpTunnelAuthDigestValid  := FALSE;
        {$ENDIF}
        end;
        FHttpTunnelServerAssigned := FHttpTunnelServer <> '';
        if FHttpTunnelServerAssigned and
           TCustomSocksWSocket(Self).FSocksServerAssigned then begin
            FHttpTunnelServer   := '';
            FHttpTunnelServerAssigned := FALSE;
            raise Exception.Create('Can''t use HTTP proxy when Socks is used as well');
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.SetHttpTunnelUsercode(const Value: String);
begin
    if State <> wsClosed then
        RaiseException('Can''t change HTTP proxy usercode if not closed')
    else begin
        if FHttpTunnelUsercode <> Value then begin
            FHttpTunnelCurAuthType := FHttpTunnelAuthType;
            FHttpTunnelUsercode := Value;
        {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
            FHttpTunnelAuthDigestCached := FALSE;
            FHttpTunnelAuthDigestValid  := FALSE;
        {$ENDIF}
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomHttpTunnelWSocket.TriggerDataAvailable(ErrCode: Word): Boolean;
var
    I       : Integer;
    LRcvd   : Integer;
    LBufIdx : Integer;
begin
    if FHttpTunnelState = htsData then
        Result := inherited TriggerDataAvailable(ErrCode)
    else if FHttpTunnelState in [htsWaitResp0, htsWaitResp1, htsWaitResp2] then begin
        if ErrCode <> 0 then begin
            FHttpTunnelLastResponse := 'THttpTunnelWSocket - DataAvailable error';
            TriggerHttpTunnelError(ErrCode);
            Result := FALSE;
            Exit;
        end;

        Result := TRUE;

        LRcvd := Receive(@FHttpTunnelBuf[FHttpTunnelRcvdCnt],
                         FHttpTunnelBufSize - FHttpTunnelRcvdCnt);
        if LRcvd <= 0 then
            Exit;

        Inc(FHttpTunnelRcvdCnt, LRcvd);
        I := 0;
        LBufIdx := 0;
        while I < FHttpTunnelRcvdCnt do begin
            { Parse header lines, omit CRLF }
            if FHttpTunnelWaitingBody or (FHttpTunnelState = htsData) then
                Break;
            if (FHttpTunnelBuf[I] = $0A) then
            begin
                { Some proxies use just a LF as the end-of-line marker }
                { which violates the HTTP specs.                       }
                if (I > 0) and (FHttpTunnelBuf[I - 1] = $0D) then
                begin
                    if not HttpTunnelProcessHdrLine(@FHttpTunnelBuf[LBufIdx],
                                                   (I - 1) - LBufIdx) then
                        Exit;
                end
                else begin
                    if not HttpTunnelProcessHdrLine(@FHttpTunnelBuf[LBufIdx],
                                                    I - LBufIdx) then
                        Exit;
                end;
                LBufIdx := I + 1;
            end;
            Inc(I);
        end;

        if not FHttpTunnelWaitingBody then begin
            { Still in header or request sent or application data follows }
            if (LBufIdx > 0) and (FHttpTunnelRcvdCnt >= LBufIdx) then begin
                Dec(FHttpTunnelRcvdCnt, LBufIdx);
                if FHttpTunnelRcvdCnt > 0 then begin
                    Move(FHttpTunnelBuf[LBufIdx], FHttpTunnelBuf[0],
                         FHttpTunnelRcvdCnt);
                    { We've buffered data, we MUST ensure it's read!         }
                    { Otherwise the application might wait for it infinitely }
                    if (FHttpTunnelState = htsData) and (FState = wsConnected) then
                        PostMessage(Handle, FMsg_WM_ASYNCSELECT, WPARAM(FHSocket),
                                     LPARAM(FD_READ));
                end;
            end;
        end
        else begin { Skip body data }
            if FHttpTunnelChunked then begin
                { Skip body data with chunked transfer encoding }
                while (LBufIdx < FHttpTunnelRcvdCnt) and
                      (FHttpTunnelChunkState <> htcsDone) do begin
                    if FHttpTunnelChunkState = htcsGetSize then begin
                        while (LBufIdx < FHttpTunnelRcvdCnt) do begin
                            if not IsXDigit(AnsiChar(FHttpTunnelBuf[LBufIdx])) then begin
                                FHttpTunnelChunkState := htcsGetExt;
                                break;
                            end;
                            FHttpTunnelChunkSize := FHttpTunnelChunkSize * 16 +
                                     XDigit(AnsiChar(FHttpTunnelBuf[LBufIdx]));
                            Inc(LBufIdx);
                        end;
                    end;

                    if FHttpTunnelChunkState = htcsGetExt then begin
                        while (LBufIdx < FHttpTunnelRcvdCnt) do begin
                            if FHttpTunnelBuf[LBufIdx] = $0A then begin
                                if FHttpTunnelChunkSize > 0 then
                                    FHttpTunnelChunkState := htcsGetData
                                else
                                    FHttpTunnelChunkState := htcsGetBoundary;
                                Inc(LBufIdx);
                                Break;
                            end;
                            Inc(LBufIdx);
                        end;
                    end;

                    if FHttpTunnelChunkState = htcsGetData then begin
                        I := FHttpTunnelRcvdCnt - LBufIdx; // Remaining
                        if FHttpTunnelChunkRcvd + I < FHttpTunnelChunkSize then begin
                            Inc(LBufIdx, I);
                            Inc(FHttpTunnelChunkRcvd, I);
                        end
                        else if FHttpTunnelChunkRcvd + I >= FHttpTunnelChunkSize then begin
                            Inc(LBufIdx, FHttpTunnelChunkSize - FHttpTunnelChunkRcvd);
                            FHttpTunnelChunkSize  := 0;
                            FHttpTunnelChunkRcvd  := 0;
                            FHttpTunnelChunkState := htcsGetSize;
                        end;
                    end;

                    if FHttpTunnelChunkState = htcsGetBoundary then begin
                        while (LBufIdx < FHttpTunnelRcvdCnt) do begin
                            if FHttpTunnelBuf[LBufIdx] = $0A then begin
                                Inc(LBufIdx);
                                FHttpTunnelChunkState := htcsDone;
                                Break;
                            end;
                            Inc(LBufIdx);
                        end;
                    end;

                    if FHttpTunnelChunkState = htcsDone then begin
                        HttpTunnelTriggerResultOrContinue;
                        FHttpTunnelChunked := FALSE;
                    end;
                end;

                if LBufIdx > 0 then begin
                    Dec(FHttpTunnelRcvdCnt, LBufIdx);
                    if FHttpTunnelRcvdCnt > 0 then begin
                        Move(FHttpTunnelBuf[LBufIdx], FHttpTunnelBuf[0],
                             FHttpTunnelRcvdCnt);
                        { We've buffered data, we MUST ensure it is read!        }
                        { Otherwise the application might wait for it infinitely }
                        if (FHttpTunnelChunkState = htcsDone) and
                           (FState = wsConnected) then
                            PostMessage(Handle, FMsg_WM_ASYNCSELECT,
                                         WPARAM(FHSocket), LPARAM(FD_READ));
                    end;
                end;
            end
            else begin
               { Skip body data with Content-Length header }
                I := FHttpTunnelRcvdCnt - LBufIdx; // Remaining
                if FHttpTunnelContentLength >= I then begin
                    FHttpTunnelRcvdCnt := 0;
                    Dec(FHttpTunnelContentLength, I);
                end
                else begin
                    Inc(LBufIdx, FHttpTunnelContentLength);
                    Dec(FHttpTunnelRcvdCnt, LBufIdx);
                    FHttpTunnelContentLength := 0;
                    if FHttpTunnelRcvdCnt > 0 then begin
                        Move(FHttpTunnelBuf[LBufIdx], FHttpTunnelBuf[0],
                             FHttpTunnelRcvdCnt);
                    end;
                end;
                if FHttpTunnelContentLength = 0 then begin
                    HttpTunnelTriggerResultOrContinue;
                    { We've buffered data, we MUST ensure it is read!        }
                    { Otherwise the application might wait for it infinitely }
                    if (FHttpTunnelRcvdCnt > 0) and (FState = wsConnected) then
                        PostMessage(Handle, FMsg_WM_ASYNCSELECT,
                                     WPARAM(FHSocket), LPARAM(FD_READ));
                end;
            end;
        end;
        if FHttpTunnelRcvdCnt = FHttpTunnelBufSize then begin
          { No CRLF found in entire buffer (default size 1024 bytes) }
            FHttpTunnelLastResponse := 'Received header line too long. ' +
                                       'Increase HttpTunnelBufferSize.';
            TriggerHttpTunnelError(ICS_HTTP_TUNNEL_PROTERR);
        end;
    end
    else begin
        Result := FALSE; // Should never happen
        FHttpTunnelLastResponse := 'THttpTunnelWSocket - Fatal: Invalid state ' +
                                  'in TriggerDataAvailable';
        TriggerHttpTunnelError(FHttpTunnelStatusCode);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.TriggerSessionClosed(ErrCode: Word);
begin
    if FHttpTunnelState <> htsData then begin
        FHttpTunnelLastResponse :=
          AnsiString(WSocketErrorMsgFromErrorCode(ICS_HTTP_TUNNEL_GENERR));
        TriggerHttpTunnelError(ICS_HTTP_TUNNEL_GENERR);
    end
    else if FHttpTunnelReconnectRequest <> htrrNone then
        { Start ansynchronous reconnect by posting a custom message. }
        { This message is processed in method WMHttpTunnelReconnect. }
        PostMessage(Handle, FMsg_WM_TUNNEL_RECONNECT, 0, 0)
    else
        inherited TriggerSessionClosed(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.TriggerHttpTunnelConnected(ErrCode: Word);
begin
    if Assigned(FOnHttpTunnelConnected) then
        FOnHttpTunnelConnected(Self, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.TriggerHttpTunnelError(ErrCode: Word);
begin
    if ErrCode < ICS_HTTP_TUNNEL_MAXSTAT then
        ErrCode := ICS_HTTP_TUNNEL_BASEERR + ErrCode;
    if Assigned(FOnHttpTunnelError) then
        FOnHttpTunnelError(Self, ErrCode, FHttpTunnelServerAuthTypes,
                           String(FHttpTunnelLastResponse));
    FHttpTunnelState            := htsData;
    FHttpTunnelReconnectRequest := htrrNone;
    if FHttpTunnelCurAuthType <> FHttpTunnelAuthType then
        FHttpTunnelCurAuthType := FHttpTunnelAuthType;
    TriggerSessionConnected(ErrCode);
    InternalClose(TRUE, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.TriggerSessionConnectedSpecial(
  ErrCode: Word);
begin
    if FHttpTunnelState = htsConnecting then begin
        if ErrCode = 0 then
            FHttpTunnelState := htsConnected;
        TriggerHttpTunnelConnected(ErrCode);
        if ErrCode <> 0 then begin
            FHttpTunnelState            := htsData;
            FHttpTunnelReconnectRequest := htrrNone;
            inherited TriggerSessionConnectedSpecial(ErrCode);
        end
        else begin
            if FHttpTunnelReconnectRequest <> htrrNone then begin
                { This block is only executed after the component performed   }
                { a silent reconnect in the background after status code 407, }
                { FHttpTunnelReconnectRequest is reset in the send-functions. }
                case FHttpTunnelReconnectRequest of
                    htrrBasic  : HttpTunnelSendAuthBasic;
                {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
                    htrrDigest : HttpTunnelSendAuthDigest;
                {$ENDIF}
                    htrrNtlm1  : HttpTunnelSendAuthNtlm_1;
                end;
            end
            else begin
                if (FHttpTunnelUsercode <> '') then begin
                    if (FHttpTunnelAuthType <> htatDetect) and
                       (FHttpTunnelCurAuthType <> FHttpTunnelAuthType) then
                        FHttpTunnelCurAuthType := FHttpTunnelAuthType;
                end
                else begin
                    FHttpTunnelCurAuthType := htatNone;
                end;
                case FHttpTunnelCurAuthType of
                    htatDetect,
                    htatNone     : HttpTunnelSendPlainConnect;
                    htatBasic    : HttpTunnelSendAuthBasic;
                {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
                    htatDigest   : if FHttpTunnelAuthDigestCached then
                                        HttpTunnelSendAuthDigest
                                   else
                                        HttpTunnelSendPlainConnect;
                {$ENDIF}
                    htatNtlm     : HttpTunnelSendAuthNtlm_1;
                end;
            end;

        end;
    end
    else
        inherited TriggerSessionConnectedSpecial(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.WMHttpTunnelReconnect(var MsgRec: TMessage);
begin
    if FHttpTunnelReconnectRequest <> htrrNone then
        Connect
    else begin // May not happen
        FHttpTunnelLastResponse := 'THttpTunnelWSocket - Fatal: Internal error ' +
                                   'in WMHttpTunnelReconnect';
        TriggerHttpTunnelError(FHttpTunnelStatusCode);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.WndProc(var MsgRec: TMessage);
begin
    if MsgRec.Msg = FMsg_WM_TUNNEL_RECONNECT then
    try
        WMHttpTunnelReconnect(MsgRec)
    except
        on E: Exception do
            HandleBackGroundException(E, 'WMHttpTunnelReconnect');    { V8.68 }
    end
    else
        inherited WndProc(MsgRec);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsAsyncSocketThread }   { Arno Garrels 8.11.2011 }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF POSIX}
procedure TIcsAsyncSocketThread.TerminateThread;
begin
  Terminate;
  if Assigned(FEventQueue) then
    FEventQueue.WakeUp; // Wakeup thread
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAsyncSocketThread.Execute;
begin
{$IFDEF DEBUG}
  IcsNameThreadForDebugging(AnsiString(ClassName));
{$ENDIF}
  while not Terminated do
  begin
    if not FEventQueue.HandleEvents then
    begin
      raise EIcsEventQueue.Create(SysErrorMessage(GetLastError));
    end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsEventQueue } { Arno Garrels 8.11.2011 }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
const
  IcsEventQueueIgnoreFlag = Cardinal(-1);

{$IFDEF NEVER}
function TIcsEventQueue.KQueueEnableReadEvent(FD: Integer; UData: NativeInt;
  Edge: Boolean): Boolean;
var
  LFlags : Word;
  Evt: TKEvent;
begin
  if Edge then // is it required?
    LFlags := EV_ENABLE or EV_CLEAR
  else
    LFlags := EV_ENABLE;

  EV_SET(@Evt, FD, EVFILT_READ, LFlags, 0, 0, Pointer(UData));
  Result := KEvent(FQueue, @Evt, 1, nil, 0, nil) > -1;
  if not Result then
    raise EIcsEventQueue.Create(_SysErrorMessage(errno));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.KQueueDisableReadEvent(FD: Integer;
  UData: NativeInt): Boolean;
var
  Evt: TKEvent;
begin
  EV_SET(@Evt, FD, EVFILT_READ, EV_DISABLE, 0, 0, Pointer(UData));
  Result := KEvent(FQueue, @Evt, 1, nil, 0, nil) > -1;
  if not Result then
    raise EIcsEventQueue.Create(_SysErrorMessage(errno));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.KQueueAddReadEvent(FD: Integer; UData: NativeInt;
  Edge: Boolean): Boolean;
var
  Evt: TKevent;
  LFlags: Word;
begin
  if Edge then
    LFlags := EV_ADD or EV_CLEAR  // edge triggered
  else
    LFlags := EV_ADD;
  EV_SET(@Evt, FD, EVFILT_READ, LFlags, 0, 0, Pointer(UData));
  Result := KEvent(FQueue, @Evt, 1, nil, 0, nil) > -1;
  if not Result then
    raise EIcsEventQueue.Create(_SysErrorMessage(errno));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.KQueueAddWriteEvent(FD: Integer; UData: NativeInt): Boolean;
var
  Evt: TKevent;
begin
  EV_SET(@Evt, FD, EVFILT_WRITE, EV_ADD or EV_CLEAR, 0, 0, Pointer(UData)); // edge triggered
  Result := KEvent(FQueue, @Evt, 1, nil, 0, nil) > -1;
  if not Result then
    raise EIcsEventQueue.Create(_SysErrorMessage(errno));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.KQueueRemoveReadEvent(FD: Integer): Boolean;
var
  Evt: TKevent;
begin
  EV_SET(@Evt, FD, EVFILT_READ, EV_DELETE, 0, 0, nil);
  Result := KEvent(FQueue, @Evt, 1, nil, 0, nil) > -1;
  if not Result then
    raise EIcsEventQueue.Create(_SysErrorMessage(errno));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.KQueueRemoveWriteEvent(FD: Integer): Boolean;
var
  Evt: TKevent;
begin
  EV_SET(@Evt, FD, EVFILT_WRITE, EV_DELETE, 0, 0, nil);
  Result := KEvent(FQueue, @Evt, 1, nil, 0, nil) > -1;
  if not Result then
    raise EIcsEventQueue.Create(_SysErrorMessage(errno));
end;
{$ENDIF NEVER}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsEventQueue.AddToObjIdentList(IEventSrc: IIcsEventSource);
begin
  if not FObjIdentList.ContainsKey(IEventSrc.ObjectID) then
    FObjIdentList.Add(IEventSrc.ObjectID, IEventSrc.GetObject);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsEventQueue.RemoveFromObjIdentList(IEventSrc: IIcsEventSource);
begin
  if FObjIdentList.ContainsKey(IEventSrc.ObjectID) then
    FObjIdentList.Remove(IEventSrc.ObjectID);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsEventQueue.AddReadEvent(FD: Integer; UData: NativeInt;
  Edge: Boolean);
{$IF DEFINED(MACOS) OR DEFINED(IOS)}
var
  LFlags: Word;
begin
  if FFreeIndex >= FCapacity then
    Grow;

  if Edge then
    LFlags := EV_ADD or EV_CLEAR  // edge triggered
  else
    LFlags := EV_ADD;

  EV_SET(@FChangeList[FFreeIndex], FD, EVFILT_READ,
         LFlags, 0, 0, Pointer(UData));
  Inc(FFreeIndex);
{$ELSEIF DEFINED(LINUX) OR DEFINED(ANDROID)}
var
  Evt: TEPoll_Event;
begin
  if FFreeIndex >= FCapacity then
    Grow;

  Evt.Events := EPOLLIN;  // Always set the EPOLLIN flag
  Evt.Data.fd := FD; // or Evt.Data.u64 := Self.UID; // or Evt.Data.fd := FD;
  if Edge then
    Evt.Events := EPOLLIN or EPOLLET;  // edge triggered

  if epoll_ctl(FQueue, EPOLL_CTL_ADD, FD, @Evt) < 0 then    // or
    raise EIcsEventQueue.Create(SysErrorMessage(errno));    // FChangeList[FFreeIndex] := Evt;

  Inc(FFreeIndex);
{$IFEND}

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsEventQueue.AddWriteEvent(FD: Integer; UData: NativeInt);
{$IF DEFINED(MACOS) OR DEFINED(IOS)}
begin
  { Write filter is always edge triggered }
  if FFreeIndex >= FCapacity then
    Grow;
  EV_SET(@FChangeList[FFreeIndex], FD, EVFILT_WRITE,
         EV_ADD or EV_CLEAR, 0, 0, Pointer(UData));
  Inc(FFreeIndex);
{$ELSEIF DEFINED(LINUX) OR DEFINED(ANDROID)}
var
  Evt: TEPoll_Event;
begin
  { Write filter is always edge-triggered }
  if FFreeIndex >= FCapacity then
    Grow;

  Evt.Events := EPOLLOUT; // Always set the EPOLLOUT flag
  Evt.Data.fd := FD;

  if epoll_ctl(FQueue, EPOLL_CTL_ADD, FD, @Evt) < 0 then
    raise EIcsEventQueue.Create(SysErrorMessage(errno));

  Inc(FFreeIndex);
{$IFEND}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsEventQueue.RemoveReadEvent(FD: Integer; UData: NativeInt);
{$IF DEFINED(MACOS) OR DEFINED(IOS)}
var
  I: Integer;
begin
  { Keeps the true change list short }
  for I := 0 to FFreeIndex -1 do
  begin
    if (FChangeList[I].Ident = NativeUInt(FD)) and
       (FChangeList[I].Filter = EVFILT_READ) and
       (FChangeList[I].UData = Pointer(UData)) then
      FChangeList[I].FFlags := IcsEventQueueIgnoreFlag;
  end;

  if FFreeIndex >= FCapacity then
    Grow;
  EV_SET(@FChangeList[FFreeIndex], FD, EVFILT_READ,
         EV_DELETE, 0, 0, nil);
{$ELSE IF DEFINED(LINUX) OR DEFINED(ANDROID)}
  var
  Evt: TEPoll_Event;
begin
  if FFreeIndex >= FCapacity then
    Grow;

  Evt.Events := 0;
  Evt.Data.fd := FD;

  if epoll_ctl(FQueue, EPOLL_CTL_DEL, FD, @Evt) < 0 then
    raise EIcsEventQueue.Create(SysErrorMessage(errno));

{$IFEND}
  Inc(FFreeIndex);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsEventQueue.RemoveWriteEvent(FD: Integer; UData: NativeInt);
{$IF DEFINED(MACOS) OR DEFINED(IOS)}
begin
  if FFreeIndex >= FCapacity then
    Grow;
  EV_SET(@FChangeList[FFreeIndex], FD, EVFILT_WRITE,
         EV_DELETE, 0, 0, nil);
{$ELSE IF DEFINED(LINUX) OR DEFINED(ANDROID)}
var
  Evt: TEPoll_Event;
begin
  if FFreeIndex >= FCapacity then
    Grow;

  Evt.Events := 0;
  Evt.Data.fd := FD;

  if epoll_ctl(FQueue, EPOLL_CTL_DEL, FD, @Evt) < 0 then
    raise EIcsEventQueue.Create(SysErrorMessage(errno));
{$IFEND}
  Inc(FFreeIndex);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsEventQueue.DisableReadEvent(FD: Integer; UData: NativeInt);
{$IF DEFINED(MACOS) OR DEFINED(IOS)}
var
  I: Integer;
begin
  { Keeps the true change list short }
  for I := 0 to FFreeIndex -1 do
  begin
    if (FChangeList[I].Ident = NativeUInt(FD)) and
       (FChangeList[I].Filter = EVFILT_READ) and
       (FChangeList[I].uData = Pointer(UData)) then
    begin
      if FChangeList[I].Flags = EV_ENABLE then  // socket wanted to enable
      begin
        FChangeList[I].Flags  := EV_DISABLE;
        FChangeList[I].FFlags := IcsEventQueueIgnoreFlag;
        Exit;
      end
      else if FChangeList[I].Flags = EV_DISABLE then
        Exit;
    end;
  end;

  if FFreeIndex >= FCapacity then
    Grow;
  EV_SET(@FChangeList[FFreeIndex], FD, EVFILT_READ,
         EV_DISABLE, 0, 0, Pointer(UData));
{$ELSE IF DEFINED(LINUX) OR DEFINED(ANDROID)}
var
  Evt: TEPoll_Event;
begin
  if FFreeIndex >= FCapacity then
    Grow;

  Evt.Events := 0;
  Evt.Data.fd := FD;

  if epoll_ctl(FQueue, EPOLL_CTL_MOD, FD, @Evt) < 0 then
    raise EIcsEventQueue.Create(SysErrorMessage(errno));
{$IFEND}
  Inc(FFreeIndex);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.EnableReadEvent(FD: Integer; UData: NativeInt): Boolean;
{$IF DEFINED(MACOS) OR DEFINED(IOS)}
var
  I: Integer;
begin
  { Keeps the true change list short }
  for I := 0 to FFreeIndex -1 do
  begin
    if (FChangeList[I].Ident = NativeUInt(FD)) and
       (FChangeList[I].Filter = EVFILT_READ) and
       (FChangeList[I].uData = Pointer(UData)) then
    begin
      if FChangeList[I].Flags = EV_DISABLE then // thread wanted to disable
      begin
        FChangeList[I].Flags  := EV_ENABLE;
        FChangeList[I].FFlags := IcsEventQueueIgnoreFlag;
        Exit(False);
      end
      else if (FChangeList[I].Flags = EV_ENABLE) then
        Exit(False);
    end;
  end;

  if FFreeIndex >= FCapacity then
    Grow;
  EV_SET(@FChangeList[FFreeIndex], FD, EVFILT_READ,
         EV_ENABLE, 0, 0, Pointer(UData));
{$ELSE IF DEFINED(LINUX) OR DEFINED(ANDROID)}
  var
  Evt: TEPoll_Event;
begin
  if FFreeIndex >= FCapacity then
    Grow;

  Evt.Events := EPOLLIN;
  Evt.Data.fd := FD;

  if epoll_ctl(FQueue, EPOLL_CTL_MOD, FD, @Evt) < 0 then
    raise EIcsEventQueue.Create(SysErrorMessage(errno));
{$IFEND}
  Inc(FFreeIndex);
  Result := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsEventQueue.InternalRemoveEvents(IEventSrc: IIcsEventSource;
  FdClosed: Boolean = False);
begin
  if not FdClosed then
  begin
    if (FD_ACCEPT and IEventSrc.EventMask = FD_ACCEPT) or
       (FD_READ and IEventSrc.EventMask = FD_READ) then
      RemoveReadEvent(IEventSrc.FileDescriptor, IEventSrc.ObjectID);
    if (FD_WRITE and IEventSrc.EventMask = FD_WRITE) or
       (FD_CONNECT and IEventSrc.EventMask = FD_CONNECT) then
      RemoveWriteEvent(IEventSrc.FileDescriptor, IEventSrc.ObjectID);
  end;

  RemoveFromObjIdentList(IEventSrc);

  IEventSrc.EventMask       := 0;
  IEventSrc.NotifyWindow    := 0;
  IEventSrc.EventState      := [];
  IEventSrc.FileDescriptor  := INVALID_SOCKET;
  IEventSrc.NotifyMessageID := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.SynchronizedRemoveEvents(IEventSrc: IIcsEventSource;
  FdClosed: Boolean): Boolean;
begin
  if (IEventSrc.EventMask <> 0) and (IEventSrc.FileDescriptor <> INVALID_SOCKET) then
  begin
    FQueueSection.Enter;
    try
      InternalRemoveEvents(IEventSrc, FdClosed);
      if (not FdClosed) and FRequireWakeup then
        Result := Notify(0)
      else
        Result := True;
    finally
      FQueueSection.Leave;
    end;
  end
  else
    Result := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.SynchronizedEnableReadEvent(IEventSrc: IIcsEventSource): Boolean;
begin
  FQueueSection.Enter;
  try
    Result := FD_READ and IEventSrc.EventMask = FD_READ;
    if Result then
    begin
      if EnableReadEvent(IEventSrc.FileDescriptor, IEventSrc.ObjectID) and
         FRequireWakeup then
        Result := Notify(0);
    end;
  finally
    FQueueSection.Leave;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.SynchronizedEnableAcceptEvent(IEventSrc: IIcsEventSource): Boolean;
begin
  FQueueSection.Enter;
  try
    Result := IEventSrc.EventMask and FD_ACCEPT = FD_ACCEPT;
    if Result then
    begin
      if EnableReadEvent(IEventSrc.FileDescriptor, IEventSrc.ObjectID) and
         FRequireWakeup then
        Result := Notify(0);
    end;
  finally
    FQueueSection.Leave;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsEventQueue.SynchronizedSetShutdownCalled(
  IEventSrc: IIcsEventSource; How: Integer);
begin
  FQueueSection.Enter;
  try
    case How of
      0 : IEventSrc.EventState := IEventSrc.EventState  + [aesShutDown0Called];
      1 : IEventSrc.EventState := IEventSrc.EventState  + [aesShutDown1Called];
      2 : IEventSrc.EventState := IEventSrc.EventState  + [aesShutDown0Called, aesShutDown1Called];
    end;
  finally
    FQueueSection.Leave;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.CheckChangeEvent(FD: Integer; UData: NativeInt;
  const OldMask: Cardinal; var NewMask: Cardinal): Boolean;
begin
  Result := False;
  if (FD_WRITE and OldMask = FD_WRITE) or (FD_CONNECT and OldMask = FD_CONNECT) then
  begin
    if not ((FD_WRITE and NewMask = FD_WRITE) or (FD_CONNECT and NewMask = FD_CONNECT)) then
    begin
      RemoveWriteEvent(FD, UData);
      Result := True;
    end;
  end
  else begin
    if (FD_WRITE and NewMask = FD_WRITE) or (FD_CONNECT and NewMask = FD_CONNECT) then
    begin
      AddWriteEvent(FD, UData);
      Result := True;
    end;
  end;

  if (FD_READ and OldMask = FD_READ) or (FD_ACCEPT and OldMask = FD_ACCEPT) then
  begin
    if not ((FD_READ and NewMask = FD_READ) or (FD_ACCEPT and NewMask = FD_ACCEPT)) then
    begin
      RemoveReadEvent(FD, UData);
      Result := True;
    end;
  end
  else begin
    if (FD_READ and NewMask = FD_READ) or (FD_ACCEPT and NewMask = FD_ACCEPT) then
    begin
      AddReadEvent(FD, UData, False); // edge trigger makes trouble, don't use
      Result := True;
    end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.InternalAsyncSelect(IEventSrc: IIcsEventSource;
  AWndHandle: HWND; AMsgID: UINT; AEvents: Cardinal; AWakeupThread: Boolean): Integer;
var
  LDirty: Boolean;
begin
  {if IEventSrc.FileDescriptor = INVALID_SOCKET then // Check done in SynchronizedAsyncSelect
    Exit(SOCKET_ERROR); }
  LDirty := CheckChangeEvent(IEventSrc.FileDescriptor, IEventSrc.ObjectID,
                             IEventSrc.EventMask, AEvents);
  IEventSrc.EventMask := AEvents;
  IEventSrc.NotifyWindow := AWndHandle;
  IEventSrc.NotifyMessageID := AMsgID;

  if LDirty then
  begin
    if (AEvents = 0) then
      RemoveFromObjIdentList(IEventSrc)
    else
      AddToObjIdentList(IEventSrc);
    if AWakeupThread and FRequireWakeup then
      Wakeup;
  end;
  Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.SynchronizedAsyncSelect(IEventSrc: IIcsEventSource;
  FD: Integer; AWndHandle: HWND; AMsgID: UINT; AEvents: Cardinal): Integer;

  function SetNonBlocking: Boolean;
  var
    LFlags : Integer;
  begin
    Result := True;
    LFlags := Posix.Fcntl.fcntl(FD, F_GETFL);
    if LFlags < 0 then
      Exit(False);
    if (LFlags and O_NONBLOCK <> 0) then
      Exit;
    LFlags := (LFlags or O_NONBLOCK);
    Result := Posix.Fcntl.fcntl(FD, F_SETFL, LFlags) <> -1;
  end;

begin
  if not SetNonBlocking then
    Exit(SOCKET_ERROR);
  if not IsWindow(AWndHandle) then
  begin
    SetLastError(ERROR_INVALID_WINDOW_HANDLE);
    Exit(SOCKET_ERROR);
  end;
  FQueueSection.Enter;
  try
    IEventSrc.FileDescriptor := FD;
    Result := InternalAsyncSelect(IEventSrc, AWndHandle, AMsgID, AEvents, True);
  finally
    FQueueSection.Leave;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.HandleEvents: Boolean;
type
  TEvtDelAction = (edaNone, edaAll, edaWrite);
var
  nEvents   : Integer;
  I         : Integer;
  IEventSrc : IIcsEventSource;
  LoParam   : Word;
  HiParam   : Word;
  CurEvt    : {$IF DEFINED(MACOS) OR DEFINED(IOS)}TKEvent{$ELSEIF DEFINED(LINUX) OR DEFINED(ANDROID)}TEPoll_Event{$IFEND};  { V9.2 }
  debugErrno: Integer;         { V9.2 }
  nChanges  : Integer;
  LLastErr  : Integer;
  LEvtMask  : Cardinal;
  LDelFlag  : TEvtDelAction;
  LEof      : Boolean;
  Buf       : Byte;
  LMsgID    : UINT;
  LHwnd     : HWND;
  LPostMsgFunc : function(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): Boolean;
begin
  FQueueSection.Enter;
  try
    if FInLoop then
      Exit(True);
    FInLoop := True;
    FRequireWakeup := True;
    nChanges := FFreeIndex;
    if nChanges > 0 then
    begin
      FFreeIndex := 0;//**
      if FThrdChangeListLen < nChanges then
      begin
        FThrdChangeListLen := nChanges * 2;
        SetLength(FThreadChangeList, FThrdChangeListLen);
      end;
      nEvents := 0;
      for I := 0 to nChanges -1 do
      begin
        {$IF DEFINED(MACOS) OR DEFINED(IOS)}
        if (FChangeList[I].FFlags <> IcsEventQueueIgnoreFlag) then
        {$ELSEIF DEFINED(LINUX) OR DEFINED(ANDROID)}
        if (FChangeList[I].Events <> 0) then
        {$IFEND}
        begin
          FThreadChangeList[nEvents] := FChangeList[I];
          Inc(nEvents);
        end; // else skip this one
      end;
      nChanges := nEvents;
      if nEvents = 0 then
        nEvents := 1;
    end
    else
      nEvents := 1;
    { FEventList length at least nEvents }
    if FEventListLen < nEvents then
    begin
      FEventListLen := nEvents;
      SetLength(FEventList, FEventListLen);
    end;
  finally
    FQueueSection.Leave;
  end;

  { Set changes and wait for events }
  {$IF DEFINED(MACOS) OR DEFINED(IOS)}
  nEvents := KEvent(FQueue, @FThreadChangeList[0], nChanges, @FEventList[0], nEvents, nil);
  {$ELSEIF DEFINED(LINUX) OR DEFINED(ANDROID)}
  nEvents := epoll_wait(FQueue, @FEventList[0], nEvents, -1);
  {$IFEND}

  FQueueSection.Enter;
  try
    FRequireWakeup := False;
    if (nEvents < 0) then
    begin
      nEvents := GetLastError;
      Exit(errno = EINTR);
    end;
    for I := 0 to nEvents -1 do
    begin
      CurEvt := FEventList[I];
      {$IF DEFINED(MACOS) OR DEFINED(IOS)}
      if (CurEvt.uData = nil) then
        Continue;
      LDelFlag      := edaNone;
      LoParam       := 0;
      HiParam       := CurEvt.FFlags; // IO error code
      LEof          := CurEvt.Flags and EV_EOF = EV_EOF;
      IEventSrc     := nil;
      {$ELSEIF DEFINED(LINUX) OR DEFINED(ANDROID)}
      if (CurEvt.Data.u64   = -1) then
        Continue;
      LDelFlag      := edaNone;
      LoParam       := 0;
      HiParam       := 0; // IO error code
      LEof          := CurEvt.Events and EPOLLHUP = EPOLLHUP;
      IEventSrc     := nil;
     {$IFEND}

      { Error on add or delete events }
      {$IF DEFINED(MACOS) OR DEFINED(IOS)}
      if (CurEvt.Flags and EV_ERROR <> 0) and (CurEvt.Data <> 0) then
      begin
        { Error descriptions from LibEvent }
        case CurEvt.Data of
          { Can occur on delete if we are not currently
            watching any events on this fd.  That can
            happen when the fd was closed and another
            file was opened with that fd. }
          ENOENT,
          { Can occur for reasons not fully understood
            on FreeBSD. }
          EINVAL:
            Continue;
          { Can occur on a delete if the fd is closed.  Can
            occur on an add if the fd was one side of a pipe,
            and the other side was closed. }
          EBADF,
          { These two can occur on an add if the fd was one side
            of a pipe, and the other side was closed. }
          EPERM,
          EPIPE:
            begin
              if IntPtr(CurEvt.uData) = -1 then  // pipe error
              begin
                SetLastError(Integer(CurEvt.Data));
                Exit(False);
              end
              else begin
                CurEvt.Data := CurEvt.Data; { Debug }
                Continue;
              end;
            end;
          else // case
            { Other errors shouldn't occur. }
            SetLastError(Word(CurEvt.Data));
            Exit(False);
        end; // case
      end // Error on add or delete events
      {$ELSEIF DEFINED(LINUX) OR DEFINED(ANDROID)}
      if (CurEvt.Events and EPOLLERR <> 0) then
      begin
        { Error descriptions from LibEvent }
        case CurEvt.Data.u64 of
          ENOENT,
          EINVAL:
            Continue;
          EBADF,
          EPERM,
          EPIPE:
            begin
              if CurEvt.Data.u64 = -1 then  // pipe error
              begin
                SetLastError(errno);
                Exit(False);
              end
              else begin
                debugErrno := errno; { Debug }
                Continue;
              end;
            end;
          else // case
            { Other errors shouldn't occur. }
            SetLastError(errno);
            Exit(False);
        end; // case
      end // Error on add or delete events
      {$IFEND}

      { Triggered Events }

      {$IF DEFINED(MACOS) OR DEFINED(IOS)}
      else if CurEvt.Ident = Cardinal(FPipeFd.Read) then
      {$ELSEIF DEFINED(LINUX) OR DEFINED(ANDROID)}
      else if CurEvt.Data.u64 = FPipeFd.Read then
      {$IFEND}
      begin
        { It's our pipe just read and continue }
        __read(FPipeFd.Read, @Buf, SizeOf(Buf));
        Continue;
      end

      { Socket events }
      {$IF DEFINED(MACOS) OR DEFINED(IOS)}
      else if FObjIdentList.ContainsKey(NativeInt(CurEvt.uData)) then
      begin
        FObjIdentList.Items[NativeInt(CurEvt.uData)].GetInterface(IIcsEventSource, IEventSrc);
        Assert(IEventSrc <> nil); { Debug }
        Assert(Integer(CurEvt.Ident) = IEventSrc.FileDescriptor); { Debug }

        LEvtMask := IEventSrc.EventMask;

        { Trigger Write, Connect or Close }
        if CurEvt.Filter = EVFILT_WRITE then
        begin
          { Trigger Connect }
          if not (aesConnectNotified in IEventSrc.EventState) then
          begin
            IEventSrc.EventState := IEventSrc.EventState + [aesConnectNotified];
            if FD_CONNECT and LEvtMask = FD_CONNECT then
              LoParam := FD_CONNECT
            else if FD_WRITE and LEvtMask = FD_WRITE then
              LoParam := FD_WRITE;
            if LEof then
              LDelFlag := edaAll; // Delete both read and write events
          end
          { Trigger Write or Close }
          else begin
            if LEof then
            begin
              { Application called shutdown(1) (disabled writes) and a    }
              { FIN ACK was sent to the peer or the peer sent us a RST.   }
              { If a read filter is still active we'll get another close  }
              { event when the peer acked the FIN request.                }
              if (aesShutDown1Called in IEventSrc.EventState) then
              begin
                if (not (aesCloseNotified in IEventSrc.EventState)) and
                   ((FD_READ and LEvtMask = FD_READ) or
                    (FD_ACCEPT and LEvtMask = FD_ACCEPT)) then
                  { So we will be notified when the peer send us FIN ACK }
                  EnableReadEvent(CurEvt.Ident, IEventSrc.ObjectID);
                LDelFlag := edaWrite; // Delete write event
              end
              else if (not (aesCloseNotified in IEventSrc.EventState)) then
              begin
                { Post a close or write message, it's likely we got a RST }
                { and an error code in HiParam.                           }
                if (FD_CLOSE and LEvtMask = FD_CLOSE) then
                  LoParam := FD_CLOSE
                else if FD_WRITE and LEvtMask = FD_WRITE then
                  LoParam := FD_WRITE; // on socket write -> socket error
                IEventSrc.EventState := IEventSrc.EventState + [aesCloseNotified];
                LDelFlag := edaAll    // Delete all
              end;
            end
            else if FD_WRITE and LEvtMask = FD_WRITE then
              LoParam := FD_WRITE;
          end;
        end

        { Trigger Read, Accept or Close }
        else if CurEvt.Filter = EVFILT_READ then
        begin
          if LEof then
          begin
            { Peer closed the connection or application called shutdown(0) }
            { (disabled reads). If the application called shutdown(0) we   }
            { should not trigger FD_CLOSE but disable read events. If the  }
            { peer closed the connection it is likely still data to be     }
            { read.                                                        }
            if aesShutDown0Called in IEventSrc.EventState then
              IEventSrc.EventState := IEventSrc.EventState - [aesShutDown0Called]
            else if not (aesCloseNotified in IEventSrc.EventState) then
            begin
              IEventSrc.EventState := IEventSrc.EventState + [aesCloseNotified];
              if (FD_CLOSE and LEvtMask = FD_CLOSE) then
                LoParam := FD_CLOSE
              else if FD_READ and LEvtMask = FD_READ then
                LoParam := FD_READ; // on socket read -> socket error
              if CurEvt.Data = 0 then
                LDelFlag := edaAll;
            end;
            if (FD_READ and LEvtMask = FD_READ) and (CurEvt.Data > 0) then
              { CurEvt.Data contains the number of bytes ready to be read. }
              { If > 0 bitwise or FD_READ, the message handler executes    }
              { FD_READ before FD_CLOSE.                                   }
              LoParam := LoParam or FD_READ;
          end // LEof
          { Trigger Accept }
          else if FD_ACCEPT and LEvtMask = FD_ACCEPT then
            LoParam := FD_ACCEPT
          { Trigger Read }
          else if FD_READ and LEvtMask = FD_READ then
            LoParam := FD_READ;

          if LDelFlag <> edaAll then
            DisableReadEvent(CurEvt.Ident, IEventSrc.ObjectID);
        end;

        { Copy these since they may be cleared when LDelFlag is handled below }
        LMsgID := IEventSrc.NotifyMessageID;
        LHwnd  := IEventSrc.NotifyWindow;

        { Process LDelFlag here since we might have to leave the critical     }
        { section when PostMessage() below failed due to a full queue         }

        case LDelFlag of
          edaAll :
            InternalRemoveEvents(IEventSrc);
          edaWrite :
            begin
              if (LEvtMask and FD_READ = FD_READ) or
                 (LEvtMask and FD_ACCEPT = FD_ACCEPT) then
              begin
                RemoveWriteEvent(CurEvt.Ident, IEventSrc.ObjectID);
                IEventSrc.EventMask := (LEvtMask and not FD_WRITE) and not FD_CONNECT;
              end
              else
                InternalRemoveEvents(IEventSrc);
            end;
        end;

        if LoParam > 0 then // finally post the socket event if any
        begin
          if (LoParam = FD_ACCEPT) then // all events have to be queued/posted
            LPostMsgFunc := PostMessage
          else
            { PostUniqueMessage() posts the message only if it doesn't already }
            { exist in the destination queue. This is especially important     }
            { with FD_READ messages if wsoNoReceiveLoop isn't in the options   }
            { in order to not overflow the queue with useless and performance  }
            { killing messages.                                                }
            LPostMsgFunc := PostUniqueMessage;

          while not LPostMsgFunc(LHwnd, LMsgID, WPARAM(CurEvt.Ident),
                                 LPARAM(IcsMakeLong(LoParam, HiParam))) do
          begin
            LLastErr := GetLastError;
            case LLastErr of
              ERROR_NOT_ENOUGH_QUOTA :
                begin
                  Result := True; // removes compiler warning
                  if FAsyncThread.Terminated then
                    Break;
                  try
                    { Raise a debug exception. Actually this should not happen }
                    { at least not caused by this thread.                      }
                    raise EIcsMessageQueueFull.Create(ClassName + ': Message queue full');
                  except
                    // silent
                  end;
                  { All we can do is wait for destination thread dequeues some  }
                  { messages, we have to leave and reenter the critical section }
                  FQueueSection.Leave;
                  FAsyncThread.Sleep(100);
                  FQueueSection.Enter;
                end;

              ERROR_INVALID_WINDOW_HANDLE,
              ERROR_ACCESS_DENIED, // destination message queue is going down and locked
              EINVAL :
                begin
                  { Should never ever happen, if so, doesn't hurt to ignore }
                  InternalRemoveEvents(IEventSrc);
                  Break;
                end;

              else
                Result := False; // removes compiler warning
                raise EIcsEventQueue.Create(ClassName + ': Unknown error on PostMessage #' +
                  IntToStr(LLastErr));
            end; // case
          end;
        end;
      end; // if FObjIdentList.ContainsKey(NativeInt(CurEvt.uData)) means IEventSrc is assigned
      {$ELSEIF DEFINED(LINUX) OR DEFINED(ANDROID)}
      else if FObjIdentList.ContainsKey(NativeInt(CurEvt.Data.u64)) then
      begin
        FObjIdentList.Items[NativeInt(CurEvt.Data.u64)].GetInterface(IIcsEventSource, IEventSrc);
        Assert(IEventSrc <> nil); { Debug }
        Assert(Integer(CurEvt.Data.fd) = IEventSrc.FileDescriptor); { Debug }

        LEvtMask := IEventSrc.EventMask;

        { Trigger Write, Connect or Close }
        if CurEvt.Events and EPOLLOUT <> 0 then
        begin
          { Trigger Connect }
          if not (aesConnectNotified in IEventSrc.EventState) then
          begin
            IEventSrc.EventState := IEventSrc.EventState + [aesConnectNotified];
            if FD_CONNECT and LEvtMask = FD_CONNECT then
              LoParam := FD_CONNECT
            else if FD_WRITE and LEvtMask = FD_WRITE then
              LoParam := FD_WRITE;
            if LEof then
              LDelFlag := edaAll; // Delete both read and write events
          end
          { Trigger Write or Close }
          else begin
            if LEof then
            begin
              if (aesShutDown1Called in IEventSrc.EventState) then
              begin
                if (not (aesCloseNotified in IEventSrc.EventState)) and
                    ((FD_READ and LEvtMask = FD_READ) or
                    (FD_ACCEPT and LEvtMask = FD_ACCEPT)) then
                  { So we will be notified when the peer send us FIN ACK }
                  EnableReadEvent(CurEvt.Data.fd, IEventSrc.ObjectID);
                LDelFlag := edaWrite; // Delete write event
              end
              else if (not (aesCloseNotified in IEventSrc.EventState)) then
              begin
                if (FD_CLOSE and LEvtMask = FD_CLOSE) then
                  LoParam := FD_CLOSE
                else if FD_WRITE and LEvtMask = FD_WRITE then
                  LoParam := FD_WRITE; // on socket write -> socket error
                IEventSrc.EventState := IEventSrc.EventState + [aesCloseNotified];
                LDelFlag := edaAll    // Delete all
              end;
            end
            else if FD_WRITE and LEvtMask = FD_WRITE then
              LoParam := FD_WRITE;
          end;
        end
        { Trigger Read, Accept or Close }
        else if CurEvt.Events and EPOLLIN <> 0 then
        begin
          if LEof then
          begin
            if aesShutDown0Called in IEventSrc.EventState then
              IEventSrc.EventState := IEventSrc.EventState - [aesShutDown0Called]
            else if not (aesCloseNotified in IEventSrc.EventState) then
            begin
              IEventSrc.EventState := IEventSrc.EventState + [aesCloseNotified];
              if (FD_CLOSE and LEvtMask = FD_CLOSE) then
                LoParam := FD_CLOSE
              else if FD_READ and LEvtMask = FD_READ then
                LoParam := FD_READ; // on socket read -> socket error
              if CurEvt.Data.fd = 0 then
                LDelFlag := edaAll;
            end;
            if (FD_READ and LEvtMask = FD_READ) and (CurEvt.Data.fd > 0) then
              LoParam := LoParam or FD_READ;
          end // LEof
          { Trigger Accept }
          else if FD_ACCEPT and LEvtMask = FD_ACCEPT then
            LoParam := FD_ACCEPT
          { Trigger Read }
          else if FD_READ and LEvtMask = FD_READ then
            LoParam := FD_READ;

          if LDelFlag <> edaAll then
            DisableReadEvent(CurEvt.Data.fd, IEventSrc.ObjectID);
        end;
        { Copy these since they may be cleared when LDelFlag is handled below }
        LMsgID := IEventSrc.NotifyMessageID;
        LHwnd  := IEventSrc.NotifyWindow;

        case LDelFlag of
          edaAll :
            InternalRemoveEvents(IEventSrc);
          edaWrite :
            begin
              if (LEvtMask and FD_READ = FD_READ) or
                  (LEvtMask and FD_ACCEPT = FD_ACCEPT) then
              begin
                RemoveWriteEvent(CurEvt.Data.fd, IEventSrc.ObjectID);
                IEventSrc.EventMask := (LEvtMask and not FD_WRITE) and not FD_CONNECT;
              end
              else
                InternalRemoveEvents(IEventSrc);
            end;
        end;

        if LoParam > 0 then // finally post the socket event if any
        begin
          if (LoParam = FD_ACCEPT) then // all events have to be queued/posted
            LPostMsgFunc := PostMessage
          else
            LPostMsgFunc := PostUniqueMessage;

          while not LPostMsgFunc(LHwnd, LMsgID, WPARAM(CurEvt.Data.fd),
                                 LPARAM(IcsMakeLong(LoParam, HiParam))) do
          begin
            LLastErr := GetLastError;
            case LLastErr of
              ERROR_NOT_ENOUGH_QUOTA :
                begin
             //     Result := True; // removes compiler warning
                  if FAsyncThread.Terminated then
                    Break;
                  try
                    raise EIcsMessageQueueFull.Create(ClassName + ': Message queue full');
                  except
                    // silent
                  end;
                  FQueueSection.Leave;
                  FAsyncThread.Sleep(100);
                  FQueueSection.Enter;
                end;

              ERROR_INVALID_WINDOW_HANDLE,
              ERROR_ACCESS_DENIED, // destination message queue is going down and locked
              EINVAL :
                begin
                  { Should never ever happen, if so, doesn't hurt to ignore }
                  InternalRemoveEvents(IEventSrc);
                  Break;
                end;

              else
          //      Result := False; // removes compiler warning
                raise EIcsEventQueue.Create(ClassName + ': Unknown error on PostMessage #' +
                  IntToStr(LLastErr));
            end; // case
          end;
        end;
      end; // if FObjIdentList.ContainsKey(NativeInt(CurEvt.uData)) means IEventSrc is assigned
      {$IFEND}
    end; // For Loop

    Result := True;

  finally
    FInLoop := False;
    FQueueSection.Leave;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.DeInit: Boolean;
begin
  if FPipeFd.Read <> INVALID_FILE_HANDLE then
  begin
    __close(FPipeFd.Read);
    FPipeFd.Read := INVALID_FILE_HANDLE;
  end;
  if FPipeFd.Write <> INVALID_FILE_HANDLE then
  begin
    __close(FPipeFd.Write);
    FPipeFd.Write := INVALID_FILE_HANDLE
  end;
  if FQueue <> INVALID_FILE_HANDLE then
  begin
    __close(FQueue);
    FQueue := INVALID_FILE_HANDLE;
  end;
  FFreeIndex := 0;
  Result := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.Init: Boolean;
{$IF DEFINED(LINUX) OR DEFINED(ANDROID)}
var
  Evt: TEPoll_Event;
{$IFEND}
begin
  Result := True;
  if FInitialized then
    Exit;
  FQueue := {$IF DEFINED(MACOS) OR DEFINED(IOS)} KQueue {$ELSEIF DEFINED(LINUX) OR DEFINED(ANDROID)} epoll_create(0) {$IFEND};
  if FQueue = INVALID_FILE_HANDLE then
    raise Exception.Create(SysErrorMessage(GetLastError));
  if pipe(@FPipeFd) <> 0 then
  begin
    FPipeFd.Read := INVALID_FILE_HANDLE;
    FPipeFd.Write := INVALID_FILE_HANDLE;
    raise Exception.Create('pipe() failed');
  end;
  Grow;
  {$IF DEFINED(MACOS) OR DEFINED(IOS)}
  EV_SET(@FChangeList[FFreeIndex], FPipeFd.Read, EVFILT_READ,
         EV_ADD, 0, 0, Pointer(-1));
  {$ELSEIF DEFINED(LINUX) OR DEFINED(ANDROID)}
  Evt.Events := EPOLLIN; // Always set the EPOLLIN flag
  Evt.Data.fd := FPipeFd.Read;
  if epoll_ctl(FQueue, EPOLL_CTL_ADD, FPipeFd.Read, @Evt) < 0 then
    raise Exception.Create(SysErrorMessage(errno));
  {$IFEND}
  Inc(FFreeIndex);
  FInitialized := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsEventQueue.Create;
begin
  inherited Create;
  FQueueSection := TIcsCriticalSection.Create;
  FObjIdentList := TDictionary<NativeInt, TObject>.Create;
  Init;
  FAsyncThread := TIcsAsyncSocketThread.Create(True);
  FAsyncThread.FreeOnTerminate := False;
  FAsyncThread.FEventQueue := Self;
  FAsyncThread.Start;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsEventQueue.Destroy;
begin
    try          { V8.71 JK }
        if Assigned(FAsyncThread) then begin
            if not FAsyncThread.Terminated then
                FAsyncThread.TerminateThread;
            FAsyncThread.WaitFor;
            FAsyncThread.Free;
        end;
        DeInit;
        FObjIdentList.Free;
        FQueueSection.Free;
    finally
        inherited;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsEventQueue.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  Inc(FCapacity, Delta);
  SetLength(FChangeList, FCapacity);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.Notify(AMsg: Byte): Boolean;
begin
  Result := __write(FPipeFd.Write, @AMsg, SizeOf(AMsg)) = SizeOf(AMsg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.Wakeup: Boolean;
begin
  Result := Notify(0);
end;
{$ENDIF POSIX}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsAsyncDnsLookupThread }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsAsyncDnsLookupThread.Create(ADnsLookup: TIcsAsyncDnsLookup);
begin
    inherited Create(TRUE);
    FDnsResultList := TStringList.Create;
    FDnsLookup := ADnsLookup;
    FEvent := TEvent.Create(nil, TRUE, TRUE, '');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsAsyncDnsLookupThread.Destroy;
begin
    try          { V8.71 JK }
        Terminate;
        FEvent.SetEvent;
    finally
        inherited Destroy;
    end;
    FDnsResultList.Free;
    FEvent.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ResolveName(
    const AName        : string;
    const AReverse     : Boolean;
    const AFamily      : TSocketFamily;
    AResultList        : TStrings;
    const AProtocol    : Integer): Integer;
var
    Hints     : TAddrInfo;
    AddrInfo  : PAddrInfo;
    NextInfo  : PAddrInfo;
    RetVal    : Integer;
    LHost     : {$IFNDEF POSIX} string; {$ELSE} AnsiString; {$ENDIF}
    UniHost   : String;   { V8.64 }
    IDX       : Integer;
begin
    AResultList.Clear;
    FillChar(Hints, SizeOf(Hints), 0);
    if AFamily = sfIPv4 then
        Hints.ai_family := AF_INET
    else if AFamily = sfIPv6 then
        Hints.ai_family := AF_INET6;
    {else
        Hints.ai_family := AF_UNSPEC;}

    if AReverse then
        Hints.ai_flags := AI_NUMERICHOST;

    AddrInfo := nil;
    Hints.ai_protocol := AProtocol;
  {$IFNDEF POSIX}
    Result   := Ics_GetAddrInfo(PChar(AName), nil, @Hints, AddrInfo);
  {$ELSE}
    Result   := GetAddrInfo(PAnsiChar(AnsiString(AName)), nil, Hints, AddrInfo);
  {$ENDIF}
    if Result = 0 then
    try
        IDX := 0;
        NextInfo := AddrInfo;
        while NextInfo <> nil do
        begin
            if (NextInfo.ai_family = AF_INET) or (NextInfo.ai_family = AF_INET6) then
            begin
                if AReverse then
                begin
                    SetLength(LHost, NI_MAXHOST);
                  {$IFDEF MSWINDOWS}
                    RetVal := Ics_GetNameInfo(NextInfo^.ai_addr,
                                          NextInfo^.ai_addrlen,
                                          PChar(LHost), NI_MAXHOST, nil, 0, 0);
                  {$ELSE}
                    RetVal := GetNameInfo(NextInfo^.ai_addr^,
                                          NextInfo^.ai_addrlen,
                                          PAnsiChar(LHost), NI_MAXHOST, nil, 0, 0);
                  {$ENDIF}
                    if RetVal = 0 then
                    begin
                 { V8.64 if result has ACE xn--. convert it to Unicode, ignore errors }
                      {$IFNDEF POSIX}
                        UniHost := IcsIDNAToUnicode(LHost);
                        AResultList.Add(PChar(UniHost));
                      {$ELSE}
                        UniHost := IcsIDNAToUnicode(String(LHost));
                        AResultList.Add(PChar(UniHost));
                      {$ENDIF}
                    end
                    else begin
                        Result := WSocket_Synchronized_WSAGetLastError;{WSAGetLastError;}// Or just the RetVal.
                        Break;
                    end;
                end
                else begin
                    if NextInfo.ai_family = AF_INET then
                    begin
                        if AFamily = sfAnyIPv4 then
                        begin
                            AResultList.Insert(IDX,
                            WSocketIPv4ToStr(PSockAddrIn(NextInfo.ai_addr)^.sin_addr.S_addr));
                            Inc(IDX);
                        end
                        else
                            AResultList.Add(
                            WSocketIPv4ToStr(PSockAddrIn(NextInfo.ai_addr)^.sin_addr.S_addr));
                    end
                    else begin
                        if AFamily = sfAnyIPv6 then
                        begin
                            AResultList.Insert(IDX,
                            WSocketIPv6ToStr(PSockAddrIn6(NextInfo.ai_addr)));
                            Inc(IDX);
                        end
                        else
                            AResultList.Add(
                            WSocketIPv6ToStr(PSockAddrIn6(NextInfo.ai_addr)));
                    end;
                end;
            end;
            NextInfo := NextInfo.ai_next;
        end;
    finally
      {$IFDEF MSWINDOWS}
        Ics_FreeAddrInfo(AddrInfo);
      {$ELSE}
        FreeAddrInfo(AddrInfo^);
      {$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_ResolveName(
    const AName      : string;
    const AReverse   : Boolean;
    const AFamily    : TSocketFamily;
    AResultList      : TStrings;
    const AProtocol  : Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_ResolveName(AName, AReverse, AFamily,
                                                   AResultList, AProtocol);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAsyncDnsLookupThread.Execute;
var
    Request : TIcsAsyncDnsLookupRequest;
    LRes : WORD;
{$IFDEF POSIX}
    LMsgPump : TIcsMessagePump;
{$ENDIF}
begin
{$IFDEF DEBUG}
    IcsNameThreadForDebugging('TIcsAsyncDnsLookupThread');
{$ENDIF}
{$IFDEF POSIX}
    LMsgPump := TIcsMessagePump.Create; // Used for SendMessage
    try
{$ENDIF}
    while not Terminated do
    begin
        case FEvent.WaitFor(FDnsLookup.FThreadIdleTimeoutMsec) of
            wrError :
                begin
                    if not Terminated then
                    {$IFDEF MSWINDOWS}
                        raise Exception.Create(SysErrorMessage(FEvent.LastError));
                    {$ELSE}
                        raise Exception.Create(SysErrorMessage(GetLastError));
                    {$ENDIF}
                end;
            wrAbandoned : Break;
            wrTimeout :
                begin
                    //if _TryEnterCriticalSection(FDnsLookup.FThreadsLock) then
                    if FDnsLookup.FThreadsLock.TryEnter then
                    try { If we cannot enter the critsec next timeout might be }
                        { successful. EnterCriticalSection may not be called   }
                        { since it might lead to a deadlock when FDnsLookup    }
                        { is freed.                                            }
                        if (not FBusy) and (not Terminated) and
                           (FDnsLookup.FThreads.Count > FDnsLookup.FMinThreads) then
                        begin
                            FDnsLookup.FThreads.Delete(FDnsLookup.FThreads.IndexOf(Self));
                            FreeOnTerminate := TRUE;
                            Break;
                        end;
                    finally
                        //_LeaveCriticalSection(FDnsLookup.FThreadsLock);
                        FDnsLookup.FThreadsLock.Leave;
                    end;
                end;
        end;
        if Terminated then
            Break;
        Request := FDnsLookup.GetNextRequest(Self);
        if Request <> nil then
        try
            if not Request.FCanceled then
            try
                LRes := WSocket_ResolveName(
                                           Request.FLookupName,
                                           Request.FReverse,
                                           Request.FSocketFamily,
                                           FDnsResultList,
                                           Request.FProtocol);
                if (not Terminated) and (not Request.FCanceled) and
                    IsWindow(Request.FWndHandle) then
                begin
                    { Ensure CancelAsyncRequest() returns correct result }
                    FDnsLookup.LockQueue;
                    try
                        if Request.FCanceled then
                            Continue
                        else
                          { Too late to cancel this request }
                            Request.FState := lrsAlready;
                    finally
                        FDnsLookup.UnlockQueue;
                    end;
                    Request.FResultList := FDnsResultList;
                    SendMessage(Request.FWndHandle, Request.FMsgID,
                                WPARAM(Request), IcsMakeLong(0, LRes));
                end;
                { If you see an AV on SendMessage() or somewhere else while }
                { debugging and stepping thru this code it's most likely a  }
                { debugger bug. If you think it's not, let me know (Arno).  }
            except
            {$IFDEF DEBUG}
                on E: Exception do
                  {$IFDEF MSWINDOWS}
                    OutputDebugString(PChar('[' + E.ClassName + '] ' + E.Message));
                  {$ENDIF}
            {$ENDIF}
            end;
        finally
            FDnsLookup.RemoveRequest(Request);
            Request.Free;
        end;
    end;
{$IFDEF POSIX}
    finally
        LMsgPump.Free;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsAsyncDnsLookup }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MACOS}
  {$HINTS OFF}
{$ENDIF}
function TIcsAsyncDnsLookup.ExecAsync(
    AWnd          : HWND;
    AMsgID        : UINT;
    ASocketFamily : TSocketFamily;
    const AName   : string;
    AReverse      : Boolean;
    AProtocol     : Integer): THandle;
var
    Req    : TIcsAsyncDnsLookupRequest;
    Thread : TIcsAsyncDnsLookupThread;
    I      : Integer;
begin
    Result := 0; // unused variable hint in OS X
    LockQueue;
    try
      {$IFDEF MSWINDOWS}
        if (ASocketFamily = sfIPv6) and not IsIPv6APIAvailable then begin    { V8.43 }
            SetLastError(WSAVERNOTSUPPORTED);
            Exit;
        end;
      {$ENDIF}
        Req := TIcsAsyncDnsLookupRequest.Create;
        Req.FWndHandle    := AWnd;
        Req.FMsgID        := AMsgID;
        Req.FSocketFamily := ASocketFamily;
        Req.FReverse      := AReverse;
        Req.FLookupName   := AName;
        Req.FProtocol     := AProtocol;
        FQueue.Add(Req);
    finally
        UnlockQueue;
    end;

    Thread := nil;
    LockThreadList;
    try
        for I := 0 to FThreads.Count - 1 do begin
            if not TIcsAsyncDnsLookupThread(FThreads[I]).FBusy then begin
                Thread := TIcsAsyncDnsLookupThread(FThreads[I]);
                Thread.FBusy := TRUE;
                Thread.FEvent.SetEvent;
                Break;
            end;
        end;
        if (Thread = nil) and (FThreads.Count < FMaxThreads) then begin
            Thread := TIcsAsyncDnsLookupThread.Create(Self);
            FThreads.Add(Thread);
            Thread.FBusy := TRUE;
        {$IF CompilerVersion < 21}
            Thread.Resume;
        {$ELSE}
            Thread.Start;
        {$IFEND}
        end;
        Result := THandle(Req);
    finally
        UnlockThreadList;
    end;
end;
{$IFDEF MACOS}
  {$HINTS ON}
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsAsyncDnsLookup.GetNextRequest(
  AThread: TIcsAsyncDnsLookupThread): TIcsAsyncDnsLookupRequest;
var
    I : Integer;
begin
    LockQueue;
    try
        Result := nil;
        if FDestroying then
            Exit;
        for I := 0 to FQueue.Count - 1 do begin
            if TIcsAsyncDnsLookupRequest(FQueue[I]).FState = lrsNone then begin
                Result := FQueue[I];
                Result.FState := lrsInWork;
                Break;
            end;
        end;
        if Result = nil then
        begin
            AThread.FEvent.ResetEvent;
            AThread.FBusy := FALSE;
        end;
    finally
        UnLockQueue;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsAsyncDnsLookup.CancelAsyncRequest(
  AReq: THandle): Integer;
var
    I   : Integer;
    Req : TIcsAsyncDnsLookupRequest;
begin
    LockQueue;
    try
        I := FQueue.IndexOf(Pointer(AReq));
        if (I > -1) then
            Req := TIcsAsyncDnsLookupRequest(FQueue[I])
        else
            Req := nil;

        if Req <> nil then begin
          {$IFDEF MSWINDOWS}
            if (Req.FSocketFamily = sfIPv6) and not IsIPv6APIAvailable then begin  { V8.43 }
                Result := -1;
                SetLastError(WSAVERNOTSUPPORTED);
                Exit;
            end;
          {$ENDIF}
            if Req.FState = lrsNone then begin
                Req.Free;
                FQueue.Delete(I);
                Result := 0;
            end
            else begin
                if Req.FState = lrsAlready then begin
                    Result := -1;
                    SetLastError(WSAEALREADY);
                end
                else begin
                    Req.FCanceled := TRUE;
                    Result := 0;
                end;
            end;
        end
        else begin
            Result := -1;
            SetLastError(WSAEINVAL);
        end;
    finally
        UnlockQueue;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAsyncDnsLookup.SetMinMaxThreads(AMinThreads, AMaxThreads: Byte);
begin
    LockThreadList;
    try
        if AMaxThreads = 0 then
            AMaxThreads := 1;
        FMaxThreads := AMaxThreads;
        if AMinThreads > AMaxThreads then
            FMinThreads := AMaxThreads
        else
            FMinThreads := AMinThreads;
    finally
        UnlockThreadList;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsAsyncDnsLookup.Create(
    const AMaxThreads           : Integer;
    const AMinThreads           : Integer = 0;
    const AThreadIdleTimeoutSec : Cardinal = 60);
begin
    inherited Create;
    FQueueLock := TIcsCriticalSection.Create;
    FThreadsLock := TIcsCriticalSection.Create;
    //FMaxThreads := AMaxThreads;
    //FMinThreads := AMinThreads;
    FThreadIdleTimeoutMsec := AThreadIdleTimeoutSec * 1000;
    FQueue   := TList.Create;
    FThreads := TList.Create;
    SetMinMaxThreads(AMinThreads, AMaxThreads);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsAsyncDnsLookup.Destroy;
var
    I : Integer;
begin
    try          { V8.71 JK }
        FDestroying := TRUE;
        LockThreadList;
        try
            if Assigned(FThreads) then begin
                for I := 0 to FThreads.Count -1 do
                    TObject(FThreads[I]).Free; // No problem since D7
                FreeAndNil(FThreads);
            end;
            if Assigned(FQueue) then begin
                for I := 0 to FQueue.Count -1 do
                    TObject(FQueue[I]).Free;
                FreeAndNil(FQueue);
            end;
        finally
            UnlockThreadList;
        end;

        FThreadsLock.Free;
        FQueueLock.Free;
    finally
        inherited;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAsyncDnsLookup.LockQueue;
begin
    FQueueLock.Enter;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAsyncDnsLookup.LockThreadList;
begin
    FThreadsLock.Enter;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsAsyncDnsLookup.RemoveRequest(AReq: TIcsAsyncDnsLookupRequest): Boolean;
var
    I : Integer;
begin
    LockQueue;
    try
        I := FQueue.IndexOf(AReq);
        Result := (I > -1);
        if Result then
            FQueue.Delete(I);
    finally
        UnlockQueue;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAsyncDnsLookup.UnlockQueue;
begin
    FQueueLock.Leave;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAsyncDnsLookup.UnlockThreadList;
begin
    FThreadsLock.Leave;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TThreadStoreTree }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TThreadStoreTree.CompareData(Data1, Data2: Pointer): Boolean;
begin
    Result := PThreadStoreItem(Data1)^.ThreadID < PThreadStoreItem(Data2)^.ThreadID;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThreadStoreTree.Notification(Data: Pointer;
  Action: TIcsAvlTreeNotification);
begin
    if Action = atnRemoved then
        Dispose(PThreadStoreItem(Data));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TThreadStoreTree.SameData(Data1, Data2: Pointer): Boolean;
begin
    Result := PThreadStoreItem(Data1)^.ThreadID = PThreadStoreItem(Data2)^.ThreadID;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TThreadLocalStore }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TThreadLocalStore.Create;
begin
    inherited Create;
    FLock := TIcsCriticalSection.Create;
    FTree:= TThreadStoreTree.Create(FALSE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TThreadLocalStore.Destroy;
begin
    try          { V8.71 JK }
        FTree.Free;
    finally
        inherited Destroy;
    end;
    FLock.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThreadLocalStore.Lock;
begin
    FLock.Enter;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TThreadLocalStore.RegisterStore(ThreadID: THandle): PPointer;
var
    Found : Boolean;
    PItem : PThreadStoreItem;
begin
    FTemp.ThreadID := ThreadID;
    PItem := FTree.Find(@FTemp, Found);
    if PItem <> nil then begin
        Inc(PItem^.RefCnt);
        Result := @PItem^.Data;
    end
    else begin
        New(PItem);
        PItem^.ThreadID := FTemp.ThreadID;
        PItem^.RefCnt   := 1;
        PItem^.Data     := nil;
        FTree.Add(PItem);
        Result := @PItem^.Data;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThreadLocalStore.Unlock;
begin
    FLock.Leave;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TThreadLocalStore.UnregisterStore(ThreadID: THandle): Pointer;
var
    Found : Boolean;
    PItem : PThreadStoreItem;
begin
    FTemp.ThreadID := ThreadID;
    PItem := FTree.Find(@FTemp, Found);
    if PItem <> nil then begin
        Dec(PItem^.RefCnt);
        if PItem^.RefCnt = 0 then
        begin
            Result := PItem^.Data;
            FTree.Remove(PItem);
        end
        else
            Result := nil;
    end
    else
        Result := nil;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
initialization
{$IFNDEF NO_ADV_MT}
    CritSecIpList := TIcsCriticalSection.Create;
{$ENDIF}
    IPList     := TStringList.Create;
    //GAsyncDnsLookup := TIcsAsyncDnsLookup.Create(TIcsAsyncDnsLookup.CpuCount); // more or less max. threads ?
    GThreadLocalStore := TThreadLocalStore.Create;
{$IFNDEF COMPILER12_UP}
    CPUCount := GetCPUCount;
{$ENDIF}
{$IFDEF POSIX}
//    GAsyncSocketQueue := TIcsEventQueue.Create;
//  GLObjectIDSection := TIcsCriticalSection.Create;
{$ENDIF}

finalization
    if Assigned(IPList) then begin
        IPList.Free;
        IPList := nil;
    end;
{$IFNDEF NO_ADV_MT}
    CritSecIpList.Free;
{$ENDIF}
    //FreeAndNil(GAsyncDnsLookup);
    FreeAndNil(GThreadLocalStore);
{$IFDEF POSIX}
//  FreeAndNil(GAsyncSocketQueue);
//  FreeAndNil(GLObjectIDSection);
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}


end.
