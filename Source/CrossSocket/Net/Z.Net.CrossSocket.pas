{******************************************************************************}
{                                                                              }
{       Delphi cross platform socket library                                   }
{                                                                              }
{       Copyright (c) 2017 WiNDDRiVER(soulawing@gmail.com)                     }
{                                                                              }
{       Homepage: https://github.com/winddriver/Delphi-Cross-Socket            }
{                                                                              }
{******************************************************************************}
unit Z.Net.CrossSocket;

// winddriver是个喜欢把程序模型写死的家伙，这并不是一个好习惯，cross的代码一旦出问题，非常难改
// 编译符号全局定义可以有效统一和优化参数，建议所有的库，包括用户自己在工程建的库都引用一下全局定义
{$I ..\..\Z.Define.inc}

interface

uses
  Z.Net.CrossSocket.Base,
  {$IFDEF MSWINDOWS}
  Z.Net.CrossSocket.Iocp
  {$ELSEIF defined(MACOS) or defined(IOS)}
  Z.Net.CrossSocket.Kqueue
  {$ELSEIF defined(LINUX) or defined(ANDROID)}
  Z.Net.CrossSocket.Epoll
  {$ENDIF};

type
  TCrossListen =
    {$IFDEF MSWINDOWS}
    TIocpListen
    {$ELSEIF defined(MACOS) or defined(IOS)}
    TKqueueListen
    {$ELSEIF defined(LINUX) or defined(ANDROID)}
    TEpollListen
    {$ENDIF};

  TCrossConnection =
    {$IFDEF MSWINDOWS}
    TIocpConnection
    {$ELSEIF defined(MACOS) or defined(IOS)}
    TKqueueConnection
    {$ELSEIF defined(LINUX) or defined(ANDROID)}
    TEpollConnection
    {$ENDIF};

  TCrossSocket =
    {$IFDEF MSWINDOWS}
    TIocpCrossSocket
    {$ELSEIF defined(MACOS) or defined(IOS)}
    TKqueueCrossSocket
    {$ELSEIF defined(LINUX) or defined(ANDROID)}
    TEpollCrossSocket
    {$ENDIF};

implementation

end.
