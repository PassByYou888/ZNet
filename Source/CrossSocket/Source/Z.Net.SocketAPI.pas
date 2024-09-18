﻿(*
https://zpascal.net
https://github.com/PassByYou888/ZNet
https://github.com/PassByYou888/zRasterization
https://github.com/PassByYou888/ZSnappy
https://github.com/PassByYou888/Z-AI1.4
https://github.com/PassByYou888/InfiniteIoT
https://github.com/PassByYou888/zMonitor_3rd_Core
https://github.com/PassByYou888/tcmalloc4p
https://github.com/PassByYou888/jemalloc4p
https://github.com/PassByYou888/zCloud
https://github.com/PassByYou888/ZServer4D
https://github.com/PassByYou888/zShell
https://github.com/PassByYou888/ZDB2.0
https://github.com/PassByYou888/zGameWare
https://github.com/PassByYou888/CoreCipher
https://github.com/PassByYou888/zChinese
https://github.com/PassByYou888/zSound
https://github.com/PassByYou888/zExpression
https://github.com/PassByYou888/ZInstaller2.0
https://github.com/PassByYou888/zAI
https://github.com/PassByYou888/NetFileService
https://github.com/PassByYou888/zAnalysis
https://github.com/PassByYou888/PascalString
https://github.com/PassByYou888/zInstaller
https://github.com/PassByYou888/zTranslate
https://github.com/PassByYou888/zVision
https://github.com/PassByYou888/FFMPEG-Header
*)
{******************************************************************************}
{                                                                              }
{       Delphi cross platform socket library                                   }
{                                                                              }
{       Copyright (c) 2017 WiNDDRiVER(soulawing@gmail.com)                     }
{                                                                              }
{       Homepage: https://github.com/winddriver/Delphi-Cross-Socket            }
{                                                                              }
{******************************************************************************}
unit Z.Net.SocketAPI;

// winddriver是个喜欢把程序模型写死的家伙，这并不是一个好习惯，cross的代码一旦出问题，非常难改
// 编译符号全局定义可以有效统一和优化参数，建议所有的库，包括用户自己在工程建的库都引用一下全局定义
{$I ..\..\Z.Define.inc}

interface

uses
  System.SysUtils,
  {$IFDEF POSIX}
  Posix.Base, Posix.UniStd, Posix.SysSocket, Posix.ArpaInet, Posix.NetinetIn,
  Posix.NetDB, Posix.NetinetTCP, Posix.Fcntl, Posix.SysSelect, Posix.StrOpts,
  Posix.SysTime, Posix.Errno
  {$IFDEF LINUX}
  ,Linuxapi.KernelIoctl
  {$ENDIF}
  {$ELSE}
  Winapi.Windows, Z.Net.Winsock2, Z.Net.Wship6
  {$ENDIF};

type
  TRawSockAddrIn = packed record
    AddrLen: Integer;
    case Integer of
      0: (Addr: sockaddr_in);
      1: (Addr6: sockaddr_in6);
  end;

  {$IFDEF POSIX}
  TRawAddrInfo = Posix.NetDB.addrinfo;
  {$ELSE}
  TRawAddrInfo = Z.Net.Winsock2.ADDRINFOW;
  {$ENDIF}
  PRawAddrInfo = ^TRawAddrInfo;
  /// <summary>
  ///   套接字基础接口封装
  /// </summary>
  TSocketAPI = class
  public
    /// <summary>
    ///   新建套接字
    /// </summary>
    class function NewSocket(const ADomain, AType, AProtocol: Integer): THandle; static;
    /// <summary>
    ///   新建 Tcp 套接字
    /// </summary>
    class function NewTcp: THandle; static;
    /// <summary>
    ///   新建 Udp 套接字
    /// </summary>
    class function NewUdp: THandle; static;
    /// <summary>
    ///   关闭套接字
    /// </summary>
    class function CloseSocket(const ASocket: THandle): Integer; static;
    /// <summary>
    ///   停止套接字(SD_RECEIVE=0, SD_SEND=1, SD_BOTH=2)
    /// </summary>
    class function Shutdown(const ASocket: THandle; const AHow: Integer = 2): Integer; static;
    /// <summary>
    ///   接受一个连接请求, 并分配 Socket
    /// </summary>
    class function Accept(const ASocket: THandle; const Addr: PSockAddr; const AddrLen: PInteger): THandle; static;
    /// <summary>
    ///   绑定套接字到指定地址和端口, 支持 IPv6
    /// </summary>
    class function Bind(const ASocket: THandle; const Addr: PSockAddr; const AddrLen: Integer): Integer; static;
    /// <summary>
    ///   连接到主机, 支持 IPv6
    /// </summary>
    class function Connect(const ASocket: THandle; const Addr: PSockAddr; const AddrLen: Integer): Integer; static;
    /// <summary>
    ///   启动监听
    /// </summary>
    class function Listen(const ASocket: THandle; const backlog: Integer = SOMAXCONN): Integer; overload; static;
    /// <summary>
    ///   接收数据
    /// </summary>
    class function Recv(const ASocket: THandle; var Buf; const len: Integer; const flags: Integer = 0): Integer; static;
    /// <summary>
    ///   发送数据
    /// </summary>
    class function Send(const ASocket: THandle; const Buf; const len: Integer; const flags: Integer = 0): Integer; static;
    /// <summary>
    ///    接收数据从指定地址端口(用于UDP)
    /// </summary>
    class function RecvFrom(const ASocket: THandle; const Addr: PSockAddr;
      var AddrLen: Integer; var Buf; const len: Integer; const flags: Integer = 0): Integer; static;
    /// <summary>
    ///    发送数据到指定地址端口(用于UDP)
    /// </summary>
    class function SendTo(const ASocket: THandle; const Addr: PSockAddr;
      const AddrLen: Integer; const Buf; const len: Integer; const flags: Integer = 0): Integer; static;
    /// <summary>
    ///   返回与套接字关联的远程协议地址
    /// </summary>
    class function GetPeerName(const ASocket: THandle; const Addr: PSockAddr;
      var AddrLen: Integer): Integer; static;
    /// <summary>
    ///   返回与套接字关联的本地协议地址
    /// </summary>
    class function GetSockName(const ASocket: THandle; const Addr: PSockAddr;
      var AddrLen: Integer): Integer; static;
    /// <summary>
    ///   获取套接字参数
    /// </summary>
    class function GetSockOpt(const ASocket: THandle; const ALevel, AOptionName: Integer;
       var AOptionValue; var AOptionLen: Integer): Integer; overload; static;
    /// <summary>
    ///   获取套接字参数
    /// </summary>
    class function GetSockOpt<T>(const ASocket: THandle; const ALevel, AOptionName: Integer;
       var AOptionValue: T): Integer; overload; static;
    /// <summary>
    ///   设置套接字参数
    /// </summary>
    class function SetSockOpt(const ASocket: THandle; const ALevel, AOptionName: Integer;
      const AOptionValue: Pointer; AOptionLen: Integer): Integer; overload; static;
    class function SetSockOpt(const ASocket: THandle; const ALevel, AOptionName: Integer;
      const AOptionValue; AOptionLen: Integer): Integer; overload; static;

    /// <summary>
    ///   设置套接字参数
    /// </summary>
    class function SetSockOpt<T>(const ASocket: THandle; const ALevel, AOptionName: Integer;
      const AOptionValue: T): Integer; overload; static;

    /// <summary>
    ///   检查套接字错误码
    /// </summary>
    class function GetError(const ASocket: THandle): Integer; static;
    /// <summary>
    ///   设置非阻塞模式
    /// </summary>
    class function SetNonBlock(const ASocket: THandle; const ANonBlock: Boolean = True): Integer; static;
    /// <summary>
    ///   设置地址重用模式
    /// </summary>
    class function SetReUseAddr(const ASocket: THandle; const AReUseAddr: Boolean = True): Integer; static;
    /// <summary>
    ///   设置心跳参数
    /// </summary>
    class function SetKeepAlive(const ASocket: THandle; const AIdleSeconds, AInterval, ACount: Integer): Integer; static;
    /// <summary>
    ///   开启TCP_NODELAY
    /// </summary>
    class function SetTcpNoDelay(const ASocket: THandle; const ANoDelay: Boolean = True): Integer; static;
    /// <summary>
    ///   设置发送缓冲区大小
    /// </summary>
    class function SetSndBuf(const ASocket: THandle; const ABufSize: Integer): Integer; static;
    /// <summary>
    ///   设置接收缓冲区大小
    /// </summary>
    class function SetRcvBuf(const ASocket: THandle; const ABufSize: Integer): Integer; static;
    /// <summary>
    ///   设置Linger参数(在closesocket()调用, 但是还有数据没发送完毕时容许逗留的秒数)
    /// </summary>
    class function SetLinger(const ASocket: THandle; const AOnOff: Boolean; const ALinger: Integer): Integer; static;
    /// <summary>
    ///   设置广播SO_BROADCAST
    /// </summary>
    class function SetBroadcast(const ASocket: THandle; const ABroadcast: Boolean = True): Integer; static;
    /// <summary>
    ///    设置接收超时(单位为ms)
    /// </summary>
    class function SetRecvTimeout(const ASocket: THandle; const ATimeout: Cardinal): Integer; static;
    /// <summary>
    ///    设置发送超时(单位为ms)
    /// </summary>
    class function SetSendTimeout(const ASocket: THandle; const ATimeout: Cardinal): Integer; static;
    /// <summary>
    ///   查看接收队列
    ///   ATimeout < 0 阻塞
    ///   ATimeout = 0 非阻塞立即返回
    ///   ATimeout > 0 等待超时时间
    /// </summary>
    class function Readable(const ASocket: THandle; const ATimeout: Integer): Integer; static;
    /// <summary>
    ///   查看发送队列
    ///   ATimeout < 0 阻塞
    ///   ATimeout = 0 非阻塞立即返回
    ///   ATimeout > 0 等待超时时间
    /// </summary>
    class function Writeable(const ASocket: THandle; const ATimeout: Integer): Integer; static;
    /// <summary>
    ///   缓存中已接收的字节数
    /// </summary>
    class function RecvdCount(const ASocket: THandle): Integer; static;
    /// <summary>
    ///   解析地址信息, 支持 IPv6
    /// </summary>
    class function GetAddrInfo(const AHostName, AServiceName: string;
      const AHints: TRawAddrInfo): PRawAddrInfo; overload; static;
    /// <summary>
    ///   解析地址信息, 支持 IPv6
    /// </summary>
    class function GetAddrInfo(const AHostName: string; const APort: Word;
      const AHints: TRawAddrInfo): PRawAddrInfo; overload; static;
    /// <summary>
    ///   释放 GetAddrInfo 返回的数据
    /// </summary>
    class procedure FreeAddrInfo(const ARawAddrInfo: PRawAddrInfo); static;
    /// <summary>
    ///   从 SockAddr 结构中解析出 IP 和 端口, 支持 IPv6
    /// </summary>
    class procedure ExtractAddrInfo(const AAddr: PSockAddr; const AAddrLen: Integer;
      var AIP: string; var APort: Word); static;
    /// <summary>
    ///   将域名解析为 IP 地址, 支持 IPv6
    /// </summary>
    class function GetIpAddrByHost(const AHost: string): string; static;
    /// <summary>
    ///    套接字是否有效
    /// </summary>
    class function IsValidSocket(const ASocket: THandle): Boolean; static;
  end;
implementation
{ TSocketAPI }
class function TSocketAPI.NewSocket(const ADomain, AType,
  AProtocol: Integer): THandle;
begin
  Result :=
    {$IFDEF POSIX}
    Posix.SysSocket.
    {$ELSE}
    Z.Net.Winsock2.
    {$ENDIF}
    socket(ADomain, AType, AProtocol);

  {$IFDEF DEBUG}
  if not IsValidSocket(Result) then
    RaiseLastOSError;
  {$ENDIF}
end;

class function TSocketAPI.NewTcp: THandle;
begin
  Result := TSocketAPI.NewSocket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
end;

class function TSocketAPI.NewUdp: THandle;
begin
  Result := TSocketAPI.NewSocket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
end;

class function TSocketAPI.Readable(const ASocket: THandle; const ATimeout: Integer): Integer;
var
  {$IFDEF POSIX}
  LFDSet: fd_set;
  LTime_val: timeval;
  {$ELSE}
  LFDSet: TFDSet;
  LTime_val: TTimeval;
  {$ENDIF}
  P: PTimeVal;
begin
  if (ATimeout >= 0) then
  begin
    LTime_val.tv_sec := ATimeout div 1000;
    LTime_val.tv_usec :=  1000 * (ATimeout mod 1000);
    P := @LTime_val;
  end else
    P := nil;
  {$IFDEF POSIX}
  FD_ZERO(LFDSet);
  _FD_SET(ASocket, LFDSet);
  Result := Posix.SysSelect.select(0, @LFDSet, nil, nil, P);
  {$ELSE}
  FD_ZERO(LFDSet);
  FD_SET(ASocket, LFDSet);
  Result := Z.Net.Winsock2.select(0, @LFDSet, nil, nil, P);
  {$ENDIF}
end;

class function TSocketAPI.Recv(const ASocket: THandle; var Buf; const len,
  flags: Integer): Integer;
begin
  Result :=
    {$IFDEF POSIX}
    Posix.SysSocket.
    {$ELSE}
    Z.Net.Winsock2.
    {$ENDIF}
    recv(ASocket, Buf, len, flags);
end;

class function TSocketAPI.RecvdCount(const ASocket: THandle): Integer;
{$IFNDEF POSIX}
var
  LTemp : Cardinal;
{$ENDIF}
begin
  {$IFDEF POSIX}
  Result := ioctl(ASocket, FIONREAD);
  {$ELSE}
  if ioctlsocket(ASocket, FIONREAD, LTemp) = SOCKET_ERROR then
    Result := -1
  else
    Result := LTemp;
  {$ENDIF}
end;

class function TSocketAPI.RecvFrom(const ASocket: THandle; const Addr: PSockAddr;
  var AddrLen: Integer; var Buf; const len, flags: Integer): Integer;
begin
  {$IFDEF POSIX}
  Result := Posix.SysSocket.recvfrom(ASocket, Buf, len, flags, Addr^, socklen_t(AddrLen));
  {$ELSE}
  Result := Z.Net.Winsock2.recvfrom(ASocket, Buf, len, flags, Addr, @AddrLen);
  {$ENDIF}
end;

class function TSocketAPI.Accept(const ASocket: THandle; const Addr: PSockAddr;
  const AddrLen: PInteger): THandle;
begin
  {$IFDEF POSIX}
  Result := Posix.SysSocket.accept(ASocket, Addr^, socklen_t(AddrLen^));
  {$ELSE}
  Result := Z.Net.Winsock2.accept(ASocket, Addr, AddrLen);
  {$ENDIF}
end;

class function TSocketAPI.Bind(const ASocket: THandle; const Addr: PSockAddr;
  const AddrLen: Integer): Integer;
begin
  {$IFDEF POSIX}
  Result := Posix.SysSocket.bind(ASocket, Addr^, AddrLen);
  {$ELSE}
  Result := Z.Net.Winsock2.bind(ASocket, Addr, AddrLen);
  {$ENDIF}
end;

class function TSocketAPI.CloseSocket(const ASocket: THandle): Integer;
begin
  {$IFDEF POSIX}
  Result := Posix.UniStd.__close(ASocket);
  {$ELSE}
  Result := Z.Net.Winsock2.closesocket(ASocket);
  {$ENDIF}
end;

class function TSocketAPI.Shutdown(const ASocket: THandle; const AHow: Integer): Integer;
begin
  {$IFDEF POSIX}
  Result := Posix.SysSocket.shutdown(ASocket, AHow);
  {$ELSE}
  Result := Z.Net.Winsock2.shutdown(ASocket, AHow);
  {$ENDIF}
end;

class function TSocketAPI.Connect(const ASocket: THandle; const Addr: PSockAddr;
  const AddrLen: Integer): Integer;
begin
  {$IFDEF POSIX}
  Result := Posix.SysSocket.connect(ASocket, Addr^, AddrLen);
  {$ELSE}
  Result := Z.Net.Winsock2.connect(ASocket, Addr, AddrLen);
  {$ENDIF}
  {$IFDEF DEBUG}
//  if (Result <> 0) and (GetLastError <> EINPROGRESS) then
//    RaiseLastOSError;
  {$ENDIF}
end;

class function TSocketAPI.GetAddrInfo(const AHostName, AServiceName: string;
  const AHints: TRawAddrInfo): PRawAddrInfo;
var
  M: TMarshaller;
  LHost, LService: Pointer;
  LRet: Integer;
  LAddrInfo: PRawAddrInfo;
begin
  Result := nil;

  {$IFDEF POSIX}
  if (AHostName <> '') then
    LHost := M.AsAnsi(AHostName).ToPointer
  else
    LHost := nil;
  if (AServiceName <> '') then
    LService := M.AsAnsi(AServiceName).ToPointer
  else
    LService := nil;
  LRet := Posix.NetDB.getaddrinfo(LHost, LService, AHints, Paddrinfo(LAddrInfo));
  {$ELSE}
  if (AHostName <> '') then
    LHost := M.OutString(AHostName).ToPointer
  else
    LHost := nil;
  if (AServiceName <> '') then
    LService := M.OutString(AServiceName).ToPointer
  else
    LService := nil;
  LRet := Z.Net.Wship6.getaddrinfo(LHost, LService, @AHints, @LAddrInfo);
  {$ENDIF}

  if (LRet <> 0) then Exit;

  Result := LAddrInfo;
end;

class function TSocketAPI.GetAddrInfo(const AHostName: string; const APort: Word;
  const AHints: TRawAddrInfo): PRawAddrInfo;
begin
  Result := GetAddrInfo(AHostName, APort.ToString, AHints);
end;

class function TSocketAPI.GetError(const ASocket: THandle): Integer;
var
  LRet, LErrLen: Integer;
begin
  LErrLen := SizeOf(Integer);
  LRet := TSocketAPI.GetSockOpt(ASocket, SOL_SOCKET, SO_ERROR, Result, LErrLen);
  if (LRet <> 0) then
    Result := LRet;
end;

class procedure TSocketAPI.FreeAddrInfo(const ARawAddrInfo: PRawAddrInfo);
begin
  {$IFDEF POSIX}
  Posix.NetDB.freeaddrinfo(ARawAddrInfo^);
  {$ELSE}
  Z.Net.Wship6.freeaddrinfo(PAddrInfoW(ARawAddrInfo));
  {$ENDIF}
end;

class procedure TSocketAPI.ExtractAddrInfo(const AAddr: PSockAddr;
  const AAddrLen: Integer; var AIP: string; var APort: Word);
var
  M: TMarshaller;
  LIP, LServInfo: TPtrWrapper;
begin
  LIP := M.AllocMem(NI_MAXHOST);
  LServInfo := M.AllocMem(NI_MAXSERV);
  {$IFDEF POSIX}
  getnameinfo(AAddr^, AAddrLen, LIP.ToPointer, NI_MAXHOST, LServInfo.ToPointer, NI_MAXSERV, NI_NUMERICHOST or NI_NUMERICSERV);
  AIP := TMarshal.ReadStringAsAnsi(LIP);
  APort := TMarshal.ReadStringAsAnsi(LServInfo).ToInteger;
  {$ELSE}
  getnameinfo(AAddr, AAddrLen, LIP.ToPointer, NI_MAXHOST, LServInfo.ToPointer, NI_MAXSERV, NI_NUMERICHOST or NI_NUMERICSERV);
  AIP := TMarshal.ReadStringAsUnicode(LIP);
  APort := TMarshal.ReadStringAsUnicode(LServInfo).ToInteger;
  {$ENDIF}
end;

class function TSocketAPI.GetIpAddrByHost(const AHost: string): string;
var
  LHints: TRawAddrInfo;
  LAddrInfo: PRawAddrInfo;
  LPort: Word;
begin
  FillChar(LHints, SizeOf(TRawAddrInfo), 0);
  LAddrInfo := GetAddrInfo(AHost, '', LHints);
  if (LAddrInfo = nil) then Exit('');
  ExtractAddrInfo(LAddrInfo.ai_addr, LAddrInfo.ai_addrlen, Result, LPort);
  FreeAddrInfo(LAddrInfo);
end;
class function TSocketAPI.GetPeerName(const ASocket: THandle; const Addr: PSockAddr;
  var AddrLen: Integer): Integer;
begin
  {$IFDEF POSIX}
  Result := Posix.SysSocket.getpeername(ASocket, Addr^, socklen_t(AddrLen));
  {$ELSE}
  Result := Z.Net.Winsock2.getpeername(ASocket, Addr, AddrLen);
  {$ENDIF}
end;

class function TSocketAPI.GetSockName(const ASocket: THandle; const Addr: PSockAddr;
  var AddrLen: Integer): Integer;
begin
  {$IFDEF POSIX}
  Result := Posix.SysSocket.getsockname(ASocket, Addr^, socklen_t(AddrLen));
  {$ELSE}
  Result := Z.Net.Winsock2.getsockname(ASocket, Addr, AddrLen);
  {$ENDIF}
end;

class function TSocketAPI.GetSockOpt(const ASocket: THandle; const ALevel, AOptionName: Integer;
  var AOptionValue; var AOptionLen: Integer): Integer;
begin
  {$IFDEF POSIX}
  Result := Posix.SysSocket.getsockopt(ASocket, ALevel, AOptionName, AOptionValue, socklen_t(AOptionLen));
  {$ELSE}
  Result := Z.Net.Winsock2.getsockopt(ASocket, ALevel, AOptionName, PAnsiChar(@AOptionValue), AOptionLen);
  {$ENDIF}
end;

class function TSocketAPI.GetSockOpt<T>(const ASocket: THandle; const ALevel,
  AOptionName: Integer; var AOptionValue: T): Integer;
var
  LOptionLen: Integer;
begin
  Result := GetSockOpt(ASocket, ALevel, AOptionName, AOptionValue, LOptionLen);
end;

class function TSocketAPI.IsValidSocket(const ASocket: THandle): Boolean;
begin
  Result := (ASocket <> INVALID_HANDLE_VALUE);
end;

class function TSocketAPI.Listen(const ASocket: THandle; const backlog: Integer): Integer;
begin
  Result :=
    {$IFDEF POSIX}
    Posix.SysSocket.
    {$ELSE}
    Z.Net.Winsock2.
    {$ENDIF}
    listen(ASocket, backlog);

  {$IFDEF DEBUG}
//  if (Result <> 0) then
//    RaiseLastOSError;
  {$ENDIF}
end;

class function TSocketAPI.Send(const ASocket: THandle; const Buf; const len,
  flags: Integer): Integer;
begin
  Result :=
    {$IFDEF POSIX}
    Posix.SysSocket.
    {$ELSE}
    Z.Net.Winsock2.
    {$ENDIF}
    send(ASocket, Buf, len, flags);
end;

class function TSocketAPI.SendTo(const ASocket: THandle; const Addr: PSockAddr;
  const AddrLen: Integer; const Buf; const len, flags: Integer): Integer;
begin
  {$IFDEF POSIX}
  Result := Posix.SysSocket.sendto(ASocket, Buf, len, flags, Addr^, AddrLen);
  {$ELSE}
  Result := Z.Net.Winsock2.sendto(ASocket, Buf, len, flags, Addr, AddrLen);
  {$ENDIF}
end;

class function TSocketAPI.SetBroadcast(const ASocket: THandle;
  const ABroadcast: Boolean): Integer;
var
  LOptVal: Integer;
begin
  if ABroadcast then
    LOptVal := 1
  else
    LOptVal := 0;
  Result := TSocketAPI.SetSockOpt(ASocket, SOL_SOCKET, SO_BROADCAST, LOptVal, SizeOf(Integer));
end;

class function TSocketAPI.SetKeepAlive(const ASocket: THandle; const AIdleSeconds,
  AInterval, ACount: Integer): Integer;
var
  LOptVal: Integer;
  {$IFDEF MSWINDOWS}
  LKeepAlive: tcp_keepalive;
  LBytes: Cardinal;
  {$ENDIF}
begin
  LOptVal := 1;
  Result := SetSockOpt(ASocket, SOL_SOCKET, SO_KEEPALIVE, LOptVal, SizeOf(Integer));
  if (Result < 0) then Exit;

  {$IFDEF MSWINDOWS}
  // Windows 下重试次数为 3 次, 无法修改
  LKeepAlive.onoff := 1;
  LKeepAlive.keepalivetime := AIdleSeconds * 1000;
  LKeepAlive.keepaliveinterval := AInterval * 1000;
  LBytes := 0;
  Result := WSAIoctl(ASocket, SIO_KEEPALIVE_VALS, @LKeepAlive, SizeOf(tcp_keepalive),
    nil, 0, @LBytes, nil, nil);
  {$ELSEIF defined(MACOS)}
  // MAC 下 TCP_KEEPALIVE 相当于 Linux 中的 TCP_KEEPIDLE
  // 暂不支持 TCP_KEEPINTVL 和 TCP_KEEPCNT
  // OSX 10.9.5下默认的心跳参数
  // sysctl -A | grep net.inet.tcp.*keep
  // **************************************
  // net.inet.tcp.keepidle: 7200000
  // net.inet.tcp.keepintvl: 75000
  // net.inet.tcp.keepinit: 75000
  // net.inet.tcp.keepcnt: 8
  // net.inet.tcp.always_keepalive: 0
  // **************************************
  Result := SetSockOpt(ASocket, IPPROTO_TCP, TCP_KEEPALIVE, AIdleSeconds, SizeOf(Integer));
  {$ELSEIF defined(LINUX) or defined(ANDROID)}
  Result := SetSockOpt(ASocket, IPPROTO_TCP, TCP_KEEPIDLE, AIdleSeconds, SizeOf(Integer));
  if (Result < 0) then Exit;

  Result := SetSockOpt(ASocket, IPPROTO_TCP, TCP_KEEPINTVL, AInterval, SizeOf(Integer));
  if (Result < 0) then Exit;

  Result := SetSockOpt(ASocket, IPPROTO_TCP, TCP_KEEPCNT, ACount, SizeOf(Integer));
  if (Result < 0) then Exit;
  {$ENDIF}
end;

class function TSocketAPI.SetLinger(const ASocket: THandle;
  const AOnOff: Boolean; const ALinger: Integer): Integer;
var
  LLinger: linger;
begin
  if AOnOff then
    LLinger.l_onoff := 1
  else
    LLinger.l_onoff := 0;
  LLinger.l_linger := ALinger;
  Result := SetSockOpt(ASocket, SOL_SOCKET, SO_LINGER, LLinger, SizeOf(linger));
end;

class function TSocketAPI.SetNonBlock(const ASocket: THandle;
  const ANonBlock: Boolean): Integer;
var
  LFlag: Cardinal;
begin
  {$IFDEF POSIX}
  LFlag := fcntl(ASocket, F_GETFL);
  if ANonBlock then
    LFlag := LFlag and not O_SYNC or O_NONBLOCK
  else
    LFlag := LFlag and not O_NONBLOCK or O_SYNC;
  Result := fcntl(ASocket, F_SETFL, LFlag);
  {$ELSE}
  if ANonBlock then
    LFlag := 1
  else
    LFlag := 0;
  Result := ioctlsocket(ASocket, FIONBIO, LFlag);
  {$ENDIF}
end;

class function TSocketAPI.SetReUseAddr(const ASocket: THandle;
  const AReUseAddr: Boolean): Integer;
var
  LOptVal: Integer;
begin
  if AReUseAddr then
    LOptVal := 1
  else
    LOptVal := 0;
  Result := TSocketAPI.SetSockOpt(ASocket, SOL_SOCKET, SO_REUSEADDR, LOptVal, SizeOf(Integer));
end;

class function TSocketAPI.SetRcvBuf(const ASocket: THandle;
  const ABufSize: Integer): Integer;
begin
  Result := TSocketAPI.SetSockOpt(ASocket, SOL_SOCKET, SO_RCVBUF, ABufSize, SizeOf(Integer));
end;

class function TSocketAPI.SetRecvTimeout(const ASocket: THandle;
  const ATimeout: Cardinal): Integer;
begin
  Result := SetSockOpt(ASocket,
    SOL_SOCKET, SO_RCVTIMEO, ATimeout, SizeOf(Cardinal));
end;

class function TSocketAPI.SetSendTimeout(const ASocket: THandle;
  const ATimeout: Cardinal): Integer;
begin
  Result := TSocketAPI.SetSockOpt(ASocket,
    SOL_SOCKET, SO_SNDTIMEO, ATimeout, SizeOf(Cardinal));
end;

class function TSocketAPI.SetSndBuf(const ASocket: THandle;
  const ABufSize: Integer): Integer;
begin
  Result := TSocketAPI.SetSockOpt(ASocket, SOL_SOCKET, SO_SNDBUF, ABufSize, SizeOf(Integer));
end;

class function TSocketAPI.SetSockOpt(const ASocket: THandle; const ALevel,
  AOptionName: Integer; const AOptionValue: Pointer;
  AOptionLen: Integer): Integer;
begin
  {$IFDEF POSIX}
  Result := Posix.SysSocket.setsockopt(ASocket, ALevel, AOptionName, AOptionValue^, Cardinal(AOptionLen));
  {$ELSE}
  Result := Z.Net.Winsock2.setsockopt(ASocket, ALevel, AOptionName, AOptionValue, AOptionLen);
  {$ENDIF}
end;

class function TSocketAPI.SetSockOpt(const ASocket: THandle; const ALevel, AOptionName: Integer;
  const AOptionValue; AOptionLen: Integer): Integer;
begin
  {$IFDEF POSIX}
  Result := Posix.SysSocket.setsockopt(ASocket, ALevel, AOptionName, AOptionValue, Cardinal(AOptionLen));
  {$ELSE}
  Result := Z.Net.Winsock2.setsockopt(ASocket, ALevel, AOptionName, PAnsiChar(@AOptionValue), AOptionLen);
  {$ENDIF}
end;

class function TSocketAPI.SetSockOpt<T>(const ASocket: THandle; const ALevel,
  AOptionName: Integer; const AOptionValue: T): Integer;
begin
  Result := SetSockOpt(ASocket, ALevel, AOptionName, AOptionValue, SizeOf(T));
end;

class function TSocketAPI.SetTcpNoDelay(const ASocket: THandle;
  const ANoDelay: Boolean): Integer;
var
  LOptVal: Integer;
begin
  if ANoDelay then
    LOptVal := 1
  else
    LOptVal := 0;
  Result := TSocketAPI.SetSockOpt(ASocket, IPPROTO_TCP, TCP_NODELAY, LOptVal, SizeOf(Integer));
end;

class function TSocketAPI.Writeable(const ASocket: THandle;
  const ATimeout: Integer): Integer;
var
  {$IFDEF POSIX}
  LFDSet: fd_set;
  LTime_val: timeval;
  {$ELSE}
  LFDSet: TFDSet;
  LTime_val: TTimeval;
  {$ENDIF}
  P: PTimeVal;
begin
  if (ATimeout >= 0) then
  begin
    LTime_val.tv_sec := ATimeout div 1000;
    LTime_val.tv_usec :=  1000 * (ATimeout mod 1000);
    P := @LTime_val;
  end else
    P := nil;
  {$IFDEF POSIX}
  FD_ZERO(LFDSet);
  _FD_SET(ASocket, LFDSet);
  Result := Posix.SysSelect.select(0, nil, @LFDSet, nil, P);
  {$ELSE}
  FD_ZERO(LFDSet);
  FD_SET(ASocket, LFDSet);
  Result := Z.Net.Winsock2.select(0, nil, @LFDSet, nil, P);
  {$ENDIF}
end;

end.

 
