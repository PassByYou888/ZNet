(*
https://zpascal.net
https://github.com/PassByYou888/ZNet
https://github.com/PassByYou888/zRasterization
https://github.com/PassByYou888/ZSnappy
https://github.com/PassByYou888/Z-AI1.4
https://github.com/PassByYou888/ZAI_1.41
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
{ ****************************************************************************** }
{ * PhysicsIO interface, written by QQ 600585@qq.com                           * }
{ ****************************************************************************** }
unit Z.Net.PhysicsIO;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.Net.Server.Synapse, Z.Net.Client.Synapse,
{$ELSE FPC}

{$IFDEF PhysicsIO_On_ICS}
  Z.Net.Server.ICS, Z.Net.Client.ICS,
{$ENDIF PhysicsIO_On_ICS}
{$IFDEF PhysicsIO_On_ICS9}
  Z.Net.Server.ICS9, Z.Net.Client.ICS9,
{$ENDIF PhysicsIO_On_ICS9}
{$IFDEF PhysicsIO_On_CrossSocket}
  Z.Net.Server.CrossSocket, Z.Net.Client.CrossSocket,
{$ENDIF PhysicsIO_On_CrossSocket}
{$IFDEF PhysicsIO_On_DIOCP}
  Z.Net.Server.DIOCP, Z.Net.Client.DIOCP,
{$ENDIF PhysicsIO_On_DIOCP}
{$IFDEF PhysicsIO_On_Indy}
  Z.Net.Server.Indy, Z.Net.Client.Indy,
{$ENDIF PhysicsIO_On_Indy}
{$IFDEF PhysicsIO_On_Synapse}
  Z.Net.Server.Synapse, Z.Net.Client.Synapse,
{$ENDIF PhysicsIO_On_Synapse}

{$ENDIF FPC}
  Z.Core;

type
{$IFDEF FPC}
  TPhysicsServer = TZNet_Server_Synapse;
  TPhysicsClient = TZNet_Client_Synapse;
{$ELSE FPC}
{$IFDEF PhysicsIO_On_ICS}
  TPhysicsServer = TZNet_Server_ICS;
  TPhysicsClient = TZNet_Client_ICS;
{$ENDIF PhysicsIO_On_ICS}
{$IFDEF PhysicsIO_On_ICS9}
  TPhysicsServer = TZNet_Server_ICS9;
  TPhysicsClient = TZNet_Client_ICS9;
{$ENDIF PhysicsIO_On_ICS9}
{$IFDEF PhysicsIO_On_CrossSocket}
  TPhysicsServer = TZNet_Server_CrossSocket;
  TPhysicsClient = TZNet_Client_CrossSocket;
{$ENDIF PhysicsIO_On_CrossSocket}
{$IFDEF PhysicsIO_On_DIOCP}
  TPhysicsServer = TZNet_Server_DIOCP;
  TPhysicsClient = TZNet_Client_DIOCP;
{$ENDIF PhysicsIO_On_DIOCP}
{$IFDEF PhysicsIO_On_Indy}
  TPhysicsServer = TZNet_Server_Indy;
  TPhysicsClient = TZNet_Client_Indy;
{$ENDIF PhysicsIO_On_Indy}
{$IFDEF PhysicsIO_On_Synapse}
  TPhysicsServer = TZNet_Server_Synapse;
  TPhysicsClient = TZNet_Client_Synapse;
{$ENDIF PhysicsIO_On_Synapse}
{$ENDIF FPC}
  TPhysicsService = TPhysicsServer;
  TZService = TPhysicsServer;
  TPhysicsTunnel = TPhysicsClient;
  TZClient = TPhysicsClient;
  TZTunnel = TPhysicsClient;

implementation

end.
 
