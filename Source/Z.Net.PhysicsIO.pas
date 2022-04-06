{ ****************************************************************************** }
{ * PhysicsIO interface, written by QQ 600585@qq.com                           * }
{ ****************************************************************************** }
unit Z.Net.PhysicsIO;

{$I ..\Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.Net.Server.Synapse, Z.Net.Client.Synapse,
{$ELSE FPC}
{$IFDEF PhysicsIO_On_ICS}
  Z.Net.Server.ICS, Z.Net.Client.ICS,
{$ENDIF PhysicsIO_On_ICS}
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

implementation

end.
