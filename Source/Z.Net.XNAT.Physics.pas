{ ****************************************************************************** }
{ * XNAT tunnel Physics interface                                              * }
{ ****************************************************************************** }
unit Z.Net.XNAT.Physics;

{$I Z.Define.inc}

interface

uses Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Net.PhysicsIO;

type
  TXPhysicsServer = class(TPhysicsServer)
  end;

  TXPhysicsClient = class(TPhysicsClient)
  end;

  TXNAT_PHYSICS_MODEL = (XNAT_PHYSICS_SERVICE, XNAT_PHYSICS_CLIENT);

procedure Build_XNAT_Buff(buff: PByte; siz: NativeInt; local_id, remote_id: Cardinal; var NewSiz: NativeInt; var NewBuff: PByte);
procedure Extract_XNAT_Buff(sour: PByte; siz: NativeInt; var local_id, remote_id: Cardinal; var destSiz: NativeInt; var destBuff: PByte);

const
  C_RequestListen: SystemString = '__@RequestListen';
  C_Connect_request: SystemString = '__@connect_request';
  C_Disconnect_request: SystemString = '__@disconnect_request';
  C_Data: SystemString = '__@data';
  C_Connect_reponse: SystemString = '__@connect_reponse';
  C_Disconnect_reponse: SystemString = '__@disconnect_reponse';
  C_Workload: SystemString = '__@workload';
  C_IPV6Listen: SystemString = '__@IPv6Listen';

implementation

procedure Build_XNAT_Buff(buff: PByte; siz: NativeInt; local_id, remote_id: Cardinal; var NewSiz: NativeInt; var NewBuff: PByte);
var
  nb: PByte;
begin
  NewSiz := siz + 8;
  nb := System.GetMemory(NewSiz);
  NewBuff := nb;
  PCardinal(nb)^ := local_id;
  inc(nb, 4);
  PCardinal(nb)^ := remote_id;
  inc(nb, 4);
  CopyPtr(buff, nb, siz);
end;

procedure Extract_XNAT_Buff(sour: PByte; siz: NativeInt; var local_id, remote_id: Cardinal; var destSiz: NativeInt; var destBuff: PByte);
begin
  destSiz := siz - 8;
  local_id := PCardinal(sour)^;
  inc(sour, 4);
  remote_id := PCardinal(sour)^;
  inc(sour, 4);
  destBuff := sour;
end;

end.
