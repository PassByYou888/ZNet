{ ****************************************************************************** }
{ * XNAT tunnel Physics interface                                              * }
{ ****************************************************************************** }
unit Z.Net.XNAT.Physics;

{$I Z.Define.inc}

interface

uses Z.Core, Z.Net.PhysicsIO;

type
  TXPhysicsServer = TPhysicsServer;
  TXPhysicsClient = TPhysicsClient;
  TXNAT_PHYSICS_MODEL = (XNAT_PHYSICS_SERVICE, XNAT_PHYSICS_CLIENT);

procedure BuildBuff(buff: PByte; siz: NativeInt; local_id, remote_id: Cardinal; var NewSiz: NativeInt; var NewBuff: PByte);
procedure ExtractBuff(sour: PByte; siz: NativeInt; var local_id, remote_id: Cardinal; var destSiz: NativeInt; var destBuff: PByte);

const
  C_RequestListen = '__@RequestListen';
  C_Connect_request = '__@connect_request';
  C_Disconnect_request = '__@disconnect_request';
  C_Data = '__@data';
  C_Connect_reponse = '__@connect_reponse';
  C_Disconnect_reponse = '__@disconnect_reponse';
  C_Workload = '__@workload';
  C_IPV6Listen = '__@IPv6Listen';

implementation

procedure BuildBuff(buff: PByte; siz: NativeInt; local_id, remote_id: Cardinal; var NewSiz: NativeInt; var NewBuff: PByte);
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

procedure ExtractBuff(sour: PByte; siz: NativeInt; var local_id, remote_id: Cardinal; var destSiz: NativeInt; var destBuff: PByte);
begin
  destSiz := siz - 8;
  local_id := PCardinal(sour)^;
  inc(sour, 4);
  remote_id := PCardinal(sour)^;
  inc(sour, 4);
  destBuff := sour;
end;

end.
 
