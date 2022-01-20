{ ****************************************************************************** }
{ * PhysicsIO interface, written by QQ 600585@qq.com                           * }
{ ****************************************************************************** }
unit Z.Net.IO;

{$I Z.Define.inc}

interface

uses Z.Net.PhysicsIO;

type
  TServer = class(Z.Net.PhysicsIO.TPhysicsServer)
  end;

  TClient = class(Z.Net.PhysicsIO.TPhysicsClient)
  end;

implementation

end.
