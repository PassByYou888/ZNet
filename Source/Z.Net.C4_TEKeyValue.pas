{ ****************************************************************************** }
{ * cloud 4.0 text engine key-value service                                    * }
{ ****************************************************************************** }
unit Z.Net.C4_TEKeyValue;

{$I Z.Define.inc}

interface

uses Variants,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.UnicodeMixedLib,
  Z.Geometry2D, Z.DFE, Z.ListEngine,
  Z.Parsing, Z.Expression, Z.OpCode,
  Z.Notify, Z.Cipher, Z.MemoryStream,
  Z.Net, Z.Net.PhysicsIO, Z.Net.DoubleTunnelIO.NoAuth, Z.Net.C4,
  Z.TextDataEngine,
  Z.ZDB2.TE, Z.ZDB2, Z.GHashList;

type
  TC40_TEKeyValue_Service_Hash_Pool = {$IFDEF FPC}specialize {$ENDIF FPC}TGeneric_String_Object_Hash<TZDB2_HashTextEngine>;

  TC40_TEKeyValue_Service = class(TC40_Base_NoAuth_Service)
  protected
    // command
    procedure cmd_Rebuild(sender: TPeerIO; InData: TDFE);
    procedure cmd_GetTE(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_SetTE(sender: TPeerIO; InData: TDFE);
    procedure cmd_MergeTE(sender: TPeerIO; InData: TDFE);
    procedure cmd_RemoveTE(sender: TPeerIO; InData: TDFE);
    procedure cmd_GetSection(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_GetKey(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_GetTextKey(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_GetKeyValue(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_GetTextKeyValue(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_ExistsSection(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_ExistsKey(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_RemoveSection(sender: TPeerIO; InData: TDFE);
    procedure cmd_RemoveKey(sender: TPeerIO; InData: TDFE);
    procedure cmd_GetValue(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_GetTextValue(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_SetValue(sender: TPeerIO; InData: TDFE);
    procedure cmd_SetTextValue(sender: TPeerIO; InData: TDFE);
    // admin
    procedure cmd_SearchTE(sender: TPeerIO; InData, OutData: TDFE);
  public
    ZDB2RecycleMemoryTimeOut: TTimeTick;
    ZDB2DeltaSpace: Int64;
    ZDB2BlockSize: Word;
    ZDB2EnabledCipher: Boolean;
    ZDB2CipherName: U_String;
    ZDB2Password: U_String;
    ZDB2Cipher: TZDB2_Cipher;
    C40_TEKeyValue_DB_FileName: U_String;
    TEKeyValue_DB: TZDB2_List_HashTextEngine;
    TEKeyValue_Hash: TC40_TEKeyValue_Service_Hash_Pool;
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;
    function GetOrCreateTE(TEName_: SystemString): TZDB2_HashTextEngine;
    function GetTEName(TE: TZDB2_HashTextEngine): SystemString;
  end;

{$REGION 'bridge_define'}

  TC40_TEKeyValue_Client = class;

  TC40_TEKeyValue_Client_GetTE_C = procedure(sender: TC40_TEKeyValue_Client; TE: THashTextEngine);
  TC40_TEKeyValue_Client_GetTE_M = procedure(sender: TC40_TEKeyValue_Client; TE: THashTextEngine) of object;
{$IFDEF FPC}
  TC40_TEKeyValue_Client_GetTE_P = procedure(sender: TC40_TEKeyValue_Client; TE: THashTextEngine) is nested;
{$ELSE FPC}
  TC40_TEKeyValue_Client_GetTE_P = reference to procedure(sender: TC40_TEKeyValue_Client; TE: THashTextEngine);
{$ENDIF FPC}

  TC40_TEKeyValue_Client_GetTE = class(TOnResultBridge)
  public
    Client: TC40_TEKeyValue_Client;
    OnResultC: TC40_TEKeyValue_Client_GetTE_C;
    OnResultM: TC40_TEKeyValue_Client_GetTE_M;
    OnResultP: TC40_TEKeyValue_Client_GetTE_P;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDFE); override;
  end;

  TC40_TEKeyValue_Client_GetSection_C = procedure(sender: TC40_TEKeyValue_Client; arry: U_StringArray);
  TC40_TEKeyValue_Client_GetSection_M = procedure(sender: TC40_TEKeyValue_Client; arry: U_StringArray) of object;
{$IFDEF FPC}
  TC40_TEKeyValue_Client_GetSection_P = procedure(sender: TC40_TEKeyValue_Client; arry: U_StringArray) is nested;
{$ELSE FPC}
  TC40_TEKeyValue_Client_GetSection_P = reference to procedure(sender: TC40_TEKeyValue_Client; arry: U_StringArray);
{$ENDIF FPC}

  TC40_TEKeyValue_Client_GetSection = class(TOnResultBridge)
  public
    Client: TC40_TEKeyValue_Client;
    OnResultC: TC40_TEKeyValue_Client_GetSection_C;
    OnResultM: TC40_TEKeyValue_Client_GetSection_M;
    OnResultP: TC40_TEKeyValue_Client_GetSection_P;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDFE); override;
  end;

  TC40_TEKeyValue_Client_GetKey_C = procedure(sender: TC40_TEKeyValue_Client; arry: U_StringArray);
  TC40_TEKeyValue_Client_GetKey_M = procedure(sender: TC40_TEKeyValue_Client; arry: U_StringArray) of object;
{$IFDEF FPC}
  TC40_TEKeyValue_Client_GetKey_P = procedure(sender: TC40_TEKeyValue_Client; arry: U_StringArray) is nested;
{$ELSE FPC}
  TC40_TEKeyValue_Client_GetKey_P = reference to procedure(sender: TC40_TEKeyValue_Client; arry: U_StringArray);
{$ENDIF FPC}

  TC40_TEKeyValue_Client_GetKey = class(TOnResultBridge)
  public
    Client: TC40_TEKeyValue_Client;
    OnResultC: TC40_TEKeyValue_Client_GetKey_C;
    OnResultM: TC40_TEKeyValue_Client_GetKey_M;
    OnResultP: TC40_TEKeyValue_Client_GetKey_P;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDFE); override;
  end;

  TC40_TEKeyValue_Client_GetTextKey_C = procedure(sender: TC40_TEKeyValue_Client; arry: U_StringArray);
  TC40_TEKeyValue_Client_GetTextKey_M = procedure(sender: TC40_TEKeyValue_Client; arry: U_StringArray) of object;
{$IFDEF FPC}
  TC40_TEKeyValue_Client_GetTextKey_P = procedure(sender: TC40_TEKeyValue_Client; arry: U_StringArray) is nested;
{$ELSE FPC}
  TC40_TEKeyValue_Client_GetTextKey_P = reference to procedure(sender: TC40_TEKeyValue_Client; arry: U_StringArray);
{$ENDIF FPC}

  TC40_TEKeyValue_Client_GetTextKey = class(TOnResultBridge)
  public
    Client: TC40_TEKeyValue_Client;
    OnResultC: TC40_TEKeyValue_Client_GetTextKey_C;
    OnResultM: TC40_TEKeyValue_Client_GetTextKey_M;
    OnResultP: TC40_TEKeyValue_Client_GetTextKey_P;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDFE); override;
  end;

  TC40_TEKeyValue_Client_GetKeyValue_C = procedure(sender: TC40_TEKeyValue_Client; L: THashVariantList);
  TC40_TEKeyValue_Client_GetKeyValue_M = procedure(sender: TC40_TEKeyValue_Client; L: THashVariantList) of object;
{$IFDEF FPC}
  TC40_TEKeyValue_Client_GetKeyValue_P = procedure(sender: TC40_TEKeyValue_Client; L: THashVariantList) is nested;
{$ELSE FPC}
  TC40_TEKeyValue_Client_GetKeyValue_P = reference to procedure(sender: TC40_TEKeyValue_Client; L: THashVariantList);
{$ENDIF FPC}

  TC40_TEKeyValue_Client_GetKeyValue = class(TOnResultBridge)
  public
    Client: TC40_TEKeyValue_Client;
    OnResultC: TC40_TEKeyValue_Client_GetKeyValue_C;
    OnResultM: TC40_TEKeyValue_Client_GetKeyValue_M;
    OnResultP: TC40_TEKeyValue_Client_GetKeyValue_P;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDFE); override;
  end;

  TC40_TEKeyValue_Client_GetTextKeyValue_C = procedure(sender: TC40_TEKeyValue_Client; L: THashStringList);
  TC40_TEKeyValue_Client_GetTextKeyValue_M = procedure(sender: TC40_TEKeyValue_Client; L: THashStringList) of object;
{$IFDEF FPC}
  TC40_TEKeyValue_Client_GetTextKeyValue_P = procedure(sender: TC40_TEKeyValue_Client; L: THashStringList) is nested;
{$ELSE FPC}
  TC40_TEKeyValue_Client_GetTextKeyValue_P = reference to procedure(sender: TC40_TEKeyValue_Client; L: THashStringList);
{$ENDIF FPC}

  TC40_TEKeyValue_Client_GetTextKeyValue = class(TOnResultBridge)
  public
    Client: TC40_TEKeyValue_Client;
    OnResultC: TC40_TEKeyValue_Client_GetTextKeyValue_C;
    OnResultM: TC40_TEKeyValue_Client_GetTextKeyValue_M;
    OnResultP: TC40_TEKeyValue_Client_GetTextKeyValue_P;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDFE); override;
  end;

  TC40_TEKeyValue_Client_ExistsSection_C = procedure(sender: TC40_TEKeyValue_Client; state_: Boolean);
  TC40_TEKeyValue_Client_ExistsSection_M = procedure(sender: TC40_TEKeyValue_Client; state_: Boolean) of object;
{$IFDEF FPC}
  TC40_TEKeyValue_Client_ExistsSection_P = procedure(sender: TC40_TEKeyValue_Client; state_: Boolean) is nested;
{$ELSE FPC}
  TC40_TEKeyValue_Client_ExistsSection_P = reference to procedure(sender: TC40_TEKeyValue_Client; state_: Boolean);
{$ENDIF FPC}

  TC40_TEKeyValue_Client_ExistsSection = class(TOnResultBridge)
  public
    Client: TC40_TEKeyValue_Client;
    OnResultC: TC40_TEKeyValue_Client_ExistsSection_C;
    OnResultM: TC40_TEKeyValue_Client_ExistsSection_M;
    OnResultP: TC40_TEKeyValue_Client_ExistsSection_P;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDFE); override;
  end;

  TC40_TEKeyValue_Client_ExistsKey_C = procedure(sender: TC40_TEKeyValue_Client; state_: Boolean);
  TC40_TEKeyValue_Client_ExistsKey_M = procedure(sender: TC40_TEKeyValue_Client; state_: Boolean) of object;
{$IFDEF FPC}
  TC40_TEKeyValue_Client_ExistsKey_P = procedure(sender: TC40_TEKeyValue_Client; state_: Boolean) is nested;
{$ELSE FPC}
  TC40_TEKeyValue_Client_ExistsKey_P = reference to procedure(sender: TC40_TEKeyValue_Client; state_: Boolean);
{$ENDIF FPC}

  TC40_TEKeyValue_Client_ExistsKey = class(TOnResultBridge)
  public
    Client: TC40_TEKeyValue_Client;
    OnResultC: TC40_TEKeyValue_Client_ExistsKey_C;
    OnResultM: TC40_TEKeyValue_Client_ExistsKey_M;
    OnResultP: TC40_TEKeyValue_Client_ExistsKey_P;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDFE); override;
  end;

  TC40_TEKeyValue_Client_GetValue_C = procedure(sender: TC40_TEKeyValue_Client; Value_: Variant);
  TC40_TEKeyValue_Client_GetValue_M = procedure(sender: TC40_TEKeyValue_Client; Value_: Variant) of object;
{$IFDEF FPC}
  TC40_TEKeyValue_Client_GetValue_P = procedure(sender: TC40_TEKeyValue_Client; Value_: Variant) is nested;
{$ELSE FPC}
  TC40_TEKeyValue_Client_GetValue_P = reference to procedure(sender: TC40_TEKeyValue_Client; Value_: Variant);
{$ENDIF FPC}

  TC40_TEKeyValue_Client_GetValue = class(TOnResultBridge)
  public
    Client: TC40_TEKeyValue_Client;
    OnResultC: TC40_TEKeyValue_Client_GetValue_C;
    OnResultM: TC40_TEKeyValue_Client_GetValue_M;
    OnResultP: TC40_TEKeyValue_Client_GetValue_P;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDFE); override;
  end;

  TC40_TEKeyValue_Client_GetTextValue_C = procedure(sender: TC40_TEKeyValue_Client; Value_: U_String);
  TC40_TEKeyValue_Client_GetTextValue_M = procedure(sender: TC40_TEKeyValue_Client; Value_: U_String) of object;
{$IFDEF FPC}
  TC40_TEKeyValue_Client_GetTextValue_P = procedure(sender: TC40_TEKeyValue_Client; Value_: U_String) is nested;
{$ELSE FPC}
  TC40_TEKeyValue_Client_GetTextValue_P = reference to procedure(sender: TC40_TEKeyValue_Client; Value_: U_String);
{$ENDIF FPC}

  TC40_TEKeyValue_Client_GetTextValue = class(TOnResultBridge)
  public
    Client: TC40_TEKeyValue_Client;
    OnResultC: TC40_TEKeyValue_Client_GetTextValue_C;
    OnResultM: TC40_TEKeyValue_Client_GetTextValue_M;
    OnResultP: TC40_TEKeyValue_Client_GetTextValue_P;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDFE); override;
  end;

  TC40_TEKeyValue_Client_SearchTE_Result = record
    match_num: Integer;
    name: SystemString;
    Instance_: THashTextEngine;
  end;

  TC40_TEKeyValue_Client_SearchTE_Result_Array = array of TC40_TEKeyValue_Client_SearchTE_Result;

  TC40_TEKeyValue_Client_SearchTE_C = procedure(sender: TC40_TEKeyValue_Client; arry: TC40_TEKeyValue_Client_SearchTE_Result_Array);
  TC40_TEKeyValue_Client_SearchTE_M = procedure(sender: TC40_TEKeyValue_Client; arry: TC40_TEKeyValue_Client_SearchTE_Result_Array) of object;
{$IFDEF FPC}
  TC40_TEKeyValue_Client_SearchTE_P = procedure(sender: TC40_TEKeyValue_Client; arry: TC40_TEKeyValue_Client_SearchTE_Result_Array) is nested;
{$ELSE FPC}
  TC40_TEKeyValue_Client_SearchTE_P = reference to procedure(sender: TC40_TEKeyValue_Client; arry: TC40_TEKeyValue_Client_SearchTE_Result_Array);
{$ENDIF FPC}

  TC40_TEKeyValue_Client_SearchTE = class(TOnResultBridge)
  public
    Client: TC40_TEKeyValue_Client;
    OnResultC: TC40_TEKeyValue_Client_SearchTE_C;
    OnResultM: TC40_TEKeyValue_Client_SearchTE_M;
    OnResultP: TC40_TEKeyValue_Client_SearchTE_P;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDFE); override;
  end;
{$ENDREGION 'bridge_define'}

  TC40_TEKeyValue_Client = class(TC40_Base_NoAuth_Client)
  public
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;

    procedure Rebuild(TEName_: U_String);
    //
    procedure GetTE_Bridge(TEName_: U_String; Bridge_IO_: TPeerIO);
    procedure GetTE_C(TEName_: U_String; OnResult: TC40_TEKeyValue_Client_GetTE_C);
    procedure GetTE_M(TEName_: U_String; OnResult: TC40_TEKeyValue_Client_GetTE_M);
    procedure GetTE_P(TEName_: U_String; OnResult: TC40_TEKeyValue_Client_GetTE_P);
    //
    procedure SetTE(TEName_: U_String; TE: THashTextEngine);
    //
    procedure MergeTE(TEName_: U_String; TE: THashTextEngine);
    //
    procedure RemoveTE(TEName_: U_String);
    //
    procedure GetSection_Bridge(TEName_: U_String; Bridge_IO_: TPeerIO);
    procedure GetSection_C(TEName_: U_String; OnResult: TC40_TEKeyValue_Client_GetSection_C);
    procedure GetSection_M(TEName_: U_String; OnResult: TC40_TEKeyValue_Client_GetSection_M);
    procedure GetSection_P(TEName_: U_String; OnResult: TC40_TEKeyValue_Client_GetSection_P);
    //
    procedure GetKey_Bridge(TEName_, Section_: U_String; Bridge_IO_: TPeerIO);
    procedure GetKey_C(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetKey_C);
    procedure GetKey_M(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetKey_M);
    procedure GetKey_P(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetKey_P);
    //
    procedure GetTextKey_Bridge(TEName_, Section_: U_String; Bridge_IO_: TPeerIO);
    procedure GetTextKey_C(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetTextKey_C);
    procedure GetTextKey_M(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetTextKey_M);
    procedure GetTextKey_P(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetTextKey_P);
    //
    procedure GetKeyValue_Bridge(TEName_, Section_: U_String; Bridge_IO_: TPeerIO);
    procedure GetKeyValue_C(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetKeyValue_C);
    procedure GetKeyValue_M(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetKeyValue_M);
    procedure GetKeyValue_P(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetKeyValue_P);
    //
    procedure GetTextKeyValue_Bridge(TEName_, Section_: U_String; Bridge_IO_: TPeerIO);
    procedure GetTextKeyValue_C(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetTextKeyValue_C);
    procedure GetTextKeyValue_M(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetTextKeyValue_M);
    procedure GetTextKeyValue_P(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetTextKeyValue_P);
    //
    procedure ExistsSection_Bridge(TEName_, Section_: U_String; Bridge_IO_: TPeerIO);
    procedure ExistsSection_C(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_ExistsSection_C);
    procedure ExistsSection_M(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_ExistsSection_M);
    procedure ExistsSection_P(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_ExistsSection_P);
    //
    procedure ExistsKey_Bridge(TEName_, Section_, Key_: U_String; Bridge_IO_: TPeerIO);
    procedure ExistsKey_C(TEName_, Section_, Key_: U_String; OnResult: TC40_TEKeyValue_Client_ExistsKey_C);
    procedure ExistsKey_M(TEName_, Section_, Key_: U_String; OnResult: TC40_TEKeyValue_Client_ExistsKey_M);
    procedure ExistsKey_P(TEName_, Section_, Key_: U_String; OnResult: TC40_TEKeyValue_Client_ExistsKey_P);
    //
    procedure RemoveSection(TEName_, Section_: U_String);
    //
    procedure RemoveKey(TEName_, Section_, Key_: U_String);
    //
    procedure GetValue_Bridge(TEName_, Section_, Key_: U_String; Default_: Variant; Bridge_IO_: TPeerIO);
    procedure GetValue_C(TEName_, Section_, Key_: U_String; Default_: Variant; OnResult: TC40_TEKeyValue_Client_GetValue_C);
    procedure GetValue_M(TEName_, Section_, Key_: U_String; Default_: Variant; OnResult: TC40_TEKeyValue_Client_GetValue_M);
    procedure GetValue_P(TEName_, Section_, Key_: U_String; Default_: Variant; OnResult: TC40_TEKeyValue_Client_GetValue_P);
    //
    procedure GetTextValue_Bridge(TEName_, Section_, Key_, Default_: U_String; Bridge_IO_: TPeerIO);
    procedure GetTextValue_C(TEName_, Section_, Key_, Default_: U_String; OnResult: TC40_TEKeyValue_Client_GetTextValue_C);
    procedure GetTextValue_M(TEName_, Section_, Key_, Default_: U_String; OnResult: TC40_TEKeyValue_Client_GetTextValue_M);
    procedure GetTextValue_P(TEName_, Section_, Key_, Default_: U_String; OnResult: TC40_TEKeyValue_Client_GetTextValue_P);
    //
    procedure SetValue(TEName_, Section_, Key_: U_String; V_: Variant);
    //
    procedure SetTextValue(TEName_, Section_, Key_, V_: U_String);
    // admin
    procedure SearchTE_Bridge(filter_, search_: U_String; search_word_: Boolean; MaxNum_: Integer; Bridge_IO_: TPeerIO);
    procedure SearchTE_C(filter_, search_: U_String; search_word_: Boolean; MaxNum_: Integer; OnResult: TC40_TEKeyValue_Client_SearchTE_C);
    procedure SearchTE_M(filter_, search_: U_String; search_word_: Boolean; MaxNum_: Integer; OnResult: TC40_TEKeyValue_Client_SearchTE_M);
    procedure SearchTE_P(filter_, search_: U_String; search_word_: Boolean; MaxNum_: Integer; OnResult: TC40_TEKeyValue_Client_SearchTE_P);
  end;

  TC40_TEKeyValue_Client_List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TC40_TEKeyValue_Client>;

implementation

const
  C_Main = '___Main___';
  C_Name = '___Name___';

procedure TC40_TEKeyValue_Service.cmd_Rebuild(sender: TPeerIO; InData: TDFE);
var
  TEName_: U_String;
  TE: TZDB2_HashTextEngine;
begin
  TEName_ := InData.R.ReadString;
  TE := GetOrCreateTE(TEName_);
  TE.Data.Rebuild;
end;

procedure TC40_TEKeyValue_Service.cmd_GetTE(sender: TPeerIO; InData, OutData: TDFE);
var
  TEName_: U_String;
begin
  TEName_ := InData.R.ReadString;
  OutData.WriteTextSection(GetOrCreateTE(TEName_).Data);
end;

procedure TC40_TEKeyValue_Service.cmd_SetTE(sender: TPeerIO; InData: TDFE);
var
  TEName_: U_String;
  TE: TZDB2_HashTextEngine;
begin
  TEName_ := InData.R.ReadString;
  TE := GetOrCreateTE(TEName_);
  TE.Data.Clear;
  InData.R.ReadTextSection(TE.Data);
  TE.Data.SetDefaultText(C_Main, C_Name, TEName_);
  TE.Save;
end;

procedure TC40_TEKeyValue_Service.cmd_MergeTE(sender: TPeerIO; InData: TDFE);
var
  TEName_: U_String;
  TE: TZDB2_HashTextEngine;
  tmp: THashTextEngine;
begin
  TEName_ := InData.R.ReadString;
  TE := GetOrCreateTE(TEName_);
  tmp := THashTextEngine.Create;
  InData.R.ReadTextSection(tmp);
  TE.Data.Merge(tmp);
  disposeObject(tmp);
  TE.Data.SetDefaultText(C_Main, C_Name, TEName_);
  TE.Save;
end;

procedure TC40_TEKeyValue_Service.cmd_RemoveTE(sender: TPeerIO; InData: TDFE);
var
  TEName_: U_String;
  TE: TZDB2_HashTextEngine;
begin
  TEName_ := InData.R.ReadString;
  TE := GetOrCreateTE(TEName_);
  TEKeyValue_Hash.Delete(TEName_);
  TEKeyValue_DB.Remove(TE, True);
end;

procedure TC40_TEKeyValue_Service.cmd_GetSection(sender: TPeerIO; InData, OutData: TDFE);
var
  TEName_: U_String;
  TE: TZDB2_HashTextEngine;
  L: TListPascalString;
  i: Integer;
begin
  TEName_ := InData.R.ReadString;
  TE := GetOrCreateTE(TEName_);
  L := TListPascalString.Create;
  TE.Data.GetSectionList(L);
  for i := 0 to L.Count - 1 do
    if not L[i].Same(C_Main) then
        OutData.WriteString(L[i]);
  disposeObject(L);
end;

procedure TC40_TEKeyValue_Service.cmd_GetKey(sender: TPeerIO; InData, OutData: TDFE);
var
  TEName_, Section_: U_String;
  TE: TZDB2_HashTextEngine;
  VL: THashVariantList;
  L: TListPascalString;
  i: Integer;
begin
  TEName_ := InData.R.ReadString;
  Section_ := InData.R.ReadString;
  TE := GetOrCreateTE(TEName_);
  VL := TE.Data.VariantList[Section_];
  L := TListPascalString.Create;
  VL.GetNameList(L);
  for i := 0 to L.Count - 1 do
      OutData.WriteString(L[i]);
  disposeObject(L);
end;

procedure TC40_TEKeyValue_Service.cmd_GetTextKey(sender: TPeerIO; InData, OutData: TDFE);
var
  TEName_, Section_: U_String;
  TE: TZDB2_HashTextEngine;
  HS: THashStringList;
  L: TListPascalString;
  i: Integer;
begin
  TEName_ := InData.R.ReadString;
  Section_ := InData.R.ReadString;
  TE := GetOrCreateTE(TEName_);
  HS := TE.Data.HStringList[Section_];
  L := TListPascalString.Create;
  HS.GetNameList(L);
  for i := 0 to L.Count - 1 do
      OutData.WriteString(L[i]);
  disposeObject(L);
end;

procedure TC40_TEKeyValue_Service.cmd_GetKeyValue(sender: TPeerIO; InData, OutData: TDFE);
var
  TEName_, Section_: U_String;
  TE: TZDB2_HashTextEngine;
  VL: THashVariantList;
begin
  TEName_ := InData.R.ReadString;
  Section_ := InData.R.ReadString;
  TE := GetOrCreateTE(TEName_);
  VL := TE.Data.VariantList[Section_];
  OutData.WriteVariantList(VL);
end;

procedure TC40_TEKeyValue_Service.cmd_GetTextKeyValue(sender: TPeerIO; InData, OutData: TDFE);
var
  TEName_, Section_: U_String;
  TE: TZDB2_HashTextEngine;
  HS: THashStringList;
begin
  TEName_ := InData.R.ReadString;
  Section_ := InData.R.ReadString;
  TE := GetOrCreateTE(TEName_);
  HS := TE.Data.HStringList[Section_];
  OutData.WriteHashStringList(HS);
end;

procedure TC40_TEKeyValue_Service.cmd_ExistsSection(sender: TPeerIO; InData, OutData: TDFE);
var
  TEName_, Section_: U_String;
  TE: TZDB2_HashTextEngine;
begin
  TEName_ := InData.R.ReadString;
  Section_ := InData.R.ReadString;
  TE := GetOrCreateTE(TEName_);
  OutData.WriteBool(TE.Data.Exists(Section_));
end;

procedure TC40_TEKeyValue_Service.cmd_ExistsKey(sender: TPeerIO; InData, OutData: TDFE);
var
  TEName_, Section_, Key_: U_String;
  TE: TZDB2_HashTextEngine;
begin
  TEName_ := InData.R.ReadString;
  Section_ := InData.R.ReadString;
  Key_ := InData.R.ReadString;
  TE := GetOrCreateTE(TEName_);
  OutData.WriteBool(TE.Data.ExistsKey(Section_, Key_));
end;

procedure TC40_TEKeyValue_Service.cmd_RemoveSection(sender: TPeerIO; InData: TDFE);
var
  TEName_, Section_: U_String;
  TE: TZDB2_HashTextEngine;
begin
  TEName_ := InData.R.ReadString;
  Section_ := InData.R.ReadString;
  TE := GetOrCreateTE(TEName_);
  TE.Data.Delete(Section_);
end;

procedure TC40_TEKeyValue_Service.cmd_RemoveKey(sender: TPeerIO; InData: TDFE);
var
  TEName_, Section_, Key_: U_String;
  TE: TZDB2_HashTextEngine;
begin
  TEName_ := InData.R.ReadString;
  Section_ := InData.R.ReadString;
  Key_ := InData.R.ReadString;
  TE := GetOrCreateTE(TEName_);
  TE.Data.DeleteKey(Section_, Key_);
end;

procedure TC40_TEKeyValue_Service.cmd_GetValue(sender: TPeerIO; InData, OutData: TDFE);
var
  TEName_, Section_, Key_: U_String;
  Default_: Variant;
  TE: TZDB2_HashTextEngine;
begin
  TEName_ := InData.R.ReadString;
  Section_ := InData.R.ReadString;
  Key_ := InData.R.ReadString;
  Default_ := InData.R.ReadVariant;
  TE := GetOrCreateTE(TEName_);
  OutData.WriteVariant(TE.Data.GetDefaultValue(Section_, Key_, Default_));
end;

procedure TC40_TEKeyValue_Service.cmd_GetTextValue(sender: TPeerIO; InData, OutData: TDFE);
var
  TEName_, Section_, Key_: U_String;
  Default_: U_String;
  TE: TZDB2_HashTextEngine;
begin
  TEName_ := InData.R.ReadString;
  Section_ := InData.R.ReadString;
  Key_ := InData.R.ReadString;
  Default_ := InData.R.ReadString;
  TE := GetOrCreateTE(TEName_);
  OutData.WriteString(TE.Data.GetDefaultText(Section_, Key_, Default_));
end;

procedure TC40_TEKeyValue_Service.cmd_SetValue(sender: TPeerIO; InData: TDFE);
var
  TEName_, Section_, Key_: U_String;
  V_: Variant;
  TE: TZDB2_HashTextEngine;
begin
  TEName_ := InData.R.ReadString;
  Section_ := InData.R.ReadString;
  Key_ := InData.R.ReadString;
  V_ := InData.R.ReadVariant;
  TE := GetOrCreateTE(TEName_);
  TE.Data.SetDefaultValue(Section_, Key_, V_);
end;

procedure TC40_TEKeyValue_Service.cmd_SetTextValue(sender: TPeerIO; InData: TDFE);
var
  TEName_, Section_, Key_: U_String;
  V_: U_String;
  TE: TZDB2_HashTextEngine;
begin
  TEName_ := InData.R.ReadString;
  Section_ := InData.R.ReadString;
  Key_ := InData.R.ReadString;
  V_ := InData.R.ReadString;
  TE := GetOrCreateTE(TEName_);
  TE.Data.SetDefaultText(Section_, Key_, V_);
end;

procedure TC40_TEKeyValue_Service.cmd_SearchTE(sender: TPeerIO; InData, OutData: TDFE);
var
  filter_, search_: U_String;
  search_word_: Boolean;
  MaxNum_: Integer;
{$IFDEF FPC}
  procedure fpc_progress_(const Name_: PSystemString; Obj_: TZDB2_HashTextEngine);
  var
    Data_is_Null_: Boolean;
    tmp: TListPascalString;
    R: Integer;
  begin
    if ((MaxNum_ <= 0) or (OutData.Count div 3 < MaxNum_)) and umlSearchMatch(filter_, Name_^) then
      begin
        Data_is_Null_ := Obj_.Data_Direct = nil;
        tmp := TListPascalString.Create;
        Obj_.Data.DataExport(tmp);
        if Data_is_Null_ then
            Obj_.RecycleMemory;

        if (search_.L = 0) then
            R := 1
        else
            R := umlReplaceSum(tmp.AsText, search_, search_word_, True, 0, 0, nil);
        if R > 0 then
          begin
            OutData.WriteInteger(R);
            OutData.WriteString(Name_^);
            OutData.WritePascalStrings(tmp);
          end;
        disposeObject(tmp);
      end;
  end;
{$ENDIF FPC}


begin
  filter_ := InData.R.ReadString;
  search_ := InData.R.ReadString;
  search_word_ := InData.R.ReadBool;
  MaxNum_ := InData.R.ReadInteger;
{$IFDEF FPC}
  TEKeyValue_Hash.ProgressP(@fpc_progress_);
{$ELSE FPC}
  TEKeyValue_Hash.ProgressP(procedure(const Name_: PSystemString; Obj_: TZDB2_HashTextEngine)
    var
      Data_is_Null_: Boolean;
      tmp: TListPascalString;
      R: Integer;
    begin
      if ((MaxNum_ <= 0) or (OutData.Count div 3 < MaxNum_)) and umlSearchMatch(filter_, Name_^) then
        begin
          Data_is_Null_ := Obj_.Data_Direct = nil;
          tmp := TListPascalString.Create;
          Obj_.Data.DataExport(tmp);
          if Data_is_Null_ then
              Obj_.RecycleMemory;

          if (search_.L = 0) then
              R := 1
          else
              R := umlReplaceSum(tmp.AsText, search_, search_word_, True, 0, 0, nil);
          if R > 0 then
            begin
              OutData.WriteInteger(R);
              OutData.WriteString(Name_^);
              OutData.WritePascalStrings(tmp);
            end;
          disposeObject(tmp);
        end;
    end);
{$ENDIF FPC}
end;

constructor TC40_TEKeyValue_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
var
  fs: TCore_Stream;
  TE: TZDB2_HashTextEngine;
  TEName_: SystemString;
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  // only instance
  ServiceInfo.OnlyInstance := True;
  UpdateToGlobalDispatch;
  ParamList.SetDefaultValue('OnlyInstance', if_(ServiceInfo.OnlyInstance, 'True', 'False'));

  // cmd
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Rebuild').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Rebuild;
  DTNoAuthService.RecvTunnel.RegisterStream('GetTE').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_GetTE;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('SetTE').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_SetTE;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('MergeTE').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_MergeTE;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('RemoveTE').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RemoveTE;
  DTNoAuthService.RecvTunnel.RegisterStream('GetSection').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_GetSection;
  DTNoAuthService.RecvTunnel.RegisterStream('GetKey').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_GetKey;
  DTNoAuthService.RecvTunnel.RegisterStream('GetTextKey').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_GetTextKey;
  DTNoAuthService.RecvTunnel.RegisterStream('GetKeyValue').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_GetKeyValue;
  DTNoAuthService.RecvTunnel.RegisterStream('GetTextKeyValue').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_GetTextKeyValue;
  DTNoAuthService.RecvTunnel.RegisterStream('ExistsSection').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_ExistsSection;
  DTNoAuthService.RecvTunnel.RegisterStream('ExistsKey').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_ExistsKey;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('RemoveSection').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RemoveSection;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('RemoveKey').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RemoveKey;
  DTNoAuthService.RecvTunnel.RegisterStream('GetValue').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_GetValue;
  DTNoAuthService.RecvTunnel.RegisterStream('GetTextValue').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_GetTextValue;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('SetValue').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_SetValue;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('SetTextValue').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_SetTextValue;
  DTNoAuthService.RecvTunnel.RegisterStream('SearchTE').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_SearchTE;

  // init DB
  ZDB2RecycleMemoryTimeOut := EStrToInt64(ParamList.GetDefaultValue('RecycleMemory', '30*1000'), 30 * 1000);
  ZDB2DeltaSpace := EStrToInt64(ParamList.GetDefaultValue('DeltaSpace', '64*1024*1024'), 64 * 1024 * 1024);
  ZDB2BlockSize := EStrToInt(ParamList.GetDefaultValue('BlockSize', '1024'), 1024);
  ZDB2EnabledCipher := EStrToBool(ParamList.GetDefaultValue('EnabledCipher', 'True'), True);
  ZDB2CipherName := ParamList.GetDefaultValue('Cipher', TCipher.CCipherSecurityName[TCipherSecurity.csRijndael]);
  ZDB2Password := ParamList.GetDefaultValue('Password', Z.Net.C4.C40_Password);

  if ZDB2EnabledCipher then
      ZDB2Cipher := TZDB2_Cipher.Create(ZDB2CipherName, ZDB2Password, 1, True, True)
  else
      ZDB2Cipher := nil;
  C40_TEKeyValue_DB_FileName := umlCombineFileName(DTNoAuthService.PublicFileDirectory, PFormat('DTC40_%s.Space', [ServiceInfo.ServiceTyp.Text]));

  if EStrToBool(ParamList.GetDefaultValue('ForeverSave', 'True'), True) and umlFileExists(C40_TEKeyValue_DB_FileName) then
      fs := TCore_FileStream.Create(C40_TEKeyValue_DB_FileName, fmOpenReadWrite)
  else
      fs := TCore_FileStream.Create(C40_TEKeyValue_DB_FileName, fmCreate);

  TEKeyValue_DB := TZDB2_List_HashTextEngine.Create(
    TZDB2_HashTextEngine,
    nil,
    ZDB2RecycleMemoryTimeOut,
    fs,
    False,
    ZDB2DeltaSpace,
    ZDB2BlockSize,
    ZDB2Cipher);
  TEKeyValue_DB.AutoFreeStream := True;

  TEKeyValue_Hash := TC40_TEKeyValue_Service_Hash_Pool.Create(False,
    EStrToInt64(ParamList.GetDefaultValue('Identifier_HashPool', '1024*1024'), 1024 * 1024),
    nil);
  TEKeyValue_Hash.AccessOptimization := True;
  TEKeyValue_Hash.IgnoreCase := True;

  if not C40_QuietMode then
      DoStatus('extract Text Engine Database.');
  if TEKeyValue_DB.Count > 0 then
    with TEKeyValue_DB.Repeat_ do
      repeat
        TE := Queue^.Data;
        TEName_ := GetTEName(TE);
        if TEKeyValue_Hash.Exists(TEName_) then
          begin
            DoStatus('repeat Text engine %s', [TEName_]);
            TEKeyValue_Hash.Add(TEName_, TE);
          end
        else
            TEKeyValue_Hash.FastAdd(TEName_, TE);
        TE.RecycleMemory;
      until not Next;
  TEKeyValue_DB.Flush;
  if not C40_QuietMode then
      DoStatus('extract Text Engine Database done.');
end;

destructor TC40_TEKeyValue_Service.Destroy;
begin
  TEKeyValue_DB.Flush;
  DisposeObjectAndNil(TEKeyValue_DB);
  DisposeObjectAndNil(ZDB2Cipher);
  inherited Destroy;
end;

procedure TC40_TEKeyValue_Service.SafeCheck;
begin
  inherited SafeCheck;
  TEKeyValue_DB.Flush;
end;

procedure TC40_TEKeyValue_Service.Progress;
begin
  inherited Progress;
  TEKeyValue_DB.Progress;
end;

function TC40_TEKeyValue_Service.GetOrCreateTE(TEName_: SystemString): TZDB2_HashTextEngine;
begin
  Result := TEKeyValue_Hash[TEName_];
  if Result = nil then
    begin
      Result := TEKeyValue_DB.NewData;
      Result.Data.SetDefaultText(C_Main, C_Name, TEName_);
      TEKeyValue_Hash.FastAdd(TEName_, Result);
    end;
end;

function TC40_TEKeyValue_Service.GetTEName(TE: TZDB2_HashTextEngine): SystemString;
begin
  Result := TE.Data.GetDefaultText(C_Main, C_Name, '');
end;

constructor TC40_TEKeyValue_Client_GetTE.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_TEKeyValue_Client_GetTE.DoStreamEvent(sender: TPeerIO; Result_: TDFE);
var
  TE: THashTextEngine;
begin
  TE := THashTextEngine.Create;
  Result_.R.ReadTextSection(TE);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, TE);
    if Assigned(OnResultM) then
        OnResultM(Client, TE);
    if Assigned(OnResultP) then
        OnResultP(Client, TE);
  except
  end;
  DelayFreeObject(1.0, self, TE);
end;

constructor TC40_TEKeyValue_Client_GetSection.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_TEKeyValue_Client_GetSection.DoStreamEvent(sender: TPeerIO; Result_: TDFE);
var
  arry: U_StringArray;
  i: Integer;
begin
  SetLength(arry, Result_.Count);
  i := 0;
  while Result_.R.NotEnd do
    begin
      arry[i] := Result_.R.ReadString;
      inc(i);
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, arry);
  except
  end;
  SetLength(arry, 0);
  DelayFreeObject(1.0, self);
end;

constructor TC40_TEKeyValue_Client_GetKey.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_TEKeyValue_Client_GetKey.DoStreamEvent(sender: TPeerIO; Result_: TDFE);
var
  arry: U_StringArray;
  i: Integer;
begin
  SetLength(arry, Result_.Count);
  i := 0;
  while Result_.R.NotEnd do
    begin
      arry[i] := Result_.R.ReadString;
      inc(i);
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, arry);
  except
  end;
  SetLength(arry, 0);
  DelayFreeObject(1.0, self);
end;

constructor TC40_TEKeyValue_Client_GetTextKey.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_TEKeyValue_Client_GetTextKey.DoStreamEvent(sender: TPeerIO; Result_: TDFE);
var
  arry: U_StringArray;
  i: Integer;
begin
  SetLength(arry, Result_.Count);
  i := 0;
  while Result_.R.NotEnd do
    begin
      arry[i] := Result_.R.ReadString;
      inc(i);
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, arry);
  except
  end;
  SetLength(arry, 0);
  DelayFreeObject(1.0, self);
end;

constructor TC40_TEKeyValue_Client_GetKeyValue.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_TEKeyValue_Client_GetKeyValue.DoStreamEvent(sender: TPeerIO; Result_: TDFE);
var
  L: THashVariantList;
begin
  L := THashVariantList.Create;
  Result_.R.ReadVariantList(L);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, L);
    if Assigned(OnResultM) then
        OnResultM(Client, L);
    if Assigned(OnResultP) then
        OnResultP(Client, L);
  except
  end;
  DelayFreeObject(1.0, self, L);
end;

constructor TC40_TEKeyValue_Client_GetTextKeyValue.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_TEKeyValue_Client_GetTextKeyValue.DoStreamEvent(sender: TPeerIO; Result_: TDFE);
var
  L: THashStringList;
begin
  L := THashStringList.Create;
  Result_.R.ReadHashStringList(L);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, L);
    if Assigned(OnResultM) then
        OnResultM(Client, L);
    if Assigned(OnResultP) then
        OnResultP(Client, L);
  except
  end;
  DelayFreeObject(1.0, self, L);
end;

constructor TC40_TEKeyValue_Client_ExistsSection.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_TEKeyValue_Client_ExistsSection.DoStreamEvent(sender: TPeerIO; Result_: TDFE);
var
  state_: Boolean;
begin
  state_ := Result_.R.ReadBool;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, state_);
    if Assigned(OnResultM) then
        OnResultM(Client, state_);
    if Assigned(OnResultP) then
        OnResultP(Client, state_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_TEKeyValue_Client_ExistsKey.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_TEKeyValue_Client_ExistsKey.DoStreamEvent(sender: TPeerIO; Result_: TDFE);
var
  state_: Boolean;
begin
  state_ := Result_.R.ReadBool;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, state_);
    if Assigned(OnResultM) then
        OnResultM(Client, state_);
    if Assigned(OnResultP) then
        OnResultP(Client, state_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_TEKeyValue_Client_GetValue.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_TEKeyValue_Client_GetValue.DoStreamEvent(sender: TPeerIO; Result_: TDFE);
var
  Value_: Variant;
  i: Integer;
begin
  Value_ := Result_.R.ReadVariant;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Value_);
    if Assigned(OnResultM) then
        OnResultM(Client, Value_);
    if Assigned(OnResultP) then
        OnResultP(Client, Value_);
  except
  end;
  Value_ := NULL;
  DelayFreeObject(1.0, self);
end;

constructor TC40_TEKeyValue_Client_GetTextValue.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_TEKeyValue_Client_GetTextValue.DoStreamEvent(sender: TPeerIO; Result_: TDFE);
var
  Value_: U_String;
  i: Integer;
begin
  Value_ := Result_.R.ReadString;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Value_);
    if Assigned(OnResultM) then
        OnResultM(Client, Value_);
    if Assigned(OnResultP) then
        OnResultP(Client, Value_);
  except
  end;
  Value_ := '';
  DelayFreeObject(1.0, self);
end;

constructor TC40_TEKeyValue_Client_SearchTE.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_TEKeyValue_Client_SearchTE.DoStreamEvent(sender: TPeerIO; Result_: TDFE);
var
  arry: TC40_TEKeyValue_Client_SearchTE_Result_Array;
  i: Integer;
begin
  SetLength(arry, Result_.Count div 3);
  i := 0;
  while Result_.R.NotEnd do
    begin
      arry[i].match_num := Result_.R.ReadInteger;
      arry[i].name := Result_.R.ReadString;
      arry[i].Instance_ := THashTextEngine.Create;
      Result_.R.ReadTextSection(arry[i].Instance_);
      inc(i);
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, arry);
  except
  end;
  for i := low(arry) to high(arry) do
    begin
      arry[i].name := '';
      disposeObject(arry[i].Instance_);
    end;
  SetLength(arry, 0);
  DelayFreeObject(1.0, self);
end;

constructor TC40_TEKeyValue_Client.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
end;

destructor TC40_TEKeyValue_Client.Destroy;
begin
  inherited Destroy;
end;

procedure TC40_TEKeyValue_Client.Progress;
begin
  inherited Progress;
end;

procedure TC40_TEKeyValue_Client.Rebuild(TEName_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(TEName_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Rebuild', D);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetTE_Bridge(TEName_: U_String; Bridge_IO_: TPeerIO);
var
  tmp: TStreamEventBridge;
  D: TDFE;
begin
  tmp := TStreamEventBridge.Create(Bridge_IO_);

  D := TDFE.Create;
  D.WriteString(TEName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetTE', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetTE_C(TEName_: U_String; OnResult: TC40_TEKeyValue_Client_GetTE_C);
var
  tmp: TC40_TEKeyValue_Client_GetTE;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetTE.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetTE', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetTE_M(TEName_: U_String; OnResult: TC40_TEKeyValue_Client_GetTE_M);
var
  tmp: TC40_TEKeyValue_Client_GetTE;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetTE.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetTE', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetTE_P(TEName_: U_String; OnResult: TC40_TEKeyValue_Client_GetTE_P);
var
  tmp: TC40_TEKeyValue_Client_GetTE;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetTE.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetTE', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.SetTE(TEName_: U_String; TE: THashTextEngine);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteTextSection(TE);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('SetTE', D);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.MergeTE(TEName_: U_String; TE: THashTextEngine);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteTextSection(TE);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('MergeTE', D);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.RemoveTE(TEName_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(TEName_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('RemoveTE', D);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetSection_Bridge(TEName_: U_String; Bridge_IO_: TPeerIO);
var
  tmp: TStreamEventBridge;
  D: TDFE;
begin
  tmp := TStreamEventBridge.Create(Bridge_IO_);

  D := TDFE.Create;
  D.WriteString(TEName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetSection', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetSection_C(TEName_: U_String; OnResult: TC40_TEKeyValue_Client_GetSection_C);
var
  tmp: TC40_TEKeyValue_Client_GetSection;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetSection.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetSection', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetSection_M(TEName_: U_String; OnResult: TC40_TEKeyValue_Client_GetSection_M);
var
  tmp: TC40_TEKeyValue_Client_GetSection;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetSection.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetSection', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetSection_P(TEName_: U_String; OnResult: TC40_TEKeyValue_Client_GetSection_P);
var
  tmp: TC40_TEKeyValue_Client_GetSection;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetSection.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetSection', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetKey_Bridge(TEName_, Section_: U_String; Bridge_IO_: TPeerIO);
var
  tmp: TStreamEventBridge;
  D: TDFE;
begin
  tmp := TStreamEventBridge.Create(Bridge_IO_);

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetKey', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetKey_C(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetKey_C);
var
  tmp: TC40_TEKeyValue_Client_GetKey;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetKey.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetKey', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetKey_M(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetKey_M);
var
  tmp: TC40_TEKeyValue_Client_GetKey;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetKey.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetKey', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetKey_P(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetKey_P);
var
  tmp: TC40_TEKeyValue_Client_GetKey;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetKey.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetKey', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetTextKey_Bridge(TEName_, Section_: U_String; Bridge_IO_: TPeerIO);
var
  tmp: TStreamEventBridge;
  D: TDFE;
begin
  tmp := TStreamEventBridge.Create(Bridge_IO_);

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetTextKey', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetTextKey_C(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetTextKey_C);
var
  tmp: TC40_TEKeyValue_Client_GetTextKey;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetTextKey.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetTextKey', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetTextKey_M(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetTextKey_M);
var
  tmp: TC40_TEKeyValue_Client_GetTextKey;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetTextKey.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetTextKey', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetTextKey_P(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetTextKey_P);
var
  tmp: TC40_TEKeyValue_Client_GetTextKey;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetTextKey.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetTextKey', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetKeyValue_Bridge(TEName_, Section_: U_String; Bridge_IO_: TPeerIO);
var
  tmp: TStreamEventBridge;
  D: TDFE;
begin
  tmp := TStreamEventBridge.Create(Bridge_IO_);

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetKeyValue', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetKeyValue_C(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetKeyValue_C);
var
  tmp: TC40_TEKeyValue_Client_GetKeyValue;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetKeyValue.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetKeyValue', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetKeyValue_M(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetKeyValue_M);
var
  tmp: TC40_TEKeyValue_Client_GetKeyValue;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetKeyValue.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetKeyValue', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetKeyValue_P(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetKeyValue_P);
var
  tmp: TC40_TEKeyValue_Client_GetKeyValue;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetKeyValue.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetKeyValue', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetTextKeyValue_Bridge(TEName_, Section_: U_String; Bridge_IO_: TPeerIO);
var
  tmp: TStreamEventBridge;
  D: TDFE;
begin
  tmp := TStreamEventBridge.Create(Bridge_IO_);

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetTextKeyValue', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetTextKeyValue_C(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetTextKeyValue_C);
var
  tmp: TC40_TEKeyValue_Client_GetTextKeyValue;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetTextKeyValue.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetTextKeyValue', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetTextKeyValue_M(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetTextKeyValue_M);
var
  tmp: TC40_TEKeyValue_Client_GetTextKeyValue;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetTextKeyValue.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetTextKeyValue', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetTextKeyValue_P(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_GetTextKeyValue_P);
var
  tmp: TC40_TEKeyValue_Client_GetTextKeyValue;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetTextKeyValue.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetTextKeyValue', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.ExistsSection_Bridge(TEName_, Section_: U_String; Bridge_IO_: TPeerIO);
var
  tmp: TStreamEventBridge;
  D: TDFE;
begin
  tmp := TStreamEventBridge.Create(Bridge_IO_);

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('ExistsSection', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.ExistsSection_C(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_ExistsSection_C);
var
  tmp: TC40_TEKeyValue_Client_ExistsSection;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_ExistsSection.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('ExistsSection', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.ExistsSection_M(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_ExistsSection_M);
var
  tmp: TC40_TEKeyValue_Client_ExistsSection;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_ExistsSection.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('ExistsSection', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.ExistsSection_P(TEName_, Section_: U_String; OnResult: TC40_TEKeyValue_Client_ExistsSection_P);
var
  tmp: TC40_TEKeyValue_Client_ExistsSection;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_ExistsSection.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('ExistsSection', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.ExistsKey_Bridge(TEName_, Section_, Key_: U_String; Bridge_IO_: TPeerIO);
var
  tmp: TStreamEventBridge;
  D: TDFE;
begin
  tmp := TStreamEventBridge.Create(Bridge_IO_);

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  D.WriteString(Key_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('ExistsKey', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.ExistsKey_C(TEName_, Section_, Key_: U_String; OnResult: TC40_TEKeyValue_Client_ExistsKey_C);
var
  tmp: TC40_TEKeyValue_Client_ExistsKey;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_ExistsKey.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  D.WriteString(Key_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('ExistsKey', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.ExistsKey_M(TEName_, Section_, Key_: U_String; OnResult: TC40_TEKeyValue_Client_ExistsKey_M);
var
  tmp: TC40_TEKeyValue_Client_ExistsKey;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_ExistsKey.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  D.WriteString(Key_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('ExistsKey', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.ExistsKey_P(TEName_, Section_, Key_: U_String; OnResult: TC40_TEKeyValue_Client_ExistsKey_P);
var
  tmp: TC40_TEKeyValue_Client_ExistsKey;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_ExistsKey.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  D.WriteString(Key_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('ExistsKey', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.RemoveSection(TEName_, Section_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('RemoveSection', D);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.RemoveKey(TEName_, Section_, Key_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  D.WriteString(Key_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('RemoveKey', D);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetValue_Bridge(TEName_, Section_, Key_: U_String; Default_: Variant; Bridge_IO_: TPeerIO);
var
  tmp: TStreamEventBridge;
  D: TDFE;
begin
  tmp := TStreamEventBridge.Create(Bridge_IO_);

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  D.WriteString(Key_);
  D.WriteVariant(Default_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetValue', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetValue_C(TEName_, Section_, Key_: U_String; Default_: Variant; OnResult: TC40_TEKeyValue_Client_GetValue_C);
var
  tmp: TC40_TEKeyValue_Client_GetValue;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetValue.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  D.WriteString(Key_);
  D.WriteVariant(Default_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetValue', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetValue_M(TEName_, Section_, Key_: U_String; Default_: Variant; OnResult: TC40_TEKeyValue_Client_GetValue_M);
var
  tmp: TC40_TEKeyValue_Client_GetValue;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetValue.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  D.WriteString(Key_);
  D.WriteVariant(Default_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetValue', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetValue_P(TEName_, Section_, Key_: U_String; Default_: Variant; OnResult: TC40_TEKeyValue_Client_GetValue_P);
var
  tmp: TC40_TEKeyValue_Client_GetValue;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetValue.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  D.WriteString(Key_);
  D.WriteVariant(Default_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetValue', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetTextValue_Bridge(TEName_, Section_, Key_, Default_: U_String; Bridge_IO_: TPeerIO);
var
  tmp: TStreamEventBridge;
  D: TDFE;
begin
  tmp := TStreamEventBridge.Create(Bridge_IO_);

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  D.WriteString(Key_);
  D.WriteVariant(Default_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetValue', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetTextValue_C(TEName_, Section_, Key_, Default_: U_String; OnResult: TC40_TEKeyValue_Client_GetTextValue_C);
var
  tmp: TC40_TEKeyValue_Client_GetTextValue;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetTextValue.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  D.WriteString(Key_);
  D.WriteString(Default_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetTextValue', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetTextValue_M(TEName_, Section_, Key_, Default_: U_String; OnResult: TC40_TEKeyValue_Client_GetTextValue_M);
var
  tmp: TC40_TEKeyValue_Client_GetTextValue;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetTextValue.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  D.WriteString(Key_);
  D.WriteString(Default_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetTextValue', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.GetTextValue_P(TEName_, Section_, Key_, Default_: U_String; OnResult: TC40_TEKeyValue_Client_GetTextValue_P);
var
  tmp: TC40_TEKeyValue_Client_GetTextValue;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_GetTextValue.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  D.WriteString(Key_);
  D.WriteString(Default_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetTextValue', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.SetValue(TEName_, Section_, Key_: U_String; V_: Variant);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  D.WriteString(Key_);
  D.WriteVariant(V_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('SetValue', D);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.SetTextValue(TEName_, Section_, Key_, V_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(TEName_);
  D.WriteString(Section_);
  D.WriteString(Key_);
  D.WriteString(V_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('SetTextValue', D);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.SearchTE_Bridge(filter_, search_: U_String; search_word_: Boolean; MaxNum_: Integer; Bridge_IO_: TPeerIO);
var
  tmp: TStreamEventBridge;
  D: TDFE;
begin
  tmp := TStreamEventBridge.Create(Bridge_IO_);

  D := TDFE.Create;
  D.WriteString(filter_);
  D.WriteString(search_);
  D.WriteBool(search_word_);
  D.WriteInteger(MaxNum_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('SearchTE', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.SearchTE_C(filter_, search_: U_String; search_word_: Boolean; MaxNum_: Integer; OnResult: TC40_TEKeyValue_Client_SearchTE_C);
var
  tmp: TC40_TEKeyValue_Client_SearchTE;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_SearchTE.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(filter_);
  D.WriteString(search_);
  D.WriteBool(search_word_);
  D.WriteInteger(MaxNum_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('SearchTE', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.SearchTE_M(filter_, search_: U_String; search_word_: Boolean; MaxNum_: Integer; OnResult: TC40_TEKeyValue_Client_SearchTE_M);
var
  tmp: TC40_TEKeyValue_Client_SearchTE;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_SearchTE.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(filter_);
  D.WriteString(search_);
  D.WriteBool(search_word_);
  D.WriteInteger(MaxNum_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('SearchTE', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

procedure TC40_TEKeyValue_Client.SearchTE_P(filter_, search_: U_String; search_word_: Boolean; MaxNum_: Integer; OnResult: TC40_TEKeyValue_Client_SearchTE_P);
var
  tmp: TC40_TEKeyValue_Client_SearchTE;
  D: TDFE;
begin
  tmp := TC40_TEKeyValue_Client_SearchTE.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(filter_);
  D.WriteString(search_);
  D.WriteBool(search_word_);
  D.WriteInteger(MaxNum_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('SearchTE', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(D);
end;

initialization

RegisterC40('TEKeyValue', TC40_TEKeyValue_Service, TC40_TEKeyValue_Client);

end.
