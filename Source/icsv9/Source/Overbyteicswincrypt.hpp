// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Overbyteicswincrypt.pas' rev: 36.00 (Windows)

#ifndef OverbyteicswincryptHPP
#define OverbyteicswincryptHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <Sysinit.hpp>
#include <Winapi.Windows.hpp>

//-- user supplied -----------------------------------------------------------
#include <BCrypt.h>
#include <NCrypt.h>

namespace Overbyteicswincrypt
{
//-- forward type declarations -----------------------------------------------
struct _PROV_ENUMALGS;
struct _PROV_ENUMALGS_EX;
struct _CERT_FORTEZZA_DATA_PROP;
struct __NCRYPT_UI_POLICY_BLOB;
//-- type declarations -------------------------------------------------------
typedef _CMS_KEY_INFO TCmsKeyInfo;

typedef PCMS_KEY_INFO PCmsKeyInfo;

typedef _HMAC_Info THMacInfo;

typedef PHMAC_INFO PHMacInfo;

typedef _SCHANNEL_ALG TSChannelAlg;

typedef PSCHANNEL_ALG PSChannelAlg;

typedef _PROV_ENUMALGS *PPROV_ENUMALGS;

struct DECLSPEC_DRECORD _PROV_ENUMALGS
{
public:
	unsigned aiAlgid;
	unsigned dwBitLen;
	unsigned dwNameLen;
	System::StaticArray<char, 20> szName;
};


typedef _PROV_ENUMALGS PROV_ENUMALGS;

typedef _PROV_ENUMALGS TProvEnumAlgs;

typedef PPROV_ENUMALGS PProvEnumAlgs;

typedef _PROV_ENUMALGS_EX *PPROV_ENUMALGS_EX;

struct DECLSPEC_DRECORD _PROV_ENUMALGS_EX
{
public:
	unsigned aiAlgid;
	unsigned dwDefaultLen;
	unsigned dwMinLen;
	unsigned dwMaxLen;
	unsigned dwProtocols;
	unsigned dwNameLen;
	System::StaticArray<char, 20> szName;
	unsigned dwLongNameLen;
	System::StaticArray<char, 40> szLongName;
};


typedef _PROV_ENUMALGS_EX PROV_ENUMALGS_EX;

typedef _PROV_ENUMALGS_EX TProvEnumAlgsEx;

typedef PPROV_ENUMALGS_EX PProvEnumAlgsEx;

typedef _PUBLICKEYSTRUC TPublicKeyStruc;

typedef _RSAPUBKEY TRsaPubKey;

typedef _DSSSEED TDssSeed;

typedef _PUBKEYVER3 TPubKeyVer3;

typedef _PRIVKEYVER3 TDssPrivKey3;

typedef _KEY_TYPE_SUBTYPE TKeyTypeSubType;

typedef PKEY_TYPE_SUBTYPE PKeyTypeSubType;

typedef _CERT_FORTEZZA_DATA_PROP *PCERT_FORTEZZA_DATA_PROP;

struct DECLSPEC_DRECORD _CERT_FORTEZZA_DATA_PROP
{
public:
	System::StaticArray<System::Byte, 8> SerialNumber;
	int CertIndex;
	System::StaticArray<System::Byte, 36> CertLabel;
};


typedef _CERT_FORTEZZA_DATA_PROP CERT_FORTEZZA_DATA_PROP;

typedef _CERT_FORTEZZA_DATA_PROP TCertFortezzaDataProp;

typedef PCERT_FORTEZZA_DATA_PROP PCertFortezzaDataProp;

typedef _CRYPTOAPI_BLOB CRYPT_INTEGER_BLOB;

typedef _CRYPTOAPI_BLOB TCryptIntegerBlob;

typedef _CRYPTOAPI_BLOB *PCRYPT_INTEGER_BLOB;

typedef PCRYPT_INTEGER_BLOB PCryptIntegerBlob;

typedef _CRYPTOAPI_BLOB TCryptUintBlob;

typedef PCRYPT_UINT_BLOB PCryptUintBlob;

typedef _CRYPTOAPI_BLOB CRYPT_OBJID_BLOB;

typedef _CRYPTOAPI_BLOB TCryptObjIdBlob;

typedef _CRYPTOAPI_BLOB *PCRYPT_OBJID_BLOB;

typedef PCRYPT_OBJID_BLOB PCryptObjIdBlob;

typedef _CRYPTOAPI_BLOB CERT_NAME_BLOB;

typedef _CRYPTOAPI_BLOB TCertNameBlob;

typedef _CRYPTOAPI_BLOB *PCERT_NAME_BLOB;

typedef PCERT_NAME_BLOB PCertNameBlob;

typedef _CRYPTOAPI_BLOB CERT_RDN_VALUE_BLOB;

typedef _CRYPTOAPI_BLOB TCertRdnValueBlob;

typedef _CRYPTOAPI_BLOB *PCERT_RDN_VALUE_BLOB;

typedef PCERT_RDN_VALUE_BLOB PCertRdnValueBlob;

typedef _CRYPTOAPI_BLOB TCertBlob;

typedef PCERT_BLOB PCertBlob;

typedef _CRYPTOAPI_BLOB TCrlBlob;

typedef PCRL_BLOB PCrlBlob;

typedef _CRYPTOAPI_BLOB DATA_BLOB;

typedef _CRYPTOAPI_BLOB TDataBlob;

typedef _CRYPTOAPI_BLOB *PDATA_BLOB;

typedef PDATA_BLOB PDataBlob;

typedef _CRYPTOAPI_BLOB TCryptDataBlob;

typedef PCRYPT_DATA_BLOB PCryptDataBlob;

typedef _CRYPTOAPI_BLOB TCryptHashBlob;

typedef PCRYPT_HASH_BLOB PCryptHashBlob;

typedef _CRYPTOAPI_BLOB TCryptDigestBlob;

typedef PCRYPT_DIGEST_BLOB PCryptDigestBlob;

typedef _CRYPTOAPI_BLOB TCyptDerBlob;

typedef PCRYPT_DER_BLOB PCyptDerBlob;

typedef _CRYPTOAPI_BLOB CRYPT_ATTR_BLOB;

typedef _CRYPTOAPI_BLOB TCryptAttrBlob;

typedef _CRYPTOAPI_BLOB *PCRYPT_ATTR_BLOB;

typedef PCRYPT_ATTR_BLOB PCryptAttrBlob;

typedef _CMS_DH_KEY_INFO TCmsDhKeyInfo;

typedef PCMS_DH_KEY_INFO PCmsDhKeyInfo;

typedef _CRYPT_BIT_BLOB TCryptBitBlob;

typedef PCRYPT_BIT_BLOB PCryptBitBlob;

typedef _CRYPT_ALGORITHM_IDENTIFIER TCryptAlgorithmIdentifier;

typedef PCRYPT_ALGORITHM_IDENTIFIER PCryptAlgorithmIdentifier;

typedef _CRYPT_OBJID_TABLE TCryptObjIdTable;

typedef PCRYPT_OBJID_TABLE PCryptObjIdTable;

typedef _CRYPT_HASH_INFO TCryptHashInfo;

typedef PCRYPT_HASH_INFO PCryptHashInfo;

typedef _CERT_EXTENSION TCertExtension;

typedef PCERT_EXTENSION PCertExtension;

typedef _CRYPT_ATTRIBUTE_TYPE_VALUE TCryptAttributeTypeValue;

typedef PCRYPT_ATTRIBUTE_TYPE_VALUE PCryptAttributeTypeValue;

typedef _CRYPT_ATTRIBUTE TCryptAttribute;

typedef PCRYPT_ATTRIBUTE PCryptAttribute;

typedef _CRYPT_ATTRIBUTES TCryptAttributes;

typedef PCRYPT_ATTRIBUTES PCryptAttributes;

typedef _CERT_RDN_ATTR TCertRdnAttr;

typedef PCERT_RDN_ATTR PCertRdnAttr;

typedef _CERT_RDN TCertRdn;

typedef PCERT_RDN PCertRdn;

typedef _CERT_NAME_INFO TCertNameInfo;

typedef PCERT_NAME_INFO PCertNameInfo;

typedef _CERT_NAME_VALUE TCertNameValue;

typedef PCERT_NAME_VALUE PCertNameValue;

typedef _CERT_PUBLIC_KEY_INFO TCertPublicKeyInfo;

typedef PCERT_PUBLIC_KEY_INFO PCertPublicKeyInfo;

typedef _CRYPT_PRIVATE_KEY_INFO TCryptPrivateKeyInfo;

typedef PCRYPT_PRIVATE_KEY_INFO PCryptPrivateKeyInfo;

typedef _CRYPT_ENCRYPTED_PRIVATE_KEY_INFO TCryptEncryptedPrivateKeyInfo;

typedef PCRYPT_ENCRYPTED_PRIVATE_KEY_INFO PCryptEncryptedPrivateKeyInfo;

typedef PCRYPT_DECRYPT_PRIVATE_KEY_FUNC PCryptDecryptPrivateKeyFunc;

typedef PCRYPT_ENCRYPT_PRIVATE_KEY_FUNC PCryptEncryptPrivateKeyFunc;

typedef PCRYPT_RESOLVE_HCRYPTPROV_FUNC PCryptResolveHCryptProvFunc;

typedef _CRYPT_PKCS8_IMPORT_PARAMS TCryptPkcs8ImportParams;

typedef PCRYPT_PKCS8_IMPORT_PARAMS PCryptPkcs8ImportParams;

typedef _CRYPT_PKCS8_EXPORT_PARAMS TCryptPkcs8ExportParams;

typedef PCRYPT_PKCS8_EXPORT_PARAMS PCryptPkcs8ExportParams;

typedef _CERT_INFO TCertInfo;

typedef PCERT_INFO PCertInfo;

typedef _CRL_ENTRY TCrlEntry;

typedef PCRL_ENTRY PCrlEntry;

typedef _CRL_INFO TCrlInfo;

typedef PCRL_INFO PCrlInfo;

typedef _CERT_REQUEST_INFO TCertRequestInfo;

typedef PCERT_REQUEST_INFO PCertRequestInfo;

typedef _CERT_KEYGEN_REQUEST_INFO TCertKeygenRequestInfo;

typedef PCERT_KEYGEN_REQUEST_INFO PCertKeygenRequestInfo;

typedef _CERT_SIGNED_CONTENT_INFO TCertSignedContentInfo;

typedef PCERT_SIGNED_CONTENT_INFO PCertSignedContentInfo;

typedef _CTL_USAGE TCtlUsage;

typedef PCTL_USAGE PCtlUsage;

typedef _CTL_ENTRY TCtlEntry;

typedef PCTL_ENTRY PCtlEntry;

typedef _CTL_INFO TCtlInfo;

typedef PCTL_INFO PCtlInfo;

typedef _CRYPT_TIME_STAMP_REQUEST_INFO TCryptTimeStampRequestInfo;

typedef PCRYPT_TIME_STAMP_REQUEST_INFO PCryptTimeStampRequestInfo;

typedef _CRYPT_ENROLLMENT_NAME_VALUE_PAIR TCryptEnrollmentNameValuePair;

typedef PCRYPT_ENROLLMENT_NAME_VALUE_PAIR PCryptEnrollmentNameValuePair;

typedef _CRYPT_CSP_PROVIDER TCryptCspProvider;

typedef PCRYPT_CSP_PROVIDER PCryptCspProvider;

typedef _CRYPT_ENCODE_PARA TCryptEncodePara;

typedef PCRYPT_ENCODE_PARA PCryptEncodePara;

typedef _CRYPT_DECODE_PARA TCryptDecodePara;

typedef PCRYPT_DECODE_PARA PCryptDecodePara;

typedef _CERT_EXTENSIONS TCertExtensions;

typedef PCERT_EXTENSIONS PCertExtensions;

typedef _CERT_AUTHORITY_KEY_ID_INFO TCertAuthorityKeyIdInfo;

typedef PCERT_AUTHORITY_KEY_ID_INFO PCertAuthorityKeyIdInfo;

typedef _CERT_PRIVATE_KEY_VALIDITY TCertPrivateKeyValidity;

typedef PCERT_PRIVATE_KEY_VALIDITY PCertPrivateKeyValidity;

typedef _CERT_KEY_ATTRIBUTES_INFO TCertKeyAttributesInfo;

typedef PCERT_KEY_ATTRIBUTES_INFO PCertKeyAttributesInfo;

typedef _CERT_POLICY_ID TCertPolicyId;

typedef PCERT_POLICY_ID PCertPolicyId;

typedef _CERT_KEY_USAGE_RESTRICTION_INFO TCertKeyUsageRestrictionInfo;

typedef PCERT_KEY_USAGE_RESTRICTION_INFO PCertKeyUsageRestrictionInfo;

typedef _CERT_OTHER_NAME TCertOtherName;

typedef PCERT_OTHER_NAME PCertOtherName;

typedef _CERT_ALT_NAME_ENTRY TCertAltNameEntry;

typedef PCERT_ALT_NAME_ENTRY PCertAltNameEntry;

typedef _CERT_ALT_NAME_INFO TCertAltNameInfo;

typedef PCERT_ALT_NAME_INFO PCertAltNameInfo;

typedef _CERT_BASIC_CONSTRAINTS_INFO TCertBasicConstraintsInfo;

typedef PCERT_BASIC_CONSTRAINTS_INFO PCertBasicConstraintsInfo;

typedef _CERT_BASIC_CONSTRAINTS2_INFO TCertBasicConstraints2Info;

typedef PCERT_BASIC_CONSTRAINTS2_INFO PCertBasicConstraints2Info;

typedef _CERT_POLICY_QUALIFIER_INFO TCertPolicyQualifierInfo;

typedef PCERT_POLICY_QUALIFIER_INFO PCertPolicyQualifierInfo;

typedef _CERT_POLICY_INFO TCertPolicyInfo;

typedef PCERT_POLICY_INFO PCertPolicyInfo;

typedef _CERT_POLICIES_INFO TCertPoliciesInfo;

typedef PCERT_POLICIES_INFO PCertPoliciesInfo;

typedef _CERT_POLICY_QUALIFIER_NOTICE_REFERENCE TCertPolicyQualifierNoticeReference;

typedef PCERT_POLICY_QUALIFIER_NOTICE_REFERENCE PCertPolicyQualifierNoticeReference;

typedef _CERT_POLICY_QUALIFIER_USER_NOTICE TCertPolicyQualifierUserNotice;

typedef PCERT_POLICY_QUALIFIER_USER_NOTICE PCertPolicyQualifierUserNotice;

typedef _CPS_URLS TCpsUrls;

typedef PCPS_URLS PCpsUrls;

typedef _CERT_POLICY95_QUALIFIER1 TCertPolicy95Qualifier1;

typedef PCERT_POLICY95_QUALIFIER1 PCertPolicy95Qualifier1;

typedef _CRYPT_CONTENT_INFO_SEQUENCE_OF_ANY TCryptContentInfoSequenceOfAny;

typedef PCRYPT_CONTENT_INFO_SEQUENCE_OF_ANY PCryptContentInfoSequenceOfAny;

typedef _CRYPT_CONTENT_INFO TCryptContentInfo;

typedef PCRYPT_CONTENT_INFO PCryptContentInfo;

typedef _CRYPT_SEQUENCE_OF_ANY TCryptSequenceOfAny;

typedef PCRYPT_SEQUENCE_OF_ANY PCryptSequenceOfAny;

typedef _CERT_AUTHORITY_KEY_ID2_INFO TCertAuthorityKeyId2Info;

typedef PCERT_AUTHORITY_KEY_ID2_INFO PCertAuthorityKeyId2Info;

typedef _CERT_ACCESS_DESCRIPTION TCertAccessDescription;

typedef PCERT_ACCESS_DESCRIPTION PCertAccessDescription;

typedef _CERT_AUTHORITY_INFO_ACCESS TCertAuthorityInfoAccess;

typedef PCERT_AUTHORITY_INFO_ACCESS PCertAuthorityInfoAccess;

typedef _CRL_DIST_POINT_NAME TCrlDistPointName;

typedef PCRL_DIST_POINT_NAME PCrlDistPointName;

typedef _CRL_DIST_POINT TCrlDistPoint;

typedef PCRL_DIST_POINT PCrlDistPoint;

typedef _CRL_DIST_POINTS_INFO TCrlDistPointsInfo;

typedef PCRL_DIST_POINTS_INFO PCrlDistPointsInfo;

typedef _CERT_DSS_PARAMETERS TCertDssParameters;

typedef PCERT_DSS_PARAMETERS PCertDssParameters;

typedef _CERT_DH_PARAMETERS TCertDhParameters;

typedef PCERT_DH_PARAMETERS PCertDhParameters;

typedef _CERT_X942_DH_VALIDATION_PARAMS TCertX942DhValidationParams;

typedef PCERT_X942_DH_VALIDATION_PARAMS PCertX942DhValidationParams;

typedef _CERT_X942_DH_PARAMETERS TCertX942DhParameters;

typedef PCERT_X942_DH_PARAMETERS PCertX942DhParameters;

typedef _CRYPT_X942_OTHER_INFO TCryptX942OtherInfo;

typedef PCRYPT_X942_OTHER_INFO PCryptX942OtherInfo;

typedef _CRYPT_RC2_CBC_PARAMETERS TCryptRc2CbcParameters;

typedef PCRYPT_RC2_CBC_PARAMETERS PCryptRc2CbcParameters;

typedef _CRYPT_SMIME_CAPABILITY TCryptSmimeCapability;

typedef PCRYPT_SMIME_CAPABILITY PCryptSmimeCapability;

typedef _CRYPT_SMIME_CAPABILITIES TCryptSmimeCapabilities;

typedef PCRYPT_SMIME_CAPABILITIES PCryptSmimeCapabilities;

typedef _CRYPT_OID_FUNC_ENTRY TCryptOidFuncEntry;

typedef PCRYPT_OID_FUNC_ENTRY PCryptOidFuncEntry;

typedef PFN_CRYPT_ENUM_OID_FUNC PFnCryptEnumOidFunc;

typedef _CRYPT_OID_INFO TCryptOidInfo;

typedef PCRYPT_OID_INFO PCryptOidInfo;

typedef PFN_CRYPT_ENUM_OID_INFO PFnCryptEnumOidInfo;

typedef _CERT_ISSUER_SERIAL_NUMBER TCertIssuerSerialNumber;

typedef PCERT_ISSUER_SERIAL_NUMBER PCertIssuerSerialNumber;

typedef _CERT_ID TCertId;

typedef PCERT_ID PCertId;

typedef _CMSG_SIGNER_ENCODE_INFO TCmsgSignerEncodeInfo;

typedef PCMSG_SIGNER_ENCODE_INFO PCmsgSignerEncodeInfo;

typedef _CMSG_SIGNED_ENCODE_INFO TCmsgSignedEncodeInfo;

typedef PCMSG_SIGNED_ENCODE_INFO PCmsgSignedEncodeInfo;

typedef _CMSG_ENVELOPED_ENCODE_INFO TCmsgEnvelopedEncodeInfo;

typedef PCMSG_ENVELOPED_ENCODE_INFO PCmsgEnvelopedEncodeInfo;

typedef _CMSG_KEY_TRANS_RECIPIENT_ENCODE_INFO TCmsgKeyTransRecipientEncodeInfo;

typedef PCMSG_KEY_TRANS_RECIPIENT_ENCODE_INFO PCmsgKeyTransRecipientEncodeInfo;

typedef _CMSG_RECIPIENT_ENCRYPTED_KEY_ENCODE_INFO TCmsgRecipientEncryptedKeyEncodeInfo;

typedef PCMSG_RECIPIENT_ENCRYPTED_KEY_ENCODE_INFO PCmsgRecipientEncryptedKeyEncodeInfo;

typedef _CMSG_KEY_AGREE_RECIPIENT_ENCODE_INFO TCmsgKeyAgreeRecipientEncodeInfo;

typedef PCMSG_KEY_AGREE_RECIPIENT_ENCODE_INFO PCmsgKeyAgreeRecipientEncodeInfo;

typedef _CMSG_MAIL_LIST_RECIPIENT_ENCODE_INFO TCmsgMailListRecipientEncodeInfo;

typedef PCMSG_MAIL_LIST_RECIPIENT_ENCODE_INFO PCmsgMailListRecipientEncodeInfo;

typedef _CMSG_RECIPIENT_ENCODE_INFO TCmsgRecipientEncodeInfo;

typedef PCMSG_RECIPIENT_ENCODE_INFO PCmsgRecipientEncodeInfo;

typedef _CMSG_RC2_AUX_INFO TCmsgRc2AuxInfo;

typedef PCMSG_RC2_AUX_INFO PCmsgRc2AuxInfo;

typedef _CMSG_SP3_COMPATIBLE_AUX_INFO TCmsgSp3CompatibleAuxInfo;

typedef PCMSG_SP3_COMPATIBLE_AUX_INFO PCmsgSp3CompatibleAuxInfo;

typedef _CMSG_RC4_AUX_INFO TCmsgRc4AuxInfo;

typedef PCMSG_RC4_AUX_INFO PCmsgRc4AuxInfo;

typedef _CMSG_SIGNED_AND_ENVELOPED_ENCODE_INFO TCmsgSignedAndEnvelopedEncodeInfo;

typedef PCMSG_SIGNED_AND_ENVELOPED_ENCODE_INFO PCmsgSignedAndEnvelopedEncodeInfo;

typedef _CMSG_HASHED_ENCODE_INFO TCmsgHashedEncodeInfo;

typedef PCMSG_HASHED_ENCODE_INFO PCmsgHashedEncodeInfo;

typedef _CMSG_ENCRYPTED_ENCODE_INFO TCmsgEncryptedEncodeInfo;

typedef PCMSG_ENCRYPTED_ENCODE_INFO PCmsgEncryptedEncodeInfo;

typedef PFN_CMSG_STREAM_OUTPUT PFnCMsgStreamOutput;

typedef _CMSG_STREAM_INFO TCmsgStreamInfo;

typedef PCMSG_STREAM_INFO PCmsgStreamInfo;

typedef _CMSG_SIGNER_INFO TCmsgSignerInfo;

typedef PCMSG_SIGNER_INFO PCmsgSignerInfo;

typedef _CMSG_CMS_SIGNER_INFO TCmsgCmsSignerInfo;

typedef PCMSG_CMS_SIGNER_INFO PCmsgCmsSignerInfo;

typedef _CMSG_KEY_TRANS_RECIPIENT_INFO TCmsgKeyTransRecipientInfo;

typedef PCMSG_KEY_TRANS_RECIPIENT_INFO PCmsgKeyTransRecipientInfo;

typedef _CMSG_RECIPIENT_ENCRYPTED_KEY_INFO TCmsgRecipientEncryptedKeyInfo;

typedef PCMSG_RECIPIENT_ENCRYPTED_KEY_INFO PCmsgRecipientEncryptedKeyInfo;

typedef _CMSG_KEY_AGREE_RECIPIENT_INFO TCmsgKeyAgreeRecipientInfo;

typedef PCMSG_KEY_AGREE_RECIPIENT_INFO PCmsgKeyAgreeRecipientInfo;

typedef _CMSG_MAIL_LIST_RECIPIENT_INFO TCmsgMailListRecipientInfo;

typedef PCMSG_MAIL_LIST_RECIPIENT_INFO PCmsgMailListRecipientInfo;

typedef _CMSG_CMS_RECIPIENT_INFO TCmsgCmsRecipientInfo;

typedef PCMSG_CMS_RECIPIENT_INFO PCmsgCmsRecipientInfo;

typedef _CMSG_CTRL_VERIFY_SIGNATURE_EX_PARA TCmsgCtrlVerifySignatureExPara;

typedef PCMSG_CTRL_VERIFY_SIGNATURE_EX_PARA PCmsgCtrlVerifySignatureExPara;

typedef _CMSG_CTRL_DECRYPT_PARA TCmsgCtrlDecryptPara;

typedef PCMSG_CTRL_DECRYPT_PARA PCmsgCtrlDecryptPara;

typedef _CMSG_CTRL_KEY_TRANS_DECRYPT_PARA TCmsgCtrlKeyTransDecryptPara;

typedef PCMSG_CTRL_KEY_TRANS_DECRYPT_PARA PCmsgCtrlKeyTransDecryptPara;

typedef _CMSG_CTRL_KEY_AGREE_DECRYPT_PARA TCmsgCtrlKeyAgreeDecryptPara;

typedef PCMSG_CTRL_KEY_AGREE_DECRYPT_PARA PCmsgCtrlKeyAgreeDecryptPara;

typedef _CMSG_CTRL_MAIL_LIST_DECRYPT_PARA TCmsgCtrlMailListDecryptPara;

typedef PCMSG_CTRL_MAIL_LIST_DECRYPT_PARA PCmsgCtrlMailListDecryptPara;

typedef _CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR_PARA TCmsgCtrlAddSignerUnauthAttrPara;

typedef PCMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR_PARA PCmsgCtrlAddSignerUnauthAttrPara;

typedef _CMSG_CTRL_DEL_SIGNER_UNAUTH_ATTR_PARA TCmsgCtrlDelSignerUnauthAttrPara;

typedef PCMSG_CTRL_DEL_SIGNER_UNAUTH_ATTR_PARA PCmsgCtrlDelSignerUnauthAttrPara;

typedef PFN_CMSG_ALLOC PFnCMsgAlloc;

typedef PFN_CMSG_FREE PFnCMsgFree;

typedef PFN_CMSG_GEN_ENCRYPT_KEY PfnCmsgGenEncryptKey;

typedef PFN_CMSG_EXPORT_ENCRYPT_KEY PfnCmsgExportEncryptKey;

typedef PFN_CMSG_IMPORT_ENCRYPT_KEY PfnCmsgImportEncryptKey;

typedef _CMSG_CONTENT_ENCRYPT_INFO TCmsgContentEncryptInfo;

typedef PCMSG_CONTENT_ENCRYPT_INFO PCmsgContentEncryptInfo;

typedef PFN_CMSG_GEN_CONTENT_ENCRYPT_KEY PfnCmsgGenContentEncryptKey;

typedef _CMSG_KEY_TRANS_ENCRYPT_INFO TCmsgKeyTransEncryptInfo;

typedef PCMSG_KEY_TRANS_ENCRYPT_INFO PCmsgKeyTransEncryptInfo;

typedef PFN_CMSG_EXPORT_KEY_TRANS PfnCmsgExportKeyTrans;

typedef _CMSG_KEY_AGREE_KEY_ENCRYPT_INFO TCmsgKeyAgreeKeyEncryptInfo;

typedef PCMSG_KEY_AGREE_KEY_ENCRYPT_INFO PCmsgKeyAgreeKeyEncryptInfo;

typedef _CMSG_KEY_AGREE_ENCRYPT_INFO TCmsgKeyAgreeEncryptInfo;

typedef PCMSG_KEY_AGREE_ENCRYPT_INFO PCmsgKeyAgreeEncryptInfo;

typedef PFN_CMSG_EXPORT_KEY_AGREE PfnCmsgExportKeyAgree;

typedef _CMSG_MAIL_LIST_ENCRYPT_INFO TCmsgMailListEncryptInfo;

typedef PCMSG_MAIL_LIST_ENCRYPT_INFO PCmsgMailListEncryptInfo;

typedef PFN_CMSG_EXPORT_MAIL_LIST PfnCmsgExportMailList;

typedef PFN_CMSG_IMPORT_KEY_TRANS PfnCmsgImportKeyTrans;

typedef PFN_CMSG_IMPORT_KEY_AGREE PfnCmsgImportKeyAgree;

typedef PFN_CMSG_IMPORT_MAIL_LIST PfnCmsgImportMailList;

typedef _CERT_CONTEXT *PCERT_CONTEXT;

typedef _CERT_CONTEXT TCertContext;

typedef PCERT_CONTEXT PCertContext;

typedef PCERT_CONTEXT *PPCCERT_CONTEXT;

typedef _CRL_CONTEXT TCrlContext;

typedef PCRL_CONTEXT PCrlContext;

typedef _CTL_CONTEXT TCtlContext;

typedef PCTL_CONTEXT PCtlContext;

typedef _CRYPT_KEY_PROV_PARAM TCryptKeyProvParam;

typedef PCRYPT_KEY_PROV_PARAM PCryptKeyProvParam;

typedef _CRYPT_KEY_PROV_INFO TCryptKeyProvInfo;

typedef PCRYPT_KEY_PROV_INFO PCryptKeyProvInfo;

typedef _CERT_KEY_CONTEXT TCertKeyContext;

typedef PCERT_KEY_CONTEXT PCertKeyContext;

typedef _CERT_SYSTEM_STORE_RELOCATE_PARA TCertSystemStoreRelocatePara;

typedef PCERT_SYSTEM_STORE_RELOCATE_PARA PCertSystemStoreRelocatePara;

typedef _CERT_REGISTRY_STORE_CLIENT_GPT_PARA TCertRegistryStoreClientGptPara;

typedef PCERT_REGISTRY_STORE_CLIENT_GPT_PARA PCertRegistryStoreClientGptPara;

typedef _CERT_REGISTRY_STORE_ROAMING_PARA TCertRegistryStoreRoamingPara;

typedef PCERT_REGISTRY_STORE_ROAMING_PARA PCertRegistryStoreRoamingPara;

typedef _CERT_STORE_PROV_INFO TCertStoreProvInfo;

typedef PCERT_STORE_PROV_INFO PCertStoreProvInfo;

typedef PFN_CERT_DLL_OPEN_STORE_PROV_FUNC PFnCertDllOpenStoreProvFunc;

typedef PFN_CERT_STORE_PROV_CLOSE PFnCertStoreProvClose;

typedef PFN_CERT_STORE_PROV_READ_CERT PfnCertStoreProvReadCert;

typedef PFN_CERT_STORE_PROV_WRITE_CERT PfnCertStoreProvWriteCert;

typedef PFN_CERT_STORE_PROV_DELETE_CERT PfnCertStoreProvDeleteCert;

typedef PFN_CERT_STORE_PROV_SET_CERT_PROPERTY PfnCertStoreProvSetCertProperty;

typedef PFN_CERT_STORE_PROV_READ_CRL PfnCertStoreProvReadCrl;

typedef PFN_CERT_STORE_PROV_WRITE_CRL PfnCertStoreProvWriteCrl;

typedef PFN_CERT_STORE_PROV_DELETE_CRL PfnCertStoreProvDeleteCrl;

typedef PFN_CERT_STORE_PROV_SET_CRL_PROPERTY PfnCertStoreProvSetCrlProperty;

typedef PFN_CERT_STORE_PROV_READ_CTL PfnCertStoreProvReadCtl;

typedef PFN_CERT_STORE_PROV_WRITE_CTL PfnCertStoreProvWriteCtl;

typedef PFN_CERT_STORE_PROV_DELETE_CTL PfnCertStoreProvDeleteCtl;

typedef PFN_CERT_STORE_PROV_SET_CTL_PROPERTY PfnCertStoreProvSetCtlProperty;

typedef PFN_CERT_STORE_PROV_CONTROL PfnCertStoreProvControl;

typedef _CERT_STORE_PROV_FIND_INFO TCertStoreProvFindInfo;

typedef PCERT_STORE_PROV_FIND_INFO PCertStoreProvFindInfo;

typedef PFN_CERT_STORE_PROV_FIND_CERT PfnCertStoreProvFindCert;

typedef PFN_CERT_STORE_PROV_FREE_FIND_CERT PfnCertStoreProvFreeFindCert;

typedef PFN_CERT_STORE_PROV_GET_CERT_PROPERTY PfnCertStoreProvGetCertProperty;

typedef PFN_CERT_STORE_PROV_FIND_CRL PfnCertStoreProvFindCrl;

typedef PFN_CERT_STORE_PROV_FREE_FIND_CRL PfnCertStoreProvFreeFindCrl;

typedef PFN_CERT_STORE_PROV_GET_CRL_PROPERTY PfnCertStoreProvGetCrlProperty;

typedef PFN_CERT_STORE_PROV_FIND_CTL PfnCertStoreProvFindCtl;

typedef PFN_CERT_STORE_PROV_FREE_FIND_CTL PfnCertStoreProvFreeFindCtl;

typedef PFN_CERT_STORE_PROV_GET_CTL_PROPERTY PfnCertStoreProvGetCtlProperty;

typedef _CTL_ANY_SUBJECT_INFO TCtlAnySubjectInfo;

typedef PCTL_ANY_SUBJECT_INFO PCtlAnySubjectInfo;

typedef _CTL_FIND_USAGE_PARA TCtlFindUsagePara;

typedef PCTL_FIND_USAGE_PARA PCtlFindUsagePara;

typedef _CTL_FIND_SUBJECT_PARA TCtlFindSubjectPara;

typedef PCTL_FIND_SUBJECT_PARA PCtlFindSubjectPara;

typedef _CERT_CREATE_CONTEXT_PARA TCertCreateContextPara;

typedef PCERT_CREATE_CONTEXT_PARA PCertCreateContextPara;

typedef _CERT_SYSTEM_STORE_INFO TCertSystemStoreInfo;

typedef PCERT_SYSTEM_STORE_INFO PCertSystemStoreInfo;

typedef _CERT_PHYSICAL_STORE_INFO TCertPhysicalStoreInfo;

typedef PCERT_PHYSICAL_STORE_INFO PCertPhysicalStoreInfo;

typedef PFN_CERT_ENUM_SYSTEM_STORE_LOCATION PfnCertEnumSystemStoreLocation;

typedef PFN_CERT_ENUM_SYSTEM_STORE PfnCertEnumSystemStore;

typedef PFN_CERT_ENUM_PHYSICAL_STORE PfnCertEnumPhysicalStore;

typedef _CTL_VERIFY_USAGE_PARA TCtlVerifyUsagePara;

typedef PCTL_VERIFY_USAGE_PARA PCtlVerifyUsagePara;

typedef _CTL_VERIFY_USAGE_STATUS TCtlVerifyUsageStatus;

typedef PCTL_VERIFY_USAGE_STATUS PCtlVerifyUsageStatus;

typedef _CERT_REVOCATION_PARA TCertRevocationPara;

typedef PCERT_REVOCATION_PARA PCertRevocationPara;

typedef _CERT_REVOCATION_STATUS TCertRevocationStatus;

typedef PCERT_REVOCATION_STATUS PCertRevocationStatus;

typedef _CRYPT_DEFAULT_CONTEXT_MULTI_OID_PARA TCryptDefaultContextMultiOidPara;

typedef PCRYPT_DEFAULT_CONTEXT_MULTI_OID_PARA PCryptDefaultContextMultiOidPara;

typedef PFN_IMPORT_PRIV_KEY_FUNC PFnImportPrivKeyFunc;

typedef PFN_EXPORT_PRIV_KEY_FUNC PfnExportPrivKeyFunc;

typedef PFN_CRYPT_GET_SIGNER_CERTIFICATE PfnCryptGetSignerCertificate;

typedef _CRYPT_SIGN_MESSAGE_PARA TCryptSignMessagePara;

typedef PCRYPT_SIGN_MESSAGE_PARA PCryptSignMessagePara;

typedef _CRYPT_VERIFY_MESSAGE_PARA TCryptVerifyMessagePara;

typedef PCRYPT_VERIFY_MESSAGE_PARA PCryptVerifyMessagePara;

typedef _CRYPT_ENCRYPT_MESSAGE_PARA TCryptEncryptMessagePara;

typedef PCRYPT_ENCRYPT_MESSAGE_PARA PCryptEncryptMessagePara;

typedef _CRYPT_DECRYPT_MESSAGE_PARA TCryptDecryptMessagePara;

typedef PCRYPT_DECRYPT_MESSAGE_PARA PCryptDecryptMessagePara;

typedef _CRYPT_HASH_MESSAGE_PARA TCryptHashMessagePara;

typedef PCRYPT_HASH_MESSAGE_PARA PCryptHashMessagePara;

typedef _CRYPT_KEY_SIGN_MESSAGE_PARA TCryptKeySignMessagePara;

typedef PCRYPT_KEY_SIGN_MESSAGE_PARA PCryptKeySignMessagePara;

typedef _CRYPT_KEY_VERIFY_MESSAGE_PARA TCryptKeyVerifyMessagePara;

typedef PCRYPT_KEY_VERIFY_MESSAGE_PARA PCryptKeyVerifyMessagePara;

typedef _CERT_CHAIN TCertChain;

typedef PCERT_CHAIN PCertChain;

typedef PFN_CRYPT_ASYNC_PARAM_FREE_FUNC PFnCryptAsyncParamFreeFunc;

typedef _CRYPT_BLOB_ARRAY TCryptBlobArray;

typedef PCRYPT_BLOB_ARRAY PCryptBlobArray;

typedef _CRYPT_CREDENTIALS TCryptCredentials;

typedef PCRYPT_CREDENTIALS PCryptCredentials;

typedef _CRYPT_PASSWORD_CREDENTIALSA TCryptPasswordCredentialsA;

typedef PCRYPT_PASSWORD_CREDENTIALSA PCryptPasswordCredentialsA;

typedef _CRYPT_PASSWORD_CREDENTIALSW TCryptPasswordCredentialsW;

typedef PCRYPT_PASSWORD_CREDENTIALSW PCryptPasswordCredentialsW;

typedef _CRYPT_PASSWORD_CREDENTIALSW TCryptPasswordCredentials;

typedef PCryptPasswordCredentialsW PCryptPasswordCredentials;

typedef PFN_FREE_ENCODED_OBJECT_FUNC PFnFreeEncodedObjectFunc;

typedef PFN_CRYPT_CANCEL_RETRIEVAL PFnCryptCancelRetrieval;

typedef PFN_CRYPT_ASYNC_RETRIEVAL_COMPLETION_FUNC PFnCryptASynchRetrievalCompletionFunc;

typedef _CRYPT_ASYNC_RETRIEVAL_COMPLETION TCryptAsyncRetrievalCompletion;

typedef PCRYPT_ASYNC_RETRIEVAL_COMPLETION PCryptAsyncRetrievalCompletion;

typedef PFN_CANCEL_ASYNC_RETRIEVAL_FUNC PFnCancelASynchRetrievalFunc;

typedef _CRYPT_URL_ARRAY TCryptUrlArray;

typedef PCRYPT_URL_ARRAY PCryptUrlArray;

typedef _CRYPT_URL_INFO TCryptUrlInfo;

typedef PCRYPT_URL_INFO PCryptUrlInfo;

typedef _CRYPTPROTECT_PROMPTSTRUCT TCryptProtectPromptStruct;

typedef PCRYPTPROTECT_PROMPTSTRUCT PCryptProtectPromptStruct;

typedef PFN_CRYPT_ENUM_KEYID_PROP PFnCryptEnumKeyIdProp;

typedef _CERT_CHAIN_ENGINE_CONFIG TCertChainEngineConfig;

typedef PCERT_CHAIN_ENGINE_CONFIG PCertChainEngineConfig;

typedef _CERT_TRUST_STATUS TCertTrustStatus;

typedef PCERT_TRUST_STATUS PCertTrustStatus;

typedef _CERT_REVOCATION_INFO TCertRevocationInfo;

typedef PCERT_REVOCATION_INFO PCertRevocationInfo;

typedef _CERT_TRUST_LIST_INFO TCertTrustListInfo;

typedef PCERT_TRUST_LIST_INFO PCertTrustListInfo;

typedef _CERT_CHAIN_ELEMENT TCertChainElement;

typedef PCERT_CHAIN_ELEMENT PCertChainElement;

typedef _CERT_SIMPLE_CHAIN TCertSimpleChain;

typedef PCERT_SIMPLE_CHAIN PCertSimpleChain;

typedef _CERT_CHAIN_CONTEXT TCertChainContext;

typedef PCERT_CHAIN_CONTEXT PCertChainContext;

typedef _CERT_USAGE_MATCH TCertUsageMatch;

typedef PCERT_USAGE_MATCH PCertUsageMatch;

typedef _CTL_USAGE_MATCH TCtlUsageMatch;

typedef PCTL_USAGE_MATCH PCtlUsageMatch;

typedef _CERT_CHAIN_PARA TCertChainPara;

typedef PCERT_CHAIN_PARA PCertChainPara;

typedef _CRL_REVOCATION_INFO TCrlRevocationInfo;

typedef PCRL_REVOCATION_INFO PCrlRevocationInfo;

typedef PFN_CERT_CHAIN_FIND_BY_ISSUER_CALLBACK PFnCertChainFindByIssuerCallback;

typedef _CERT_CHAIN_FIND_BY_ISSUER_PARA TCertChainFindByIssuerPara;

typedef PCERT_CHAIN_FIND_BY_ISSUER_PARA PCertChainFindByIssuerPara;

typedef _CERT_CHAIN_POLICY_PARA TCertChainPolicyPara;

typedef PCERT_CHAIN_POLICY_PARA PCertChainPolicyPara;

typedef _CERT_CHAIN_POLICY_STATUS TCertChainPolicyStatus;

typedef PCERT_CHAIN_POLICY_STATUS PCertChainPolicyStatus;

typedef _AUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_PARA TAuthenticodeExtraCertChainPolicyPara;

typedef PAUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_PARA PAuthenticodeExtraCertChainPolicyPara;

typedef _AUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_STATUS TAuthenticodeExtraCertChainPolicyStatus;

typedef PAUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_STATUS PAuthenticodeExtraCertChainPolicyStatus;

typedef _AUTHENTICODE_TS_EXTRA_CERT_CHAIN_POLICY_PARA TAuthenticodeTsExtraCertChainPolicyPara;

typedef PAUTHENTICODE_TS_EXTRA_CERT_CHAIN_POLICY_PARA PAuthenticodeTsExtraCertChainPolicyPara;

typedef _HTTPSPolicyCallbackData THttpsPolicyCallbackData;

typedef _HTTPSPolicyCallbackData TSSLExtraCertChainPolicyPara;

typedef PHTTPSPolicyCallbackData PSSLExtraCertChainPolicyPara;

typedef _CRYPT_PKCS12_PBE_PARAMS *PCryptPKCS12PbeParams;

typedef _CRYPT_PKCS12_PBE_PARAMS TCryptPKCS12PbeParams;

typedef _BCryptBuffer TBCryptBuffer;

typedef _BCryptBufferDesc TBCryptBufferDesc;

typedef _BCryptBuffer TNCryptBuffer;

typedef _BCryptBufferDesc TNCryptBufferDesc;

typedef _NCryptAlgorithmName *PNCryptAlgorithmName;

typedef PNCryptAlgorithmName *PPNCryptAlgorithmName;

typedef _NCryptAlgorithmName TNCryptAlgorithmName;

typedef NCryptKeyName *PNCryptKeyName;

typedef NCryptKeyName TNCryptKeyName;

typedef NCryptProviderName *PNCryptProviderName;

typedef NCryptProviderName TNCryptProviderName;

typedef __NCRYPT_UI_POLICY_BLOB *PNCryptUIPolicyBlob;

struct DECLSPEC_DRECORD __NCRYPT_UI_POLICY_BLOB
{
public:
	unsigned dwVersion;
	unsigned dwFlags;
	unsigned cbCreationTitle;
	unsigned cbFriendlyName;
	unsigned cbDescription;
};


typedef __NCRYPT_UI_POLICY_BLOB NCRYPT_UI_POLICY_BLOB;

typedef __NCRYPT_UI_POLICY_BLOB TNCryptUIPolicyBlob;

typedef __NCRYPT_UI_POLICY *PNCryptUIPolicy;

typedef __NCRYPT_UI_POLICY TNCryptUIPolicy;

typedef __NCRYPT_SUPPORTED_LENGTHS *PNCryptSupportedLengths;

typedef __NCRYPT_SUPPORTED_LENGTHS TNCryptSupportedLengths;

typedef _BCRYPT_INTERFACE_VERSION *PBCryptInterfaceVersion;

typedef _BCRYPT_INTERFACE_VERSION TBCryptInterfaceVersion;

//-- var, const, procedure ---------------------------------------------------

//static _DELPHI_CONST System::Int8 CRYPT_STRING_BASE64HEADER = System::Int8(0x0);
//static _DELPHI_CONST System::Int8 CRYPT_STRING_BASE64 = System::Int8(0x1);
//static _DELPHI_CONST System::Int8 CRYPT_STRING_BINARY = System::Int8(0x2);
//static _DELPHI_CONST System::Int8 CRYPT_STRING_BASE64REQUESTHEADER = System::Int8(0x3);
//static _DELPHI_CONST System::Int8 CRYPT_STRING_HEX = System::Int8(0x4);
//static _DELPHI_CONST System::Int8 CRYPT_STRING_HEXASCII = System::Int8(0x5);
//static _DELPHI_CONST System::Int8 CRYPT_STRING_BASE64_ANY = System::Int8(0x6);
//static _DELPHI_CONST System::Int8 CRYPT_STRING_ANY = System::Int8(0x7);
//static _DELPHI_CONST System::Int8 CRYPT_STRING_HEX_ANY = System::Int8(0x8);
//static _DELPHI_CONST System::Int8 CRYPT_STRING_BASE64X509CRLHEADER = System::Int8(0x9);
//static _DELPHI_CONST System::Int8 CRYPT_STRING_HEXADDR = System::Int8(0xa);
//static _DELPHI_CONST System::Int8 CRYPT_STRING_HEXASCIIADDR = System::Int8(0xb);
//static _DELPHI_CONST unsigned CRYPT_STRING_NOCR = unsigned(0x80000000);
//extern "C" int __stdcall NCryptDeriveKey(unsigned hSharedSecret, System::WideChar * pwszKDF, PBCryptBufferDesc pParameterList, Winapi::Windows::PByte pbDerivedKey, unsigned cbDerivedKey, /* out */ unsigned &pcbResult, unsigned dwFlags);
}	/* namespace Overbyteicswincrypt */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_OVERBYTEICSWINCRYPT)
using namespace Overbyteicswincrypt;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// OverbyteicswincryptHPP
