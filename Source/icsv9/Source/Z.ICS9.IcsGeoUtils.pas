{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  This unit contains geographic utilities, looking up countries
              to which IP address blocks are allocated, and information
              about those countries.
Creation:     Aug 2025
Updated:      Oct 2025
Version:      V9.5
Delphi:       Needs Delphi 10.2 or later, maybe a couple of earlier versions.
EMail:        francois.piette@overbyte.be  https://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2025 by Angus Robertson, Magenta Systems Ltd,
              Croydon, England. delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
              This unit also contains code from Rudy Velthuis, Albert de Weerd
              and italy Yakovlev, see separate copyright notices later.

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



MaxMind DB Reader Component
---------------------------

This unit includes a Delphi reader for the MaxMind DB file format from:

https://github.com/optinsoft/MMDBReader

See below for legal information.

Added overloaded constructor TMMDBReader with a TStream to loaded from program resource.



db-ip.com Free IP geolocation databases
---------------------------------------

https://db-ip.com/db/lite.php

The DB-IP Lite databases are subsets of the commercial databases with reduced coverage and accuracy.
Lite downloads are updated monthly and distributed under the Creative Commons Attribution License.

Licensing terms
The free DB-IP Lite database by DB-IP is licensed under a Creative Commons Attribution 4.0 International License.
You are free to use this database in your application, provided you give attribution to DB-IP.com for the data.
In the case of a web application, you must include a link back to DB-IP.com on pages that display or use results
from the database. You may do it by pasting the HTML code snippet below into your code :
Free IP geolocation databases

The DB-IP Lite databases are subsets of the commercial databases with reduced coverage and accuracy. Lite downloads are updated monthly and distributed under the Creative Commons Attribution License.
<a href='https://db-ip.com'>IP Geolocation by DB-IP</a>

This unit contains the db-ip.com 'IP to Country Lite' database as a resource file. The database is updated monthly and
can be downlaoded from https://download.db-ip.com/free/dbip-country-lite-2025-08.mmdb.gz  (changing the date)

This unit contains the db-ip.com 'IP to ASN Lite' database as a resource file. The database is updated monthly and
can be downlaoded from https://download.db-ip.com/free/dbip-asn-lite-2025-08.mmdb.gz  (changing the date)

These two database files are in the ICS source directory with RC and RES versions for resource files,
ie dbip-country-lite.RES and dbip-asn-lite.RES, and provided the ICS-OpenSSL directory has been copied
to ProgramData, as:

C:\ProgramData\ICS-OpenSSL\ICS-Geodb\dbip-country-lite.mmdb
C:\ProgramData\ICS-OpenSSL\ICS-Geodb\dbip-asn-lite.mmdb

These file names are the defaults the TIcsGeoTools will open if the resource files are not linked
into the application.  The two files total about 16MB which will typically double an EXE file,
so sometimes better to share the files rather than linking them into each EXE.  Also means they
can be updated without rebuilding the EXE.  But sometimes no external files is a bonus.

Applications using TIcsGeoTools need to specifically load each of the databases, they may
not be needed.

Note TIcsGeoTools is planned to support a third database dbip-city-lite.mmdb that combines
countries with city and regional names, but is much larger at over 120MB, it does not yet work.

db-ip.com also offers commercial databases updated daily, containing City and ASN information.
Currently this component does not support database fields other than Country ISOA2, and would
need new methods adding to do so.


Maxmind
-------

https://www.maxmind.com/en/geoip-databases

Maxmind offers a wide range of commercial databases with complex licensing agreements, no real
pricing on web site, but include anonymous IP or proxy, ISP, City, Connection Type, many of
these will need TMMDBxxInfo classes to read new fields and probably new classes for databases
not previously tested.




Baseline - V9.5 - 4th September 2025




Pending - FindISO2ACity fails to find any database fields in TMMDBIPCountryCityInfoEx

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Z.ICS9.IcsGeoUtils;

{$I Include\Z.ICS9.OverbyteIcsDefs.inc}

{ set or remove RES includes if you want to only load databases from file }
{ these RES includes could be in an application instead }

{ MMDB file built as a resource file by dbip-country-lite.RC, about 7 MBytes  }
{.. $R dbip-country-lite.RES}

{ MMDB file built as a resource file by dbip-asn-lite.RC, about 9 MBytes  }
{.. $R dbip-asn-lite.RES}

{ ICS-countries.csv file built as a resource file by ICS-Countries.RC, 11 KBytes }
{$R ICS-Countries.RES}


{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$B-}             { Enable partial boolean evaluation   }
{$T-}             { Untyped pointers                    }
{$X+}             { Enable extended syntax              }
{$H+}             { Use long strings                    }
{$IFDEF BCB}
    {$ObjExportAll On}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
    Winapi.Windows,
{$ENDIF}
    System.Classes, System.Sysutils, System.DateUtils,
    System.TypInfo, System.Generics.Collections, System.Rtti,
    System.Contnrs, System.SyncObjs, System.RegularExpressions,
    System.Math, System.StrUtils,
    Z.ICS9.OverbyteIcsTypes,
    Z.ICS9.OverbyteIcsUtils,
    Z.ICS9.OverbyteIcsIpUtils;

type
  TIcsRegion = record
    ID: integer;
    Name: string;
  end;

const
    IcsRegionAfrica          = 002;  // major
    IcsRegionOceania         = 009;  // major
    IcsRegionNorthAfrica     = 015;
    IcsRegionAmericas        = 019;  // major
    IcsRegionNorthAmerica    = 021;
    IcsRegionEasternAsia     = 030;
    IcsRegionSouthernAsia    = 034;
    IcsRegionSouthEastAsia   = 035;
    IcsRegionSouthEurope     = 039;
    IcsRegionAustraliaNZ     = 053;
    IcsRegionMelanesia       = 054;
    IcsRegionPolynesia       = 061;
    IcsRegionAsia            = 142;  // major
    IcsRegionCentralAsia     = 143;
    IcsRegionWesternAsia     = 145;
    IcsRegionEurope          = 150;  // major
    IcsRegionEastEurope      = 151;
    IcsRegionNorthEurope     = 154;
    IcsRegionWestEurope      = 155;
    IcsRegionSaharanAfrica   = 202;
    IcsRegionLatinAmerica    = 419;
    IcsUnknownISOA2          = '??';

    TotRegionNames = 21;
    IcsRegionNames: array[1..TotRegionNames] Of TIcsRegion = (
        (ID: IcsRegionAfrica;       Name: 'Africa'),
        (ID: IcsRegionOceania;      Name: 'Oceania'),
        (ID: IcsRegionNorthAfrica;  Name: 'Northern Africa'),
        (ID: IcsRegionAmericas;     Name: 'Americas'),
        (ID: IcsRegionNorthAmerica; Name: 'Northern America'),
        (ID: IcsRegionEasternAsia;  Name: 'Eastern Asia'),
        (ID: IcsRegionSouthernAsia; Name: 'Southern Asia'),
        (ID: IcsRegionSouthEastAsia; Name: 'South-eastern Asia'),
        (ID: IcsRegionSouthEurope; Name: 'Southern Europe'),
        (ID: IcsRegionAustraliaNZ; Name: 'Australia and New Zealand'),
        (ID: IcsRegionMelanesia;   Name: 'Melanesia'),
        (ID: IcsRegionPolynesia;   Name: 'Polynesia'),
        (ID: IcsRegionAsia;        Name: 'Asia'),
        (ID: IcsRegionCentralAsia; Name: 'Central Asia'),
        (ID: IcsRegionWesternAsia; Name: 'Western Asia'),
        (ID: IcsRegionEurope;      Name: 'Europe'),
        (ID: IcsRegionEastEurope;  Name: 'Eastern Europe'),
        (ID: IcsRegionNorthEurope; Name: 'Northern Europe'),
        (ID: IcsRegionWestEurope;  Name: 'Western Europe'),
        (ID: IcsRegionSaharanAfrica; Name: 'Sub-Saharan Africa'),
        (ID: IcsRegionLatinAmerica; Name: 'Latin America and the Caribbean') );


{----------------------------------------------------------------------------}
{                                                                            }
{ File:       Velthuis.BigIntegers.pas                                       }
{ Function:   A big integer implementation, with critical parts written in   }
{             Win32 or Win64 assembler, or "Pure Pascal" for other           }
{             platforms, or if explicitly specified.                         }
{ Language:   Delphi version XE2 or later                                    }
{ Author:     Rudy Velthuis                                                  }
{ Copyright:  (c) 2015,2016,2017 Rudy Velthuis                               }
{                                                                            }
{             For tests, see BigIntegerDevelopmentTests.dproj. The data      }
{             for these tests are generated by a C# program, in the          }
{             DataGenerators\BigIntegers\BigIntegerTestGenerator             }
{             subdirectory, or by a Java program, in the                     }
{             DataGenerators\BigIntegers\Java\BigIntegerTestDataGenerator    }
{             subdirectory.                                                  }
{                                                                            }
{ Credits:    Thanks to Peter Cordes, Nils Pipenbrinck and Johan Bontes for  }
{             their help on StackOverflow:                                   }
{             - http://stackoverflow.com/a/32298732/95954                    }
{             - http://stackoverflow.com/a/32087095/95954                    }
{             - http://stackoverflow.com/a/32084357/95954                    }
{                                                                            }
{             Thanks to Agner Fog for his excellent optimization guides.     }
{                                                                            }
{ Literature: 1. Donald Knuth, "The Art Of Computer Programming", 2nd ed.    }
{                Vol I-III.                                                  }
{             2. Karl Hasselström,                                           }
{                "Fast Division of Large Integers - A Comparison of          }
{                 Algorithms"                                                }
{                 bioinfo.ict.ac.cn/~dbu/AlgorithmCourses/                   }
{                 Lectures/Hasselstrom2003.pdf                               }
{             3. Richard P. Brent and Paul Zimmermann,                       }
{                "Modern Computer Arithmetic"                                }
{                http://arxiv.org/pdf/1004.4710v1.pdf                        }
{                https://members.loria.fr/PZimmermann/mca/mca-cup-0.5.9.pdf  }
{             4. Christoph Burnikel, Joachim Ziegler                         }
{                "Fast Recursive Division"                                   }
{                cr.yp.to/bib/1998/burnikel.ps                               }
{             5. Hacker's Delight, e.g.                                      }
{                http://www.hackersdelight.org/basics2.pdf                   }
{             6. Wikipedia                                                   }
{                https://en.wikipedia.org                                    }
{             7. Rosetta Code                                                }
{                http://rosettacode.org/wiki/Rosetta_Code                    }
{             8. Michael Malenkov, Christopher J. Dutra, Marco T. Morazán    }
{                "A New Bignum Multiplication Algorithm"                     }
{                http://prolangs.cs.vt.edu/rutgers/meetings/                 }
{                masplas06/papers/2_Malenkov.pdf                             }
{                                                                            }
{ -------------------------------------------------------------------------- }
{                                                                            }
{ License:    Redistribution and use in source and binary forms, with or     }
{             without modification, are permitted provided that the          }
{             following conditions are met:                                  }
{                                                                            }
{             * Redistributions of source code must retain the above         }
{               copyright notices, this list of conditions and the           }
{               following disclaimer.                                        }
{             * Redistributions in binary form must reproduce the above      }
{               copyright notice, this list of conditions and the following  }
{               disclaimer in the documentation and/or other materials       }
{               provided with the distribution.                              }
{                                                                            }
{ Disclaimer: THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS "AS IS"     }
{             AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      }
{             LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND      }
{             FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO         }
{             EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE      }
{             FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,      }
{             OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,       }
{             PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,      }
{             DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED     }
{             AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT    }
{             LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)         }
{             ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF    }
{             ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                     }
{                                                                            }
{----------------------------------------------------------------------------}

// from Velthuis.Sizes;

const
  CUInt8Bits    = 8;
  CInt8Bits     = CUInt8Bits - 1;
  CUInt16Bits   = 16;
  CInt16Bits    = CUInt16Bits - 1;
  CUInt32Bits   = 32;
  CInt32Bits    = CUInt32Bits - 1;
  CUInt64Bits   = 64;
  CInt64Bits    = CUInt64Bits - 1;
  CByteBits     = CUInt8Bits;
  CShortintBits = CByteBits - 1;
  CWordBits     = CByteBits * SizeOf(Word);
  CSmallintBits = CWordBits - 1;

  // Note: up to XE8, Longword and Longint were fixed sizes (32 bit). This has changed in XE8.
  CLongwordBits = CByteBits * SizeOf(Longword);
  CLongintBits  = CLongwordBits - 1;

  // Note: up to XE8, Integer and Cardinal were platform dependent. This has changed in XE8.
  CCardinalBits = CByteBits * SizeOf(Cardinal);
  CIntegerBits  = CCardinalBits - 1;

// unit Velthuis.Numerics;    - added V to functions since many duplicate Delphi units

{$LEGACYIFEND ON}
{$CODEALIGN 16}
{$ALIGN 16}

{$INLINE AUTO}


// Return the number of set (1) bits in the given integers.
function VBitCount(U: UInt8): Integer; overload;
function VBitCount(U: UInt16): Integer; overload;
function VBitCount(S: Int32): Integer; overload;
function VBitCount(U: UInt32): Integer; overload;
function VBitCount(S: Int64): Integer; overload;
function VBitCount(S: UInt64): Integer; overload;

// Return the number of significant bits, excluding the sign bit.
function VBitLength(S: Int32): Integer; overload;
function VBitLength(U: UInt32): Integer; overload;
function VBitLength(S: Int64): Integer; overload;
function VBitLength(U: UInt64): Integer; overload;

// Return the number of significant digits.
function VDigitCount(S: Int32): Int32; overload;
function VDigitCount(U: UInt32): UInt32; overload;

// Return an integer value with at most a single one-bit, in the position
// of the most significant one-bit in the specified integer value.
function VHighestOneBit(S: Int32): Int32; overload;
function VHighestOneBit(U: UInt32): UInt32; overload;

// Checks if the given integer is a power of two.
function VIsPowerOfTwo(S: Int32): Boolean; overload;
function VIsPowerOfTwo(U: UInt32): Boolean; overload;

// Return an integer value with at most a single one-bit, in the position
// of the least significant one-bit in the given integers value.
function VLowestOneBit(S: Int32): Int32; overload;
function VLowestOneBit(U: UInt32): UInt32; overload;

// Return the number of leading (high order) zero-bits (excluding the sign bit) of
// the given integers.
function VNumberOfLeadingZeros(U: UInt16): Integer; overload;
function VNumberOfLeadingZeros(S: Int32): Integer; overload;
function VNumberOfLeadingZeros(U: UInt32): Integer; overload;
function VNumberOfLeadingZeros(S: Int64): Integer; overload;
function VNumberOfLeadingZeros(U: UInt64): Integer; overload;

// Return the number of trailing (low order) zero-bits of the given integers.
function VNumberOfTrailingZeros(U: UInt32): Integer; overload;
function VNumberOfTrailingZeros(U: UInt64): Integer; overload;

// Reverse the bits of the given integers.
function VReverse(U: UInt8): UInt8; overload;
function VReverse(U: UInt16): UInt16; overload;
function VReverse(S: Int32): Int32; overload;
function VReverse(U: UInt32): UInt32; overload;

// Reverse the bytes of the given integers.
function VReverseBytes(S: Int32): Int32; overload;
function VReverseBytes(U: UInt32): UInt32; overload;

// Rotate the given integers left by Distance bits.
function VRotateLeft(S: Int32; Distance: Integer): Int32; overload;
function VRotateLeft(U: UInt32; Distance: Integer): UInt32; overload;

// Rotate the given integers right by Distance bits.
function VRotateRight(S: Int32; Distance: Integer): Int32; overload;
function VRotateRight(U: UInt32; Distance: Integer): UInt32; overload;

// Returns the sign of the integer: -1 for negative, 0 for zero and 1 for positive.
function VSign(S: Int32): TValueSign;

// Return a binary representation of the given integers.
function VToBinaryString(S: Int32): string; overload;
function VToBinaryString(U: UInt32): string; overload;

// Return a hexadecimal representation of the given integers.
function VToHexString(S: Int32): string; overload;
function VToHexString(U: UInt32): string; overload;

// Return an octal representation of the given integers.
function VToOctalString(S: Int32): string; overload;
function VToOctalString(U: UInt32): string; overload;

// Return a string representation of the given integers, in the given numerical base.
function VToString(S: Int32; Base: Byte): string; overload;
function VToString(U: UInt32; Base: Byte): string; overload;
function VToString(S: Int32): string; overload;
function VToString(U: UInt32): string; overload;

// Compare the given integers and return -1 for less, 0 for equal and 1 for greater.
function VCompare(Left, Right: Int32): Integer; overload;
function VCompare(Left, Right: UInt32): Integer; overload;
function VCompare(Left, Right: Int64): Integer; overload;
function VCompare(Left, Right: UInt64): Integer; overload;

// Calculate a hash code for the given integers.
function VHashCode(Value: Int32): UInt32; overload;
function VHashCode(Value: UInt32): UInt32; overload;
function VHashCode(Value: Int64): UInt32; overload;
function VHashCode(Value: UInt64): UInt32; overload;


// from unit Velthuis.FloatUtils;

{ Note: in newer versions of Delphi, most of these functions are superceded by functions in the
        record helpers for floating point types. This unit is made to make provide the
        functions for several older versions as well.
}

function IsNegativeInfinity(const AValue: Single): Boolean; overload;
function IsNegativeInfinity(const AValue: Double): Boolean; overload;
function IsNegativeInfinity(const AValue: Extended): Boolean; overload;

function IsPositiveInfinity(const AValue: Single): Boolean; overload;
function IsPositiveInfinity(const AValue: Double): Boolean; overload;
function IsPositiveInfinity(const AValue: Extended): Boolean; overload;

function GetSignificand(const AValue: Single): UInt32; overload;
function GetSignificand(const AValue: Double): UInt64; overload;
function GetSignificand(const AValue: Extended): UInt64; overload;
function GetMantissa(const AValue: Single): UInt32; overload;
function GetMantissa(const AValue: Double): UInt64; overload;
function GetMantissa(const AValue: Extended): UInt64; overload;

function GetExponent(const AValue: Single): Integer; overload;
function GetExponent(const AValue: Double): Integer; overload;
function GetExponent(const AValue: Extended): Integer; overload;

function IsDenormal(const AValue: Single): Boolean; overload;
function IsDenormal(const AValue: Double): Boolean; overload;
// function IsDenormal(const AValue: Extended): Boolean; overload;

function MakeSingle(Sign: TValueSign; Significand: UInt32; Exponent: Integer): Single;
function MakeDouble(Sign: TValueSign; Significand: UInt64; Exponent: Integer): Double;
function MakeExtended(Sign: TValueSign; Significand: UInt64; Exponent: Integer): Extended;

type
{ PUInt8  = ^UInt8;
  PUInt16 = ^UInt16;
  PUInt32 = ^UInt32;
  PUInt64 = ^UInt64;  }

  PExt80Rec = ^TExt80Rec;
  TExt80Rec = packed record
    Significand: UInt64;
    ExponentAndSign: Word;
  end;

const
  CSingleExponentShift   = 23;
  CDoubleExponentShift   = 52;
  CSingleExponentMask    = $FF;
  CDoubleExponentMask    = $7FF;
  CExtendedExponentMask  = $7FFF;
  CSingleBias            = CSingleExponentMask shr 1;
  CDoubleBias            = CDoubleExponentMask shr 1;
  CExtendedBias          = CExtendedExponentMask shr 1;
  CSingleSignificandMask = UInt32(1) shl CSingleExponentShift - 1;
  CDoubleSignificandMask = UInt64(1) shl CDoubleExponentShift - 1;
  CSingleSignMask        = UInt32(1) shl 31;
  CDoubleSignMask        = UInt64(1) shl 63;

// from unit Velthuis.StrConsts;

resourcestring
  SErrorParsingFmt         = '''%s'' is not a valid %s value';
  SDivisionByZero          = 'Division by zero';
  SOverflow                = 'Resulting value too big to represent';
  SOverflowFmt             = '%s: Resulting value too big to represent';
  SInvalidOperation        = 'Invalid operation';
  SConversionFailedFmt     = '%s value too large for conversion to %s';
  SInvalidArgumentFloatFmt = '%s parameter may not be NaN or +/- Infinity';
  SInvalidArgumentBase     = 'Base parameter must be in the range 2..36';
  SInvalidArgumentFmt      = 'Invalid argument: %s';
  SOverflowInteger  = 'Value %g cannot be converted to an integer ratio';
  SNegativeRadicand        = '%s: Negative radicand not allowed';
  SNoInverse               = 'No modular inverse possible';
  SNegativeExponent        = 'Negative exponent %s not allowed';
  SUnderflow               = 'Resulting value too small to represent';
  SRounding                = 'Rounding necessary';
  SExponent                = 'Exponent to IntPower outside the allowed range';
  SZeroDenominator         = 'BigRational denominator cannot be zero';

// from unit Velthuis.BigInteger;

// --- User settings ---

//------------------------------------------------------------------------------------------------------------------//
// Setting PUREPASCAL forces the use of plain Object Pascal for all routines, i.e. no assembler is used.            //
//------------------------------------------------------------------------------------------------------------------//
  { $DEFINE PUREPASCAL}
//------------------------------------------------------------------------------------------------------------------//
// Setting RESETSIZE forces the Compact routine to shrink the dynamic array when that makes sense.                  //
// This can slow down code a little.                                                                                //
//------------------------------------------------------------------------------------------------------------------//
  { $DEFINE RESETSIZE}
//------------------------------------------------------------------------------------------------------------------//
// If set, none of the public methods modifies the instance it is called upon.                                      //
// If necessary, a new instance is returned.                                                                        //
//------------------------------------------------------------------------------------------------------------------//
  {$DEFINE BIGINTEGERIMMUTABLE}
//------------------------------------------------------------------------------------------------------------------//
// EXPERIMENTAL is set for code that tries something new without deleting the original code yet.                    //
// Undefine it to get the original code.                                                                            //
//------------------------------------------------------------------------------------------------------------------//
  { $DEFINE EXPERIMENTAL}

{$IFDEF BIGINTEGERIMMUTABLE}
  {$UNDEF RESETSIZE}
{$ENDIF}

// --- Permanent settings ---

{$OPTIMIZATION ON}
{$STACKFRAMES OFF}
{$INLINE ON}
{$LEGACYIFEND ON}
{$CODEALIGN 16}
{$ALIGN 16}
{$IF SizeOf(Extended) > SizeOf(Double)}
  {$DEFINE HasExtended}
{$IFEND}
{$IF NOT DECLARED(PAnsiChar)}
  {$DEFINE NoAnsi}
{$IFEND}

// Assembler is only supplied for Windows targets. For other targets, PUREPASCAL must be defined.
{$IF not defined(PUREPASCAL) and not defined(MSWINDOWS)}
  {$DEFINE PUREPASCAL}
{$IFEND}

const
{$IFDEF PUREPASCAL}
  PurePascal = True;
{$ELSE}
  PurePascal = False;
{$ENDIF}

{$IFDEF EXPERIMENTAL}
  ExperimentalCode = True;
{$ELSE}
  ExperimentalCode = False;
{$ENDIF}

  // This assumes an unroll factor of 4. Unrolling more (e.g. 8) does not improve performance anymore.
  // That was tested and removed again.
  CUnrollShift     = 2;
  CUnrollIncrement = 1 shl CUnrollShift;
  CUnrollMask      = CUnrollIncrement - 1;

type
  TNumberBase = 2..36;                          // Number base or radix.

{$IF not declared(TRandom32Proc)}
  TRandom32Proc = function: UInt32;
  TRandomizeProc = procedure(NewSeed: UInt64);
{$IFEND}

  PLimb = ^TLimb;                               // Knuth calls them "limbs".
  TLimb = type UInt32;                          // FWIW, I also like the recently spotted term "bigit".
  TMagnitude = TArray<TLimb>;                   // These BigIntegers use sign-magnitude format, hence the name.

  // BigInteger uses a sign-magnitude representation, i.e. the magnitude is always interpreted as an
  // unsigned big integer, while the sign bit represents the sign. Currently, the sign bit is stored as the
  // top bit of the FSize member.

  PBigInteger = ^BigInteger;
  BigInteger = record
  public
  {$REGION 'public constants, types and variables'}
    type
      /// <summary>TRoundingMode governs which rounding mode is used to convert from Double to BigInteger.</summary>
      /// <param name="rmTruncate">Truncates any fraction</param>
      /// <param name="rmSchool">Rounds any fraction >= 0.5 away from zero</param>
      /// <param name="rmRound">Rounds any fraction > 0.5 away from zero</param>
      TRoundingMode = (rmTruncate, rmSchool, rmRound);

      TNumberBaseInfo = record
        MaxPower: NativeUInt;
        MaxDigits: Integer;
        PowerOfTwo: Boolean;
        MaxFactor: UInt32;
      end;

    class var
      MinusOne: BigInteger;
      Zero: BigInteger;
      One: BigInteger;
      Ten: BigInteger;

    const
{$IFDEF BIGINTEGERIMMUTABLE}
      Immutable    = True;
{$ELSE}
      Immutable    = False;
{$ENDIF}

      CapacityMask = High(Integer) - 3; // Mask ensuring that FData lengths are a multiple of 4, e.g. $7FFFFFFC
      SizeMask     = High(Integer);     // Mask to extract size part of FSize member, e.g. $7FFFFFFF
      SignMask     = Low(Integer);      // Mask to extract sign bit of FSize member, e.g. $80000000

  {$IFDEF PUREPASCAL}
    {$IFDEF CPU64BITS}                                          // 64PP = 64 bit, Pure Pascal
      KaratsubaThreshold             =   80;    // Checked
      ToomCook3Threshold             =  272;    // Checked
      BurnikelZieglerThreshold       =   91;    // Checked
      BurnikelZieglerOffsetThreshold =    5;    // Unchecked
      KaratsubaSqrThreshold          =   48;    // Unchecked
    {$ELSE CPU32BITS}                                           // 32PP = 32 bit, Pure Pascal
      KaratsubaThreshold             =   40;    // Checked
      ToomCook3Threshold             =  144;    // Checked
      BurnikelZieglerThreshold       =   91;    // Checked
      BurnikelZieglerOffsetThreshold =    5;    // Unchecked
      KaratsubaSqrThreshold          =   48;    // Unchecked
    {$ENDIF CPU64BITS}
  {$ELSE !PUREPASCAL}
    {$IFDEF CPU64BITS}                                          // 64A  = 64 bit, Assembler
      KaratsubaThreshold             =  128;    // Checked
      ToomCook3Threshold             = 1024;    // Checked
      BurnikelZieglerThreshold       =  160;    // Checked
      BurnikelZieglerOffsetThreshold =   80;    // Unchecked
      KaratsubaSqrThreshold          =  256;    // Unchecked
    {$ELSE CPU32BITS}                                           // 32A  = 32 bit, Assembler
      KaratsubaThreshold             =   64;    // Checked
      ToomCook3Threshold             =  256;    // Checked
      BurnikelZieglerThreshold       =   80;    // Checked
      BurnikelZieglerOffsetThreshold =   40;    // Unchecked
      KaratsubaSqrThreshold          =  128;    // Unchecked
    {$ENDIF CPU64BITS}
  {$ENDIF PUREPASCAL}

      RecursiveToStringThreshold     =    4;    // Checked
      ToomCook3SqrThreshold          =  216;    // Unchecked
  {$ENDREGION}

  {$REGION 'public methods'}

    // -- Constructors --

    /// <summary>Initializes class variables before first use.</summary>
    class constructor Initialize;

    /// <summary>Creates a new BigInteger from the data in limbs and the sign specified in Negative.</summary>
    /// <param name="Limbs">data for the magnitude of the BigInteger. The data is interpreted as unsigned,
    ///   and comes low limb first.</param>
    /// <param name="Negative">Indicates if the BigInteger is negative.</param>
    constructor Create(const Limbs: array of TLimb; Negative: Boolean); overload;

    /// <summary>Creates a new BigInteger from the data in limbs and the sign specified in Negative.</summary>
    /// <param name="Magnitude">data for the magnitude of the BigInteger. The data is interpreted as unsigned,
    ///   and comes low limb first.</param>
    /// <param name="Negative">Indicates if the BigInteger is negative.</param>
    constructor Create(const Magnitude: TMagnitude; Negative: Boolean); overload;

    /// <summary>Creates a new BigInteger with the same value as the specified BigInteger.</summary>
    constructor Create(const Value: BigInteger); overload;

    /// <summary>Creates a new BigInteger with the value of the specified Integer.<summary>
    constructor Create(const Value: Int32); overload;

    /// <summary>Creates a new BigInteger with the value of the specified Cardinal.<summary>
    constructor Create(const Value: UInt32); overload;

    /// <summary>Creates a new BigInteger with the value of the specified 64 bit integer.<summary>
    constructor Create(const Value: Int64); overload;

    /// <summary>Creates a new BigInteger with the value of the specified Integer.<summary>
    constructor Create(const Value: UInt64); overload;

    /// <summary>Creates a new BigInteger with the integer value of the specified Double.</summary>
    constructor Create(const Value: Double); overload;

  {$IFNDEF NoAnsi}
    /// <summary>Creates a new BigInteger with the value of the specified string.</summary>
    constructor Create(const Value: PAnsiChar); overload;
  {$ENDIF}

    /// <summary>Creates a new BigInteger with the value of the specified string.</summary>
    constructor Create(const Value: PWideChar); overload;

    /// <summary>Creates a new BigInteger from the value in the byte array.
    /// The byte array is considered to be in two's complement.</summary>
    /// <remarks>This is the complementary function of ToByteArray</remarks>
    constructor Create(const Bytes: array of Byte); overload;

    /// <summary>Creates a new random BigInteger of the given size. Uses the given IRandom to
    ///   generate the random value.</summary>
//    constructor Create(NumBits: Integer; const Random: IRandom); overload;

    /// <summary>Creates a new random BigInteger of the given size. Uses the given Random32Proc function to
    ///   generate the random value.</summary>
    constructor Create(NumBits: Integer; Random: TRandom32Proc); overload;


    // -- Global numeric base related functions --

    /// <summary>Sets the global numeric base for big integers to 10.</summary>
    /// <remarks>The global numeric base is used for input or output if there is no override in the input string or
    ///   the output function.</remarks>
    class procedure Decimal; static;

    /// <summary>Sets the global numeric base for big integers to 16.</summary>
    /// <remarks>The global numeric base is used for input or output if there is no override in the input string or
    ///   the output function.</remarks>
    class procedure Hexadecimal; static;

    /// <summary>Sets the global numeric base for big integers to 16.</summary>
    /// <remarks>The global numeric base is used for input or output if there is no override in the input string or
    ///   the output function.</remarks>
    class procedure Hex; static;

    /// <summary>Sets the global numeric base for big integers to 2.</summary>
    /// <remarks>The global numeric base is used for input or output if there is no override in the input string or
    ///   the output function.</remarks>
    class procedure Binary; static;

    /// <summary>Sets the global numeric base for big integers to 8.</summary>
    /// <remarks>The global numeric base is used for input or output if there is no override in the input string or
    ///   the output function.</remarks>
    class procedure Octal; static;


    // -- String input functions --

    /// <summary>Tries to parse the specified string into a valid BigInteger value in the specified numeric base.
    ///   Returns False if this failed.</summary>
    /// <param name="S">The string that represents a big integer value in the specified numeric base.</param>
    /// <param name="ABase">The numeric base that is assumed when parsing the string. Valid values are 2..36.</param>
    /// <param name="AValue">The resulting BigInteger, if the parsing succeeds. AValue is undefined if the
    ///   parsing fails.</param>
    /// <returns>Returns True if S could be parsed into a valid BigInteger in AVaLue. Returns False on failure.</returns>
    class function TryParse(const S: string; ABase: TNumberBase; var AValue: BigInteger): Boolean; overload; static;

    // -------------------------------------------------------------------------------------------------------------//
    // Note: most of the parse format for BigIntegers was taken from or inspired by Common Lisp (e.g. '%nnR' or     //
    // '_'), some was inspired by other languages, including Delphi (e.g. the '$ 'for hex values), some was         //
    // something I prefer (e.g. '0k' additional to '0o' for octal format). It should be usable in Delphi as well    //
    // as in C++Builder, as it contains the default formats for integer values in these languages too.              //
    // -- Rudy Velthuis.                                                                                            //
    //--------------------------------------------------------------------------------------------------------------//

    /// <summary>Tries to parse the specified string into a valid BigInteger value in the default BigInteger
    ///   numeric base.</summary>
    /// <param name="S">The string that represents a big integer value in the default numeric base, unless
    ///   specified otherwise. See <see cref="BigInteger.Base" /></param>
    /// <param name="Value">The resulting BigInteger, if the parsing succeeds. Value is undefined if the parsing
    ///   fails.</param>
    /// <returns>Returns True if S could be parsed into a valid BigInteger in Res. Returns False on failure.</returns>
    /// <remarks>
    ///   <para>To make it easier to increase the legibility of large numbers, any '_' in the numeric string
    ///      will completely be ignored, so '1_000_000_000' is exactly equivalent to '1000000000'.</para>
    ///   <para>The string to be parsed is considered case insensitive, so '$ABC' and '$abc' represent exactly
    ///     the same value.</para>
    ///   <para>The format of a string to be parsed is as follows:</para>
    ///   <para><c>[sign][base override]digits</c></para>
    ///   <para>
    ///     <param name="sign">This can either be '-' or '+'. It will make the BigInteger negative or
    ///       positive, respectively. If no sign is specified, a positive BigInteger is generated.</param>
    ///     <param name="base override">There are several ways to override the default numeric base.
    ///       <para>Specifying '0x' or '$' here will cause the string to be interpreted as representing a
    ///       hexadecimal (base 16) value.</para><para>Specifying '0b' will cause it to be interpreted as
    ///       binary (base 2).</para><para>Specifying '0d' will cause it to be interpreted as
    ///       decimal (base 10).</para>
    ///       <para>Specifying '0o' or '0k' will cause it to be interpreted as octal (base 8).</para>
    ///       <para>Finally, to specify any base,
    ///       using an override in the format '%nnR' (R for radix) will cause the number to be interpreted to be
    ///       in base 'nn', where 'nn' represent one or two decimal digits. So '%36rRudyVelthuis' is a valid
    ///       BigInteger value with base 36.</para>
    ///     </param>
    ///   </para>
    /// </remarks>
    class function TryParse(const S: string; var Value: BigInteger): Boolean; overload; static;

    /// <summary>Parses the specified string into a BigInteger, using the default numeric base.</summary>
    class function Parse(const S: string): BigInteger; static;


    // -- Sign related functions --

    /// <summary>Returns True if the BigInteger is zero.</summary>
    function IsZero: Boolean; inline;

    /// <summary>Returns True if the BigInteger is negative (&lt; 0).</summary>
    function IsNegative: Boolean; inline;

    /// <summary>Returns True if the BigInteger is positive (&gt; 0).</summary>
    function IsPositive: Boolean; inline;

    /// <summary>Returns True if the BigInteger is even (0 is considered even too).</summary>
    function IsEven: Boolean; inline;

    /// <summary>Returns True if the magnitude of the BigInteger value is exactly a power of two.</summary>
    function IsPowerOfTwo: Boolean;

    /// <summary>Returns True if the BigInteger represents a value of 1.</summary>
    function IsOne: Boolean;


    // -- Bit fiddling --

    /// <summary>Tests if the bit at the given bit index is set.</summary>
    /// <remarks>If the index is outside the magnitude, the bit value is calculated: if the BigInteger is
    /// negative, it is assumed to be set, otherwise it is assumed to be clear.</remarks>
    function TestBit(Index: Integer): Boolean;

    /// <summary>Returns a new BigInteger which is a copy of the current one, but with the bit at the given index
    /// set. If necessary, the new BigInteger is expanded.</summary>
    function SetBit(Index: Integer): BigInteger;

    /// <summary>Returns a new BigInteger which is a copy of the current one, but with the bit at the given index
    /// cleared. If necessary, the new BigInteger is expanded.</summary>
    function ClearBit(Index: Integer): BigInteger;

    /// <summary>Returns a new BigInteger which is a copy of the current one, but with the bit at the given index
    /// toggled. If necessary, the new BigInteger is expanded.</summary>
    function FlipBit(Index: Integer): BigInteger;


    // -- String output functions --

    /// <summary>Returns the string interpretation of the specified BigInteger in the default numeric base,
    ///   see <see cref="BigInteger.Base" />.
    /// </summary>
    function ToString: string; overload;

    /// <summary>Returns the string interpretation of the specified BigInteger in the specified numeric base.</summary>
    function ToString(Base: Integer): string; overload;

    /// <summary>Old, slow, but secure routine.</summary>
    /// <remarks>This should only be used for debugging purposes. May be removed anytime.
    /// For regular code, use <c>ToString(Base)</c>.</remarks>
    function ToStringClassic(Base: Integer): string;

    /// <summary>Returns the string interpretation of the specified BigInteger in numeric base 10. Equivalent
    ///   to ToString(10).</summary>
    function ToDecimalString: string;

    /// <summary>Returns the string interpretation of the specified BigInteger in numeric base 16. Equivalent
    ///   to ToString(16).</summary>
    function ToHexString: string;

    /// <summary>Returns the string interpretation of the specified BigInteger in numeric base 2. Equivalent
    ///   to ToString(2).</summary>
    function ToBinaryString: string;

    /// <summary>Returns the string interpretation of the specified BigInteger in numeric base 8. Equivalent
    ///   to ToString(8).</summary>
    function ToOctalString: string;


    // -- Arithmetic operators --

    /// <summary>Adds two BigIntegers.</summary>
    class operator Add(const Left, Right: BigInteger): BigInteger;

    /// <summary>Subtracts the second BigInteger from the first.</summary>
    class operator Subtract(const Left, Right: BigInteger): BigInteger;

    /// <summary>Multiplies two BigIntegers.</summary>
    class operator Multiply(const Left, Right: BigInteger): BigInteger;

    /// <summary>Multiplies the specified BigInteger with the specified Word value.</summary>
    class operator Multiply(const Left: BigInteger; Right: Word): BigInteger;

    /// <summary>multiplies the specified Wirdvalue with the specified BigInteger.</summary>
    class operator Multiply(Left: Word; const Right: BigInteger): BigInteger;

    /// <summary>Performs an integer divide of the first BigInteger by the second.
    class operator IntDivide(const Left, Right: BigInteger): BigInteger;

    /// <summary>Performs an integer divide of the first BigInteger by the second.
    class operator IntDivide(const Left: BigInteger; Right: UInt16): BigInteger;

    /// <summary>Performs an integer divide of the first BigInteger by the second.
    class operator IntDivide(const Left: BigInteger; Right: UInt32): BigInteger;

    /// <summary>Returns the remainder of an integer divide of the first BigInteger by the second.</summary>
    class operator Modulus(const Left, Right: BigInteger): BigInteger;

    /// <summary>Returns the remainder of an integer divide of the first BigInteger by the second.</summary>
    class operator Modulus(const Left: BigInteger; Right: UInt32): BigInteger;

    /// <summary>Returns the remainder of an integer divide of the first BigInteger by the second.</summary>
    class operator Modulus(const Left: BigInteger; Right: UInt16): BigInteger;

    /// <summary>Unary minus. Negates the value of the specified BigInteger.</summary>
    class operator Negative(const Value: BigInteger): BigInteger;

{$IFDEF BIGINTEGERIMMUTABLE}
  private
{$ENDIF}
    /// <summary>Increment. Adds 1 to the value of the specified BigInteger very fast.</summary>
    class operator Inc(const Value: BigInteger): BigInteger;

    /// <summary>Decrement. Subtracts 1 from the value of the specified BigInteger very fast.</summary>
    class operator Dec(const Value: BigInteger): BigInteger;
{$IFDEF BIGINTEGERIMMUTABLE}
  public
{$ENDIF}

    // -- Logical and bitwise operators --

    /// <summary>Returns the result of the bitwise AND operation on its BigInteger operands. The result
    /// has two's complement semantics, e.g. '-1 and 7' returns '7'.</summary>
    class operator BitwiseAnd(const Left, Right: BigInteger): BigInteger;

    /// <summary>Returns the result of the bitwise OR operation on its BigInteger operands. The result
    /// has two's complement semantics, e.g. '-1 or 7' returns '-1'.</summary>
    class operator BitwiseOr(const Left, Right: BigInteger): BigInteger;

    /// <summary>Returns the result of the bitwise XOR operation on its BigIntegers operands. The result
    /// has two's complement semantics, e.g. '-1 xor 7' returns '-8'.</summary>
    class operator BitwiseXor(const Left, Right: BigInteger): BigInteger;

    /// <summary>Returns the result of the bitwise NOT operation on its BigInteger operand. The result
    /// has two's complement semantics, e.g. 'not 1' returns '-2'.</summary>
    class operator LogicalNot(const Value: BigInteger): BigInteger;


    // -- Shift operators --

    /// <summary>Shifts the specified BigInteger value the specified number of bits to the left (away from 0).
    ///   The size of the BigInteger is adjusted accordingly.</summary>
    /// <remarks>Note that this is an arithmetic shift, i.e. the sign is preserved. This is unlike normal
    ///   integer shifts in Delphi.</remarks>
    class operator LeftShift(const Value: BigInteger; Shift: Integer): BigInteger;

    /// <summary>Shifts the specified BigInteger value the specified number of bits to the right (toward 0).
    ///   The size of the BigInteger is adjusted accordingly.</summary>
    /// <remarks>Note that this is an arithmetic shift, i.e. the sign is preserved. This is unlike normal
    ///   integer shifts in Delphi. This means that negative values do not finally end up as 0, but
    ///   as -1, since the sign bit is always shifted in.</remarks>
    class operator RightShift(const Value: BigInteger; Shift: Integer): BigInteger;


    // -- Comparison operators --

    /// <summary>Returns True if the specified BigIntegers have the same value.</summary>
    class operator Equal(const Left, Right: BigInteger): Boolean;

    /// <summary>Returns True if the specified BigInteger do not have the same value.</summary>
    class operator NotEqual(const Left, Right: BigInteger): Boolean;

    /// <summary>Returns true if the value of Left is mathematically greater than the value of Right.</summary>
    class operator GreaterThan(const Left, Right: BigInteger): Boolean;

    /// <summary>Returns true if the value of Left is mathematically greater than or equal to the value
    ///   of Right.</summary>
    class operator GreaterThanOrEqual(const Left, Right: BigInteger): Boolean;

    /// <summary>Returns true if the value of Left is mathematically less than the value of Right.</summary>
    class operator LessThan(const Left, Right: BigInteger): Boolean;

    /// <summary>Returns true if the value of Left is mathematically less than or equal to the
    ///   value of Right.</summary>
    class operator LessThanOrEqual(const Left, Right: BigInteger): Boolean;


    // -- Implicit conversion operators --

    /// <summary>Implicitly (i.e. without a cast) converts the specified Integer to a BigInteger.</summary>
    class operator Implicit(const Value: Int32): BigInteger;

    /// <summary>Implicitly (i.e. without a cast) converts the specified Cardinal to a BigInteger.</summary>
    class operator Implicit(const Value: UInt32): BigInteger;

    /// <summary>Implicitly (i.e. without a cast) converts the specified Int64 to a BigInteger.</summary>
    class operator Implicit(const Value: Int64): BigInteger;

    /// <summary>Implicitly (i.e. without a cast) converts the specified UInt64 to a BigInteger.</summary>
    class operator Implicit(const Value: UInt64): BigInteger;

    /// <summary>Implicitly (i.e. without a cast) converts the specified string to a BigInteger. The BigInteger
    ///   is the result of a call to Parse(Value).</summary>
    class operator Implicit(const Value: string): BigInteger;

  {$IFNDEF NoAnsi}
    /// <summary>Implicitly (i.e. without a cast) converts the specified string to a BigInteger. The BigInteger
    ///   is the result of a call to Parse(Value).</summary>
    /// <remark>Added for compatibility with C++Builder.</remark>
    class operator Implicit(const Value: PAnsiChar): BigInteger;
  {$ENDIF}

    /// <summary>Implicitly (i.e. without a cast) converts the specified string to a BigInteger. The BigInteger
    ///   is the result of a call to Parse(Value).</summary>
    /// <remark>Added for compatibility with C++Builder.</remark>
    class operator Implicit(const Value: PWideChar): BigInteger;


    // -- Explicit conversion operators --

    /// <summary>Explicitly (i.e. with a cast) converts the specified BigInteger to an Integer. If necessary, the
    ///   value of the BigInteger is truncated or sign-extended to fit in the result.</summary>
    class operator Explicit(const Value: BigInteger): Int32;

    /// <summary>Explicitly (i.e. with a cast) converts the specified BigInteger to a Cardinal. If necessary, the
    ///   value of the BigInteger is truncated to fit in the result.</summary>
    class operator Explicit(const Value: BigInteger): UInt32;

    /// <summary>Explicitly (i.e. with a cast) converts the specified BigInteger to an Int64. If necessary, the
    ///   value of the BigInteger is truncated or sign-extended to fit in the result.</summary>
    class operator Explicit(const Value: BigInteger): Int64;

    /// <summary>Explicitly (i.e. with a cast) converts the specified BigInteger to an UInt64. If necessary, the
    ///   value of the BigInteger is truncated to fit in the result.</summary>
    class operator Explicit(const Value: BigInteger): UInt64;

  {$IFDEF HasExtended}
    /// <summary>Explicitly (i.e. with a cast) converts the specified BigInteger to an Extended.</summary>
    class operator Explicit(const Value: BigInteger): Extended;
  {$ENDIF}

    /// <summary>Explicitly (i.e. with a cast) converts the specified BigInteger to a Double.</summary>
    class operator Explicit(const Value: BigInteger): Double;

    /// <summary>Explicitly (i.e. with a cast) converts the specified BigInteger to a Single.</summary>
    class operator Explicit(const Value: BigInteger): Single;

    /// <summary>Explicitly (i.e. with a cast) converts the specified Double to a BigInteger.</summary>
    class operator Explicit(const Value: Double): BigInteger;

    /// <summary>Explicitly (i.e. with a cast) converts the specified BigInteger to a string.</summary>
    /// <remarks>Calls Value.ToString to generate the result.</remarks>
    class operator Explicit(const Value: BigInteger): string;


    // -- Conversion functions --

    /// <summary>Converts the specified BigInteger to a Single, if this is possible. Returns an infinity if the
    ///   value of the BigInteger is too large.</summary>
    function AsSingle: Single;

    /// <summary>Converts the specified BigInteger to a Double, if this is possible. Returns an infinity if the
    ///   value of the BigInteger is too large.</summary>
    function AsDouble: Double;

  {$IFDEF HasExtended}
    /// <summary>Converts the specified BigInteger to an Extended, if this is possible. Returns an infinity if the
    ///   value of the BigInteger is too large.</summary>
    function AsExtended: Extended;
  {$ENDIF}

    /// <summary>Converts the specified BigInteger to an Integer, if this is possible. Returns an exception if the
    ///   value of the BigInteger is too large.</summary>
    function AsInteger: Integer;

    /// <summary>Converts the specified BigInteger to a Cardinal, if this is possible. Returns an exception if the
    ///   value of the BigInteger is too large or is negative.</summary>
    function AsCardinal: Cardinal;

    /// <summary>Converts the specified BigInteger to an Int64, if this is possible. Returns an exception if the
    ///   value of the BigInteger is too large.</summary>
    function AsInt64: Int64;

    /// <summary>Converts the specified BigInteger to a UInt64, if this is possible. Returns an exception if the
    ///   value of the BigInteger is too large or is negative.</summary>
    function AsUInt64: UInt64;


    // -- Operators as functions --

    /// <summary>The function equivalent to the operator '+'.</summary>
    class function Add(const Left, Right: BigInteger): BigInteger; overload; static;
    class procedure Add(const Left, Right: BigInteger; var Result: BigInteger); overload; static;

    /// <summary>The function equivalent to the operator '-'.</summary>
    class function Subtract(const Left, Right: BigInteger): BigInteger; overload; static;
    class procedure Subtract(const Left, Right: BigInteger; var Result: BigInteger); overload; static;

    /// <summary>The function equivalent to the operator '*'.</summary>
    class function Multiply(const Left, Right: BigInteger): BigInteger; overload; static;
    class procedure Multiply(const Left, Right: BigInteger; var Result: BigInteger); overload; static;

    /// <summary>Function performing "schoolbook" multiplication.</summary>
    class procedure MultiplyBaseCase(const Left, Right: BigInteger; var Result: BigInteger); static;

    /// <summary>Function performing multiplcation using Karatsuba algorithm. Has more overhead, so only
    ///  applied to large BigIntegers.</summary>
    class procedure MultiplyKaratsuba(const Left, Right: BigInteger; var Result: BigInteger); static;

    /// <summary>Function performing multiplication using Toom-Cook 3-way algorithm. Faster than Karatsuba, but,
    /// due to its overhead, only for very large BigIntegers.</summary>
    class function MultiplyToomCook3(const Left, Right: BigInteger): BigInteger; static;

    /// <summary>The function equivalent to the operators 'div' and 'mod'. Since calculation of the quotient
    ///   automatically leaves a remainder, this function allows you to get both for more or less the "price"
    ///   (performance-wise) of one.</summary>
    class procedure DivMod(const Dividend, Divisor: BigInteger; var Quotient, Remainder: BigInteger); static;

    /// <summary>Simple "schoolbook" division according to Knuth, with limb-size digits.</summary>
    class procedure DivModKnuth(const Left, Right: BigInteger; var Quotient, Remainder: BigInteger); static;

    /// <summary>Recursive "schoolbook" division, as described by Burnikel and Ziegler. Faster than
    /// <see cref="DivModKnuth" />, but with more overhead, so should only be applied for
    ///   larger BigIntegers.</summary>
    /// <remark>For smaller BigIntegers, this routine falls back to DivModKnuth.
    class procedure DivModBurnikelZiegler(const Left, Right: BigInteger; var Quotient, Remainder: BigInteger); static;

    /// <summary>The function equivalent to the operator 'div'.</summary>
    class function Divide(const Left, Right: BigInteger): BigInteger; overload; static;

    /// <summary>The function equivalent to the operator 'div'.</summary>
    class function Divide(const Left: BigInteger; Right: UInt16): BigInteger; overload; static;

    /// <summary>The function equivalent to the operator 'div'.</summary>
    class function Divide(const Left:BigInteger; Right: UInt32): BigInteger; overload; static;

    /// <summary>The function equivalent to the operator 'mod'. Like for integers, the remainder gets
    ///   the sign - if any - of the dividend (i.e. of Left).</summary>
    class function Remainder(const Left, Right: BigInteger): BigInteger; overload; static;

    /// <summary>The function equivalent to the operator 'mod'. Like for integers, the remainder gets
    ///   the sign - if any - of the dividend (i.e. of Left).</summary>
    class function Remainder(const Left: BigInteger; Right: UInt32): BigInteger; overload; static;

    /// <summary>The function equivalent to the operator 'mod'. Like for integers, the remainder gets
    ///   the sign - if any - of the dividend (i.e. of Left).</summary>
    class function Remainder(const Left: BigInteger; Right: UInt16): BigInteger; overload; static;

    class function SqrKaratsuba(const Value: BigInteger): BigInteger; static;

    /// <summary>Returns the negation of Value.</summary>
    class function Negate(const Value: BigInteger): BigInteger; static;

    /// <summary>The procedural equivalent of the operator 'shl'.</summary>
    class procedure ShiftLeft(const Value: BigInteger; Shift: Integer; var Result: BigInteger); overload; static;
    /// <summary>The function equivalent of the operator 'shl'.</summary>
    class function ShiftLeft(const Value: BigInteger; Shift: Integer): BigInteger; overload; static;

    /// <summary>The procedural equivalent of the operator 'shr'.</summary>
    class procedure ShiftRight(const Value: BigInteger; Shift: Integer; var Result: BigInteger); overload; static;
    /// <summary>The function equivalent of the operator 'shr'.</summary>
    class function ShiftRight(const Value: BigInteger; Shift: Integer): BigInteger; overload; static;

    // -- Self-referential operator functions --

  {$IFNDEF BIGINTEGERIMMUTABLE}
    /// <summary>
    ///   <para>The functional equivalent to</para>
    ///   <code>    A := A + Other;</code>
    ///   <para>This can be chained, as the function returns a pointer to itself:</para>
    ///   <code>    A.Add(First).Add(Second);</code></summary>
    /// <remarks><para>This was added in the hope to gain speed by avoiding some allocations.
    ///   This is not so, although a longer chain seems to improve performance, compared to normal addition
    ///   using operators, a bit.</para></remarks>
    function Add(const Other: BigInteger): PBigInteger; overload;

    /// <summary>The functional equivalent to Self := Self + Other;</summary>
    function Subtract(const Other: BigInteger): PBigInteger; overload;

    /// <summary>The functional equivalent to Self := Self div Other;</summary>
    function Divide(const Other: BigInteger): PBigInteger; overload;

    /// <summary>The functional equivalent to Self := Self mod Other;</summary>
    function Remainder(const Other: BigInteger): PBigInteger; overload;

    /// <summar>The functional equivalent to Self := Self * Other;</summary>
    function Multiply(const Other: BigInteger): PBigInteger; overload;
  {$ENDIF}


    // -- Math functions --

    /// <summary>Returns the absolute value of the value in the BigInteger.</summary>
    class function Abs(const Value: BigInteger): BigInteger; overload; static;

    /// <summary>Returns the absolute value of the current BigInteger.<summary>
    function Abs: BigInteger; overload;

    /// <summary>Returns the predecessor of the current BigInteger, i.e. its value minus one.</summary>
    function Pred: BigInteger; overload;

    // <summary>Returns the successor of the current BigInteger, i.e. its value plus one.</summary>
    function Succ: BigInteger; overload;

    /// <summary>Returns the bit length, the minimum number of bits needed to represent the value, excluding
    ///   the sign bit.</summary>
    function BitLength: Integer;

    /// <summary>Returns the number of all bits that are set, assuming two's complement. The sign bit is
    ///   included in the count.</summary>
    function BitCount: Integer;

    /// <summary>Returns the index of the rightmost (lowest) bit set. The lowest bit has index 0. Returns -1 if
    ///   this BigInteger is zero. </summary>
    function LowestSetBit: Integer;

    /// <summary>Returns a copy of the current BigInteger, with a unique copy of the data.</summary>
    function Clone: BigInteger;

    /// <summary>Returns +1 if the value in Left is greater than the value in Right, 0 if they are equal and
    ///   1 if it is lesser.</summary>
    class function Compare(const Left, Right: BigInteger): Integer; static;

    /// <summary>Returns N!, i.e. N * (N - 1) * (N - 2) * ... * 2 as BigInteger.
    class function Factorial(N: Integer): BigInteger; static;

    /// <summary>Returns a single Fibonacci number; 0 --> 0; 1 --> 1; N --> F(N-1) + F(N-2)</summary>
    class function Fibonacci(N: Integer): BigInteger; static;

    /// <summary>Returns the (positive) greatest common divisor of the specified BigInteger values.</summary>
    class function GreatestCommonDivisor(const Left, Right: BigInteger): BigInteger; static;

    /// <summary>Returns the natural logarithm of the BigInteger value.</summary>
    class function Ln(const Value: BigInteger): Double; overload; static;

    /// <summary>Returns the natural logarithm of the current BigInteger.</summary>
    function Ln: Double; overload;

    /// <summary>Returns the logarithm to the specified base of the BigInteger value.</summary>
    class function Log(const Value: BigInteger; Base: Double): Double; overload; static;

    /// <summary>Returns the logarithm to the specified base of the current BigInteger.</summary>
    function Log(Base: Double): Double; overload;

    /// <summary>Returns the logarithm to base 2 of the BigInteger value.</summary>
    class function Log2(const Value: BigInteger): Double; overload; static;

    /// <summary>Returns the logarithm to base 2 of the current BigInteger.</summary>
    function Log2: Double; overload;

    /// <summary>Returns the logarithm to base 10 of the BigInteger value.</summary>
    class function Log10(const Value: BigInteger): Double; overload; static;

    /// <summary>Returns the logarithm to base 10 of the current BigInteger.</summary>
    function Log10: Double; overload;

    /// <summary>The reverse of BigInteger.Ln. Returns e^Value, for very large Value, as BigInteger
    class function Exp(const b: Double): BigInteger; static;

    /// <summary>Returns the larger of two specified values.</summary>
    class function Max(const Left, Right: BigInteger): BigInteger; static;

    /// <summary>Returns the smaller of two specified values.</summary>
    class function Min(const Left, Right: BigInteger): BigInteger; static;

    /// <summary>Returns the modular inverse of Value mod Modulus.</summary>
    /// <exception>Returns an exception if there is no modular inverse.</exception>
    class function ModInverse(const Value, Modulus: BigInteger): BigInteger; static;

    /// <summary>Returns the specified modulus value of the specified value raised to the specified power.</summary>
    class function ModPow(const ABase, AExponent, AModulus: BigInteger): BigInteger; static;

    /// <summary>Returns the specified value raised to the specified power.</summary>
    class function Pow(const ABase: BigInteger; AExponent: Integer): BigInteger; overload; static;

    /// <summary>Returns the specified value raised to the spefied power in Result,</summary>
    class procedure Pow(const ABase: BigInteger; AExponent: Integer; var Result: BigInteger); overload; static;

    /// <summary>Returns the nth root R of a BigInteger such that R^index <= Radicand < (R+1)^index.</summary>
    class function NthRoot(const Radicand: BigInteger; Index: Integer): BigInteger; static;

    /// <summary>If R is the nth root of Radicand, returns Radicand - R^index.</summary>
    class procedure NthRootRemainder(const Radicand: BigInteger; Index: Integer;
      var Root, Remainder: BigInteger); static;

    /// <summary>Returns the square root R of Radicand, such that R^2 < Radicand < (R+1)^2</summary>
    class function BaseCaseSqrt(const Radicand: BigInteger): BigInteger; static;

    /// <summary>If R is the square root of Radicand, returns Radicand - R^2.</summary>
    class procedure BaseCaseSqrtRemainder(const Radicand: BigInteger; var Root, Remainder: BigInteger); static;

    /// <summary>Returns the square root R of the radicand, such that R^2 < radicand < (R+1)^2.</summary>
    class function Sqrt(const Radicand: BigInteger): BigInteger; static;

    /// <summary>Returns square root and remainder of the radicand.</summary>
    class procedure SqrtRemainder(const Radicand: BigInteger; var Root, Remainder: BigInteger); static;

    /// <summary>Returns the square of Value, i.e. Value*Value</summary>
    class function Sqr(const Value: BigInteger): BigInteger; static;


    // -- Utility functions --

    /// <summary>Sets whether partial-flags stall must be avoided with modified routines.</summary>
    /// <remarks>
    ///   <para><b>USING THE WRONG SETTING MAY AFFECT THE TIMING OF CERTAIN ROUTINES CONSIDERABLY, SO USE
    ///   THIS WITH EXTREME CARE!</b></para>
    ///   <para>The unit is usually able to determine the right settings automatically.</para>
    /// </remarks>
    class procedure AvoidPartialFlagsStall(Value: Boolean); static;

    // -- Array function(s) --

    /// <summary>Converts a BigInteger value to a byte array.</summary>
    /// <returns><para>A TArray&lt;Byte&gt;, see remarks.</para></returns>
    /// <remarks>
    ///   <para>The individual bytes in the array returned by this method appear in little-endian order.</para>
    ///   <para>Negative values are written to the array using two's complement representation in the most compact
    ///   form possible. For example, -1 is represented as a single byte whose value is $FF instead of as an array
    ///   with multiple elements, such as $FF, $FF or $FF, $FF, $FF, $FF.</para>
    ///   <para>Because two's complement representation always interprets the highest-order bit of the last byte in
    ///   the array (the byte at position High(Array)) as the sign bit, the method returns a byte array with
    ///   an extra element whose value is zero to disambiguate positive values that could otherwise be interpreted
    ///   as having their sign bits set. For example, the value 120 or $78 is represented as a single-byte array:
    ///   $78. However, 129, or $81, is represented as a two-byte array: $81, $00. Something similar applies to
    ///   negative values: -179 (or -$B3) must be represented as $4D, $FF.</para>
    /// </remarks>
    function ToByteArray: TArray<Byte>;

    // -- Information functions --

    /// <summary>Returns the number of allocated limbs for the current BigInteger.</summary>
    function GetAllocated: Integer;

    /// <summary>Returns the number of used limbs for the current BigInteger.</summary>
    function GetSize: Integer; inline;

    /// <summary>Returns a pointer to the first limb of the magnitude.</summary>
    function Data: PLimb; inline;

    /// <summary>Returns the sign for the current BigInteger: -1 for negative values, 0 for zero and 1 for
    /// positive values.</summary>
    function GetSign: Integer; inline;

    /// <summary>Sets the sign of the current BigInteger: -1 for negative values, 0 for zero and 1 for
    /// positive values.</summary>
    procedure SetSign(Value: Integer); inline;
  {$ENDREGION}

  private
  {$REGION 'private constants, types and variables'}
    type
      TErrorCode = (ecParse, ecDivByZero, ecConversion, ecInvalidBase, ecOverflow, ecInvalidArg, ecInvalidArgFloat, ecNoInverse,
                    ecNegativeExponent, ecNegativeRadicand);
      TBinaryOperator = procedure(Left, Right, Result: PLimb; LSize, RSize: Integer);
    var
      // The limbs of the magnitude, least significant limb at lowest address.
      FData: TMagnitude;
      // The top bit is the sign bit. Other bits form the unsigned number of valid limbs of the magnitude.
      FSize: Integer;
    class var
      // The currently actual (global) number base.
      FBase: TNumberBase;
      // Flag indicating need to test for partial flag stall.
      FAvoidStall: Boolean;
      // The current rounding mode.
      FRoundingMode: TRoundingMode;

      // The internal functions used to add and subtract. These differ depending on the need to avoid
      // a partial flag stall.
      FInternalAdd: TBinaryOperator;
      FInternalSubtract: TBinaryOperator;
      FLog2: Double;
  {$ENDREGION}

  {$REGION 'private functions'}
  {$IFNDEF PUREPASCAL}
    // Function detecting of current CPU could suffer from partial flag stall.
    class procedure DetectPartialFlagsStall; static;

    // Internal function adding two magnitudes. Contains code to avoid a partial flag stall.
    class procedure InternalAddModified(Left, Right, Result: PLimb; LSize, RSize: Integer); static;
    // Internal function adding two magnitudes. Does not contain code to avoid partial flag stall.
    class procedure InternalAddPlain(Left, Right, Result: PLimb; LSize, RSize: Integer); static;
    // Internal function subtracting two magnitudes. Contains code to avoid a partial flag stall.
    class procedure InternalSubtractModified(Larger, Smaller, Result: PLimb; LSize, SSize: Integer); static;
    // Internal func9tion subtracting two magnitudes. Does not contain code to avoid a partial flag stall.
    class procedure InternalSubtractPlain(Larger, Smaller, Result: PLimb; LSize, SSize: Integer); static;
    // Internal perfect division by 3 (guaranteed that there is no remainder).
    class procedure InternalDivideBy3(Value, Result: PLimb; ASize: Integer); static;
    // Internal function dividing magnitude by 100, in-place. Leaves quotient in place, returns remainder.
    class function InternalDivMod100(var X: NativeUInt): NativeUInt; static;
    // Function performing int to string conversion, writing to WritePtr.
    class procedure InternalIntToStrDecimal(const Value: NativeUInt; var WritePtr: PChar; MaxDigits: Integer); static;
    // Function calculating floating point components out of a BigInteger.
  {$ELSE}
    // Internal function adding two magnitudes. Pure Pascal (non-assembler) implementation.
    class procedure InternalAddPurePascal(Left, Right, Result: PLimb; LSize, RSize: Integer); static;
    // Internal function subtracting two magnitudes. Pure Pascal (non-assembler) implementation.
    class procedure InternalSubtractPurePascal(Larger, Smaller, Result: PLimb; LSize, SSize: Integer); static;
  {$ENDIF}
    class procedure ConvertToFloatComponents(const Value: BigInteger; SignificandSize: Integer;
      var Sign: Integer; var Significand: UInt64; var Exponent: Integer); static;
    // Internal function comparing two magnitudes.
    class function InternalCompare(Left, Right: PLimb; LSize, RSize: Integer): Integer; static; {$IFDEF PUREPASCAL} inline; {$ENDIF}
    // Internal function and-ing two magnitudes.
    class procedure InternalAnd(Left, Right, Result: PLimb; LSize, RSize: Integer); static;
    // Internal function or-ing two magnitudes.
    class procedure InternalOr(Left, Right, Result: PLimb; LSize, RSize: Integer); static;
    // Internal funciton xor-ing two magnitudes.
    class procedure InternalXor(Left, Right, Result: PLimb; LSize, RSize: Integer); static;
    // Internal function and-not-ing two magnitudes (Left^ and not Right^).
    class procedure InternalAndNot(Left, Right, Result: PLimb; LSize, RSize: Integer); static;
    // Internal function not-and-ing two magnitudes (not Left^ and Right^).
    class procedure InternalNotAnd(Left, Right, Result: PLimb; LSize, RSize: Integer); static; inline;
    // Internal function performing bitwise operations. The bitwise operations share similar code.
    class procedure InternalBitwise(const Left, Right: BigInteger; var Result: BigInteger;
      PlainOp, OppositeOp, InversionOp: TBinaryOperator); static;
    // Internal function icrementing a magnitude by one, in-place.
    class procedure InternalIncrement(Limbs: PLimb; Size: Integer); static;
    // Internal function decrementing a magnitude by one, in-place.
    class procedure InternalDecrement(Limbs: PLimb; Size: Integer); static;
    // Internal function parsing a decimal string into a BigInteger. Returns False if string not valid.
    class function InternalParseDecimal(P: PChar; var Value: BigInteger): Boolean; static;
    // Internal function parsing a hex string into a BigInteger. Returns False if string not valid.
    class function InternalParseHex(P: PChar; var Value: BigInteger): Boolean; static;
    // Internal function shifting a magnitude left into a new magnitude.
    class procedure InternalShiftLeft(Source, Dest: PLimb; Shift, Size: Integer); static;
    // Internal function shifting a magnitude right into a new magnitude.
    class procedure InternalShiftRight(Source, Dest: PLimb; Shift, Size: Integer); static;
    // Internal function performing int to string function for given numeric base.
    class procedure InternalIntToStrBase(const Value: NativeUInt; Base: Cardinal;
      var WritePtr: PChar; MaxDigits: Integer); static;
    // Internal function performing int to string conversion for bases 2, 4, and 16, doing simple shifts.
    class procedure InternalShiftedToString(const Value: BigInteger; Base: Integer; var WritePtr: PChar); static;
    // Internal function performing int to string conversion, repeatedly dividing by 10 (simple algorithm).
    class procedure InternalPlainToString(const Value: BigInteger; Base: Integer; const BaseInfo: TNumberBaseInfo;
      var WritePtr: PChar; SectionCount: Integer); static;
    // Internal function performing int to string conversion, using recursive divide-and-conquer algorithm.
    class procedure InternalRecursiveToString(const Value: BigInteger; Base: Integer; const BaseInfo: TNumberBaseInfo;
      var WritePtr: PChar; SectionCount: Integer); static;
    // Internal function performing division of two magnitudes, returning quotient and remainder.
    class function InternalDivMod(Dividend, Divisor, Quotient, Remainder: PLimb;
      LSize, RSize: Integer): Boolean; static;
    // Internal function performing division of magnitude by 32 bit integer.
    class function InternalDivMod32(Dividend: PLimb; Divisor: UInt32; Quotient, Remainder: PLimb;
      LSize: Integer): Boolean; static;
    // Internal function performing division of magnitude by 16 bit integer (needed for Pure Pascal division).
    class function InternalDivMod16(Dividend: PLimb; Divisor: UInt16; Quotient, Remainder: PLimb;
      LSize: Integer): Boolean; static;
    // performs a Knuth divmod. Does not compare magnitudes. Called by DivModKnuth.
    class procedure UncheckedDivModKnuth(const Left, Right: BigInteger; var Quotient, Remainder: BigInteger); static;
    // Internal function multiplying two magnitudes.
    class procedure InternalMultiply(Left, Right, Result: PLimb; LSize, RSize: Integer); static;
    // Internal function dividing magnitude by given base value. Leaves quotient in place, returns remainder.
    class function InternalDivideByBase(Mag: PLimb; Base: Integer; var Size: Integer): UInt32; static;
    // Internal function multiplying by 16 bit integer and then adding 16 bit value. Used by parser.
    class procedure InternalMultiply16(const Left: TMagnitude; var Result: TMagnitude; LSize: Integer; Right: Word); static;
    // Internal function multiplying by a base and adding a digit. Condition: ADigit < ABase. Size is updated if necessary.
    // Cf. code of TryParse on how to set up Value.
    class procedure InternalMultiplyAndAdd16(Value: PLimb; ABase, ADigit: Word; var Size: Integer); static;

    // Internal function negating magnitude (treating it as two's complement).
    class procedure InternalNegate(Source, Dest: PLimb; Size: Integer); static;

    // Burnikel-Ziegler and helper functions.
    // Divides two magnitudes using Burnikel-Ziegler algorithm.
    class procedure InternalDivModBurnikelZiegler(const Left, Right: BigInteger;
      var Quotient, Remainder: BigInteger); static;
    // Divides a BigInteger by 3 exactly. BigInteger is guaranteed to be a positive multiple of 3.
    class function DivideBy3Exactly(const A: BigInteger): BigInteger; static;
    // Helper function for Burnikel-Ziegler division. See explanation in implementation section.
    class procedure DivThreeHalvesByTwo(const LeftUpperMid, LeftLower, Right, RightUpper: BigInteger;
      const RightLower: BigInteger;
      N: Integer; var Quotient, Remainder: BigInteger); static;
    // Helper function for Burnikel-Ziegler division.
    class procedure DivTwoDigitsByOne(const Left, Right: BigInteger; N: Integer;
      var Quotient, Remainder: BigInteger); static;

    // Karatsuba and Toom-Cook helper function
    // Split BigInteger into smaller BigIntegers of size BlockSize.
    function Split(BlockSize, BlockCount: Integer): TArray<BigInteger>;

    // Sets global numeric base.
    class procedure SetBase(const Value: TNumberBase); static;
    // Raises exceptions depending on given error code.
    class procedure Error(ErrorCode: TErrorCode; const ErrorInfo: array of const); static;

    class procedure Compact(var Data: TMagnitude; var Size: Integer); overload; static;
    // Resets size thus that there are no leading zero limbs.
    procedure Compact; overload; inline;
    // Reallocates magnitude to ensure a given size.
    procedure EnsureSize(RequiredSize: Integer);
    // Creates a new magnitude.
    procedure MakeSize(RequiredSize: Integer);
  {$ENDREGION}

  public
  {$REGION 'public properties'}
    /// <summary>Number of valid limbs in the magnitude</summary>
    property Size: Integer read GetSize;
    /// <summary>Number of allocated limbs in the mangitude</summary>
    property Allocated: Integer read GetAllocated;
    /// <summary>Indicates whether BigInteger is negative</summary>
    property Negative: Boolean read IsNegative;
    /// <summary>The sign of the BigInteger: -1, 0 or 1</summary>
    property Sign: Integer read GetSign write SetSign;
    /// <summary>Magnitude, dynamic array of TLimb, containing the (unsigned) value of the BigInteger</summary>
    property Magnitude: TMagnitude read FData;

    /// <summary>Global numeric base for BigIntegers</summary>
    class property Base: TNumberBase read FBase write SetBase;
    /// <summary>A pure alias for Base</summary>
    class property Radix: TNumberBase read FBase write SetBase;
    /// <summary>Global rounding mode used for conversion to floating point</summary>
    class property RoundingMode: TRoundingMode read FRoundingMode write FRoundingMode;
    /// <summary>Global flag indicating if partial flag stall is avoided</summary>
    class property StallAvoided: Boolean read FAvoidStall;
  {$ENDREGION}

  end;

/// <summary>Returns sign bit (top bit) of an integer.</summary>
function SignBitOf(Value: Integer): Integer; inline;

var
  // Set this to True if you want to generate debug output.
  DoDebug: Boolean = True;

{$HPPEMIT END '#include "Velthuis.BigIntegers.operators.hpp"'}

{ *************************************************************************** }
{                                                                             }
{ IPTypes  -  www.nldelphi.com Open Source Delphi runtime library             }
{                                                                             }
{ Initiator: Albert de Weerd (aka NGLN)                                       }
{ License: Free to use, free to modify                                        }
{ Website: None                                                               }
{ SVN path: http://svn.nldelphi.com/nldelphi/opensource/ngln/                 }
{                                                                             }
{ *************************************************************************** }
{                                                                             }
{ Last edit by: Albert de Weerd                                               }
{ Date: August 27, 2010                                                       }
{ Version: 1.2                                                                }
{                                                                             }
{ Modified by Vitaly Yakovlev                                                 }
{ Website: http://optinsoft.net/                                              }
{ Date: October 22, 2019                                                      }
{                                                                             }
{ *************************************************************************** }

const
  IPv4BitSize = SizeOf(Byte) * 4 * 8;
  IPv6BitSize = SizeOf(Word) * 8 * 8;
  DefPortNumber = 80;
  DefProtocol = 'http';

type
  T4 = 0..3;
  T8 = 0..7;
  TIPv4ByteArray = array[T4] of Byte;
  TIPv6WordArray = array[T8] of Word;
  TIPv6CardinalArray = array[T4] of Cardinal;

  TIPv4 = packed record
    case Integer of
      0: (D, C, B, A: Byte);
      1: (Groups: TIPv4ByteArray);
      2: (Value: Cardinal);
  end;

  TIPv6 = packed record
    case Integer of
      0: (H, G, F, E, D, C, B, A: Word);
      1: (Groups: TIPv6WordArray);
      2: (Values: TIPv6CardinalArray);
  end;

  TCharCase = (ccUpperCase, ccLowerCase);

  EIPv4Error = class(Exception);
  EIPv6Error = class(Exception);
(*
function IsValidIPv4(const S: String): Boolean; //Vitaly 22.10.2019
function StrToIPv4(const S: String): TIPv4;
function TryStrToIPv4(const S: String; ARaiseErrors: Boolean; out AValidIP: Boolean): TIPv4; //Vitaly 22.10.2019

function IPv4Compare(const AIPv41, AIPv42: TIPv4): Integer;
procedure IPv4ToBits(const AIPv4: TIPv4; ABits: TBits);
function IPv4ToIPv6(const AIPv4: TIPv4): TIPv6;
function IPv4ToStr(const AIPv4: TIPv4): String;
function IPv4ToStrHex(const AIPv4: TIPv4): String;
function IPv4ToStrOutwr(const AIPv4: TIPv4): String;
function IPv4ToURL(const AIPv4: TIPv4; const Protocol: String = DefProtocol;
  const PortNumber: Word = DefPortNumber): String;

function IsValidIPv6(const S: String): Boolean; //Vitaly 22.10.2019
function StrToIPv6(const S: String): TIPv6;
function TryStrToIPv6(const S: String; ARaiseErrors: Boolean; out AValidIP: Boolean): TIPv6; //Vitaly 22.10.2019

function IPv6Compare(const AIPv61, AIPv62: TIPv6): Integer;
procedure IPv6ToBits(const AIPv6: TIPv6; ABits: TBits);
function IPv6ToIPv4(const AIPv6: TIPv6): TIPv4;
function IPv6ToStr(const AIPv6: TIPv6): String;
function IPv6ToStrCompr(const AIPv6: TIPv6): String;
function IPv6ToStrOutwr(const AIPv6: TIPv6): String;
function IPv6ToURL(const AIPv6: TIPv6; const Protocol: String = DefProtocol;
  const PortNumber: Word = DefPortNumber): String;

function IPv6AddOp(const Left, Right: TIPv6): TIPv6;
function IPv6AndOp(const Left, Right: TIPv6): TIPv6;
function IPv6OrOp(const Left, Right: TIPv6): TIPv6;
function IPv6SubtractOp(const Left, Right: TIPv6): TIPv6;
function IPv6XorOp(const Left, Right: TIPv6): TIPv6;
*)

const
  ZeroIPv4: TIPv4 = (D: 0; C: 0; B: 0; A: 0);
  ZeroIPv6: TIPv6 = (H: 0; G: 0; F: 0; E: 0; D: 0; C: 0; B: 0; A: 0);

var
  IPCharCase: TCharCase = ccUpperCase;


{ **************************************************************************** }
{                                                                              }
{ uMMDBReader - Delphi reader for the MaxMind DB file format                   }
{                                                                              }
{ Created by Vitaly Yakovlev                                                   }
{ Date: October 22, 2019                                                       }
{ Copyright: (c) 2019 Vitaly Yakovlev                                          }
{ Website: http://optinsoft.net/                                               }
{                                                                              }
{ License: BSD 2-Clause License.                                               }
{                                                                              }
{ Redistribution and use in source and binary forms, with or without           }
{ modification, are permitted provided that the following conditions are met:  }
{                                                                              }
{ 1. Redistributions of source code must retain the above copyright notice,    }
{    this list of conditions and the following disclaimer.                     }
{                                                                              }
{ 2. Redistributions in binary form must reproduce the above copyright notice, }
{    this list of conditions and the following disclaimer in the documentation }
{    and/or other materials provided with the distribution.                    }
{                                                                              }
{ THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  }
{ AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,        }
{ THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR       }
{ PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR            }
{ CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,        }
{ EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,          }
{ PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;  }
{ OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,     }
{ WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR      }
{ OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF       }
{ ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                                   }
{                                                                              }
{ Last edit by: Vitaly Yakovlev                                                }
{ Date: March 07, 2024                                                         }
{ Version: 1.3                                                                 }
{                                                                              }
{ Changelog:                                                                   }
{                                                                              }
{ v1.3:                                                                        }
{ - added debug exceptions:                                                    }
{     EMMDBException,                                                          }
{     EMMDBNotFoundException                                                   }
{ - TMMDBReader.Find: T raises EMMDBNotFoundException when ipAddress not found }
{                                                                              }
{ v1.2:                                                                        }
{ - added debug macros: DEBUG_OUT, DEBUG_OUT_FILE, DEBUG_OUT_FILTER            }
{ - changed implementation of function TMMDBDecoder.Decode:                    }
{   call "Create" for classes; that allows to make arrays and dictionaries of  }
{   objects.                                                                   }
{                                                                              }
{ v1.1:                                                                        }
{ - added debug parameter: const keyPath: String                               }
{                                                                              }
{ v1.0:                                                                        }
{ - Initial release                                                            }
{                                                                              }
{ **************************************************************************** }

type
  TMMDBIPAddress = record
  public
    constructor Create(ipv4: TIPv4); overload;
    constructor Create(ipv6: TIPv6); overload;
    constructor Create(const address: TBytes); overload;
    function GetAddressBytes: TBytes;
    class function TryParse(const ipString: String; out address: TMMDBIPAddress): Boolean; static;
    class function Parse(const ipString: String): TMMDBIPAddress; static;
    function ToString(): String;
  private
    case _v6: Boolean of
      False: (_ipv4: TIPv4);
      True:  (_ipv6: TIPv6);
  end;

type
  TMMDBBuffer = class
  private
    _Length: Integer;
  public
    property Length: Integer read _Length;
  protected
    constructor Create(length: Integer);
  public
    function Read(offset: Int64; count: Integer): TBytes; virtual; abstract;
    function ReadString(offset: Int64; count: Integer): String; virtual; abstract;
    function ReadOne(offset: Int64): Byte; virtual; abstract;
    procedure Copy(offset: Int64; arr: TBytes); virtual; abstract;
    function ReadBigInteger(offset: Int64; size: Integer): BigInteger;
    function ReadDouble(offset: Int64): Double;
    function ReadFloat(offset: Int64): Single;
    function ReadInteger(val: Integer; offset: Int64; size: Integer): Integer;
    function ReadLong(offset: Int64; size: Integer): Int64;
    function ReadULong(offset: Int64; size: Integer): UInt64;
  end;

  TMMDBDecoder = class
  private
    _pointerBase: Int64;
    _database: TMMDBBuffer;
    _followPointers: Boolean;
  public
    type ObjectType =
    (
      otExtended,
      otPointer,
      otUtf8String,
      otDouble,
      otBytes,
      otUint16,
      otUint32,
      otMap,
      otInt32,
      otUint64,
      otUint128,
      otArray,
      otContainer,
      otEndMarker,
      otBoolean,
      otFloat
    );
    constructor Create(ownerObjects: TObjectList; database: TMMDBBuffer; pointerBase: Int64; followPointers: Boolean = True);
    function Decode<T>(offset: Int64; out outOffset: Int64): T; overload;
    procedure Decode<T>(offset: Int64; out outOffset: Int64; var tResult: T); overload;
  private
    function Decode(expectedType: PTypeInfo; offset: Int64; out outOffset: Int64{$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF}): TValue; overload;
    procedure Decode(expectedType: PTypeInfo; offset: Int64; out outOffset: Int64; var valResult: TValue{$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF}); overload;
    function CtrlData(offset: Int64; out size: Integer; out outOffset: Int64): ObjectType;
    function DecodeByType(expectedType: PTypeInfo; _type: ObjectType; offset: Int64; size: Integer; out outOffset: Int64{$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF}): TValue; overload;
    procedure DecodeByType(expectedType: PTypeInfo; _type: ObjectType; offset: Int64; size: Integer; out outOffset: Int64; var valResult: TValue{$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF}); overload;
    function DecodeBoolean(expectedType: PTypeInfo; size: Integer): Boolean;
    function DecodeDouble(expectedType: PTypeInfo; offset: Int64; size: Integer): Double;
    function DecodeFloat(expectedType: PTypeInfo; offset: Int64; size: Integer): Single;
    function DecodeString(expectedType: PTypeInfo; offset: Int64; size: Integer): String;
    function DecodeBytes(expectedType: PTypeInfo; offset: Int64; size: Integer): TBytes;
    function DecodeMap(expectedType: PTypeInfo; offset: Int64; size: Integer; out outOffset: Int64{$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF}): TObject; overload;
    procedure DecodeMap(expectedType: PTypeInfo; offset: Int64; size: Integer; out outOffset: Int64; objResult: TObject{$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF}); overload;
    function DecodeMapToDictionary(expectedType: PTypeInfo; offset: Int64; size: Integer; out outOffset: Int64{$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF}): TObject; overload;
    procedure DecodeMapToDictionary(expectedType: PTypeInfo; offset: Int64; size: Integer; out outOffset: Int64; objResult: TObject{$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF}); overload;
    function DecodeMapToType(expectedType: PTypeInfo; offset: Int64; size: Integer; out outOffset: Int64{$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF}): TObject; overload;
    procedure DecodeMapToType(expectedType: PTypeInfo; offset: Int64; size: Integer; out outOffset: Int64; objResult: TObject{$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF}); overload;
    function DecodeKey(offset: Int64; out outOffset: Int64): TBytes;
    function DecodeLong(expectedType: PTypeInfo; offset: Int64; size: Integer): Int64;
    function DecodeArray(expectedType: PTypeInfo; size: Integer; offset: Int64; out outOffset: Int64{$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF}): TObject; overload;
    procedure DecodeArray(expectedType: PTypeInfo; size: Integer; offset: Int64; out outOffset: Int64; objResult: TObject{$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF}); overload;
    function DecodeUInt64(expectedType: PTypeInfo; offset: Int64; size: Integer): UInt64;
    function DecodeBigInteger(expectedType: PTypeInfo; offset: Int64; size: Integer): BigInteger;
    function DecodePointer(offset: Int64; size: Integer; out outOffset: Int64): Int64;
    function DecodeInteger(expectedType: PTypeInfo; offset: Int64; size: Integer): Integer;
    procedure CheckType(expected, from: PTypeInfo);
    function IsDictType(expected: PTypeInfo): Boolean;
    function GetInstanceTypeMethod(instance: TRttiInstanceType; const methodName: String;
      out method: TRttiMethod; paramCount: Integer = -1): Boolean;
    function NextValueOffset(offset: Int64; numberToSkip: Integer): Int64;
  private
    FContext: TRttiContext;
    FOwnerObjects: TObjectList;
  end;

  TMMDBAttribute = class(TCustomAttribute)
  private
    FName: String;
  public
    constructor Create(const AName: String);
    property Name: String read FName write FName;
  end;

  TMMDBMetadata = class
  private
    _BinaryFormatMajorVersion: Integer;
    _BinaryFormatMinorVersion: Integer;
    _BuildEpoch: UInt64;
    _DatabaseType: String;
    _Description: TDictionary<string, string>;
    _IPVersion: Integer;
    _Languages: TList<string>;
    _NodeCount: Int64;
    _RecordSize: Integer;
    function GetBuildDate: TDateTime; inline;
    function GetNodeByteSize: Int64; inline;
    function GetSearchTreeSize: Int64; inline;
  public
    constructor Create;
    destructor Destroy; override;
    //
    [TMMDBAttribute('binary_format_major_version')]
    property BinaryFormatMajorVersion: Integer read _BinaryFormatMajorVersion write _BinaryFormatMajorVersion;
    [TMMDBAttribute('binary_format_minor_version')]
    property BinaryFormatMinorVersion: Integer read _BinaryFormatMinorVersion write _BinaryFormatMinorVersion;
    [TMMDBAttribute('build_epoch')]
    property BuildEpoch: Uint64 read _BuildEpoch write _BuildEpoch;
    [TMMDBAttribute('database_type')]
    property DatabaseType: String read _DatabaseType write _DatabaseType;
    [TMMDBAttribute('description')]
    property Description: TDictionary<string, string> read _Description;
    [TMMDBAttribute('ip_version')]
    property IPVersion: Integer read _IPVersion write _IPVersion;
    [TMMDBAttribute('languages')]
    property Languages: TList<string> read _Languages;
    [TMMDBAttribute('node_count')]
    property NodeCount: Int64 read _NodeCount write _NodeCount;
    [TMMDBAttribute('record_size')]
    property RecordSize: Integer read _RecordSize write _RecordSize;
    //
    property BuildDate: TDateTime read GetBuildDate;
    property NodeByteSize: Int64 read GetNodeByteSize;
    property SearchTreeSize: Int64 read GetSearchTreeSize;
  end;

  TMMDBReader = class; //forward declaration

  TMMDBNetNode = class
  private
    _IPBytes: TBytes;
    _Bit: Integer;
    _Pointer: Integer;
  public
    constructor Create(byteCount: Integer; Pointer: Integer = 0); overload;
    constructor Create(IPBytes: TBytes; Bit, Pointer: Integer); overload;
    destructor Destroy; override;
    property IPBytes: TBytes read _IPBytes;
    property Bit: Integer read _Bit write _Bit;
    property Pointer: Integer read _Pointer write _Pointer;
  end;

  TMMDBIteratorNode<T> = class; //forward declaration

  IMMDBIterator<T> = interface(IInterface)
    function GetNode: TMMDBIteratorNode<T>;
    property Node: TMMDBIteratorNode<T> read GetNode;
  end;

  TMMDBIteratorNode<T> = class(TInterfacedObject, IMMDBIterator<T>)
  private
    _Start: TMMDBIPAddress;
    _Prefix: Integer;
    _Data: T;
  public
    constructor Create(data: T); overload;
    destructor Destroy; override;
    function GetNode: TMMDBIteratorNode<T>;
    property Start: TMMDBIPAddress read _Start;
    property Prefix: Integer read _Prefix;
    property Data: T read _Data;
  end;

  TMMDBEnumerator<T> = class(TInterfacedObject, IEnumerator<IMMDBIterator<T>>)
  private
    _reader: TMMDBReader;
    byteCount: Integer;
    nodes: TList<TMMDBNetNode>;
    node: TMMDBNetNode;
    _iterator: IMMDBIterator<T>;
    _dataCache: TDictionary<Integer, T>;
    function GetCurrentGeneric(): IMMDBIterator<T>;
  public
    constructor Create(reader: TMMDBReader; IPv4Only: Boolean;
      iterator: IMMDBIterator<T>; cacheSize: Integer);
    destructor Destroy; override;
    function getCurrent: TObject;
    function IEnumerator<IMMDBIterator<T>>.GetCurrent = GetCurrentGeneric;
    function MoveNext : boolean;
    procedure Reset;
  end;

  TMMDBEnumerable<T> = class(TInterfacedObject, IEnumerable<IMMDBIterator<T>>)
  private
    _reader: TMMDBReader;
    _IPv4Only: Boolean;
    _iterator: IMMDBIterator<T>;
    _cacheSize: Integer;
    function GetEnumeratorGeneric: IEnumerator<IMMDBIterator<T>>;
  public
    constructor Create(reader: TMMDBReader; IPv4Only: Boolean;
      iterator: IMMDBIterator<T>; cacheSize: Integer);
    destructor Destroy; override;
    function GetEnumerator: IEnumerator;
    function IEnumerable<IMMDBIterator<T>>.GetEnumerator = GetEnumeratorGeneric;
  end;

  TMMDBReader = class
  protected const
    DataSectionSeparatorSize: Integer = 16;
  protected
    _database: TMMDBBuffer;
    _fileName: String;
    _IPv4Start: Integer;
  protected
    function IPv4Start: Integer;
    function StartNode(bitLength: Integer): Integer;
    function FindMetadataStart: Integer;
    function ReadNode(nodeNumber, index: Integer): Integer;
    function ResolveDataPointer<T>(pointer: Integer): T; overload;
    procedure ResolveDataPointer<T>(pointer: Integer; var tResult: T); overload;
    function FindAddressInTree(const address: TMMDBIPAddress; out prefixLength: Integer): Integer;
  public
    constructor Create(const Filename: String); overload;
    constructor Create(Stream: TStream); overload;          { V9.5 }
    destructor Destroy; override;
    function Find<T>(const ipAddress: TMMDBIPAddress; out prefixLength: Integer): T; overload;
    function Find<T>(const ipAddress: TMMDBIPAddress; out prefixLength: Integer; var tResult: T): Boolean; overload;
    function FindAll<T>(IPv4Only: Boolean = False; cacheSize: Integer = 16384): IEnumerable<IMMDBIterator<T>>; overload;
    function FindAll<T>(data: T; IPv4Only: Boolean = False; cacheSize: Integer = 16384): IEnumerable<IMMDBIterator<T>>; overload;
  protected
    FMetadata: TMMDBMetadata;
    FDecoder: TMMDBDecoder;
  public
    property Metadata: TMMDBMetadata read FMetadata;
    property Decoder: TMMDBDecoder read FDecoder;
  private
    FMetaOwnerObjects: TObjectList;
    FFindOwnerObjects: TObjectList;
  end;

  EMMDBException = class(Exception);
  EMMDBNotFoundException = class(EMMDBException);

type
  TMMDBContinentInfo = class(TObject)
  private
    _code: String;
    _geoname_id: Int64;
  public
    [TMMDBAttribute('code')]
    property Code: String read _code write _code;
    [TMMDBAttribute('geoname_id')]
    property GeonameId: Int64 read _geoname_id write _geoname_id;
  end;

  TMMDBCountryInfo = class(TObject)
  private
    _geoname_id: Int64;
    _iso_code: String;
    _is_in_european_union: Boolean;
  public
    [TMMDBAttribute('geoname_id')]
    property GeonameId: Int64 read _geoname_id write _geoname_id;
    [TMMDBAttribute('iso_code')]
    property ISOCode: String read _iso_code write _iso_code;
    [TMMDBAttribute('is_in_european_union')]
    property IsInEuropeanUnion: Boolean read _is_in_european_union write _is_in_european_union;
  end;

  TMMDBRepresentedCountryInfo = class(TMMDBCountryInfo)
  private
    _type: String;
  public
    [TMMDBAttribute('type')]
    property RType: String read _type write _type;
  end;

  TMMDBTraitsInfo = class(TObject)
  private
    _is_anonymous_proxy: Boolean;
    _is_satellite_provider: Boolean;
  public
    [TMMDBAttribute('is_anonymous_proxy')]
    property IsAnonymousProxy: Boolean read _is_anonymous_proxy write _is_anonymous_proxy;
    [TMMDBAttribute('is_satellite_provider')]
    property IsSatelliteProvider: Boolean read _is_satellite_provider write _is_satellite_provider;
  end;

  TMMDBIPCountryInfo = class(TObject)
  private
    _continent: TMMDBContinentInfo;
    _country: TMMDBCountryInfo;
    _registered_country: TMMDBCountryInfo;
    _represented_country: TMMDBRepresentedCountryInfo;
    _traits: TMMDBTraitsInfo;
  public
    constructor Create;
    destructor Destroy; override;
    //
    [TMMDBAttribute('continent')]
    property Continent: TMMDBContinentInfo read _continent;
    [TMMDBAttribute('country')]
    property Country: TMMDBCountryInfo read _country;
    [TMMDBAttribute('registered_country')]
    property RegisteredCountry: TMMDBCountryInfo read _registered_country;
    [TMMDBAttribute('represented_country')]
    property RepresentedCountry: TMMDBRepresentedCountryInfo read _represented_country;
    [TMMDBAttribute('traits')]
    property Traits: TMMDBTraitsInfo read _traits;
  end;

  TMMDBCityInfo = class(TObject)
  private
    _geoname_id: Int64;
  public
    constructor Create;
    destructor Destroy; override;
    [TMMDBAttribute('geoname_id')]
    property GeonameId: Int64 read _geoname_id write _geoname_id;
  end;

  TMMDBLocation = class(TObject)
  private
    _accuracy_radius: Integer;
    _latitude: Double;
    _longitude: Double;
    _metro_code: Integer;
    _time_zone: String;
  public
    [TMMDBAttribute('accuracy_radius')]
    property Accuracy: Integer read _accuracy_radius write _accuracy_radius;
    [TMMDBAttribute('latitude')]
    property Latitude: Double read _latitude write _latitude;
    [TMMDBAttribute('longitude')]
    property Longitude: Double read _longitude write _longitude;
    [TMMDBAttribute('metro_code')]
    property MetroCode: Integer read _metro_code write _metro_code;
    [TMMDBAttribute('time_zone')]
    property TimeZone: String read _time_zone write _time_zone;
  end;

  TMMDBPostal = class(TObject)
  private
    _code: String;
  public
    [TMMDBAttribute('code')]
    property Code: String read _code write _code;
  end;

  TMMDBSubdivision = class(TMMDBCountryInfo)
  end;

  TMMDBIPCountryCityInfo = class(TMMDBIPCountryInfo)
  private
    _city: TMMDBCityInfo;
    _location: TMMDBLocation;
    _postal: TMMDBPostal;
    _subdivisions: TList<TMMDBSubdivision>;
  public
    constructor Create;
    destructor Destroy; override;
    //
    [TMMDBAttribute('city')]
    property City: TMMDBCityInfo read _city;
    [TMMDBAttribute('location')]
    property Location: TMMDBLocation read _location;
    [TMMDBAttribute('postal')]
    property Postal: TMMDBPostal read _postal;
    [TMMDBAttribute('subdivisions')]
    property Subdivisions: TList<TMMDBSubdivision> read _subdivisions;
  end;

  TMMDBContinentInfoEx = class(TMMDBContinentInfo)
  private
    _names: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;
    //
    [TMMDBAttribute('names')]
    property Names: TDictionary<string, string> read _names;
  end;

  TMMDBCountryInfoEx = class(TMMDBCountryInfo)
  private
    _names: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;
    //
    [TMMDBAttribute('names')]
    property Names: TDictionary<string, string> read _names;
  end;

  TMMDBRepresentedCountryInfoEx = class(TMMDBCountryInfoEx)
  private
    _type: String;
  public
    [TMMDBAttribute('type')]
    property RType: String read _type write _type;
  end;

  TMMDBIPCountryInfoEx = class(TObject)
  private
    _continent: TMMDBContinentInfoEx;
    _country: TMMDBCountryInfoEx;
    _registered_country: TMMDBCountryInfoEx;
    _represented_country: TMMDBRepresentedCountryInfoEx;
    _traits: TMMDBTraitsInfo;
  public
    constructor Create;
    destructor Destroy; override;
    //
    [TMMDBAttribute('continent')]
    property Continent: TMMDBContinentInfoEx read _continent;
    [TMMDBAttribute('country')]
    property Country: TMMDBCountryInfoEx read _country;
    [TMMDBAttribute('registered_country')]
    property RegisteredCountry: TMMDBCountryInfoEx read _registered_country;
    [TMMDBAttribute('represented_country')]
    property RepresentedCountry: TMMDBRepresentedCountryInfoEx read _represented_country;
    [TMMDBAttribute('traits')]
    property Traits: TMMDBTraitsInfo read _traits;
  end;

  TMMDBCityInfoEx = class(TMMDBCityInfo)
  private
    _names: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;
    //
    [TMMDBAttribute('names')]
    property Names: TDictionary<string, string> read _names;
  end;

  TMMDBSubdivisionEx = class(TMMDBCountryInfoEx)
{$IFDEF DEBUG_TMMDBSubdivision}
  public
    constructor Create;
    destructor Destroy; override;
{$ENDIF}
  end;

  TMMDBIPCountryCityInfoEx = class(TMMDBIPCountryInfoEx)
  private
    _city: TMMDBCityInfoEx;
    _location: TMMDBLocation;
    _postal: TMMDBPostal;
    _subdivisions: TObjectList<TMMDBSubdivisionEx>;
  public
    constructor Create;
    destructor Destroy; override;
    //
    [TMMDBAttribute('city')]
    property City: TMMDBCityInfoEx read _city;
    [TMMDBAttribute('location')]
    property Location: TMMDBLocation read _location;
    [TMMDBAttribute('postal')]
    property Postal: TMMDBPostal read _postal;
    [TMMDBAttribute('subdivisions')]
    property Subdivisions: TObjectList<TMMDBSubdivisionEx> read _subdivisions;
  end;

  TMMDBASN = class(TObject)
  private
    _autonomous_system_number: Int64;
    _autonomous_system_organization: String;
  public
    [TMMDBAttribute('autonomous_system_number')]
    property AutonomousSystemNumber: Int64 read _autonomous_system_number write _autonomous_system_number;
    [TMMDBAttribute('autonomous_system_organization')]
    property AutonomousSystemOrganization: String read _autonomous_system_organization write _autonomous_system_organization;
  end;

  TMMDBISP = class(TMMDBASN)
  private
    _isp: String;
    _organization: String;
  public
    [TMMDBAttribute('isp')]
    property ISP: String read _isp write _isp;
    [TMMDBAttribute('organization')]
    property Organization: String read _organization write _organization;
  end;

  TMMDBAnonymousIP = class(TObject)
  private
    _is_anonymous: Boolean;
    _is_anonymous_vpn: Boolean;
    _is_hosting_provider: Boolean;
    _is_public_proxy: Boolean;
    _is_residential_proxy: Boolean;
    _is_tor_exit_node: Boolean;
  public
    [TMMDBAttribute('is_anonymous')]
    property IsAnonymous: Boolean read _is_anonymous write _is_anonymous;
    [TMMDBAttribute('is_anonymous_vpn')]
    property IsAnonymousVPN: Boolean read _is_anonymous_vpn write _is_anonymous_vpn;
    [TMMDBAttribute('is_hosting_provider')]
    property IsHostingProvider: Boolean read _is_hosting_provider write _is_hosting_provider;
    [TMMDBAttribute('is_public_proxy')]
    property IsPublicProxy: Boolean read _is_public_proxy write _is_public_proxy;
    [TMMDBAttribute('is_residential_proxy')]
    property IsResidentialProxy: Boolean read _is_residential_proxy write _is_residential_proxy;
    [TMMDBAttribute('is_tor_exit_node')]
    property IsTorExitNode: Boolean read _is_tor_exit_node write _is_tor_exit_node;
  end;

  TMMDBDomain = class(TObject)
  private
    _domain: String;
  public
    [TMMDBAttribute('domain')]
    property Domain: String read _domain write _domain;
  end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

// ICS component

    TDBCityInfo = record    // from TMMDBIPCountryCityInfoEx class
        IpStr: String;
        IS0A2: String;
        CountryName: String;     // may be other language versions available
        CountryID: Integer;
        Continent: String;
        StateProv: String;
        City: String;
        GeoLat: Double;
        GeoLong: Double;
        Accuracy: Integer;
    end;

    TIcsCountry = record   // file "ICS-Countries.csv"
        ISOA2: String;
        Country: String;
        DialCode: String;
        AkaCountry: String;
        ISOA3: String;
        ISONUM: String;
        Internet: String;
        Region: Integer;     // major region, only five, constants earlier, ie IcsRegionEurope
        SubRegion: Integer;  // sub regions, 16, constants earlier, ie IcsRegionNorthEurope
        NumISO: Integer;
    end;
    PTIcsCountry = ^TIcsCountry;
    TIcsCountries = array of TIcsCountry;

    TIcsGeoTools = class(TComponent)
    private
        FCountries: TIcsCountries;
        FISOA2Idx: TIcsFindList ;
        FTotCountries: Integer;
        FCountryFile: String;
        FASNFile: String;
        FCityFile: String;
        FCountryFName: String;
        FUseResources: Boolean;
        FOnAppLog: TIcsAppLogEvent;
        FLoadIPDB: Boolean;
        FLoadCity: Boolean;
        FLoadASN: Boolean;
        FLoadCnty: Boolean;
        FMMDBCountry: TMMDBReader;
        FMMDBASN: TMMDBReader;
        FMMDBCity: TMMDBReader;
        FCountryInfo: TMMDBIPCountryInfo;
        FASNInfo: TMMDBASN;
        FCityInfo: TMMDBIPCountryCityInfoEx;
    public
        DBTot: Integer;
        DBDate: TDateTime;
        DBType: String;
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure AppLog(const Msg: string);
        procedure UnLoad;
        function Load: Boolean;
        function IsLoaded: Boolean;
        function LoadDBCountry: Boolean;
        function LoadDBASN: Boolean;
        function LoadDBCity: Boolean;
        function LoadIcsCountries: Boolean;
        function FindISOA2Code(ASockAddr: TSockAddrIn6; const IpStr: String): string; overload;
        function FindISOA2Code(const IpStr: String): string; overload;
        function FindISOA2Code(ASockAddr: TSockAddrIn6): string; overload;
        function FindASNCode(const IpStr: String; var OrgNum: Int64): string;
        function FindISO2ACity(const IpStr: String): TDBCityInfo;
        function FindNameRec(const ISOA2: String): TIcsCountry;
        function FindCountry(const ISOA2: String): string ;
        function FindRegion(ID: Integer): string ;
        property Countries: TIcsCountries read FCountries;
        property TotCountries: Integer read FTotCountries;
        property DBCountryFile: string read FCountryFile write FCountryFile;
        property DBASBFile: string read FASNFile write FASNFile;
        property DBCityFile: string read FCityFile write FCityFile;
        property CountryFName: String read FCountryFName write FCountryFName;
        property UseResources: Boolean read FUseResources write FUseResources;
        property OnAppLog: TIcsAppLogEvent read FOnAppLog write FOnAppLog;
    end;



implementation


// from unit Velthuis.Numerics;  with V added to most declarations to make unique

const
  // Currently not used.
  NLZDeBruijn32Mult = $07C4ACDD;
  NLZDeBruijn32: array[0..31] of Byte =
  (
    31, 22, 30, 21, 18, 10, 29,  2, 20, 17, 15, 13,  9,  6, 28,  1,
    23, 19, 11,  3, 16, 14,  7, 24, 12,  4,  8, 25,  5, 26, 27,  0
  );

  NTZDeBruijn32Mult = $077CB531;
  NTZDeBruijn32: array[0..31] of Byte =
  (
     0,  1, 28,  2, 29, 14, 24,  3, 30, 22, 20, 15, 25, 17,  4,  8,
    31, 27, 13, 23, 21, 19, 16,  7, 26, 12, 18,  6, 11,  5, 10,  9
  );

  BitCounts: array[0..15] of Byte = (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4);

function VBitCount(U: UInt8): Integer;
begin
  Result := BitCounts[U and $0F] + BitCounts[U shr 4];
end;

function VBitCount(U: UInt16): Integer;
{$IF DEFINED(WIN32)}
asm
        MOV     DX,AX
        SHR     DX,1
        AND     DX,$5555
        SUB     AX,DX
        MOV     DX,AX
        AND     AX,$3333
        SHR     DX,2
        AND     DX,$3333
        ADD     AX,DX
        MOV     DX,AX
        SHR     DX,4
        ADD     AX,DX
        AND     AX,$0F0F
        MOV     DX,AX
        SHR     AX,8
        ADD     AX,DX
        AND     EAX,$7F
end;
{$ELSEIF DEFINED(WIN64)}
asm
        .NOFRAME

        MOV     AX,CX
        SHR     CX,1
        AND     CX,$5555
        SUB     AX,CX
        MOV     CX,AX
        AND     AX,$3333
        SHR     CX,2
        AND     CX,$3333
        ADD     AX,CX
        MOV     CX,AX
        SHR     CX,4
        ADD     AX,CX
        AND     AX,$0F0F
        MOV     CX,AX
        SHR     AX,8
        ADD     AX,CX
        AND     EAX,$7F
end;
{$ELSE PUREPASCAL}
begin
  U := U - ((U shr 1) and $5555);
  U := (U and $3333) + ((U shr 2) and $3333);
  U := (U + (U shr 4)) and $0F0F;
  U := U + (U shr 8);
  Result := U and $7F;
end;
{$IFEND PUREPASCAL}

function VBitCount(S: Int32): Integer;
begin
  Result := VBitCount(UInt32(S));
end;

// Faster than 16 bit table lookups
function VBitCount(U: UInt32): Integer;
{$IF DEFINED(WIN32)}
asm
        MOV     EDX,EAX
        SHR     EDX,1
        AND     EDX,$55555555
        SUB     EAX,EDX
        MOV     EDX,EAX
        AND     EAX,$33333333
        SHR     EDX,2
        AND     EDX,$33333333
        ADD     EAX,EDX
        MOV     EDX,EAX
        SHR     EDX,4
        ADD     EAX,EDX
        AND     EAX,$0F0F0F0F
        MOV     EDX,EAX
        SHR     EAX,8
        ADD     EAX,EDX
        MOV     EDX,EAX
        SHR     EDX,16
        ADD     EAX,EDX
        AND     EAX,$7F
end;
{$ELSEIF DEFINED(WIN64)}
asm
        .NOFRAME

        MOV     EAX,ECX
        SHR     ECX,1
        AND     ECX,$55555555
        SUB     EAX,ECX
        MOV     ECX,EAX
        AND     EAX,$33333333
        SHR     ECX,2
        AND     ECX,$33333333
        ADD     EAX,ECX
        MOV     ECX,EAX
        SHR     ECX,4
        ADD     EAX,ECX
        AND     EAX,$0F0F0F0F
        MOV     ECX,EAX
        SHR     EAX,8
        ADD     EAX,ECX
        MOV     ECX,EAX
        SHR     ECX,16
        ADD     EAX,ECX
        AND     EAX,$7F
end;
{$ELSE PUREPASCAL}
begin
  U := U - ((U shr 1) and $55555555);
  U := (U and $33333333) + ((U shr 2) and $33333333);
  U := (U + (U shr 4)) and $0F0F0F0F;
  U := U + (U shr 8);
  U := U + (U shr 16);
  Result := U and $7F;
end;
{$IFEND PUREPASCAL}

function VBitCount(S: Int64): Integer; overload;
begin
  Result := VBitCount(UInt32(S)) + VBitCount(Int32(S shr 32));
end;

function VBitCount(S: UInt64): Integer; overload;
begin
  Result := VBitCount(UInt32(S)) + VBitCount(UInt32(S shr 32));
end;

function VBitLength(S: Int32): Integer;
begin
  Result := VBitLength(UInt32(S));
end;

function VBitLength(U: UInt32): Integer;
begin
  Result := 32 - VNumberOfLeadingZeros(U);
end;

function VBitLength(S: Int64): Integer;
begin
  Result := 64 - VNumberOfLeadingZeros(S);
end;

function VBitLength(U: UInt64): Integer;
begin
  Result := 64 - VNumberOfLeadingZeros(U);
end;

function VDigitCount(S: Int32): Int32; overload;
begin
  if S <> Low(Int32) then
    Result := VDigitCount(UInt32(Abs(S)))
  else
    Result := 9;
end;

function VDigitCount(U: UInt32): UInt32; overload;
begin
  Result := 1;
  if U >= 100000000 then
  begin
    Inc(Result, 8);
    U := U div 100000000;
  end;
  if U >= 10000 then
  begin
    Inc(Result, 4);
    U := U div 10000;
  end;
  if U >= 100  then
  begin
    Inc(Result, 2);
    U := U div 100;
  end;
  if U >= 10 then
    Inc(Result);
end;

function VIsPowerOfTwo(S: Int32): Boolean;
begin
  if S <> Low(Int32) then
    Result := VIsPowerofTwo(UInt32(Abs(S)))
  else
    Result := True;
end;

function VIsPowerOfTwo(U: UInt32): Boolean;
begin
  Result := (U and (U - 1)) = 0;
end;

function VHighestOneBit(S: Int32): Int32;
begin
  Result := Int32(VHighestOneBit(UInt32(S)));
end;

function VHighestOneBit(U: UInt32): UInt32;
begin
  if U = 0 then
    Result := 0
  else
    Result := UInt32(1) shl (31 - VNumberOfLeadingZeros(U));
end;

function VLowestOneBit(S: Int32): Int32;
begin
  Result := Int32(VLowestOneBit(UInt32(S)));
end;

function VLowestOneBit(U: UInt32): UInt32;
begin
  Result := U and -Int32(U);
end;

function VNumberOfLeadingZeros(U: UInt16): Integer;
{$IF DEFINED(WIN32)}
asm
        MOVZX   EAX,AX
        BSR     EDX,EAX
        JNZ     @Invert
        MOV     EAX,16
        RET

@Invert:

        MOV     EAX,15
        SUB     EAX,EDX
end;
{$ELSEIF DEFINED(WIN64)}
asm
        .NOFRAME

        MOVZX   EAX,CX
        BSR     ECX,EAX
        JNZ     @Invert
        MOV     EAX,16
        RET

@Invert:

        MOV     EAX,15
        SUB     EAX,ECX
end;
{$ELSE PUREPASCAL}
begin
  if U = 0 then
    Result := 16
  else
  begin
    Result := 0;
    if U <= High(Word) shr 8 then
    begin
      Result := Result + 8;
      U := U shl 8;
    end;
    if U <= High(Word) shr 4 then
    begin
      Result := Result + 4;
      U := U shl 4;
    end;
    if U <= High(Word) shr 2 then
    begin
      Result := Result + 2;
      U := U shl 2;
    end;
    if U <= High(Word) shr 1 then
      Result := Result + 1;
  end;
end;
{$IFEND PUREPASCAL}

function VNumberOfLeadingZeros(S: Int32): Integer;
begin
  Result := VNumberOfLeadingZeros(UInt32(Abs(S)));
end;

function VNumberOfLeadingZeros(U: UInt32): Integer;
{$IF DEFINED(WIN32)}
asm
        BSR     EDX,EAX
        JNZ     @Invert
        MOV     EAX,32
        RET

@Invert:

        MOV     EAX,31
        SUB     EAX,EDX

@Exit:
end;
{$ELSEIF DEFINED(WIN64)}
asm
         .NOFRAME

         BSR    EDX,ECX
         JNZ    @Invert
         MOV    EAX,32
         RET

@Invert:

         MOV    EAX,31
         SUB    EAX,EDX

@Exit:
end;
{$ELSE PUREPASCAL}

// Faster than X := X or X shr 1..16; Result := NLZDeBruijn32[...];

begin
  if U = 0 then
    Result := 32
  else
  begin
    Result := 0;
    if U <= High(Cardinal) shr 16 then
    begin
      Result := Result + 16;
      U := U shl 16;
    end;
    if U <= High(Cardinal) shr 8 then
    begin
      Result := Result + 8;
      U := U shl 8;
    end;
    if U <= High(Cardinal) shr 4 then
    begin
      Result := Result + 4;
      U := U shl 4;
    end;
    if U <= High(Cardinal) shr 2 then
    begin
      Result := Result + 2;
      U := U shl 2;
    end;
    if U <= High(Cardinal) shr 1 then
      Result := Result + 1;
  end;
end;
{$IFEND PUREPASCAL}

function VNumberOfLeadingZeros(S: Int64): Integer;
begin
  Result := VNumberOfLeadingZeros(UInt64(Abs(S)));
end;

function VNumberOfLeadingZeros(U: UInt64): Integer;
begin
  if U = 0 then
    Exit(1);
  if U <= High(UInt32) then
    Result := VNumberOfLeadingZeros(UInt32(U)) + 32
  else
    Result := VNumberOfLeadingZeros(UInt32(U shr 32));
end;

// Faster than NumberOfTrailingZeros2().
function VNumberOfTrailingZeros(U: UInt32): Integer;
{$IF DEFINED(WIN32)}
asm
        BSF     EAX,EAX
        JNZ     @Exit
        MOV     EAX,32

@Exit:
end;
{$ELSEIF DEFINED(WIN64)}
asm
        .NOFRAME

        BSF     EAX,ECX
        JNZ     @Exit
        MOV     EAX,32

@Exit:
end;
{$ELSE PUREPASCAL}
begin
  if U = 0 then
    Result := 32
  else
    Result := NTZDeBruijn32[((U and (-Integer(U))) * NTZDeBruijn32Mult) shr 27];
end;
{$IFEND PUREPASCAL}

function VNumberOfTrailingZeros(U: UInt64): Integer;
{$IF DEFINED(WIN32)}
asm
        BSF    EAX,DWORD PTR [U]
        JNZ    @Exit
        BSF    EAX,DWORD PTR [U+TYPE DWORD]
        JZ     @Ret64
        ADD    EAX,32
        JMP    @Exit
@Ret64:
        MOV    EAX,64
@Exit:
end;
{$ELSEIF DEFINED(WIN64)}
asm
        .NOFRAME

        BSF    RAX,RCX
        JNZ    @Exit
        MOV    EAX,64
@Exit:
end;
{$ELSE PUREPASCAL}
type
  TUInt64 = packed record
    Lo, Hi: UInt32;
  end;
begin
  if UInt32(U) = 0 then
    Result := 32 + NumberOfTrailingZeros(TUInt64(U).Hi)
  else
    Result := NumberOfTrailingZeros(UInt32(U));
end;
{$IFEND PUREPASCAL}

function VReverse(U: UInt8): UInt8;
begin
  U := ((U shr 1) and $55) or ((U and $55) shl 1);
  U := ((U shr 2) and $33) or ((U and $33) shl 2);
  U := (U shr 4) or (U shl 4);
  Result := U;
end;

function VReverse(U: UInt16): UInt16;
begin
  U := ((U shr 1) and $5555) or ((U and $5555) shl 1);
  U := ((U shr 2) and $3333) or ((U and $3333) shl 2);
  U := ((U shr 4) and $0F0F) or ((U and $0F0F) shl 4);
  U := Swap(U);
  Result := U;
end;

function VReverse(S: Int32): Int32;
begin
  Result := Int32(VReverse(UInt32(S)));
end;

// See http://stackoverflow.com/questions/746171/best-algorithm-for-bit-reversal-from-msb-lsb-to-lsb-msb-in-c too.
// http://stackoverflow.com/a/9144870/95954
function VReverse(U: UInt32): UInt32;
begin
  U := ((U shr 1) and $55555555) or ((U and $55555555) shl 1);  // Swap adjacent bits.
  U := ((U shr 2) and $33333333) or ((U and $33333333) shl 2);  // Swap adjacent bit pairs.
  U := ((U shr 4) and $0F0F0F0F) or ((U and $0F0F0F0F) shl 4);  // Swap nibbles.
  U := ((U shr 8) and $00FF00FF) or ((U and $00FF00FF) shl 8);  // Swap bytes.
  U := (U shr 16) or (U shl 16);                                // Swap words.
  Result := U;
end;

function VReverseBytes(S: Int32): Int32;
begin
  Result := Int32(VReverseBytes(UInt32(S)));
end;

// Byte and word swaps of Reverse(U).
function VReverseBytes(U: UInt32): UInt32;
begin
  U := ((U shr 8) and $00FF00FF) or ((U and $00FF00FF) shl 8);  // Swap bytes.
  U := (U shr 16) or (U shl 16);                                // Swap words.
  Result := U;
end;

function VRotateLeft(S: Int32; Distance: Integer): Int32;
begin
  Result := Int32(VRotateLeft(UInt32(S), Distance));
end;

function VRotateLeft(U: UInt32; Distance: Integer): UInt32;
begin
  Distance := Distance and 31;
  Result := (U shl Distance) or (U shr (32 - Distance));
end;

function VRotateRight(S: Int32; Distance: Integer): Int32;
begin
  Result := Int32(VRotateRight(UInt32(S), Distance));
end;

function VRotateRight(U: UInt32; Distance: Integer): UInt32;
begin
  Distance := Distance and 31;
  Result := (U shr Distance) or (U shl (32- Distance));
end;

function VSign(S: Int32): TValueSign;
begin
  Result := System.Math.Sign(S);
end;

function VToBinaryString(S: Int32): string;
begin
  Result := VToString(S, 2);
end;

function VToBinaryString(U: UInt32): string;
begin
  Result := VToString(U, 2);
end;

function VToHexString(S: Int32): string;
begin
  Result := VToString(S, 16);
end;

function VToHexString(U: UInt32): string;
begin
  Result := VToString(U, 16);
end;

function VToOctalString(S: Int32): string;
begin
  Result := VToString(S, 8);
end;

function VToOctalString(U: UInt32): string;
begin
  Result := VToString(U, 8);
end;

const
  Digits: array[0..35] of Char = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';

function VToString(S: Int32; Base: Byte): string;
begin
  if S < 0 then
    Result := '-' + VToString(UInt32(Abs(S)), Base)
  else
    Result := VToString(UInt32(S), Base);
end;

function VToString(U: UInt32; Base: Byte): string;
begin
  if not (Base in [2..36]) then
    raise EInvalidArgument.Create(SInvalidArgumentBase);

  if U = 0 then
    Result := '0'
  else
  begin
    Result := '';
    while U > 0 do
    begin
      Result := Digits[U mod Base] + Result;
      U := U div Base;
    end;
  end;
end;

function VToString(S: Int32): string;
begin
  Result := VToString(S, 10);
end;

function VToString(U: UInt32): string;
begin
  Result := VToString(U, 10);
end;

function VCompare(Left, Right: Int32): Integer;
begin
  if Left > Right then
    Exit(1)
  else if Left < Right then
    Exit(-1)
  else
    Exit(0);
end;

function VCompare(Left, Right: UInt32): Integer;
begin
  if Left > Right then
    Exit(1)
  else if Left < Right then
    Exit(-1)
  else
    Exit(0);
end;

function VCompare(Left, Right: Int64): Integer;
begin
  if Left > Right then
    Exit(1)
  else if Left < Right then
    Exit(-1)
  else
    Exit(0);
end;

function VCompare(Left, Right: UInt64): Integer;
begin
  if Left > Right then
    Exit(1)
  else if Left < Right then
    Exit(-1)
  else
    Exit(0);
end;

function VHashCode(Value: Int32): UInt32;
begin
  Result := UInt32(Value);
end;

function VHashCode(Value: UInt32): UInt32;
begin
  Result := Value;
end;

function VHashCode(Value: Int64): UInt32;
begin
  Result := UInt32(Value) xor UInt32(Value shr 32);
end;

function VHashCode(Value: UInt64): UInt32;
begin
  Result := UInt32(Value) xor UInt32(Value shr 32);
end;


// from unit Velthuis.FloatUtils;

function GetRawSignificand(const AValue: Single): UInt32; overload; inline;
begin
  Result := PUInt32(@AValue)^ and CSingleSignificandMask;
end;

function GetRawSignificand(const AValue: Double): UInt64; overload; inline;
begin
  Result := PUInt64(@AValue)^ and CDoubleSignificandMask;
end;

function GetRawExponent(const AValue: Single): Integer; overload; inline;
begin
  Result := PUInt32(@AValue)^ shr CSingleExponentShift and CSingleExponentMask;
end;

(*    ??? compiler error
function GetRawExponent(const AValue: Double): Integer; overload; inline;
begin
  Result := PUInt16(@AValue)[3] shr 4 and CDoubleExponentMask;
end;

function GetRawExponent(const AValue: Extended): Integer; overload; inline;
begin
  Result := PUInt16(@AValue)[4] and CExtendedExponentMask;
end;  *)

function IsNegativeInfinity(const AValue: Single): Boolean; overload;
begin
  Result := System.Math.IsInfinite(AValue) and (System.Math.Sign(AValue) < 0);
end;

function IsNegativeInfinity(const AValue: Double): Boolean; overload;
begin
  Result := System.Math.IsInfinite(AValue) and (Sign(AValue) < 0);
end;

function IsNegativeInfinity(const AValue: Extended): Boolean; overload;
begin
  Result := System.Math.IsInfinite(AValue) and (Sign(AValue) < 0);
end;

function IsPositiveInfinity(const AValue: Single): Boolean; overload;
begin
  Result := System.Math.IsInfinite(AValue) and (Sign(AValue) > 0);
end;

function IsPositiveInfinity(const AValue: Double): Boolean; overload;
begin
  Result := System.Math.IsInfinite(AValue) and (Sign(AValue) > 0);
end;

function IsPositiveInfinity(const AValue: Extended): Boolean; overload;
begin
  Result := System.Math.IsInfinite(AValue) and (Sign(AValue) > 0);
end;

function GetSignificand(const AValue: Single): UInt32; overload;
var
  E: Integer;
begin
  E := GetRawExponent(AValue);
  Result := GetRawSignificand(AValue);
  if (0 < E) and (E < CSingleExponentMask) then
    Result := Result or (UInt32(1) shl CSingleExponentShift);
end;

function GetSignificand(const AValue: Double): UInt64; overload;
var
  E: Integer;
begin
  E := GetRawExponent(AValue);
  Result := GetRawSignificand(AValue);
  if (0 < E) and (E < CDoubleExponentMask) then
    Result := Result or ((UInt64(1) shl CDoubleExponentShift));
end;

function GetSignificand(const AValue: Extended): UInt64; overload;
begin
  Result := PUInt64(@AValue)^;
end;

function GetMantissa(const AValue: Single): UInt32; overload;
begin
  Result := GetSignificand(AValue);
end;

function GetMantissa(const AValue: Double): UInt64; overload;
begin
  Result := GetSignificand(AValue);
end;

function GetMantissa(const AValue: Extended): UInt64; overload;
begin
  Result := GetSignificand(AValue);
end;

function GetExponent(const AValue: Single): Integer; overload;
var
  M: UInt32;
  E: Int32;
begin
  M := GetRawSignificand(AValue);
  E := GetRawExponent(AValue);
  if (0 < E) and (E < CSingleExponentMask) then
    Result := E - CSingleBias
  else if E = 0 then
    if M = 0 then
      // +/- Zero
      Result := 0
    else
      // Denormal
      Result := 1 - CSingleBias
  else
    // NaN or +/-Infinity
    Result := 0;
end;

function GetExponent(const AValue: Double): Integer; overload;
var
  M: UInt64;
  E: Int32;
begin
  M := GetRawSignificand(AValue);
  E := GetRawExponent(AValue);
  if (0 < E) and (E < CDoubleExponentMask) then
    Result := E - CDoubleBias
  else if E = 0 then
    if M = 0 then
      // +/-Zero
      Result := 0
    else
      // Denormal
      Result := 1 - CDoubleBias
  else
    // NaN or +/-Infinity
    Result := 0;
end;

function GetExponent(const AValue: Extended): Integer; overload;
var
  M: UInt64;
  E: Int32;
begin
  M := PUInt64(@AValue)^;
  E := GetRawExponent(AValue);
  if (0 < E) and (E < CExtendedExponentMask) then
    Result := E - CExtendedBias
  else if E = 0 then
    if M = 0 then
      // +/- Zero
      Result := 0
    else
      // Denormal
      Result := 1 - CExtendedBias
  else
    // NaN or +/-Infinity
    Result := 0;
end;

function IsDenormal(const AValue: Single): Boolean; overload;
begin
  Result := ((PUInt32(@AValue)^ shr CSingleExponentShift and CSingleExponentMask) = 0) and (GetSignificand(AValue) <> 0);
end;

function IsDenormal(const AValue: Double): Boolean; overload;
begin
  Result := ((PUInt64(@AValue)^ shr 52) = 0) and (GetSignificand(AValue) <> 0);
end;
{
function IsDenormal(const AValue: Extended): Boolean; overload;
begin
  Result := ((PUInt16(@AValue)[4] and $7FFF) = 0) and (GetSignificand(AValue) <> 0);
end;    }

function MakeSingle(Sign: TValueSign; Significand: UInt32; Exponent: Integer): Single;
var
  U: UInt32;
begin
  U := (Sign and CSingleSignMask) or
       ((UInt32(Exponent + CSingleBias) and CSingleExponentMask) shl CSingleExponentShift) or
       (Significand and CSingleSignificandMask);
  PUInt32(@Result)^ := U;
end;

function MakeDouble(Sign: TValueSign; Significand: UInt64; Exponent: Integer): Double;
var
  U: UInt64;
begin
  U := UInt64(Int64(Sign) and CDoubleSignMask) or
       (UInt64((Exponent + CDoubleBias) and CDoubleExponentMask) shl CDoubleExponentShift) or
       (Significand and CDoubleSignificandMask);
  PUInt64(@Result)^ := U;
end;

function MakeExtended(Sign: TValueSign; Significand: UInt64; Exponent: Integer): Extended;
var
  E: TExt80Rec;
begin
  E.Significand := Significand;
  E.ExponentAndSign := (Sign and $8000) or ((Exponent + CExtendedBias) and CExtendedExponentMask);
  PExt80Rec(@Result)^ := E;
end;


// from unit Velthuis.BigIntegers;

// To switch PUREPASCAL for debugging purposes, $UNDEF PUREPASCAL before the routine and $DEFINE PUREPASCAL
// after the routine, if PP was defined.
{$IFDEF PUREPASCAL}
{$DEFINE PP}
{$ENDIF}

// Copy the following around the routine for which you want to switch off PUREPASCAL

{$UNDEF PUREPASCAL}
// Routine here.
{$IFDEF PP}
{$DEFINE PUREPASCAL}
{$UNDEF PP}
{$ENDIF}

//uses
//  Velthuis.Sizes, Velthuis.Numerics, Velthuis.FloatUtils, Velthuis.StrConsts;

{$POINTERMATH ON}

const
  KZero: NativeUInt = 0;

{$REGION 'Debug related tools -- can eventually be removed'}
{$IFDEF DEBUG}
function Join(const Delimiter: string; const Values: array of string): string;
var
  I: Integer;
begin
  if Length(Values) > 0 then
  begin
    Result := Values[0];
    for I := 1 to High(Values) do
      Result := Delimiter + Result;
  end;
end;

function DumpPLimb(P: PLimb; Size: Integer): string;
var
  SL: TArray<string>;
  I: Integer;
begin
  Result := '';
  SetLength(SL, Size);
  for I := 0 to Size - 1 do
    SL[I] := Format('%.8x', [P[Size - I - 1]]);
  Result := Result + Join(' ', SL);
end;

procedure Debug(const Msg: string; const Params: array of const); overload;
begin
  if not DoDebug then
    Exit;

  if IsConsole then
    // Write to console.
    Writeln(System.ErrOutput, Format(Msg, Params))
{$IFDEF MSWINDOWS}
  else

    // Inside the IDE, this will be displayed in the Event Log.
    OutputDebugString(PChar(Format(Msg, Params)));
{$ELSE}
    ;
{$ENDIF}

end;

procedure Debug(const Msg: string); overload;
begin
  Debug(Msg, []);
end;
{$ELSE}
procedure Debug(const Msg: string; const Params: array of const);
begin
end;
{$ENDIF}
{$ENDREGION}

{$REGION 'Partial flag stall avoidance code'}
const
  CTimingLoops = $40000;

{$IFNDEF PUREPASCAL}
procedure Timing(var T1, T2, T3: UInt64); stdcall;
{$IFDEF WIN32}
asm
        RDTSC
        MOV     ECX,T1
        MOV     DWORD PTR [ECX],EAX
        MOV     DWORD PTR [ECX+4],EDX
        XOR     EAX,EAX
        MOV     EDX,CTimingLoops

@ADCLoop:

        ADC     EAX,[ECX]       // Partial-flags stall on some "older" processors causes a measurable
        DEC     EDX             //   timing difference. DEC only changes one flag, not entire flags register,
        JNE     @ADCLoop        //   causing a stall when ADC reads flag register.

        RDTSC
        MOV     ECX,T2
        MOV     [ECX],EAX
        MOV     [ECX+4],EDX
        XOR     EAX,EAX
        MOV     EDX,CTimingLoops

        .ALIGN  16

@ADDLoop:

        ADD     EAX,[ECX]       // ADD does not read carry flag, so no partial-flags stall.
        DEC     EDX
        JNE     @ADDLoop

        RDTSC
        MOV     ECX,T3
        MOV     [ECX],EAX
        MOV     [ECX+4],EDX
end;
{$ELSE}
asm
        MOV     R9,RDX
        RDTSC
        MOV     [RCX],EAX
        MOV     [RCX+4],EDX
        XOR     EAX,EAX
        MOV     EDX,CTimingLoops

        .ALIGN  16

@ADCLoop:

        ADC     EAX,[RCX]
        DEC     EDX
        JNE     @ADCLoop

        RDTSC
        MOV     [R9],EAX
        MOV     [R9+4],EDX
        XOR     EAX,EAX
        MOV     EDX,CTimingLoops

        .ALIGN  16

@ADDLoop:

        ADD     EAX,[RCX]
        DEC     EDX
        JNE     @ADDLoop

        RDTSC
        MOV     [R8],EAX
        MOV     [R8+4],EDX
end;
{$ENDIF}

class procedure BigInteger.DetectPartialFlagsStall;
var
  T1, T2, T3: UInt64;
  I1, I2: UInt64;
begin
  repeat
    Timing(T1, T2, T3);
    I1 := T2 - T1;
    I2 := T3 - T2;
//    Debug('Timing: %d / %d = %.2f', [I1, I2, I1 / I2]);

    // Make sure timings are far enough apart. Repeat if in "grey area" inbetween.
    if I1 / I2 > 4.0 then
    begin
      AvoidPartialFlagsStall(True);
      Exit;
    end
    else if I1 / I2 < 2.0 then
    begin
      AvoidPartialFlagsStall(False);
      Exit;
    end;
  until False;
end;
{$ENDIF !PUREPASCAL}
{$ENDREGION}

{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
{$POINTERMATH ON}
{$STACKFRAMES OFF}

{$DEFINE LIBDIVIDE}

type
  TUInt64 = record
    Lo, Hi: UInt32;
  end;

const
  // Size of a single limb, used in e.g. asm blocks.
  CLimbSize = SizeOf(TLimb);

  // Double limb, for 64 bit access
  DLimbSize = 2 * CLimbSize;

  // Array mapping a digit in a specified base to its textual representation.
  CBaseChars: array[0..35] of Char = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  CNumBase = Ord('0');
  CAlphaBase = Ord('A');

  // Array mapping a specified base to the maximum number of digits required to represent one limb in that base.
  // They map a specified base to Ceil(32 / Log2(base)).
  CStringMaxLengths: array[TNumberBase] of Integer =
  (
    32, 21, 16, 14, 13, 12, 11,
    11, 10, 10,  9,  9,  9,  9,
     8,  8,  8,  8,  8,  8,  8,
     8,  7,  7,  7,  7,  7,  7,
     7,  7,  7,  7,  7,  7,  7
  );

  CStringMinLengths: array[TNumberBase] of Integer =
  (
    32, 20, 16, 13, 12, 11, 10,
    10,  9,  9,  8,  8,  8,  8,
     8,  7,  7,  7,  7,  7,  7,
     7,  6,  6,  6,  6,  6,  6,
     6,  6,  6,  6,  6,  6,  6
  );

  // Various useful sizes and bitcounts.
  CLimbBits     = CByteBits * CLimbSize;
  CLimbWords    = CLimbSize div SizeOf(Word);
  CUInt64Limbs  = SizeOf(UInt64) div CLimbSize;
  CInt64Limbs   = SizeOf(Int64) div CLimbSize;

const
  CMaxFactorShift = 24;
  // Maximum powers of given bases that fit into UInt32
  CBaseInfos: array[TNumberBase] of BigInteger.TNumberBaseInfo =
  (
    (MaxPower:           2147483648; MaxDigits: 31; PowerofTwo:  True; MaxFactor: 541201),  // Base 2
    (MaxPower:           3486784401; MaxDigits: 20; PowerofTwo: False; MaxFactor: 529262),  // Base 3
    (MaxPower:           1073741824; MaxDigits: 15; PowerofTwo:  True; MaxFactor: 559241),  // Base 4
    (MaxPower:           1220703125; MaxDigits: 13; PowerofTwo: False; MaxFactor: 555812),  // Base 5
    (MaxPower:           2176782336; MaxDigits: 12; PowerofTwo: False; MaxFactor: 540859),  // Base 6
    (MaxPower:           1977326743; MaxDigits: 11; PowerofTwo: False; MaxFactor: 543288),  // Base 7
    (MaxPower:           1073741824; MaxDigits: 10; PowerofTwo:  True; MaxFactor: 559241),  // Base 8
    (MaxPower:           3486784401; MaxDigits: 10; PowerofTwo: False; MaxFactor: 529262),  // Base 9
    (MaxPower:           1000000000; MaxDigits:  9; PowerofTwo: False; MaxFactor: 561161),  // Base 10
    (MaxPower:           2357947691; MaxDigits:  9; PowerofTwo: False; MaxFactor: 538856),  // Base 11
    (MaxPower:            429981696; MaxDigits:  8; PowerofTwo: False; MaxFactor: 584986),  // Base 12
    (MaxPower:            815730721; MaxDigits:  8; PowerofTwo: False; MaxFactor: 566730),  // Base 13
    (MaxPower:           1475789056; MaxDigits:  8; PowerofTwo: False; MaxFactor: 550816),  // Base 14
    (MaxPower:           2562890625; MaxDigits:  8; PowerofTwo: False; MaxFactor: 536783),  // Base 15
    (MaxPower:            268435456; MaxDigits:  7; PowerofTwo:  True; MaxFactor: 599186),  // Base 16
    (MaxPower:            410338673; MaxDigits:  7; PowerofTwo: False; MaxFactor: 586365),  // Base 17
    (MaxPower:            612220032; MaxDigits:  7; PowerofTwo: False; MaxFactor: 574769),  // Base 18
    (MaxPower:            893871739; MaxDigits:  7; PowerofTwo: False; MaxFactor: 564215),  // Base 19
    (MaxPower:           1280000000; MaxDigits:  7; PowerofTwo: False; MaxFactor: 554555),  // Base 20
    (MaxPower:           1801088541; MaxDigits:  7; PowerofTwo: False; MaxFactor: 545668),  // Base 21
    (MaxPower:           2494357888; MaxDigits:  7; PowerofTwo: False; MaxFactor: 537455),  // Base 22
    (MaxPower:           3404825447; MaxDigits:  7; PowerofTwo: False; MaxFactor: 529836),  // Base 23
    (MaxPower:            191102976; MaxDigits:  6; PowerofTwo: False; MaxFactor: 609864),  // Base 24
    (MaxPower:            244140625; MaxDigits:  6; PowerofTwo: False; MaxFactor: 602129),  // Base 25
    (MaxPower:            308915776; MaxDigits:  6; PowerofTwo: False; MaxFactor: 594881),  // Base 26
    (MaxPower:            387420489; MaxDigits:  6; PowerofTwo: False; MaxFactor: 588069),  // Base 27
    (MaxPower:            481890304; MaxDigits:  6; PowerofTwo: False; MaxFactor: 581651),  // Base 28
    (MaxPower:            594823321; MaxDigits:  6; PowerofTwo: False; MaxFactor: 575589),  // Base 29
    (MaxPower:            729000000; MaxDigits:  6; PowerofTwo: False; MaxFactor: 569852),  // Base 30
    (MaxPower:            887503681; MaxDigits:  6; PowerofTwo: False; MaxFactor: 564411),  // Base 31
    (MaxPower:           1073741824; MaxDigits:  6; PowerofTwo:  True; MaxFactor: 559241),  // Base 32
    (MaxPower:           1291467969; MaxDigits:  6; PowerofTwo: False; MaxFactor: 554319),  // Base 33
    (MaxPower:           1544804416; MaxDigits:  6; PowerofTwo: False; MaxFactor: 549626),  // Base 34
    (MaxPower:           1838265625; MaxDigits:  6; PowerofTwo: False; MaxFactor: 545145),  // Base 35
    (MaxPower:           2176782336; MaxDigits:  6; PowerofTwo: False; MaxFactor: 540859)   // Base 36
  );

var
  CBasePowers: array[TNumberBase] of TArray<BigInteger>;

  ValueCache: array[-15..15] of BigInteger;

type
  PDynArrayRec = ^TDynArrayRec;
  TDynArrayRec = packed record
  {$IFDEF CPU64BITS}
    _Padding: Integer; // Make 16 byte align for payload.
  {$ENDIF}
    RefCnt: Integer;
    Length: NativeInt;
  end;

procedure SwapIntegers(var L, R: Integer); inline;
var
  Temp: Integer;
begin
  Temp := L;
  L := R;
  R := Temp;
end;

procedure SwapPLimbs(var L, R: PLimb); inline;
var
  Temp: PLimb;
begin
  Temp := L;
  L := R;
  R := Temp;
end;

function ActualSize(Limb: PLimb; Size: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  while (Size > 0) and (Limb[Size - 1] = 0) do
    Dec(Size);
  Result := Size;
end;
{$ELSE}
{$IFDEF WIN32}
asm

        LEA     EAX,[EAX + EDX * CLimbSize - CLimbSize]
        XOR     ECX,ECX

@Loop:

        CMP     [EAX],ECX
        JNE     @Exit
        LEA     EAX,[EAX - CLimbSize]
        DEC     EDX
        JNE     @Loop

@Exit:

        MOV     EAX,EDX

end;
{$ELSE !WIN32}
asm

        LEA     RAX,[RCX + RDX * CLimbSize - CLimbSize]
        XOR     ECX,ECX

@Loop:

        CMP     [RAX],ECX
        JNE     @Exit
        LEA     RAX,[RAX - CLimbSize]
        DEC     EDX
        JNE     @Loop

@Exit:

        MOV     EAX,EDX

end;
{$ENDIF !WIN32}
{$ENDIF}

function IntMax(Left, Right: UInt32): UInt32;
{$IFNDEF PUREPASCAL}
{$IFDEF WIN32}
asm
        CMP    EAX,EDX
        CMOVB  EAX,EDX
end;
{$ELSE WIN64}
asm
        MOV    EAX,ECX
        CMP    EAX,EDX
        CMOVB  EAX,EDX
end;
{$ENDIF}
{$ELSE}
begin
  Result := Left;
  if Left < Right then
    Result := Right;
end;
{$ENDIF}

function IntMin(Left, Right: UInt32): UInt32;
{$IFNDEF PUREPASCAL}
{$IFDEF WIN32}
asm
        CMP    EAX,EDX
        CMOVA  EAX,EDX
end;
{$ELSE WIN64}
asm
        MOV    EAX,ECX
        CMP    EAX,EDX
        CMOVA  EAX,EDX
end;
{$ENDIF}
{$ELSE}
begin
  Result := Left;
  if Left > Right then
    Result := Right;
end;
{$ENDIF}

function ShouldUseBurnikelZiegler(LSize, RSize: Integer): Boolean; inline;
begin
  // http://mail.openjdk.java.net/pipermail/core-libs-dev/2013-November/023493.html
  Result := (RSize >= BigInteger.BurnikelZieglerThreshold) and
            ((LSize - RSize) >= BigInteger.BurnikelZieglerOffsetThreshold);
end;

function SizeBitsOf(Value: Integer): Integer; inline;
begin
  Result := Value and BigInteger.SizeMask;
end;

function SignBitOf(Value: Integer): Integer; inline;
begin
  Result := Value and BigInteger.SignMask;
end;

function Min(const A, B: BigInteger): BigInteger; inline;
begin
  Result := BigInteger.Min(A, B);
end;

function Max(const A, B: BigInteger): BigInteger; inline;
begin
  Result := BigInteger.Max(A, B);
end;

function AllocLimbs(Size: Integer): PLimb; inline;
begin
  GetMem(Result, Size * CLimbSize);
end;

procedure CopyLimbs(Src, Dest: PLimb; Count: Integer); inline;
begin
  Move(Src^, Dest^, Count * CLimbSize);
end;

// Replacement for SetLength() only for TMagnitudes, i.e. dynamic arrays of TLimb.
procedure AllocNewMagnitude(var AData: TMagnitude; RequiredSize: Integer);
var
  NewData: PByte;
  NewSize: Integer;
begin
  NewSize := (RequiredSize + 3) and BigInteger.CapacityMask;
  NewData := AllocMem(NewSize * CLimbSize + SizeOf(TDynArrayRec));
  PDynArrayRec(NewData).RefCnt := 1;
  PDynArrayRec(NewData).Length := NewSize;
  PByte(AData) := NewData + SizeOf(TDynArrayRec);
end;

{ BigInteger }

procedure ShallowCopy(const Value: BigInteger; var Result: BigInteger); inline;
begin
  Result.FSize := Value.FSize;
  Result.FData := Value.FData;
end;

procedure DeepCopy(const Value: BigInteger; var Result: BigInteger); inline;
begin
  Result.FSize := Value.FSize;
  Result.FData := Copy(Value.FData);
end;

function BigInteger.Abs: BigInteger;
begin
  ShallowCopy(Self, Result);
  Result.FSize := Result.FSize and SizeMask;
end;

class function BigInteger.Abs(const Value: BigInteger): BigInteger;
begin
  ShallowCopy(Value, Result);
  Result.SetSign(0);
end;

function BigInteger.Pred: BigInteger;
begin
  ShallowCopy(Self, Result);
  Dec(Result);
end;

function BigInteger.Succ: BigInteger;
begin
  ShallowCopy(Self, Result);
  Inc(Result);
end;

class function BigInteger.Add(const Left, Right: BigInteger): BigInteger;
begin
  Add(Left, Right, Result);
end;

class procedure BigInteger.Add(const Left, Right: BigInteger; var Result: BigInteger);
var
  LSize, RSize, ResSize: Integer;
  LSign, RSign, ResSign: Integer;
  NewSize: Integer;
  Comparison: Integer;
  ResData: TMagnitude;
begin
  if not Assigned(Left.FData) then
  begin
    Result.FSize := Right.FSize;
    Result.FData := Right.FData;
    Exit;
  end
  else if not Assigned(Right.FData) then
  begin
    Result.FSize := Left.FSize;
    Result.FData := Left.FData;
    Exit;
  end;

  LSize := Left.FSize and SizeMask;
  RSize := Right.FSize and SizeMask;
  LSign := Left.FSize and SignMask;
  RSign := Right.FSize and SignMask;
  ResSize := IntMax(LSize, RSize) + 1;
  AllocNewMagnitude(ResData, ResSize);

  if LSign = RSign then
  begin
    // Same sign: add both magnitudes and transfer sign.
    FInternalAdd(PLimb(Left.FData), PLimb(Right.FData), PLimb(ResData), LSize, RSize);
    ResSign := LSign;
  end
  else
  begin
    Comparison := InternalCompare(PLimb(Left.FData), PLimb(Right.FData), LSize, RSize);

    if Comparison = 0 then
    begin
      Result.FSize := 0;
      Result.FData := nil;
      Exit;
    end;

    if Comparison < 0 then
    begin
      FInternalSubtract(PLimb(Right.FData), PLimb(Left.FData), PLimb(ResData), RSize, LSize);
      ResSign := RSign;
    end
    else
    begin
      FInternalSubtract(PLimb(Left.FData), PLimb(Right.FData), PLimb(ResData), LSize, RSize);
      ResSign := LSign;
    end;
  end;

  NewSize := ActualSize(PLimb(ResData), ResSize);
  if NewSize = 0 then
  begin
    Result.FSize := 0;
    Result.FData := nil;
  end
  else
  begin
  {$IFDEF RESETSIZE}
    if NewSize < (2 * ResSize div 3) then
      SetLength(ResData, NewSize);
  {$ENDIF}
    Result.FSize := NewSize or ResSign;
    Result.FData := ResData;
  end;
end;

class operator BigInteger.Add(const Left, Right: BigInteger): BigInteger;
begin
  Add(Left, Right, Result);
end;

class procedure BigInteger.Binary;
begin
  FBase := 2;
end;

class procedure BigInteger.InternalAnd(Left, Right, Result: PLimb; LSize, RSize: Integer);
{$IFDEF PUREPASCAL}
var
  I: Integer;
begin
  if RSize > LSize then
    RSize := LSize;
  for I := 0 to RSize - 1 do
    Result[I] := Left[I] and Right[I];
end;
{$ELSE !PUREPASCAL}
{$IFDEF WIN32}
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX

        MOV     EBX,RSize
        MOV     EDI,LSize

        CMP     EDI,EBX
        JAE     @SkipSwap
        XCHG    EBX,EDI
        XCHG    EAX,EDX

@SkipSwap:

        MOV     EDI,EBX
        AND     EDI,CUnrollMask
        SHR     EBX,CUnrollShift
        JE      @MainTail

@MainLoop:

        MOV     ESI,[EAX]
        AND     ESI,[EDX]
        MOV     [ECX],ESI

        MOV     ESI,[EAX + CLimbSize]
        AND     ESI,[EDX + CLimbSize]
        MOV     [ECX + CLimbSize],ESI

        MOV     ESI,[EAX + 2*CLimbSize]
        AND     ESI,[EDX + 2*CLimbSize]
        MOV     [ECX + 2*CLimbSize],ESI

        MOV     ESI,[EAX + 3*CLimbSize]
        AND     ESI,[EDX + 3*CLimbSize]
        MOV     [ECX + 3*CLimbSize],ESI

        LEA     EAX,[EAX + 4*CLimbSize]
        LEA     EDX,[EDX + 4*CLimbSize]
        LEA     ECX,[ECX + 4*CLimbSize]
        DEC     EBX
        JNE     @MainLoop

@MainTail:

        LEA     EAX,[EAX + EDI*CLimbSize]
        LEA     EDX,[EDX + EDI*CLimbSize]
        LEA     ECX,[ECX + EDI*CLimbSize]
        LEA     EBX,[@JumpsMain]
        JMP     [EBX + EDI*TYPE Pointer]

        .ALIGN  16

@JumpsMain:

        DD      @Exit
        DD      @Main1
        DD      @Main2
        DD      @Main3

@Main3:

        MOV     ESI,[EAX - 3*CLimbSize]
        AND     ESI,[EDX - 3*CLimbSize]
        MOV     [ECX - 3*CLimbSize],ESI

@Main2:

        MOV     ESI,[EAX - 2*CLimbSize]
        AND     ESI,[EDX - 2*CLimbSize]
        MOV     [ECX - 2*CLimbSize],ESI

@Main1:

        MOV     ESI,[EAX - CLimbSize]
        AND     ESI,[EDX - CLimbSize]
        MOV     [ECX - CLimbSize],ESI

@Exit:

        POP     EBX
        POP     EDI
        POP     ESI
end;
{$ELSE WIN64}
asm
        MOV     R10D,RSize

        CMP     R9D,R10D
        JAE     @SkipSwap
        XCHG    R10D,R9D
        XCHG    RCX,RDX

@SkipSwap:

        MOV     R9D,R10D
        AND     R9D,CUnrollMask
        SHR     R10D,CUnrollShift
        JE      @MainTail

@MainLoop:

        MOV     RAX,[RCX]
        AND     RAX,[RDX]
        MOV     [R8],RAX
        MOV     RAX,[RCX + DLimbSize]
        AND     RAX,[RDX + DLimbSize]
        MOV     [R8 + DLimbSize],RAX
        LEA     RCX,[RCX + 2*DLimbSize]
        LEA     RDX,[RDX + 2*DLimbSize]
        LEA     R8,[R8 + 2*DLimbSize]
        DEC     R10D
        JNE     @MainLoop

@MainTail:

        LEA     RCX,[RCX + R9*CLimbSize]
        LEA     RDX,[RDX + R9*CLimbSize]
        LEA     R8,[R8 + R9*CLimbSize]
        LEA     R10,[@JumpsMain]
        JMP     [R10 + R9*TYPE Pointer]

        .ALIGN  16

@JumpsMain:

        DQ      @Exit
        DQ      @Main1
        DQ      @Main2
        DQ      @Main3

@Main3:

        MOV     EAX,[RCX - 3*CLimbSize]
        AND     EAX,[RDX - 3*CLimbSize]
        MOV     [R8 - 3*CLimbSize],EAX

@Main2:

        MOV     EAX,[RCX - 2*CLimbSize]
        AND     EAX,[RDX - 2*CLimbSize]
        MOV     [R8 - 2*CLimbSize],EAX

@Main1:

        MOV     EAX,[RCX - CLimbSize]
        AND     EAX,[RDX - CLimbSize]
        MOV     [R8 - CLimbSize],EAX

@Exit:

end;
{$ENDIF WIN64}
{$ENDIF !PUREPASCAL}

class procedure BigInteger.InternalXor(Left, Right, Result: PLimb; LSize, RSize: Integer);
{$IFDEF PUREPASCAL}
var
  I: Integer;
begin
  if LSize < RSize then
  begin
    SwapIntegers(LSize, RSize);
    SwapPLimbs(Left, Right);
  end;
  for I := 0 to RSize - 1 do
    Result[I] := Left[I] xor Right[I];
  for I := RSize to LSize - 1 do
    Result[I] := Left[I];
end;
{$ELSE !PUREPASCAL}
{$IFDEF WIN32}
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX

        MOV     EBX,RSize
        MOV     EDI,LSize

        CMP     EDI,EBX
        JAE     @SkipSwap
        XCHG    EBX,EDI
        XCHG    EAX,EDX

@SkipSwap:

        SUB     EDI,EBX
        PUSH    EDI                             // Number of "tail" loops
        MOV     EDI,EBX
        AND     EDI,CUnrollMask
        SHR     EBX,CUnrollShift
        JE      @MainTail

@MainLoop:

        MOV     ESI,[EAX]
        XOR     ESI,[EDX]
        MOV     [ECX],ESI

        MOV     ESI,[EAX + CLimbSize]
        XOR     ESI,[EDX + CLimbSize]
        MOV     [ECX + CLimbSize],ESI

        MOV     ESI,[EAX + 2*CLimbSize]
        XOR     ESI,[EDX + 2*CLimbSize]
        MOV     [ECX + 2*CLimbSize],ESI

        MOV     ESI,[EAX + 3*CLimbSize]
        XOR     ESI,[EDX + 3*CLimbSize]
        MOV     [ECX + 3*CLimbSize],ESI

        LEA     EAX,[EAX + 4*CLimbSize]
        LEA     EDX,[EDX + 4*CLimbSize]
        LEA     ECX,[ECX + 4*CLimbSize]
        DEC     EBX
        JNE     @MainLoop

@MainTail:

        LEA     EAX,[EAX + EDI*CLimbSize]
        LEA     EDX,[EDX + EDI*CLimbSize]
        LEA     ECX,[ECX + EDI*CLimbSize]
        LEA     EBX,[@JumpsMain]
        JMP     [EBX + EDI*TYPE Pointer]

        .ALIGN  16

@JumpsMain:

        DD      @DoRestLoop
        DD      @Main1
        DD      @Main2
        DD      @Main3

@Main3:

        MOV     ESI,[EAX - 3*CLimbSize]
        XOR     ESI,[EDX - 3*CLimbSize]
        MOV     [ECX - 3*CLimbSize],ESI

@Main2:

        MOV     ESI,[EAX - 2*CLimbSize]
        XOR     ESI,[EDX - 2*CLimbSize]
        MOV     [ECX - 2*CLimbSize],ESI

@Main1:

        MOV     ESI,[EAX - CLimbSize]
        XOR     ESI,[EDX - CLimbSize]
        MOV     [ECX - CLimbSize],ESI

@DoRestLoop:

        XOR     EDX,EDX
        POP     EBX
        MOV     EDI,EBX
        AND     EDI,CUnrollMask
        SHR     EBX,CunrollShift
        JE      @RestLast3

@RestLoop:

        MOV     EDX,[EAX]
        MOV     [ECX],EDX

        MOV     EDX,[EAX + CLimbSize]
        MOV     [ECX + CLimbSize],EDX

        MOV     EDX,[EAX + 2*CLimbSize]
        MOV     [ECX + 2*CLimbSize],EDX

        MOV     EDX,[EAX + 3*CLimbSize]
        MOV     [ECX + 3*CLimbSize],EDX

        LEA     EAX,[EAX + 4*CLimbSize]
        LEA     ECX,[ECX + 4*CLimbSize]
        DEC     EBX
        JNE     @RestLoop

@RestLast3:

        LEA     EAX,[EAX + EDI*CLimbSize]
        LEA     ECX,[ECX + EDI*CLimbSize]
        LEA     EBX,[@RestJumps]
        JMP     [EBX + EDI*TYPE Pointer]

        .ALIGN  16

@RestJumps:

        DD      @Exit
        DD      @Rest1
        DD      @Rest2
        DD      @Rest3

@Rest3:

        MOV     EDX,[EAX - 3*CLimbSize]
        MOV     [ECX - 3*CLimbSize],EDX

@Rest2:

        MOV     EDX,[EAX - 2*CLimbSize]
        MOV     [ECX - 2*CLimbSize],EDX

@Rest1:

        MOV     EDX,[EAX - CLimbSize]
        MOV     [ECX - CLimbSize],EDX

@Exit:

        POP     EBX
        POP     EDI
        POP     ESI
end;
{$ELSE WIN64}
asm
        MOV     R10D,RSize

        CMP     R9D,R10D
        JAE     @SkipSwap
        XCHG    R10D,R9D
        XCHG    RCX,RDX

@SkipSwap:

        SUB     R9D,R10D
        PUSH    R9
        MOV     R9D,R10D
        AND     R9D,CUnrollMask
        SHR     R10D,CUnrollShift
        JE      @MainTail

@MainLoop:

        MOV     RAX,[RCX]
        XOR     RAX,[RDX]
        MOV     [R8],RAX

        MOV     RAX,[RCX + DLimbSize]
        XOR     RAX,[RDX + DLimbSize]
        MOV     [R8 + DLimbSize],RAX

        LEA     RCX,[RCX + 2*DLimbSize]
        LEA     RDX,[RDX + 2*DLimbSize]
        LEA     R8,[R8 + 2*DLimbSize]
        DEC     R10D
        JNE     @MainLoop

@MainTail:

        LEA     RCX,[RCX + R9*CLimbSize]
        LEA     RDX,[RDX + R9*CLimbSize]
        LEA     R8,[R8 + R9*CLimbSize]
        LEA     R10,[@JumpsMain]
        JMP     [R10 + R9*TYPE Pointer]

@JumpsMain:

        DQ      @DoRestLoop
        DQ      @Main1
        DQ      @Main2
        DQ      @Main3

@Main3:

        MOV     EAX,[RCX - 3*CLimbSize]
        XOR     EAX,[RDX - 3*CLimbSize]
        MOV     [R8 - 3*CLimbSize],EAX

@Main2:

        MOV     EAX,[RCX - 2*CLimbSize]
        XOR     EAX,[RDX - 2*CLimbSize]
        MOV     [R8 - 2*CLimbSize],EAX

@Main1:

        MOV     EAX,[RCX - CLimbSize]
        XOR     EAX,[RDX - CLimbSize]
        MOV     [R8 - CLimbSize],EAX

@DoRestLoop:

        POP     R10
        TEST    R10D,R10D
        JE      @Exit
        MOV     R9D,R10D
        AND     R9D,CUnrollMask
        SHR     R10D,CUnrollShift
        JE      @RestLast3

@RestLoop:

        MOV     RAX,[RCX]
        MOV     [R8],RAX

        MOV     RAX,[RCX + DLimbSize]
        MOV     [R8 + DLimbSize],RAX

        LEA     RCX,[RCX + 2*DLimbSize]
        LEA     R8,[R8 + 2*DLimbSize]
        DEC     R10D
        JNE     @RestLoop

@RestLast3:

        LEA     RCX,[RCX + R9*CLimbSize]
        LEA     R8,[R8 + R9*CLimbSize]
        LEA     R10,[@RestJumps]
        JMP     [R10 + R9*TYPE Pointer]

@RestJumps:

        DQ      @Exit
        DQ      @Rest1
        DQ      @Rest2
        DQ      @Rest3

@Rest3:

        MOV     EAX,[RCX - 3*CLimbSize]
        MOV     [R8 - 3*CLimbSize],EAX

@Rest2:

        MOV     EAX,[RCX - 2*CLimbSize]
        MOV     [R8 - 2*CLimbSize],EAX

@Rest1:

        MOV     EAX,[RCX - CLimbSize]
        MOV     [R8 - CLimbSize],EAX

@Exit:

end;
{$ENDIF WIN64}
{$ENDIF !PUREPASCAL}

class procedure BigInteger.InternalOr(Left, Right, Result: PLimb; LSize, RSize: Integer);
{$IFDEF PUREPASCAL}
var
  I: Integer;
begin
  // Ensure Left/LSize belong to largest BigInteger.
  if LSize < RSize then
  begin
    SwapIntegers(LSize, RSize);
    SwapPLimbs(Left, Right);
  end;
  for I := 0 to RSize - 1 do
    Result[I] := Left[I] or Right[I];
  for I := RSize to LSize - 1 do
    Result[I] := Left[I];
end;
{$ELSE !PUREPASCAL}
{$IFDEF WIN32}
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX

        MOV     EBX,RSize
        MOV     EDI,LSize

        CMP     EDI,EBX
        JAE     @SkipSwap
        XCHG    EBX,EDI
        XCHG    EAX,EDX

@SkipSwap:

        SUB     EDI,EBX
        PUSH    EDI                             // Number of "rest" loops
        MOV     EDI,EBX
        AND     EDI,CUnrollMask
        SHR     EBX,CUnrollShift
        JE      @MainTail

@MainLoop:

        MOV     ESI,[EAX]
        OR      ESI,[EDX]
        MOV     [ECX],ESI

        MOV     ESI,[EAX + CLimbSize]
        OR      ESI,[EDX + CLimbSize]
        MOV     [ECX + CLimbSize],ESI

        MOV     ESI,[EAX + 2*CLimbSize]
        OR      ESI,[EDX + 2*CLimbSize]
        MOV     [ECX + 2*CLimbSize],ESI

        MOV     ESI,[EAX + 3*CLimbSize]
        OR      ESI,[EDX + 3*CLimbSize]
        MOV     [ECX + 3*CLimbSize],ESI

        LEA     EAX,[EAX + 4*CLimbSize]
        LEA     EDX,[EDX + 4*CLimbSize]
        LEA     ECX,[ECX + 4*CLimbSize]
        DEC     EBX
        JNE     @MainLoop

@MainTail:

        LEA     EAX,[EAX + EDI*CLimbSize]
        LEA     EDX,[EDX + EDI*CLimbSize]
        LEA     ECX,[ECX + EDI*CLimbSize]
        LEA     EBX,[@JumpsMain]
        JMP     [EBX + EDI*TYPE Pointer]

        .ALIGN  16

@JumpsMain:

        DD      @DoRestLoop
        DD      @Main1
        DD      @Main2
        DD      @Main3

@Main3:

        MOV     ESI,[EAX - 3*CLimbSize]
        OR      ESI,[EDX - 3*CLimbSize]
        MOV     [ECX - 3*CLimbSize],ESI

@Main2:

        MOV     ESI,[EAX - 2*CLimbSize]
        OR      ESI,[EDX - 2*CLimbSize]
        MOV     [ECX - 2*CLimbSize],ESI

@Main1:

        MOV     ESI,[EAX - CLimbSize]
        OR      ESI,[EDX - CLimbSize]
        MOV     [ECX - CLimbSize],ESI

@DoRestLoop:

        XOR     EDX,EDX
        POP     EBX
        MOV     EDI,EBX
        AND     EDI,CUnrollMask
        SHR     EBX,CUnrollShift
        JE      @RestLast3

@RestLoop:

        MOV     EDX,[EAX]
        MOV     [ECX],EDX

        MOV     EDX,[EAX + CLimbSize]
        MOV     [ECX + CLimbSize],EDX

        MOV     EDX,[EAX + 2*CLimbSize]
        MOV     [ECX + 2*CLimbSize],EDX

        MOV     EDX,[EAX + 3*CLimbSize]
        MOV     [ECX + 3*CLimbSize],EDX

        LEA     EAX,[EAX + 4*CLimbSize]
        LEA     ECX,[ECX + 4*CLimbSize]
        DEC     EBX
        JNE     @RestLoop

@RestLast3:

        LEA     EAX,[EAX + EDI*CLimbSize]
        LEA     ECX,[ECX + EDI*CLimbSize]
        LEA     EBX,[@RestJumps]
        JMP     [EBX + EDI*TYPE Pointer]

        .ALIGN  16

@RestJumps:

        DD      @Exit
        DD      @Rest1
        DD      @Rest2
        DD      @Rest3

@Rest3:

        MOV     EDX,[EAX - 3*CLimbSize]
        MOV     [ECX - 3*CLimbSize],EDX

@Rest2:

        MOV     EDX,[EAX - 2*CLimbSize]
        MOV     [ECX - 2*CLimbSize],EDX

@Rest1:

        MOV     EDX,[EAX - CLimbSize]
        MOV     [ECX - CLimbSize],EDX

@Exit:

        POP     EBX
        POP     EDI
        POP     ESI
end;
{$ELSE WIN64}
asm
        MOV     R10D,RSize

        CMP     R9D,R10D
        JAE     @SkipSwap
        XCHG    R10D,R9D
        XCHG    RCX,RDX

@SkipSwap:

        SUB     R9D,R10D
        PUSH    R9
        MOV     R9D,R10D
        AND     R9D,CUnrollMask
        SHR     R10D,CUnrollShift
        JE      @MainTail

@MainLoop:

        MOV     RAX,[RCX]
        OR      RAX,[RDX]
        MOV     [R8],RAX

        MOV     RAX,[RCX + DLimbSize]
        OR      RAX,[RDX + DLimbSize]
        MOV     [R8 + DLimbSize],RAX

        LEA     RCX,[RCX + 2*DLimbSize]
        LEA     RDX,[RDX + 2*DLimbSize]
        LEA     R8,[R8 + 2*DLimbSize]
        DEC     R10D
        JNE     @MainLoop

@MainTail:

        LEA     RCX,[RCX + R9*CLimbSize]
        LEA     RDX,[RDX + R9*CLimbSize]
        LEA     R8,[R8 + R9*CLimbSize]
        LEA     R10,[@JumpsMain]
        JMP     [R10 + R9*TYPE Pointer]

        // Align jump table manually, with NOPs.

        DB      $90,$90,$90,$90,$90,$90

@JumpsMain:

        DQ      @DoRestLoop
        DQ      @Main1
        DQ      @Main2
        DQ      @Main3

@Main3:

        MOV     EAX,[RCX - 3*CLimbSize]
        OR      EAX,[RDX - 3*CLimbSize]
        MOV     [R8 - 3*CLimbSize],EAX

@Main2:

        MOV     EAX,[RCX - 2*CLimbSize]
        OR      EAX,[RDX - 2*CLimbSize]
        MOV     [R8 - 2*CLimbSize],EAX

@Main1:

        MOV     EAX,[RCX - CLimbSize]
        OR      EAX,[RDX - CLimbSize]
        MOV     [R8 - CLimbSize],EAX

@DoRestLoop:

        POP     R10
        TEST    R10D,R10D
        JE      @Exit
        MOV     R9D,R10D
        AND     R9D,CUnrollMask
        SHR     R10D,CUnrollShift
        JE      @RestLast3

@RestLoop:

        MOV     RAX,[RCX]
        MOV     [R8],RAX

        MOV     RAX,[RCX + DLimbSize]
        MOV     [R8 + DLimbSize],RAX

        LEA     RCX,[RCX + 2*DLimbSize]
        LEA     R8,[R8 + 2*DLimbSize]
        DEC     R10D
        JNE     @RestLoop

@RestLast3:

        LEA     RCX,[RCX + R9*CLimbSize]
        LEA     R8,[R8 + R9*CLimbSize]
        LEA     R10,[@RestJumps]
        JMP     [R10 + R9*TYPE Pointer]

        // Align jump table manually, with NOPs.

        // -- Aligned.

@RestJumps:

        DQ      @Exit
        DQ      @Rest1
        DQ      @Rest2
        DQ      @Rest3

@Rest3:

        MOV     EAX,[RCX - 3*CLimbSize]
        MOV     [R8 - 3*CLimbSize],EAX

@Rest2:

        MOV     EAX,[RCX - 2*CLimbSize]
        MOV     [R8 - 2*CLimbSize],EAX

@Rest1:

        MOV     EAX,[RCX - CLimbSize]
        MOV     [R8 - CLimbSize],EAX

@Exit:

end;
{$ENDIF WIN64}
{$ENDIF !PUREPASCAL}

class procedure BigInteger.InternalAndNot(Left, Right, Result: PLimb; LSize, RSize: Integer);
{$IFDEF PUREPASCAL}
var
  I: Integer;
begin

  // Note: AndNot is - of course - not commutative.
  if LSize < RSize then
    RSize := LSize;
  for I := 0 to RSize - 1 do
    Result[I] := not Right[I] and Left[I];
  for I := RSize to LSize - 1 do
    Result[I] := Left[I];
end;
{$ELSE !PUREPASCAL}
{$IFDEF WIN32}
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX

        MOV     EBX,RSize
        MOV     EDI,LSize

        CMP     EDI,EBX
        JAE     @SkipSwap
        MOV     EBX,EDI

@SkipSwap:

        SUB     EDI,EBX
        PUSH    EDI                             // Number of "rest" loops
        MOV     EDI,EBX
        AND     EDI,CUnrollMask
        SHR     EBX,CUnrollShift
        JE      @MainTail

@MainLoop:

        MOV     ESI,[EDX]
        NOT     ESI
        AND     ESI,[EAX]
        MOV     [ECX],ESI

        MOV     ESI,[EDX + CLimbSize]
        NOT     ESI
        AND     ESI,[EAX + CLimbSize]
        MOV     [ECX + CLimbSize],ESI

        MOV     ESI,[EDX + 2*CLimbSize]
        NOT     ESI
        AND     ESI,[EAX + 2*CLimbSize]
        MOV     [ECX + 2*CLimbSize],ESI

        MOV     ESI,[EDX + 3*CLimbSize]
        NOT     ESI
        AND     ESI,[EAX + 3*CLimbSize]
        MOV     [ECX + 3*CLimbSize],ESI

        LEA     EAX,[EAX + 4*CLimbSize]
        LEA     EDX,[EDX + 4*CLimbSize]
        LEA     ECX,[ECX + 4*CLimbSize]
        DEC     EBX
        JNE     @MainLoop

@MainTail:

        LEA     EAX,[EAX + EDI*CLimbSize]
        LEA     EDX,[EDX + EDI*CLimbSize]
        LEA     ECX,[ECX + EDI*CLimbSize]
        LEA     EBX,[@JumpsMain]
        JMP     [EBX + EDI*TYPE Pointer]

        .ALIGN  16

@JumpsMain:

        DD      @DoRestLoop
        DD      @Main1
        DD      @Main2
        DD      @Main3

@Main3:

        MOV     ESI,[EDX - 3*CLimbSize]
        NOT     ESI
        AND     ESI,[EAX - 3*CLimbSize]
        MOV     [ECX - 3*CLimbSize],ESI

@Main2:

        MOV     ESI,[EDX - 2*CLimbSize]
        NOT     ESI
        AND     ESI,[EAX - 2*CLimbSize]
        MOV     [ECX - 2*CLimbSize],ESI

@Main1:

        MOV     ESI,[EDX - CLimbSize]
        NOT     ESI
        AND     ESI,[EAX - CLimbSize]
        MOV     [ECX - CLimbSize],ESI

@DoRestLoop:

        XOR     EDX,EDX
        POP     EBX
        MOV     EDI,EBX
        AND     EDI,CUnrollMask
        SHR     EBX,CUnrollShift
        JE      @RestLast3

@RestLoop:

        //      X AND NOT 0 = X AND -1 = X
        MOV     EDX,[EAX]
        MOV     [ECX],EDX

        MOV     EDX,[EAX + CLimbSize]
        MOV     [ECX + CLimbSize],EDX

        MOV     EDX,[EAX + 2*CLimbSize]
        MOV     [ECX + 2*CLimbSize],EDX

        MOV     EDX,[EAX + 3*CLimbSize]
        MOV     [ECX + 3*CLimbSize],EDX

        LEA     EAX,[EAX + 4*CLimbSize]
        LEA     ECX,[ECX + 4*CLimbSize]
        DEC     EBX
        JNE     @RestLoop

@RestLast3:

        LEA     EAX,[EAX + EDI*CLimbSize]
        LEA     ECX,[ECX + EDI*CLimbSize]
        LEA     EBX,[@RestJumps]
        JMP     [EBX + EDI*TYPE Pointer]

        // Align jump table manually, with NOPs.

@RestJumps:

        DD      @Exit
        DD      @Rest1
        DD      @Rest2
        DD      @Rest3

@Rest3:

        MOV     EDX,[EAX - 3*CLimbSize]
        MOV     [ECX - 3*CLimbSize],EDX

@Rest2:

        MOV     EDX,[EAX - 2*CLimbSize]
        MOV     [ECX - 2*CLimbSize],EDX

@Rest1:

        MOV     EDX,[EAX - CLimbSize]
        MOV     [ECX - CLimbSize],EDX

@Exit:

        POP     EBX
        POP     EDI
        POP     ESI
end;
{$ELSE WIN64}
asm
        MOV     R10D,RSize

        CMP     R9D,R10D
        JAE     @SkipSwap
        MOV     R10D,R9D

@SkipSwap:

        SUB     R9D,R10D
        PUSH    R9
        MOV     R9D,R10D
        AND     R9D,CUnrollMask
        SHR     R10D,CUnrollShift
        JE      @MainTail

@MainLoop:

        MOV     RAX,[RDX]
        NOT     RAX
        AND     RAX,[RCX]
        MOV     [R8],RAX

        MOV     RAX,[RDX + DLimbSize]
        NOT     RAX
        AND     RAX,[RCX + DLimbSize]
        MOV     [R8 + DLimbSize],RAX

        LEA     RCX,[RCX + 2*DLimbSize]
        LEA     RDX,[RDX + 2*DLimbSize]
        LEA     R8,[R8 + 2*DLimbSize]
        DEC     R10D
        JNE     @MainLoop

@MainTail:

        LEA     RCX,[RCX + R9*CLimbSize]
        LEA     RDX,[RDX + R9*CLimbSize]
        LEA     R8,[R8 + R9*CLimbSize]
        LEA     R10,[@JumpsMain]
        JMP     [R10 + R9*TYPE Pointer]

        // Align jump table manually, with NOPs.

        DB      $90,$90,$90

@JumpsMain:

        DQ      @DoRestLoop
        DQ      @Main1
        DQ      @Main2
        DQ      @Main3

@Main3:

        MOV     EAX,[RDX - 3*CLimbSize]
        NOT     EAX
        AND     EAX,[RCX - 3*CLimbSize]
        MOV     [R8 - 3*CLimbSize],EAX

@Main2:

        MOV     EAX,[RDX - 2*CLimbSize]
        NOT     EAX
        AND     EAX,[RCX - 2*CLimbSize]
        MOV     [R8 - 2*CLimbSize],EAX

@Main1:

        MOV     EAX,[RDX - CLimbSize]
        NOT     EAX
        AND     EAX,[RCX - CLimbSize]
        MOV     [R8 - CLimbSize],EAX

@DoRestLoop:

        POP     R10
        TEST    R10D,R10D
        JE      @Exit
        MOV     R9D,R10D
        AND     R9D,CUnrollMask
        SHR     R10D,CUnrollShift
        JE      @RestLast3

@RestLoop:

        //      X AND NOT 0 = X AND -1 = X

        MOV     RAX,[RCX]
        MOV     RDX,[RCX + DLimbSize]
        MOV     [R8],RAX
        MOV     [R8 + DLimbSize],RDX

        LEA     RCX,[RCX + 2*DLimbSize]
        LEA     R8,[R8 + 2*DLimbSize]
        DEC     R10D
        JNE     @RestLoop

@RestLast3:

        LEA     RCX,[RCX + R9*CLimbSize]
        LEA     R8,[R8 + R9*CLimbSize]
        LEA     R10,[@RestJumps]
        JMP     [R10 + R9*TYPE Pointer]

        // Align jump table manually, with NOPs.

        DB      $90,$90

@RestJumps:

        DQ      @Exit
        DQ      @Rest1
        DQ      @Rest2
        DQ      @Rest3

@Rest3:

        MOV     EAX,[RCX - 3*CLimbSize]
        MOV     [R8 - 3*CLimbSize],EAX

@Rest2:

        MOV     EAX,[RCX - 2*CLimbSize]
        MOV     [R8 - 2*CLimbSize],EAX

@Rest1:

        MOV     EAX,[RCX - CLimbSize]
        MOV     [R8 - CLimbSize],EAX

@Exit:

end;
{$ENDIF WIN64}
{$ENDIF !PUREPASCAL}

class procedure BigInteger.InternalNotAnd(Left, Right, Result: PLimb; LSize, RSize: Integer);
begin
  InternalAndNot(Right, Left, Result, RSize, LSize);
end;

class operator BigInteger.BitwiseAnd(const Left, Right: BigInteger): BigInteger;
begin

  // Special handling for 0.
  if (Left.FData = nil)  or (Right.FData = nil) then
  begin
    Result.FData := nil;
    Result.FSize := 0;
    Exit;
  end;

  InternalBitwise(Left, Right, Result, InternalAnd, InternalOr, InternalAndNot);
end;

class operator BigInteger.BitwiseOr(const Left, Right: BigInteger): BigInteger;
begin

  // Special handling for 0.
  if Left.FData = nil then
  begin
    Result.FSize := Right.FSize;
    Result.FData := Right.FData;
    Exit;
  end
  else if Right.FData = nil then
  begin
    Result.FSize := Left.FSize;
    Result.FData := Left.FData;
    Exit;
  end;

  InternalBitwise(Left, Right, Result, InternalOr, InternalAnd, InternalNotAnd);
end;

class operator BigInteger.BitwiseXor(const Left, Right: BigInteger): BigInteger;
begin

  // Special handling for 0.
  if Left.FData = nil then
  begin
    ShallowCopy(Right, Result);
    Exit;
  end
  else if Right.FData = nil then
  begin
    ShallowCopy(Left, Result);
    Exit;
  end;

  InternalBitwise(Left, Right, Result, InternalXor, InternalXor, InternalXor);
end;

function BigInteger.Clone: BigInteger;
begin
  DeepCopy(Self, Result);
end;

class procedure BigInteger.Compact(var Data: TMagnitude; var Size: Integer);
var
  NewSize: Integer;
begin
  if Data = nil then
  begin
    Size := 0;
    Exit;
  end;

  NewSize := ActualSize(PLimb(Data), Size and SizeMask);
  if NewSize < (Size and SizeMask) then
  begin
    if NewSize = 0 then
    begin
      Size := 0;
      Data := nil;
    end
    else
    begin
      Size := SignBitOf(Size) or NewSize;
    {$IFDEF RESETSIZE}
      SetLength(Data, (NewSize + 4) and CapacityMask);
    {$ENDIF}
    end;
  end;
end;

procedure BigInteger.Compact;
begin
  Compact(FData, FSize);
end;

class function BigInteger.Compare(const Left, Right: BigInteger): Integer;
const
  Results: array[Boolean] of Integer = (-1, 1);
var
  LSize, RSize: Integer;
begin
  if Left.FData = nil then
    if Right.FData = nil then
      Exit(0)                           // Compare(0, 0) = 0
    else
      Exit(Results[Right.FSize < 0])    // Compare(0, negative) = 1
  else if Right.FData = nil then
    Exit(Results[Left.FSize > 0]);      // Compare(positive, 0) = 1

  if ((Left.FSize xor Right.FSize) and SignMask) <> 0 then
    Exit(Results[Left.FSize > 0]);      // Compare(positive, negative) = 1; Compare(negative, positive) = -1

  // Same sign:
  LSize := Left.FSize and SizeMask;
  RSize := Right.FSize and SizeMask;
  Result := InternalCompare(PLimb(Left.FData), PLimb(Right.FData), LSize, RSize);

  if Left.FSize < 0 then
      Result := -Result;
end;

constructor BigInteger.Create(const Value: BigInteger);
begin
  Self.FSize := Value.FSize;
  Self.FData := Value.FData;
end;

constructor BigInteger.Create(const Magnitude: TMagnitude; Negative: Boolean);
begin
  FSize := Length(Magnitude) or (Ord(Negative) * SignMask);
  FData := Copy(Magnitude);     // Must copy; otherwise modifying magnitude would modify this BigInteger.
  Compact;
end;

constructor BigInteger.Create(const Value: Int32);
begin
  if (Value >= Low(ValueCache)) and (Value <= High(ValueCache)) then
    Self := ValueCache[Value]
  else
  begin
    Create(UInt32(System.Abs(Value)));
    if Value < 0 then
      FSize := FSize or SignMask;
    Compact;
  end;
end;

constructor BigInteger.Create(const Value: Int64);
begin
  if (Value >= Low(ValueCache)) and (Value <= High(ValueCache)) then
    Self := ValueCache[Value]
  else
  begin
    Create(UInt64(System.Abs(Value)));
    if Value < 0 then
      FSize := FSize or SignMask;
    Compact;
  end;
end;

constructor BigInteger.Create(const Value: Cardinal);
begin
  if Value <= UInt32(High(ValueCache)) then
    Self := ValueCache[Value]
  else
  begin
    if Value <> 0 then
    begin
      FSize := 1;
      SetLength(FData, 4);
      FData[0] := Value;
    end
    else
    begin
      FData := nil;
      FSize := 0;
    end;
    Compact;
  end;
end;

constructor BigInteger.Create(const Value: UInt64);
begin
  if Value <= High(ValueCache) then
    Self := ValueCache[Value]
  else
  begin
    FData := nil;
    if Value <> 0 then
    begin
      if Value > High(UInt32) then
        FSize := CUInt64Limbs
      else
        FSize := 1;
      SetLength(FData, 4);
      Move(Value, FData[0], SizeOf(Value));
    end
    else
    begin
      FData := nil;
      FSize := 0;
    end;
    Compact;
  end;
end;

const
  CMantissaBits = 52;
  CMaxShift     = 62;

constructor BigInteger.Create(const Value: Double);
var
  Exponent: Integer;
  Mantissa: UInt64;
  Sign, Guard, Round, Sticky: Boolean;
  Shift: Integer;
  ZeroExponentLimit: Integer;
begin
  FSize := 0;
//  FData := nil;

  // Error for special values.
  if IsNan(Value) or IsInfinite(Value) then
    Error(ecInvalidArgFloat, ['Double']);

  // Get the required values from TDoubleHelper.
  Mantissa := GetSignificand(Value);
  Exponent := GetExponent(Value);
  Sign := PInt64(@Value)^ < 0;

  // Make 0 for denormal values and values < 0.5.
  if FRoundingMode <> rmTruncate then
    ZeroExponentLimit := -1
  else
    ZeroExponentLimit := 0;

  // Denormals and values with small exponent convert to 0.
  if IsDenormal(Value) or (Exponent < ZeroExponentLimit) then
  begin
    Self := BigInteger.Zero;
    Exit;
  end;

  // Internal shift of the mantissa.
  Shift := Exponent;
  if Shift > CMaxShift then
    Shift := CMaxShift;

  // Guard, Round and Sticky bits are used to determine rounding.
  Guard := False;
  Round := False;
  Sticky := False;
  if (FRoundingMode <> rmTruncate) and (Exponent < CMantissaBits) then
  begin
    // Round anything with a fraction >= 0.5 away from 0. No Round and Sticky bits required.
    Guard := ((UInt64(1) shl (CMantissaBits - 1 - Exponent)) and Mantissa) <> 0;

    if FRoundingMode = rmRound then
    begin
      // Only if full rounding (like System.Round() performs) is required: Round any fraction > 0.5 away from 0.
      Round := ((UInt64(1) shl (CMantissaBits - 2 - Exponent)) and Mantissa) <> 0;
      Sticky := ((Int64(-1) shr (Exponent + (64 - CMantissaBits + 2))) and Mantissa) <> 0;
    end;
  end;

  // Shift mantissa left or right to get the most bits out of it before converting to BigInteger.
  if Shift > CMantissaBits then
    Mantissa := Mantissa shl (Shift - CMantissaBits)
  else
    Mantissa := Mantissa shr (CMantissaBits - Shift);

  // Round shifted mantissa.
  if ((RoundingMode = rmSchool) and Guard) or
     ((RoundingMode = rmRound) and (Guard and (Round or Sticky))) then
    Inc(Mantissa);

  // Turn shifted mantissa (a UInt64) into BigInteger.
  Self := 0;
  Self.Create(UInt64(Mantissa));

  // Shift left by the remaining value of the exponent.
  if Exponent > Shift then
    Self := Self shl (Exponent - Shift);
  if Sign then
    FSize := FSize or SignMask;
  Compact;
end;

{$IFNDEF NoAnsi}
constructor BigInteger.Create(const Value: PAnsiChar);
begin
  if not TryParse(string(AnsiString(Value)), Self) then
    Error(ecParse, [string(AnsiString(Value)), 'BigInteger']);
end;
{$ENDIF}

constructor BigInteger.Create(const Value: PWideChar);
begin
  if not TryParse(Value, Self) then
    Error(ecParse, [Value, 'BigInteger']);
end;

// Bytes are considered to contain value in two's complement format.
constructor BigInteger.Create(const Bytes: array of Byte);
var
  Limbs: TMagnitude;
  Negative: Boolean;
begin
  Negative := Bytes[High(Bytes)] > Byte(High(Shortint));
  SetLength(Limbs, (Length(Bytes) + 3) div 4);
  if Negative then
    Limbs[High(Limbs)] := TLimb(-1);
  Move((@Bytes[0])^, PLimb(Limbs)^, Length(Bytes));
  if Negative then
    InternalNegate(PLimb(Limbs), PLimb(Limbs), Length(Limbs));
  Create(Limbs, Negative);
  Compact;
end;

// This assumes sign-magnitude format.
constructor BigInteger.Create(const Limbs: array of TLimb; Negative: Boolean);
var
  LSize: Integer;
begin
  LSize := Length(Limbs);
  if LSize > 0 then
  begin
    MakeSize(LSize);
    FSize := LSize or (Ord(Negative) * SignMask);
    CopyLimbs(@Limbs[0], PLimb(FData), LSize);
    Compact;
  end
  else
    FSize := 0;
end;

constructor BigInteger.Create(NumBits: Integer; Random: TRandom32Proc);
var
  I: Integer;
begin
  if NumBits <= 0 then
  begin
    FSize := 0;
    FData := nil;
    Exit;
  end;

  FSize := (NumBits + CLimbBits - 1) div CLimbBits;
  SetLength(FData, (4 * FSize + 3) div 4);
  for I := 0 to FSize - 1 do
    FData[I] := Random();

  // At most Numbits bits, so mask top limb.
  FData[FSize - 1] := FData[FSize - 1] and (1 shl (NumBits and CLimbBits) - 1);
  Compact;
end;

(*
constructor BigInteger.Create(NumBits: Integer; const Random: IRandom);
var
  Bytes: TArray<Byte>;
  Bits: Byte;
begin
  if NumBits = 0 then
  begin
    ShallowCopy(Zero, Self);
    Exit;
  end;

  SetLength(Bytes, (NumBits + 7) shr 3 + 1);
  Random.NextBytes(Bytes);

  // One byte too many was allocated, to get a top byte of 0, i.e. always positive.
  Bytes[High(Bytes)] := 0;

  // Set bits above required bit length to 0.
  Bits := NumBits and $07;
  if Bits = 0 then
    Bits := 8;
  Bytes[High(Bytes) - 1] := Bytes[High(Bytes) - 1] and ($FF shr (8 - Bits));
  Create(Bytes);
  Compact;
//  Assert(BitLength <= Numbits, Format('BitLength (%d) >= NumBits (%d): %s', [BitLength, NumBits, Self.ToString(2)]));
end;     *)

function BigInteger.GetAllocated: Integer;
begin
  Result := Length(FData);
end;

function BigInteger.IsEven: Boolean;
begin
  Result := IsZero or ((FData[0] and 1) = 0);
end;

function BigInteger.IsNegative: Boolean;
begin
  Result := Assigned(FData) and (FSize < 0);
end;

function BigInteger.IsOne: Boolean;
begin
  Result := Assigned(FData) and (FSize = 1) and (FData[0] = 1);
end;

function BigInteger.IsPositive: Boolean;
begin
  Result := Assigned(FData) and (FSize > 0);
end;

function BigInteger.IsPowerOfTwo: Boolean;
var
  FirstNonZeroIndex: Integer;
  AHigh: Integer;
begin
  AHigh := (FSize and SizeMask) - 1;
  if (FData = nil) or not {Velthuis.Numerics.}VIsPowerOfTwo(FData[AHigh]) then
    Result := False
  else
  begin
    FirstNonZeroIndex := 0;

    // All limbs below top one must be 0
    while FData[FirstNonZeroIndex] = 0 do
      Inc(FirstNonZeroIndex);

    // Top limb must be power of two.
    Result := (FirstNonZeroIndex = AHigh);
  end;
end;

function BigInteger.GetSign: Integer;
begin
  if FData = nil then
  begin
    FSize := 0;
    Exit(0);
  end;

  Result := 2 * Ord(FSize > 0) - 1;
end;

function BigInteger.GetSize: Integer;
begin
  if FData = nil then
    FSize := 0;
  Result := FSize and SizeMask;
end;

function BigInteger.Data: PLimb;
begin
  Result := PLimb(FData);
end;

class operator BigInteger.GreaterThan(const Left, Right: BigInteger): Boolean;
begin
  Result := Compare(Left, Right) > 0;
//  Result := not (Left <= Right);
end;

class operator BigInteger.GreaterThanOrEqual(const Left, Right: BigInteger): Boolean;
begin
  Result := Compare(left, Right) >= 0;
end;

// Divide and Conquer. For N = 100,000, this is 20 x as fast as a plain iterative multiplication.
class function BigInteger.Factorial(N: Integer): BigInteger;

  ////////////////////////////////////////////////////////////////////////////
  // Alternative algorithm:                                                 //
  // [1 2 3 4 5 6 7 8 9] --> [1*9 2*8 3*7 4*6 5] = [9 16 21 24 5]           //
  // [9 16 21 24 5] --> [9*5 16*24 21] = [45 384 21]                        //
  // [45 384 21] --> [45*21 384] = [945 384]                                //
  // Result = 945 * 384 = 362880 = 9!                                       //
  // But that is a little slower than the following and needs an array      //
  // of (N div 2) BigIntegers.                                              //
  ////////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////////
  // Optimization: every even integer is shifted right by 1. The end result //
  // is shifted back by an equal amount of bits (n div 2).                  //
  ////////////////////////////////////////////////////////////////////////////

  function MultiplyRange(First, Last: Integer): BigInteger;
  var
    Split: Integer;
  begin
    if Last - First <= 3 then
    begin
      if Odd(First) then
        case Last - First of
          0: Result := BigInteger(First);
          1: Result := BigInteger(First) * BigInteger(Last shr 1);
          2: Result := BigInteger(First) * BigInteger((First + 1) shr 1) * BigInteger(Last);
          3: Result := BigInteger(First) * BigInteger((First + 1) shr 1) * BigInteger(First + 2) * BigInteger(Last shr 1);
        end
      else
        case Last - First of
          0: Result := BigInteger(First shr 1);
          1: Result := BigInteger(First shr 1) * BigInteger(Last);
          2: Result := BigInteger(First shr 1) * BigInteger(First + 1) * BigInteger(Last shr 1);
          3: Result := BigInteger(First shr 1) * BigInteger(First + 1) * BigInteger(First shr 1 + 1) * BigInteger(Last);
        end;
    end
    else
    begin
      Split := (First + Last) shr 1;
      Result := MultiplyRange(First, Split) * MultiplyRange(Split + 1, Last);
    end;
  end;

begin
  if N <= 0 then
    Result := 0
  else if N = 1 then
    Result := 1
  else
    Result := MultiplyRange(2, N) shl (N shr 1);
end;

// https://www.nayuki.io/page/fast-fibonacci-algorithms
// https://codegolf.stackexchange.com/questions/3191/write-the-fastest-fibonacci
// https://math.stackexchange.com/questions/1124590/need-help-understanding-fibonacci-fast-doubling-proof
class function BigInteger.Fibonacci(N: Integer): BigInteger;

////////////////////////////////////////////////////////////////////////////////////////////////////
// So called "fast doubling". Relies on the following formulas:                                   //
//                                                                                                //
//   fib(2n)     = fib(n)*(2*fib(n+1) – fib(n))                                                       //
//   fib(2n + 1) = fib(n)^2 + fib(n+1)^2                                                          //
//                                                                                                //
// Another method relies on the fact that if we exponentiate the simple matrix below, we get:     //
//                                                                                                //
//   [  1   1  ]^n  = [ fib(n+1)  fib(n)   ]                                                      //
//   [  1   0  ]      [ fib(n)    fib(n-1) ]                                                      //
//                                                                                                //
// Most methods use exponentiation by squaring to exponentiate the matrix. But we must write      //
// a (rather slow?) matrix multiplication algorithm, which generally makes it slower than         //
// fast doubling.                                                                                 //
////////////////////////////////////////////////////////////////////////////////////////////////////

var
  FibOfN, FibOfNPlus1, Temp: BigInteger;
  FibOf2N, FibOf2NPlus1: BigInteger;
  Bit: Integer;
begin
  FibOfN := BigInteger.Zero;
  FibOfNPlus1 := BigInteger.One;
  Bit := VHighestOneBit(N);
  Temp := N;
  while Bit <> 0 do
  begin
    FibOf2N := FibOfN * ((FibOfNPlus1 shl 1) - FibOfN);          // fib(2n)     = fib(n) * (2 * fib(n + 1) - fib(n))
    FibOf2NPlus1 := FibOfN * FibOfN + FibOfNPlus1 * FibOfNPlus1; // fib(2n + 1) = fib(n)^2 + fib(n + 1)^2
    FibOfN := FibOf2N;
    FibOfNPlus1 := FibOf2NPlus1;

    // Advance by one conditionally
    if (N and Bit) <> 0 then
    begin
      Temp := FibOfN + FibOfNPlus1;
      FibOfN := FibOfNPlus1;
      FibOfNPlus1 := Temp;
    end;

    Bit := Bit shr 1;
  end;
  Result := FibOfN;
end;

// http://en.wikipedia.org/wiki/Binary_GCD_algorithm
class function BigInteger.GreatestCommonDivisor(const Left, Right: BigInteger): BigInteger;
var
  Shift: Integer;
  ALeft, ARight: BigInteger;
  Temp: BigInteger;
begin
  // GCD(left, 0) = left; GCD(0, right) = right; GCD(0, 0) = 0
  if Left.IsZero then
    Exit(Abs(Right));
  if Right.IsZero then
    Exit(Abs(Left));

  ALeft := Abs(Left);
  ARight := Abs(Right);

  // Let Shift = Log2(K), where K is the greatest power of 2 dividing
  // both ALeft and ARight.
  Shift := IntMin(Left.LowestSetBit, Right.LowestSetBit);
  ALeft := ALeft shr Shift;
  ARight := ARight shr Shift;

  while ALeft.IsEven do
    ALeft := ALeft shr 1;

  // Now, ALeft is always odd.
  repeat
    // Remove all factors of 2 in ARight, since they are not in common.
    // ARight is not 0, so the loop will terminate
    while ARight.IsEven do
      ARight := ARight shr 1;

    // ALeft and ARight are both odd. Swap if necessary, so that ALeft <= ARight,
    // then set ARight to ARight - ALeft (which is even).
    if ALeft > ARight then
    begin
      // Swap ALeft and ARight.
      Temp := ALeft;
      Aleft := ARight;
      ARight := Temp;
    end;
    ARight := ARight - ALeft;
  until ARight = 0;

  // Restore common factors of 2.
  Result := ALeft shl Shift;
end;

class procedure BigInteger.Hexadecimal;
begin
  FBase := 16;
end;

class procedure BigInteger.Hex;
begin
  FBase := 16;
end;

class operator BigInteger.Implicit(const Value: Int32): BigInteger;
begin
  // Note: Create will also get BigIntegers from the ValueCache, but this is a little faster.
  if (Value >= Low(ValueCache)) and (Value <= High(ValueCache)) then
    Result := ValueCache[Value]
  else
    Result := BigInteger.Create(Value);
end;

class operator BigInteger.Implicit(const Value: UInt32): BigInteger;
begin
  if Value <= UInt32(High(ValueCache)) then
    Result := ValueCache[Value]
  else
    Result := BigInteger.Create(Value);
end;

class operator BigInteger.Implicit(const Value: Int64): BigInteger;
begin
  if (Value >= Low(ValueCache)) and (Value <= High(ValueCache)) then
    Result := ValueCache[Value]
  else
    Result := BigInteger.Create(Value);
end;

class operator BigInteger.Implicit(const Value: UInt64): BigInteger;
begin
  if Value <= High(ValueCache) then
    Result := ValueCache[Value]
  else
    Result := BigInteger.Create(Value);
end;

class constructor BigInteger.Initialize;
var
  I: Integer;
  J: Integer;
  LPower: BigInteger;
  LMaxPower: BigInteger;
begin
  for I := Low(ValueCache) to High(ValueCache) do
  begin
    if I <> 0 then
    begin
      SetLength(ValueCache[I].FData, 4);
      if I < 0 then
      begin
        ValueCache[I].FData[0] := -I;
        ValueCache[I].FSize := 1 or SignMask;
      end
      else
      begin
        ValueCache[I].FData[0] := I;
        ValueCache[I].FSize := 1;
      end;
    end
    else
    begin
      ValueCache[0].FData := nil;
      Valuecache[0].FSize := 0;
    end;
  end;
  MinusOne := ValueCache[-1];
  Zero := ValueCache[0];
  One := ValueCache[1];
  Ten := ValueCache[10];
  FBase := 10;
  FRoundingMode := rmTruncate;
  FLog2 := System.Ln(2.0);
{$IFNDEF PUREPASCAL}
  // See comments for BigInteger.InternalAddEmu.
  BigInteger.DetectPartialFlagsStall;
{$ELSE}
  FInternalAdd := InternalAddPurePascal;
  FInternalSubtract := InternalSubtractPurePascal;
{$ENDIF}
  for I := Low(TNumberBase) to High(TNumberBase) do
  begin
    LMaxPower := CBaseInfos[I].MaxPower;
    SetLength(CBasePowers[I], 10);
    LPower := BigInteger.One;
    for J := 0 to High(CBasePowers[I]) do
    begin
      CBasePowers[I, J] := LPower;
      LPower := LPower * LMaxPower;
    end;
//    LMaxPower := BigInteger.Zero; // $$RV Rio: leak if not set to zero.
  end;
end;

class operator BigInteger.IntDivide(const Left, Right: BigInteger): BigInteger;
begin
  Result := Divide(Left, Right);
end;

class operator BigInteger.IntDivide(const Left: BigInteger; Right: UInt16): BigInteger;
begin
  Result := Divide(Left, Right);
end;

class operator BigInteger.IntDivide(const Left: BigInteger; Right: UInt32): BigInteger;
begin
  Result := Divide(Left, Right);
end;

{$IFNDEF PUREPASCAL}
class procedure BigInteger.InternalAddModified(Left, Right, Result: PLimb; LSize, RSize: Integer);
{$IFDEF WIN32}
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX

        MOV     ESI,EAX                 // ESI: Left
        MOV     EDI,EDX                 // EDI: Right
        MOV     EBX,ECX                 // EBX: Result

        MOV     ECX,RSize
        MOV     EDX,LSize
        CMP     EDX,ECX                 // Swap Left/Right and LSize/RSize if RSize > LSize
        JAE     @SkipSwap
        XCHG    ECX,EDX
        XCHG    ESI,EDI

@SkipSwap:

        // Here: Left longer than or equal length as Right.

        SUB     EDX,ECX
        PUSH    EDX                     // Difference in sizes --> length of rest loop.
        MOV     EDX,ECX                 // Smallest size.
        AND     EDX,CUnrollMask         // Tail counter.
        SHR     ECX,CUnrollShift        // Unrolled loop counter.
        CLC
        JE      @MainTail

// Intel proposal:
//   Intel 64 and IA-32 Architectures Optimization Reference Manual
//   3.5.2.6 Partial Flag Register Stalls -- Example 3-29

        XOR     EAX,EAX

        .ALIGN  16

@MainLoop:

        // Unrolled main loop.

        ADD     EAX,[ESI]
        ADC     EAX,[EDI]
        MOV     [EBX],EAX

        MOV     EAX,[ESI + CLimbSize]
        ADC     EAX,[EDI + CLimbSize]
        MOV     [EBX + CLimbSize],EAX

        MOV     EAX,[ESI + 2*CLimbSize]
        ADC     EAX,[EDI + 2*CLimbSize]
        MOV     [EBX + 2*CLimbSize],EAX

        MOV     EAX,[ESI + 3*CLimbSize]
        ADC     EAX,[EDI + 3*CLimbSize]
        MOV     [EBX + 3*CLimbSize],EAX

        SETC    AL
        MOVZX   EAX,AL

        LEA     ESI,[ESI + CUnrollIncrement*CLimbSize]
        LEA     EDI,[EDI + CUnrollIncrement*CLimbSize]
        LEA     EBX,[EBX + CUnrollIncrement*CLimbSize]

        DEC     ECX
        JNZ     @MainLoop

@MainTail:

        LEA     ESI,[ESI + EDX*CLimbSize]
        LEA     EDI,[EDI + EDX*CLimbSize]
        LEA     EBX,[EBX + EDX*CLimbSize]

        LEA     ECX,[@JumpsMain]
        JMP     [ECX + EDX*TYPE Pointer]

        .ALIGN  4

@JumpsMain:

        DD      @DoRestLoop
        DD      @Main1
        DD      @Main2
        DD      @Main3

@Main3:

        MOV     EAX,[ESI - 3*CLimbSize]
        ADC     EAX,[EDI - 3*CLimbSize]
        MOV     [EBX - 3*CLimbSize],EAX

@Main2:

        MOV     EAX,[ESI - 2*CLimbSize]
        ADC     EAX,[EDI - 2*CLimbSize]
        MOV     [EBX - 2*CLimbSize],EAX

@Main1:

        MOV     EAX,[ESI - CLimbSize]
        ADC     EAX,[EDI - CLimbSize]
        MOV     [EBX - CLimbSize],EAX

@DoRestLoop:

        SETC    AL                      // Save Carry Flag
        XOR     EDI,EDI
        POP     ECX
        MOV     EDX,ECX
        AND     EDX,CUnrollMask         // Tail counter
        SHR     ECX,CUnrollShift        // Unrolled loop counter
        ADD     AL,255                  // Restore Carry Flag.
        JECXZ   @RestLastN

        .ALIGN  16

@RestLoop:

        /////////////////////////////////////////////////////////////////////
        // Tests showed that branching out of the loop as soon as the      //
        // carry is clear (using JNC @label, where @label is in a second   //
        // loop that only copies and does not add anymore) actually makes  //
        // the code slightly SLOWER, most of the time.                     //
        /////////////////////////////////////////////////////////////////////

        MOV     EAX,[ESI]
        ADC     EAX,EDI
        MOV     [EBX],EAX

        MOV     EAX,[ESI + CLimbSize]
        ADC     EAX,EDI
        MOV     [EBX + CLimbSize],EAX

        MOV     EAX,[ESI + 2*CLimbSize]
        ADC     EAX,EDI
        MOV     [EBX + 2*CLimbSize],EAX

        MOV     EAX,[ESI + 3*CLimbSize]
        ADC     EAX,EDI
        MOV     [EBX + 3*CLimbSize],EAX

        SETC    AL
        MOVZX   EAX,AL

        LEA     ESI,[ESI + CUnrollIncrement*CLimbSize]
        LEA     EBX,[EBX + CUnrollIncrement*CLimbSize]

        LOOP    @RestLoop

@RestLastN:

        LEA     ESI,[ESI + EDX*CLimbSize]
        LEA     EBX,[EBX + EDX*CLimbSize]

        LEA     ECX,[@RestJumps]
        JMP     [ECX + EDX*TYPE Pointer]

        .ALIGN  4

@RestJumps:

        DD      @LastLimb
        DD      @Rest1
        DD      @Rest2
        DD      @Rest3

@Rest3:

        MOV     EAX,[ESI - 3*CLimbSize]
        ADC     EAX,EDI
        MOV     [EBX - 3*CLimbSize],EAX

@Rest2:

        MOV     EAX,[ESI - 2*CLimbSize]
        ADC     EAX,EDI
        MOV     [EBX - 2*CLimbSize],EAX

@Rest1:

        MOV     EAX,[ESI - CLimbSize]
        ADC     EAX,EDI
        MOV     [EBX - CLimbSize],EAX

@LastLimb:

        ADC     EDI,EDI
        MOV     [EBX],EDI

@Exit:

        POP     EBX
        POP     EDI
        POP     ESI
end;
{$ELSE WIN64}
asm
        MOV     R10,RCX
        MOV     ECX,RSize

        CMP     R9D,ECX
        JAE     @SkipSwap
        XCHG    ECX,R9D
        XCHG    R10,RDX

@SkipSwap:

        SUB     R9D,ECX
        PUSH    R9

        MOV     R9D,ECX
        AND     R9D,CUnrollMask
        SHR     ECX,CUnrollShift

        CLC
        JE      @MainTail

        .ALIGN  16

@MainLoop:

        MOV     RAX,[R10]
        ADC     RAX,[RDX]
        MOV     [R8],RAX

        MOV     RAX,[R10 + DLimbSize]
        ADC     RAX,[RDX + DLimbSize]
        MOV     [R8 + DLimbSize],RAX

        LEA     R10,[R10 + 2*DLimbSize]
        LEA     RDX,[RDX + 2*DLimbSize]
        LEA     R8,[R8 + 2*DLimbSize]

        LEA     RCX,[RCX - 1]
        JECXZ   @MainTail
        JMP     @MainLoop

@MainTail:

        LEA     RCX,[@MainJumps]
        JMP     [RCX + R9*TYPE Pointer]

        .ALIGN  16

@MainJumps:

        DQ      @DoRestLoop
        DQ      @Main1
        DQ      @Main2
        DQ      @Main3

@Main3:

        MOV     RAX,[R10]
        ADC     RAX,[RDX]
        MOV     [R8],RAX

        MOV     EAX,[R10 + 2*CLimbSize]
        ADC     EAX,[RDX + 2*CLimbSize]
        MOV     [R8 + 2*CLimbSize],EAX

        LEA     R10,[R10 + 3*CLimbSize]
        LEA     RDX,[RDX + 3*CLimbSize]
        LEA     R8,[R8 + 3*CLimbSize]

        JMP     @DoRestLoop

@Main2:

        MOV     RAX,[R10]
        ADC     RAX,[RDX]
        MOV     [R8],RAX

        LEA     R10,[R10 + 2*CLimbSize]
        LEA     RDX,[RDX + 2*CLimbSize]
        LEA     R8,[R8 + 2*CLimbSize]

        JMP     @DoRestLoop

@Main1:

        MOV     EAX,[R10]
        ADC     EAX,[RDX]
        MOV     [R8],EAX

        LEA     R10,[R10 + CLimbSize]
        LEA     RDX,[RDX + CLimbSize]
        LEA     R8,[R8 + CLimbSize]

@DoRestLoop:

        SETC    AL                      // Save Carry Flag

        XOR     EDX,EDX

        POP     RCX
        MOV     R9D,ECX
        AND     R9D,CUnrollMask
        SHR     ECX,CUnrollShift

        ADD     AL,255                  // Restore Carry Flag.

        JECXZ   @RestLast3

        .ALIGN  16

@RestLoop:

        MOV     RAX,[R10]
        ADC     RAX,RDX
        MOV     [R8],RAX

        MOV     RAX,[R10 + DLimbSize]
        ADC     RAX,RDX
        MOV     [R8 + DLimbSize],RAX

        LEA     R10,[R10 + 2*DLimbSize]
        LEA     R8,[R8 + 2*DLimbSize]

        LEA     RCX,[RCX - 1]
        JECXZ   @RestLast3
        JMP     @RestLoop

@RestLast3:

        LEA     RCX,[@RestJumps]
        JMP     [RCX + R9*TYPE Pointer]

        .ALIGN  16

@RestJumps:

        DQ      @LastLimb
        DQ      @Rest1
        DQ      @Rest2
        DQ      @Rest3

@Rest3:

        MOV     RAX,[R10]
        ADC     RAX,RDX
        MOV     [R8],RAX

        MOV     EAX,[R10 + 2*CLimbSize]
        ADC     EAX,EDX
        MOV     [R8 + 2*CLimbSize],EAX

        LEA     R8,[R8 + 3*CLimbSize]

        JMP     @LastLimb

@Rest2:

        MOV     RAX,[R10]
        ADC     RAX,RDX
        MOV     [R8],RAX

        LEA     R8,[R8 + 2*CLimbSize]

        JMP     @LastLimb

@Rest1:

        MOV     EAX,[R10]
        ADC     EAX,EDX
        MOV     [R8],EAX

        LEA     R8,[R8 + CLimbSize]

@LastLimb:

        ADC     EDX,EDX
        MOV     [R8],EDX

@Exit:

end;
{$ENDIF WIN32/WIN64}

class procedure BigInteger.InternalAddPlain(Left, Right, Result: PLimb; LSize, RSize: Integer);

////////////////////////////////////////////////////
/// To understand the code, please read this:    ///
///                                              ///
///   http://stackoverflow.com/q/32084204/95954  ///
///                                              ///
/// especially Peter Cordes' answer:             ///
///                                              ///
///   http://stackoverflow.com/a/32087095/95954  ///
////////////////////////////////////////////////////

{$IFDEF WIN32}
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX

        MOV     ESI,EAX                         // Left
        MOV     EDI,EDX                         // Right
        MOV     EBX,ECX                         // Result

        MOV     ECX,RSize
        MOV     EDX,LSize

        CMP     EDX,ECX
        JAE     @SkipSwap
        XCHG    ECX,EDX
        XCHG    ESI,EDI

@SkipSwap:

        SUB     EDX,ECX
        PUSH    EDX
        XOR     EDX,EDX

        XOR     EAX,EAX

        MOV     EDX,ECX
        AND     EDX,CUnrollMask
        SHR     ECX,CUnrollShift

        CLC
        JE      @MainTail

@MainLoop:

        MOV     EAX,[ESI]
        ADC     EAX,[EDI]
        MOV     [EBX],EAX

        MOV     EAX,[ESI + CLimbSize]
        ADC     EAX,[EDI + CLimbSize]
        MOV     [EBX + CLimbSize],EAX

        MOV     EAX,[ESI + 2*CLimbSize]
        ADC     EAX,[EDI + 2*CLimbSize]
        MOV     [EBX + 2*CLimbSize],EAX

        MOV     EAX,[ESI + 3*CLimbSize]
        ADC     EAX,[EDI + 3*CLimbSize]
        MOV     [EBX + 3*CLimbSize],EAX

        LEA     ESI,[ESI + 4*CLimbSize]
        LEA     EDI,[EDI + 4*CLimbSize]
        LEA     EBX,[EBX + 4*CLimbSize]

        DEC     ECX                     // Does not affect carry flag, but that can cause partial flags stall.
        JNE     @MainLoop

@MainTail:

        LEA     ESI,[ESI + EDX*CLimbSize]
        LEA     EDI,[EDI + EDX*CLimbSize]
        LEA     EBX,[EBX + EDX*CLimbSize]

        LEA     ECX,[@JumpsMain]
        JMP     [ECX + EDX*TYPE Pointer]

        .ALIGN  16

@JumpsMain:

        DD      @DoRestLoop
        DD      @Main1
        DD      @Main2
        DD      @Main3

@Main3:

        MOV     EAX,[ESI - 3*CLimbSize]
        ADC     EAX,[EDI - 3*CLimbSize]
        MOV     [EBX - 3*CLimbSize],EAX

@Main2:

        MOV     EAX,[ESI - 2*CLimbSize]
        ADC     EAX,[EDI - 2*CLimbSize]
        MOV     [EBX - 2*CLimbSize],EAX

@Main1:

        MOV     EAX,[ESI - CLimbSize]
        ADC     EAX,[EDI - CLimbSize]
        MOV     [EBX - CLimbSize],EAX

@DoRestLoop:

        SETC    AL                      // Save Carry Flag

        XOR     EDI,EDI

        POP     ECX
        MOV     EDX,ECX
        AND     EDX,CUnrollMask
        SHR     ECX,CUnrollShift

        ADD     AL,255                  // Restore Carry Flag.

        INC     ECX
        DEC     ECX
        JE      @RestLast3              // JECXZ is slower than INC/DEC/JE

@RestLoop:

        MOV     EAX,[ESI]
        ADC     EAX,EDI
        MOV     [EBX],EAX

        MOV     EAX,[ESI + CLimbSize]
        ADC     EAX,EDI
        MOV     [EBX + CLimbSize],EAX

        MOV     EAX,[ESI + 2*CLimbSize]
        ADC     EAX,EDI
        MOV     [EBX + 2*CLimbSize],EAX

        MOV     EAX,[ESI + 3*CLimbSize]
        ADC     EAX,EDI
        MOV     [EBX + 3*CLimbSize],EAX

        LEA     ESI,[ESI + 4*CLimbSize]
        LEA     EBX,[EBX + 4*CLimbSize]

        DEC     ECX
        JNE     @RestLoop

@RestLast3:

        LEA     ESI,[ESI + EDX*CLimbSize]
        LEA     EBX,[EBX + EDX*CLimbSize]

        LEA     ECX,[@RestJumps]
        JMP     [ECX + EDX*TYPE Pointer]

        .ALIGN  16

@RestJumps:

        DD      @LastLimb
        DD      @Rest1
        DD      @Rest2
        DD      @Rest3

@Rest3:

        MOV     EAX,[ESI - 3*CLimbSize]
        ADC     EAX,EDI
        MOV     [EBX - 3*CLimbSize],EAX

@Rest2:

        MOV     EAX,[ESI - 2*CLimbSize]
        ADC     EAX,EDI
        MOV     [EBX - 2*CLimbSize],EAX

@Rest1:

        MOV     EAX,[ESI - CLimbSize]
        ADC     EAX,EDI
        MOV     [EBX - CLimbSize],EAX

@LastLimb:

        ADC     EDI,EDI
        MOV     [EBX],EDI

@Exit:

        POP     EBX
        POP     EDI
        POP     ESI
end;
{$ELSE WIN64}
asm
        MOV     R10,RCX
        MOV     ECX,RSize

        CMP     R9D,ECX
        JAE     @SkipSwap
        XCHG    ECX,R9D
        XCHG    R10,RDX

@SkipSwap:

        SUB     R9D,ECX
        PUSH    R9

        MOV     R9D,ECX
        AND     R9D,CUnrollMask
        SHR     ECX,CUnrollShift

        CLC
        JE      @MainTail

@MainLoop:

        MOV     RAX,[R10]
        ADC     RAX,[RDX]
        MOV     [R8],RAX

        MOV     RAX,[R10 + DLimbSize]
        ADC     RAX,[RDX + DLimbSize]
        MOV     [R8 + DLimbSize],RAX

        LEA     R10,[R10 + 2*DLimbSize]
        LEA     RDX,[RDX + 2*DLimbSize]
        LEA     R8,[R8 + 2*DLimbSize]

        DEC     ECX
        JNE     @MainLoop

@MainTail:

        LEA     RCX,[@MainJumps]
        JMP     [RCX + R9*TYPE Pointer]

        .ALIGN  16

@MainJumps:

        DQ      @DoRestLoop
        DQ      @Main1
        DQ      @Main2
        DQ      @Main3

@Main3:

        MOV     RAX,[R10]
        ADC     RAX,[RDX]
        MOV     [R8],RAX

        MOV     EAX,[R10 + 2*CLimbSize]
        ADC     EAX,[RDX + 2*CLimbSize]
        MOV     [R8 + 2*CLimbSize],EAX

        LEA     R10,[R10 + 3*CLimbSize]
        LEA     RDX,[RDX + 3*CLimbSize]
        LEA     R8,[R8 + 3*CLimbSize]

        JMP     @DoRestLoop

@Main2:

        MOV     RAX,[R10]
        ADC     RAX,[RDX]
        MOV     [R8],RAX

        LEA     R10,[R10 + 2*CLimbSize]
        LEA     RDX,[RDX + 2*CLimbSize]
        LEA     R8,[R8 + 2*CLimbSize]

        JMP     @DoRestLoop

@Main1:

        MOV     EAX,[R10]
        ADC     EAX,[RDX]
        MOV     [R8],EAX

        LEA     R10,[R10 + CLimbSize]
        LEA     RDX,[RDX + CLimbSize]
        LEA     R8,[R8 + CLimbSize]

@DoRestLoop:

        SETC    AL                      // Save Carry Flag

        XOR     EDX,EDX

        POP     RCX
        MOV     R9D,ECX
        AND     R9D,CUnrollMask
        SHR     ECX,CUnrollShift

        ADD     AL,255                  // Restore Carry Flag.

        INC     ECX
        DEC     ECX
        JE      @RestLast3

@RestLoop:

        MOV     RAX,[R10]
        ADC     RAX,RDX
        MOV     [R8],RAX

        MOV     RAX,[R10 + DLimbSize]
        ADC     RAX,RDX
        MOV     [R8 + DLimbSize],RAX

        LEA     R10,[R10 + 2*DLimbSize]
        LEA     R8,[R8 + 2*DLimbSize]

        DEC     ECX
        JNE     @RestLoop

@RestLast3:

        LEA     RCX,[@RestJumps]
        JMP     [RCX + R9*TYPE Pointer]

        // If necessary, align second jump table with NOPs

        // -- Aligned.

@RestJumps:

        DQ      @LastLimb
        DQ      @Rest1
        DQ      @Rest2
        DQ      @Rest3

@Rest3:

        MOV     RAX,[R10]
        ADC     RAX,RDX
        MOV     [R8],RAX

        MOV     EAX,[R10 + DLimbSize]
        ADC     EAX,EDX
        MOV     [R8 + DLimbSize],EAX

        LEA     R8,[R8 + 3*CLimbSize]

        JMP     @LastLimb

@Rest2:

        MOV     RAX,[R10]
        ADC     RAX,RDX
        MOV     [R8],RAX

        LEA     R8,[R8 + DLimbSize]

        JMP     @LastLimb

@Rest1:

        MOV     EAX,[R10]
        ADC     EAX,EDX
        MOV     [R8],EAX

        LEA     R8,[R8 + CLimbSize]

@LastLimb:

        ADC     EDX,EDX
        MOV     [R8],EDX

@Exit:

end;
{$ENDIF !WIN32}
{$ENDIF !PUREPASCAL}

{$IFDEF PUREPASCAL}
class procedure BigInteger.InternalAddPurePascal(Left, Right, Result: PLimb; LSize, RSize: Integer);
var
  LCount, LTail: Integer;
  Sum: NativeUInt;
  I: Integer;
  L: PLimb;
begin
  if LSize < RSize then
  begin
    I := LSize;
    LSize := RSize;
    RSize := I;
    L := Left;
    Left := Right;
    Right := L;
  end;

  Sum := 0;

  Dec(LSize, RSize);          // LSize is length of non-overlapping part.

  LTail := RSize and CUnrollMask;
  LCount := RSize shr CUnrollShift;

  while LCount > 0 do
  begin
  {$IFDEF CPU64BITS}
    Sum := UInt64(Left[0]) + Right[0] + (Sum shr 32);
    Result[0] := TLimb(Sum);

    Sum := UInt64(Left[1]) + Right[1] + (Sum shr 32);
    Result[1] := TLimb(Sum);

    Sum := UInt64(Left[2]) + Right[2] + (Sum shr 32);
    Result[2] := TLimb(Sum);

    Sum := UInt64(Left[3]) + Right[3] + (Sum shr 32);
    Result[3] := TLimb(Sum);
  {$ELSE}
    Sum := UInt32(PUInt16(Left)[0]) + PUInt16(Right)[0] + (Sum shr 16);
    PUInt16(Result)[0] := UInt16(Sum);

    Sum := UInt32(PUInt16(Left)[1]) + PUInt16(Right)[1] + (Sum shr 16);
    PUInt16(Result)[1] := UInt16(Sum);

    Sum := UInt32(PUInt16(Left)[2]) + PUInt16(Right)[2] + (Sum shr 16);
    PUInt16(Result)[2] := UInt16(Sum);

    Sum := UInt32(PUInt16(Left)[3]) + PUInt16(Right)[3] + (Sum shr 16);
    PUInt16(Result)[3] := UInt16(Sum);

    Sum := UInt32(PUInt16(Left)[4]) + PUInt16(Right)[4] + (Sum shr 16);
    PUInt16(Result)[4] := UInt16(Sum);

    Sum := UInt32(PUInt16(Left)[5]) + PUInt16(Right)[5] + (Sum shr 16);
    PUInt16(Result)[5] := UInt16(Sum);

    Sum := UInt32(PUInt16(Left)[6]) + PUInt16(Right)[6] + (Sum shr 16);
    PUInt16(Result)[6] := UInt16(Sum);

    Sum := UInt32(PUInt16(Left)[7]) + PUInt16(Right)[7] + (Sum shr 16);
    PUInt16(Result)[7] := UInt16(Sum);
  {$ENDIF}

    Inc(Left, CUnrollIncrement);
    Inc(Right, CUnrollIncrement);
    Inc(Result, CUnrollIncrement);
    Dec(LCount);
  end;
  while LTail > 0 do
  begin
  {$IFDEF CPU64BITS}
    Sum := UInt64(Left[0]) + Right[0] + (Sum shr 32);
    Result[0] := TLimb(Sum);
  {$ELSE}
    Sum := UInt32(PUInt16(Left)[0]) + PUInt16(Right)[0] + (Sum shr 16);
    PUInt16(Result)[0] := UInt16(Sum);

    Sum := UInt32(PUInt16(Left)[1]) + PUInt16(Right)[1] + (Sum shr 16);
    PUInt16(Result)[1] := UInt16(Sum);
  {$ENDIF}

    Inc(Left);
    Inc(Right);
    Inc(Result);
    Dec(LTail);
  end;

  LTail := LSize and CUnrollMask;
  LCount := LSize shr CUnrollShift;

  while LCount > 0 do
  begin
  {$IFDEF CPU64BITS}
    Sum := UInt64(Left[0]) + (Sum shr 32);
    Result[0] := TLimb(Sum);

    Sum := UInt64(Left[1]) + (Sum shr 32);
    Result[1] := TLimb(Sum);

    Sum := UInt64(Left[2]) + (Sum shr 32);
    Result[2] := TLimb(Sum);

    Sum := UInt64(Left[3]) + (Sum shr 32);
    Result[3] := TLimb(Sum);
  {$ELSE}
    Sum := UInt32(PUInt16(Left)[0]) + (Sum shr 16);
    PUInt16(Result)[0] := UInt16(Sum);

    Sum := UInt32(PUInt16(Left)[1]) + (Sum shr 16);
    PUInt16(Result)[1] := UInt16(Sum);

    Sum := UInt32(PUInt16(Left)[2]) + (Sum shr 16);
    PUInt16(Result)[2] := UInt16(Sum);

    Sum := UInt32(PUInt16(Left)[3]) + (Sum shr 16);
    PUInt16(Result)[3] := UInt16(Sum);

    Sum := UInt32(PUInt16(Left)[4]) + (Sum shr 16);
    PUInt16(Result)[4] := UInt16(Sum);

    Sum := UInt32(PUInt16(Left)[5]) + (Sum shr 16);
    PUInt16(Result)[5] := UInt16(Sum);

    Sum := UInt32(PUInt16(Left)[6]) + (Sum shr 16);
    PUInt16(Result)[6] := UInt16(Sum);

    Sum := UInt32(PUInt16(Left)[7]) + (Sum shr 16);
    PUInt16(Result)[7] := UInt16(Sum);
  {$ENDIF}

    Inc(Left, CUnrollIncrement);
    Inc(Result, CUnrollIncrement);
    Dec(LCount);
  end;

  while LTail > 0 do
  begin
  {$IFDEF CPU64BITS}
    Sum := UInt64(Left[0]) + (Sum shr 32);
    Result[0] := TLimb(Sum);
  {$ELSE}
    Sum := UInt32(PUInt16(Left)[0]) + (Sum shr 16);
    PUInt16(Result)[0] := UInt16(Sum);

    Sum := UInt32(PUInt16(Left)[1]) + (Sum shr 16);
    PUInt16(Result)[1] := UInt16(Sum);
  {$ENDIF}

    Inc(Left);
    Inc(Result);
    Dec(LTail);
  end;

{$IFDEF CPU64BITS}
  Result[0] := Sum shr 32;
{$ELSE}
  Result[0] := Sum shr 16;
{$ENDIF};

end;
{$ENDIF}

class procedure BigInteger.InternalMultiply(Left, Right, Result: PLimb; LSize, RSize: Integer);
{$IFDEF PUREPASCAL}

//////////////////////////////////////////////////////////////////////////////
// Tests with using a UInt32 Product and emulating 16 bit limbs produced    //
// 50% slower code.                                                         //
// 64 bit multiplication and addition doesn't generate optimal code, but    //
// it is apparently still faster than faking 16 bit limbs.                  //
//////////////////////////////////////////////////////////////////////////////
// What is really needed is a function that multiplies two UInt32 and       //
// produces an UInt64 directly, i.e. without conversion of the UInt32s into //
// UInt64 first. This is easy in assembler, but not in PUREPASCAL.          //
//////////////////////////////////////////////////////////////////////////////

type
  TUInt64 = packed record
    Lo, Hi: TLimb;
  end;
var
  Product: UInt64;
  LTail, LCount: Integer;
  CurrentRightLimb: TLimb;
  PLeft, PDest, PRight, PDestRowStart: PLimb;
  LCarry: TLimb;
begin
  // Ensure that Left is the longer of both magnitudes.
  if RSize > LSize then
  begin
    SwapPLimbs(Left, Right);
    SwapIntegers(LSize, RSize);
  end;

  // Each new row is one limb further to the left.
  PRight := Right;
  PDestRowStart := Result;

  PLeft := Left;
  PDest := PDestRowStart;
  Inc(PDestRowStart);
  CurrentRightLimb := PRight^;
  Inc(PRight);
  TUInt64(Product).Hi := 0;
  Dec(RSize);
  LCount := LSize;
  LCarry := 0;

  // First row. No previous result, so no need to add it in.
  while LCount > 0 do
  begin
    Product := UInt64(PLeft^) * CurrentRightLimb;
    Inc(Product, LCarry);
    PDest^ := TUInt64(Product).Lo;
    LCarry := TUInt64(Product).Hi;
    Inc(PLeft);
    Inc(PDest);
    Dec(LCount);
  end;
  PDest^ := TUInt64(Product).Hi;

  LTail := LSize and CUnrollMask; // Low 2 bits: 0..3.
  LSize := LSize shr CUnrollShift; // Divide by 4.
  while RSize > 0 do
  begin
    PLeft := Left;
    PDest := PDestRowStart;
    Inc(PDestRowStart);
    CurrentRightLimb := PRight^;
    Inc(PRight);

    if CurrentRightLimb <> 0 then
    begin
      LCarry := 0;
      LCount := LSize;

      // Inner loop, unrolled.
      while LCount > 0 do
      begin

        // Note: The following will not produce an overflow.
        // Proof: say B = High(TLimb) + 1 = $100000000
        // Assume PLeft[0], CurrentRightLimb, PRight[0] and Product.Hi are all
        // the maximum value (B - 1) (i.e. $FFFFFFFF).
        // Then Product = (B - 1)^2 + (B - 1) + (B - 1)
        //              = B^2 - 2*B + 1 + 2*B - 2
        //              = B^2 - 1 = $FFFFFFFFFFFFFFFF = High(UInt64)
        // so no overflow possible!

        // Note2: The previous code was
        //
        //          Product := UInt64(PLeft[0]) * CurrentRightLimb + PDest[0] + TUInt64(Product).Hi;
        //          etc...
        //
        //        The following source produces shorter generated code, but is only slightly faster
        //        than the above (3% speed increase).

        Product := UInt64(PLeft[0]) * CurrentRightLimb;
        Inc(Product, PDest[0]);
        Inc(Product, LCarry);
        PDest[0] := TLimb(Product);
        LCarry := TUInt64(Product).Hi;

        Product := UInt64(PLeft[1]) * CurrentRightLimb;
        Inc(Product, PDest[1]);
        Inc(Product, LCarry);
        PDest[1] := TLimb(Product);
        LCarry := TUInt64(Product).Hi;

        Product := UInt64(PLeft[2]) * CurrentRightLimb;
        Inc(Product, PDest[2]);
        Inc(Product, LCarry);
        PDest[2] := TLimb(Product);
        LCarry := TUInt64(Product).Hi;

        Product := UInt64(PLeft[3]) * CurrentRightLimb;
        Inc(Product, PDest[3]);
        Inc(Product, LCarry);
        PDest[3] := TLimb(Product);
        LCarry := TUInt64(Product).Hi;

        Inc(PLeft, CUnrollIncrement);
        Inc(PDest, CunrollIncrement);
        Dec(LCount);
      end;

      // Rest loop.
      LCount := LTail;
      while LCount > 0 do
      begin
        Product := UInt64(PLeft^) * CurrentRightLimb;
        Inc(Product, PDest^);
        Inc(Product, LCarry);
        LCarry := TUInt64(Product).Hi;
        PDest^ := TUInt64(Product).Lo;

        Inc(PLeft);
        Inc(PDest);
        Dec(LCount);
      end;

      // Last (top) limb of this row.
      PDest^ := TUInt64(Product).Hi;
    end;
    Dec(RSize);
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF WIN32)}
var
  SaveResult: PLimb;
  LRest, LCount: Integer;
  PRight, PDestRowStart: PLimb;
  LLeft, LRight: PLimb;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX

        MOV     SaveResult,ECX

        MOV     ESI,LSize
        MOV     EDI,RSize
        CMP     ESI,EDI
        JA      @SkipSwap

        XCHG    EAX,EDX
        XCHG    ESI,EDI
        MOV     LSize,ESI
        MOV     RSize,EDI

// The longest loop should ideally be unrolled. After this, Left should be longer or same length.

@SkipSwap:

        MOV     LLeft,EAX
        MOV     LRight,EDX

// First loop, setting up first row:

        MOV     PRight,EDX
        MOV     EDI,SaveResult
        MOV     PDestRowStart,EDI               // EDI = PDest

        MOV     ESI,LLeft                       // ESI = PLeft

// If CurrentLimbRight = 0, we can skip a lot, and simply do a FillChar

        MOV     ECX,[EDX]                       // CurrentRightLimb
        XOR     EBX,EBX                         // PreviousProductHi
        ADD     PDestRowStart,CLimbSize
        ADD     PRight,CLimbSize
        MOV     EAX,LSize
        MOV     LCount,EAX

// The setup loop fills the row without an attempt to add to the data already in the result.

@SetupLoop:

        MOV     EAX,[ESI]
        MUL     EAX,ECX                         // Uses MUL EAX,ECX syntax because of bug in XE2 assembler.
        ADD     EAX,EBX
        ADC     EDX,KZero
        MOV     [EDI],EAX
        MOV     EBX,EDX
        LEA     ESI,[ESI + CLimbSize]
        LEA     EDI,[EDI + CLimbSize]
        DEC     LCount
        JNE     @SetupLoop
        MOV     [EDI],EDX

        MOV     EAX,LSize
        MOV     EDX,EAX
        SHR     EAX,CUnrollShift
        MOV     LSize,EAX
        AND     EDX,CUnrollMask
        MOV     LRest,EDX

        DEC     RSize
        JE      @Exit

// The outer loop iterates over the limbs of the shorter operand. After the setup loop, the lowest limb
// has already been taken care of.

@OuterLoop:

        MOV     ESI,LLeft
        MOV     EDI,PDestRowStart
        ADD     PDestRowStart,CLimbSize
        MOV     EAX,PRight
        ADD     PRight,CLimbSize

// If PRight^ is 0, then we can skip multiplication for the entire row.

        MOV     ECX,[EAX]
        TEST    ECX,ECX
        JE      @NextOuterLoop

        XOR     EBX,EBX
        MOV     EAX,LSize
        MOV     LCount,EAX


        CMP     EAX,KZero
        JE      @EndInnerLoop

        .ALIGN  16

@InnerLoop:

        // Loop unrolled. Approx. 70% faster than simple loop.

        // TODO: Use MMX registers for multiplication and addition.
        // E.g.
        // MOV     MM7,ECX
        // PXOR    MM6,MM6
        // @Innerloop0:
        // MOV     MM0,[ESI]
        // MOV     MM1,[ESI + CLimbSize]
        // MOV     MM2,[ESI + 2*CLimbSize]
        // MOV     MM3,[ESI + 3*CLimbSize]
        // PMULUDQ MM0,MM7
        // PADDQ   MM6,MM0
        // MOV     [ESI],MM6
        // PSHRQ   MM6,32
        // PMULUDQ MM1,MM7
        // PADDQ   MM6,MM1
        // MOV     [ESI+CLimbSize],MM6
        // PSHRQ   MM6,32
        // etc...
        // @InnerLoopRest:
        // Do the same as above, but add to existing content in [ESI+...]

        MOV     EAX,[ESI]                  // The following pattern is not faster:
        MUL     ECX                        // MOV    EAX,[ESI]
        ADD     EAX,[EDI]                  // MUL    ECX
        ADC     EDX,0                      // ADD    EAX,EBX
        ADD     EAX,EBX                    // ADC    EDX,0
        ADC     EDX,0                      // ADD    [EDI],EAX
        MOV     [EDI],EAX                  // ADC    EDX,0
        MOV     EBX,EDX                    // MOV    EBX,EDX

        MOV     EAX,[ESI + CLimbSize]
        MUL     ECX
        ADD     EAX,[EDI + CLimbSize]
        ADC     EDX,0
        ADD     EAX,EBX
        ADC     EDX,0
        MOV     [EDI + CLimbSize],EAX
        MOV     EBX,EDX

        MOV     EAX,[ESI + 2*CLimbSize]
        MUL     ECX
        ADD     EAX,[EDI + 2*CLimbSize]
        ADC     EDX,0
        ADD     EAX,EBX
        ADC     EDX,0
        MOV     [EDI + 2*CLimbSize],EAX
        MOV     EBX,EDX

        MOV     EAX,[ESI + 3*CLimbSize]
        MUL     ECX
        ADD     EAX,[EDI + 3*CLimbSize]
        ADC     EDX,0
        ADD     EAX,EBX
        ADC     EDX,0
        MOV     [EDI + 3*CLimbSize],EAX
        MOV     EBX,EDX

        LEA     ESI,[ESI + 4*CLimbSize]
        LEA     EDI,[EDI + 4*CLimbSize]

        DEC     LCount
        JNE     @InnerLoop

@EndInnerLoop:

        // The remaining limbs to be handled.

        MOV     EAX,LRest
        MOV     LCount,EAX
        CMP     EAX,0
        JE      @EndInnerRestLoop

@InnerRestLoop:

        MOV     EAX,[ESI]
        MUL     EAX,ECX
        ADD     EAX,EBX
        ADC     EDX,0
        ADD     EAX,[EDI]
        ADC     EDX,0
        MOV     [EDI],EAX
        MOV     EBX,EDX
        LEA     ESI,[ESI + CLimbSize]
        LEA     EDI,[EDI + CLimbSize]
        DEC     LCount
        JNE     @InnerRestLoop

@EndInnerRestLoop:

        // The last (left) limb gets the top of the 64 bit product.

        MOV     [EDI],EBX

@NextOuterLoop:

        DEC     RSize
        JNE     @OuterLoop

@Exit:
        POP     EBX
        POP     EDI
        POP     ESI
end;
{$ELSE WIN64}

// This uses 64 bit multiplication as much as possible. The logic handles any odd (top) limbs especially.

var
  LeftOdd, RightOdd: Boolean;                   // Left, Right (resp.): odd number of limbs?
  SaveLeft: PLimb;
  LeftSize, RightSize: Integer;
asm
        .PUSHNV RSI
        .PUSHNV RDI
        .PUSHNV RBX
        .PUSHNV R12

        MOV     EDI,RSize
        CMP     R9D,EDI
        JAE     @SwapEnd

        XCHG    RCX,RDX
        XCHG    R9D,EDI

@SwapEnd:

        MOV     SaveLeft,RCX
        XOR     R12,R12

        MOV     EAX,R9D
        SHR     R9D,1
        MOV     LeftSize,R9D            // Number of double limbs of Left
        AND     AL,1
        MOV     LeftOdd,AL              // Does Left have an odd number of limbs?

        MOV     EAX,EDI
        SHR     EDI,1
        MOV     RightSize,EDI           // Number of double limbs of Right
        AND     AL,1
        MOV     RightOdd,AL             // Does Right have an odd number of limbs?

        MOV     R10,RDX                 // Current limb to be multiplied
        XOR     RBX,RBX                 // Top DWORD (EDX) of previous multiplication

        // If no more 64 bit limbs in Right, we must skip to final odd limb.

        CMP     RightSize,0
        JE      @FinalOddPart

        MOV     RCX,[R10]               // Current Right limb's value
        MOV     RDI,R8                  // Result limb pointer
        MOV     RSI,SaveLeft            // Left limb pointer
        ADD     R8,DLimbSize            // Result's pointer to start of current row
        ADD     R10,DLimbSize           // Current Right limb pointer

        MOV     R11D,LeftSize           // Loop counter
        CMP     R11D,0
        JE      @SetupOddPart

// Setup loop (64 bit part)

@SetupLoop64:

        MOV     RAX,[RSI]
        MUL     RCX
        ADD     RAX,RBX
        ADC     RDX,R12
        MOV     [RDI],RAX
        MOV     RBX,RDX
        LEA     RSI,[RSI + DLimbSize]
        LEA     RDI,[RDI + DLimbSize]
        DEC     R11D
        JNE     @SetupLoop64

// Setup loop, last limb ("odd" part).

@SetupOddPart:

        CMP     LeftOdd,0
        JE      @SkipSetupOddPart

        MOV     EAX,[RSI]               // 32 bit register to read odd limb of this loop
        MUL     RCX
        ADD     RAX,RBX
        ADC     RDX,R12
        MOV     [RDI],RAX
        MOV     [RDI + DLimbSize],RDX
        JMP     @SkipSkipSetupOddPart

@SkipSetupOddPart:

        MOV     [RDI],RDX

@SkipSkipSetupOddPart:

        DEC     RightSize
        JE      @FinalOddPart

@OuterLoop:

        MOV     RDI,R8
        ADD     R8,DLimbSize
        MOV     RCX,[R10]
        ADD     R10,DLimbSize

        TEST    RCX,RCX
        JE      @NextOuterLoop

        MOV     RSI,SaveLeft
        XOR     RBX,RBX
        MOV     R11D,LeftSize
        CMP     R11D,0
        JE      @InnerLoopOddPart

        SHR     R11D,CUnrollShift
        JE      @InnerTail64

@InnerLoop64:

        MOV     RAX,[RSI]               // Get double limb from Left data
        MUL     RCX                     // multiply it with current Right double limb's value --> RDX:RAX
        ADD     RAX,RBX                 // Add top limb from previous multiplication to RDX:RAX
        ADC     RDX,R12
        ADD     [RDI],RAX               // Add RAX to result array
        ADC     RDX,R12                 // And adjust top limb again
        MOV     RBX,RDX                 // And save top limb as "carry".

        MOV     RAX,[RSI + DLimbSize]
        MUL     RCX
        ADD     RAX,RBX
        ADC     RDX,R12
        ADD     [RDI + DLimbSize],RAX
        ADC     RDX,R12
        MOV     RBX,RDX

        MOV     RAX,[RSI + 2*DLimbSize]
        MUL     RCX
        ADD     RAX,RBX
        ADC     RDX,R12
        ADD     [RDI + 2*DLimbSize],RAX
        ADC     RDX,R12
        MOV     RBX,RDX

        MOV     RAX,[RSI + 3*DLimbSize]
        MUL     RCX
        ADD     RAX,RBX
        ADC     RDX,R12
        ADD     [RDI + 3*DLimbSize],RAX
        ADC     RDX,R12
        MOV     RBX,RDX

        LEA     RSI,[RSI + 4*DLimbSize]
        LEA     RDI,[RDI + 4*DLimbSize]
        DEC     R11D
        JNE     @InnerLoop64

@InnerTail64:

        MOV     R11D,LeftSize
        AND     R11D,CUnrollMask
        JE      @InnerLoopOddPart

@InnerTailLoop64:

        MOV     RAX,[RSI]
        MUL     RCX
        ADD     RAX,[RDI]
        ADC     RDX,R12
        ADD     RAX,RBX
        ADC     RDX,R12
        MOV     [RDI],RAX
        MOV     RBX,RDX
        LEA     RSI,[RSI + DLimbSize]
        LEA     RDI,[RDI + DLimbSize]
        DEC     R11D
        JNE     @InnerTailLoop64

@InnerLoopOddPart:

        CMP     LeftOdd,0               // If Left's size is odd, handle last limb.
        JE      @InnerLoopLastLimb

        MOV     RAX,[RSI]
        MUL     RCX
        ADD     RAX,[RDI]
        ADC     RDX,R12
        ADD     RAX,RBX
        ADC     RDX,R12
        MOV     [RDI],RAX
        MOV     [RDI + DLimbSize],RDX
        JMP     @NextOuterLoop

@InnerLoopLastLimb:

        MOV     [RDI],RDX

@NextOuterLoop:

        DEC     RightSize
        JNE     @OuterLoop

@FinalOddPart:

        CMP     RightOdd,0
        JE      @Exit

        MOV     RDI,R8
        MOV     RSI,SaveLeft
        MOV     RAX,R10
        MOV     ECX,[RAX]                      // Right is odd, so read single TLimb
        XOR     RBX,RBX
        MOV     R11D,LeftSize
        CMP     R11D,0
        JE      @SkipFinalLoop

        .ALIGN  16

@FinalLoop:

        MOV     RAX,[RSI]
        MUL     RCX
        ADD     RAX,[RDI]
        ADC     RDX,0
        ADD     RAX,RBX
        ADC     RDX,0
        MOV     [RDI],RAX
        MOV     RBX,RDX
        LEA     RSI,[RSI + DLimbSize]
        LEA     RDI,[RDI + DLimbSize]
        DEC     R11D
        JNE     @FinalLoop

@SkipFinalLoop:

        CMP    LeftOdd,0
        JE     @LastLimb

        MOV    EAX,[RSI]
        MUL    RCX
        ADD    RAX,[RDI]
        ADC    RDX,0
        ADD    RAX,RBX
        ADC    RDX,0
        MOV    [RDI],RAX
        MOV    [RDI + DLimbSize],RDX
        JMP    @Exit

@LastLimb:

        MOV    [RDI],RDX

@Exit:

end;
{$ENDIF !WIN32}
{$ENDIF !PUREPASCAL}

function BigInteger.ToBinaryString: string;
begin
  Result := ToString(2);
end;

function BigInteger.ToByteArray: TArray<Byte>;
var
  Mag: TMagnitude;
  Bytes, Bits: Integer;
  ExtraByte: Byte;
begin
  if IsZero then
  begin
    SetLength(Result, 1);
    Result[0] := 0;
    Exit;
  end;

  Bytes := BitLength;
  Bits := Bytes and $07;
  Bytes := (Bytes + 7) shr 3;
  if FSize > 0 then
  begin
    Mag := FData;
    ExtraByte := $00;
  end
  else
  begin
    SetLength(Mag, Size);
    InternalNegate(PLimb(FData), PLimb(Mag), Size);
    ExtraByte := $FF;
  end;
  SetLength(Result, Bytes + Byte(Bits = 0));
  Move(Mag[0], Result[0], Bytes);
  if Bits = 0 then
    Result[Bytes] := ExtraByte;
end;

function BigInteger.ToDecimalString: string;
begin
  Result := ToString(10);
end;

function BigInteger.ToHexString: string;
begin
  Result := ToString(16);
end;

function BigInteger.ToOctalString: string;
begin
  Result := ToString(8);
end;

{$IFNDEF PUREPASCAL}
procedure DivModNativeUInts(Dividend, Divisor: NativeUInt; var Quotient, Remainder: NativeUint);
{$IFDEF WIN32}
asm
        PUSH    EBX
        MOV     EBX,EDX
        XOR     EDX,EDX
        DIV     EAX,EBX
        MOV     [ECX],EAX
        MOV     EBX,Remainder
        MOV     [EBX],EDX
        POP     EBX
end;
{$ELSE WIN64}
asm
        .NOFRAME

        MOV     RAX,RCX
        MOV     RCX,RDX
        XOR     EDX,EDX
        DIV     RAX,RCX
        MOV     [R8],RAX
        MOV     [R9],RDX
end;
{$ENDIF WIN64}
{$ENDIF !PUREPASCAL}

const
  TwoDigitTable: array[0..99, 0..1] of Char =
  (
    '00', '01', '02', '03', '04', '05', '06', '07', '08', '09',
    '10', '11', '12', '13', '14', '15', '16', '17', '18', '19',
    '20', '21', '22', '23', '24', '25', '26', '27', '28', '29',
    '30', '31', '32', '33', '34', '35', '36', '37', '38', '39',
    '40', '41', '42', '43', '44', '45', '46', '47', '48', '49',
    '50', '51', '52', '53', '54', '55', '56', '57', '58', '59',
    '60', '61', '62', '63', '64', '65', '66', '67', '68', '69',
    '70', '71', '72', '73', '74', '75', '76', '77', '78', '79',
    '80', '81', '82', '83', '84', '85', '86', '87', '88', '89',
    '90', '91', '92', '93', '94', '95', '96', '97', '98', '99'
  );

{$IF DEFINED(WIN32)}
  // Checked
  Div100Const = UInt32(UInt64($1FFFFFFFFF) div 100 + 1);
  Div100PostShift = 5;
{$ELSEIF DEFINED(WIN64)}
{$IFDEF LIBDIVIDE}
  // Parameters calculated using
  // https://github.com/ridiculousfish/libdivide/blob/master/divide_by_constants_codegen_reference.c
  Div100Const = $47AE147AE147AE15;
  Div100PostShift = 6;
{$ELSE}
  Div100Const = $A3D70A3D70A3D70B; // UInt64(UInt128($3F FFFF FFFF FFFF FFFF) div 100 + 1)
  Div100PostShift = 6;
{$ENDIF}
{$IFEND}

{$IFNDEF PUREPASCAL}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///  The following calculates X div 100 using multiplication by a constant, taking the high part of the 64 bit  ///
///  result and shifting right. The return value is the remainder, calculated as X - quotient * 100.            ///
///                                                                                                             ///
///  This was tested to work safely and quickly for all values of UInt32.                                       ///
///                                                                                                             ///
///  The 64 bit part is taken from: https://raw.github.com/ridiculousfish/libdivide/master/libdivide.h          ///
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///
class function BigInteger.InternalDivMod100(var X: NativeUInt): NativeUInt;
{$IFDEF WIN32}
asm
        PUSH    EBX
        MOV     EDX,Div100Const
        MOV     ECX,EAX
        MOV     EAX,[ECX]
        MOV     EBX,EAX
        MUL     EDX
        SHR     EDX,Div100PostShift
        MOV     [ECX],EDX               // Quotient

        // Slightly faster than MUL

        LEA     EDX,[EDX + 4*EDX]       // EDX := EDX * 5;
        LEA     EDX,[EDX + 4*EDX]       // EDX := EDX * 5;
        SHL     EDX,2                   // EDX := EDX * 4; 5*5*4 = 100.

        MOV     EAX,EBX
        SUB     EAX,EDX                 // Remainder
        POP     EBX
end;
{$ELSE WIN64}
asm
        .NOFRAME

        // See libdivide.h: libdivide_u64_do() after a call to libdivide_u64_gen(100)

        MOV     RAX,[RCX]
        MOV     R8,RAX
        XOR     RDX,RDX
        MOV     R9,Div100Const
        MUL     R9              // RDX = Q

{$IFDEF LIBDIVIDE}
        MOV     R9,R8           // Q := Q + (X - Q) shr 1;
        SUB     R9,RDX
        SHR     R9,1
        ADD     RDX,R9
{$ENDIF}

        SHR     RDX,Div100PostShift // Q := Q shr 6;
        MOV     [RCX],RDX       // X := Q;

        // Faster than LEA and SHL

        MOV     RAX,RDX
        MOV     R9D,100
        MUL     R9
        SUB     R8,RAX
        MOV     RAX,R8         // Remainder
end;
{$ENDIF WIN32}
{$ENDIF !PUREPASCAL}

{$IFNDEF PUREPASCAL}
class procedure BigInteger.InternalIntToStrDecimal(const Value: NativeUInt; var WritePtr: PChar; MaxDigits: Integer);
var
  LRemainder, LDividend: NativeUInt;
  LSectionStart: PChar;
begin
  LSectionStart := WritePtr - MaxDigits;
  LDividend := Value;
  if Odd(MaxDigits) and (LDividend <> 0) then
  begin
    DivModNativeUInts(LDividend, 10, LDividend, LRemainder);
    Dec(WritePtr);
    WritePtr^ := Char(LRemainder + Ord('0'));
  end;
  while LDividend > 0 do
  begin
    LRemainder := InternalDivMod100(LDividend);
    Dec(WritePtr, 2);
    WritePtr[0] := TwoDigitTable[LRemainder, 0];
    WritePtr[1] := TwoDigitTable[LRemainder, 1];
  end;

  while WritePtr > LSectionStart do
  begin
    Dec(WritePtr);
    WritePtr^ := '0';
  end;
end;
{$ENDIF}

// Simple version of IntToStr for any given base, for unsigned integers only.
class procedure BigInteger.InternalIntToStrBase(const Value: NativeUInt; Base: Cardinal; var WritePtr: PChar;
  MaxDigits: Integer);
var
{$IFDEF PUREPASCAL}
  LRemainder: UInt64;
  LDividend: UInt64;
{$ELSE}
  LRemainder: NativeUInt;
  LDividend: NativeUInt;
{$ENDIF PUREPASCAL}
  LSectionStart: PChar;
begin
{$IFNDEF PUREPASCAL}
  if Base = 10 then
  begin
    InternalIntToStrDecimal(Value, WritePtr, MaxDigits);
    Exit;
  end;
{$ENDIF}
  LSectionStart := WritePtr - MaxDigits;
  LDividend := Value;
  while LDividend > 0 do
  begin
  {$IFDEF PUREPASCAL}
    System.Math.DivMod(LDividend, Base, LDividend, LRemainder);
  {$ELSE}
    DivModNativeUInts(LDividend, Base, LDividend, LRemainder);
  {$ENDIF PUREPASCAL}
    Dec(WritePtr);
    WritePtr^ := CBaseChars[LRemainder];
  end;

  while WritePtr > LSectionStart do
  begin
    Dec(WritePtr);
    WritePtr^ := '0';
  end;
end;

// This should be easy. Simply shift (beginning from the back) and output. This can be done limb-wise.
class procedure BigInteger.InternalShiftedToString(const Value: BigInteger; Base: Integer; var WritePtr: PChar);
var
  LMaxDigits: Integer;
  LShift: Integer;
  LMask, LLImb: TLimb;
  LSectionStart: PChar;
  I: Integer;
begin
  Assert(Base in [2, 4, 16]);
  case Base of
    2:
      begin
        LMaxDigits := 32;
        LShift := 1;
        LMask := $00000001;
      end;
    4:
      begin
        LMaxDigits := 16;
        LShift := 2;
        LMask := $00000003;
      end;
    else
      begin
        LMaxDigits := 8;
        LShift := 4;
        LMask := $0000000F;
      end;
  end;
  Assert(Value.FSize >= 0);
  for I := 0 to Value.FSize - 1 do
  begin
    LLimb := Value.FData[I];
    LSectionStart := WritePtr - LMaxDigits;
    while LLimb <> 0 do
    begin
      Dec(WritePtr);
      WritePtr^ := CBaseChars[LLimb and LMask];
      LLimb := LLimb shr LShift;
    end;
    while WritePtr > LSectionStart do
    begin
      Dec(WritePtr);
      WritePtr^ := '0';
    end;
  end;
end;

// This is pretty self-documenting, but also cf. Brent, Zimmermann [3], "Modern Computer Arithmetic", algorithm 1.24
class procedure BigInteger.InternalPlainToString(const Value: BigInteger; Base: Integer;
  const BaseInfo: TNumberBaseInfo; var WritePtr: PChar; SectionCount: Integer);
var
  LQuotient, LRemainder: BigInteger;
  LSectionStart: PChar;
begin
  LQuotient := Value;
  LSectionStart := WritePtr - SectionCount * BaseInfo.MaxDigits;

  while Assigned(LQuotient.FData) do
  begin
    BigInteger.DivMod(LQuotient, BaseInfo.MaxPower, LQuotient, LRemainder);
{$IFDEF CPU32BITS}
    if Assigned(LRemainder.FData) then
      InternalIntToStrBase(LRemainder.FData[0], Base, WritePtr, BaseInfo.MaxDigits)
    else
      InternalIntToStrBase(0, Base, WritePtr, BaseInfo.MaxDigits);
{$ELSE}
    InternalIntToStrBase(UInt64(LRemainder), Base, WritePtr, BaseInfo.MaxDigits);
{$ENDIF}
  end;

  while WritePtr > LSectionStart do
  begin
    Dec(WritePtr);
    WritePtr^ := '0';
  end;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///  About sections: conversion is finally done in digit sections. A section is, depending on base, the number      ///
///  of digits that corresponds to the maximum power of the given base that fits in a NativeUInt.                   ///
///                                                                                                                 ///
///  Example: the highest power of base 10 that fits in a UInt32 is 9 (so MaxPower for base 10 is 10^9 and the      ///
///  number of digits that corresponds with it is 9: MaxDigits). These 9 digits form a section. Since these fit     ///
///  in a UInt32, simple conversion can be done by dividing a UInt32 repeatedly by 10, which is considerably        ///
///  faster than dividing a BigInteger by 10. That is why conversion is done in multiples of a section. FWIW, in    ///
///  64 bit code, the maxium power of 10 that fits in a UInt64 is 19, so in that case, a section for base 10 is     ///
///  19 digits.                                                                                                     ///
///                                                                                                                 ///
///  See bases.inc for MaxPower, MaxDigits and MaxFactor for each base and NativeUInt size.                         ///
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

function GetSectionCount(Size, Base: Integer): Integer;
begin
  Result := (UInt64(Size) * CBaseInfos[Base].MaxFactor) shr (CMaxFactorShift - 5) + 1;
end;

// This makes InternalRecursiveToString approx. 27% faster (on huge strings, like the large prime 2^74207281 - 1).
function GetBasePower(Base, Exponent: Integer; MaxPower: NativeUInt): BigInteger;
begin
  if Exponent > High(CBasePowers[Base]) then
    SetLength(CBasePowers[Base], Exponent + 1);
  Result := CBasePowers[Base, Exponent];

  // Note that "uninitialized" BigIntegers have an FData of nil, so they return True on IsZero.
  if Result.IsZero then
  begin

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    ///  Note: I tried using a LastExponent, and if the current exponent was above the last, it would  ///
    ///  multiply the lastly found value with Pow(MaxPower, difference). But that did not provide any  ///
    ///  improvement.                                                                                  ///
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    Result := BigInteger.Pow(MaxPower, Exponent);
    CBasePowers[Base, Exponent] := Result;
  end;
end;

// cf. Brent, Zimmermann [3], "Modern Computer Arithmetic", algorithm 1.26
class procedure BigInteger.InternalRecursiveToString(const Value: BigInteger; Base: Integer;
  const BaseInfo: TNumberBaseInfo; var WritePtr: PChar; SectionCount: Integer);
var
  LHalfSectionCount: Integer;
  LDivisor, LQuotient, LRemainder: BigInteger;
  LSectionStart: PChar;
begin
  LSectionStart := WritePtr - SectionCount * BaseInfo.MaxDigits;

  if SectionCount < RecursiveToStringThreshold then
  begin
    InternalPlainToString(Value, Base, BaseInfo, WritePtr, SectionCount);
    Exit;
  end;

  LHalfSectionCount:= SectionCount shr 1;

  LDivisor := GetBasePower(Base, LHalfSectionCount, BaseInfo.MaxPower);
  BigInteger.DivMod(Value, LDivisor, LQuotient, LRemainder);

  InternalRecursiveToString(LRemainder, Base, BaseInfo, WritePtr, LHalfSectionCount);
  InternalRecursiveToString(LQuotient, Base, BaseInfo, WritePtr, SectionCount - LHalfSectionCount);

  while WritePtr > LSectionStart do
  begin
    Dec(WritePtr);
    WritePtr^ := '0';
  end;

end;

function BigInteger.ToString: string;
begin
  Result := ToString(FBase);
end;

function BigInteger.ToString(Base: Integer): string;
var
  WritePtr: PChar;
  LBuffer: PChar;
  LAbsValue: BigInteger;
  LSectionCount: Integer;       // Expected number of digit sections.
  LBufLen: Integer;
  LBaseInfo: TNumberBaseInfo;
begin
  if not Base in [2..36] then
    Error(ecInvalidBase, []);

  if FData = nil then
    Exit('0');

  if FSize < 0 then
    LAbsValue := -Self
  else
    LAbsValue := Self;

  LBaseInfo := CBaseInfos[Base];

  LSectionCount := GetSectionCount(LAbsValue.Size, Base);
  LBufLen := (LBaseInfo.MaxDigits + 1) * (LSectionCount) + 1;
  GetMem(LBuffer, LBufLen * SizeOf(Char));
  try
    WritePtr := LBuffer + LBufLen - 1;
    WritePtr^ := #0;

    if Base in [2, 4, 16] then
      // 2, 4 and 16 are easy: just take each limb, shift and output, from bottom (could also start from top,
      // but this should be compatible with the other methods).
      InternalShiftedToString(LAbsValue, Base, WritePtr)
    else if (FSize and SizeMask) < RecursiveToStringThreshold then
      // "Small" BigIntegers take the simple approach.
      InternalPlainToString(LAbsValue, Base, LBaseInfo, WritePtr, LSectionCount)
    else
      // Large BigIntegers take the recursive divide-and-conquer approach.
      InternalRecursiveToString(LAbsValue, Base, LBaseInfo, WritePtr, LSectionCount);

    while WritePtr^ = '0' do
      Inc(WritePtr);
    if FSize < 0 then
    begin
      Dec(WritePtr);
      WritePtr^ := '-';
    end;

    Result := string(WritePtr);

  finally
    FreeMem(LBuffer);
  end;
end;

// For debugging purposes. May be removed, so don't use it for regular purposes.
function BigInteger.ToStringClassic(Base: Integer): string;
var
  P: PChar;
  LBuffer: TArray<Char>;
  LMagnitude: TMagnitude;
  LSize: Integer;
begin
  if not Base in [2..36] then
    Error(ecInvalidBase, []);
  if FData = nil then
  begin
    Result := '0';
    Exit;
  end;
  LSize := FSize and SizeMask;
  SetLength(LBuffer, LSize * CStringMaxLengths[Base] + 1);
  LMagnitude := System.Copy(FData);
  P := PChar(LBuffer) + Length(LBuffer);
  Dec(P);
  P^ := #0;
  while LSize > 0 do
  begin
    Dec(P);
    P^ := CBaseChars[InternalDivideByBase(PLimb(LMagnitude), Base, LSize)];
  end;
  if FSize < 0 then
  begin
    Dec(P);
    P^ := '-';
  end;
  Result := P;
end;

// By default, uses FBase as numeric base, otherwise, if string "starts" with $, 0x, 0b or 0o, uses
// 16, 16 (both hex), 2 (binary) and 8 (octal) respectively.
class function BigInteger.TryParse(const S: string; var Value: BigInteger): Boolean;
var
  LTrimmed: string;
  LIsNegative: Boolean;
  P: PChar;
  LBase, LBaseNew: Integer;
begin
  Result := False;
  LTrimmed := UpperCase(Trim(S)); // Make string case insensitive.
  if LTrimmed = '' then
    Exit;
  LIsNegative := False;
  P := PChar(LTrimmed);
  if (P^ = '-') or (P^ = '+') then
  begin
    LIsNegative := (P^ = '-');
    Inc(P);
  end;
  LBase := FBase;               // By default, use global numeric base.
  case P^ of
    '$':                        // $ prefix indicates hexadecimal (equivalent to 0x and %16r)
      begin
        Inc(P);
        LBase := 16;
      end;
    '0':
      begin
        Inc(P);
        case P^ of
          #0:
            begin
              Value := Zero;
              Exit(True);
            end;
          'B':                  // 0b prefix indicates binary (equivalent to %2r)
            LBase := 2;
          'O', 'K':             // 0o17, 0k17 prefixes indicate octal (equivalent to %8r)
            LBase := 8;
          'X':                  // 0x prefix indicates hexadecimal (equivalent to $ and %16r)
            LBase := 16;
          'D':
            LBase := 10;
          else
            Dec(P);
        end;
        Inc(P);
      end;
    '%':                        // %nnr prefix indicates base n (nn is always decimal)
      begin
        Inc(P);
        LBaseNew := 0;
        while P^ <> 'R' do
        begin
          if P^ = #0 then
            Exit;
          LBaseNew := LBaseNew * 10 + Ord(P^) - CNumBase;
          Inc(P);
        end;
        Inc(P);
        if not (LBaseNew in [2..36]) then
          Exit;
        LBase := LBaseNew;
      end;
  end;
  Result := TryParse(P, LBase, Value);
  if Result and LIsNegative then
    Value := -Value;
end;

// cf. Brent, Zimmermann, "Modern Computer Arithmetic", algorithm 1.23
class function BigInteger.TryParse(const S: string; ABase: TNumberBase; var AValue: BigInteger): Boolean;
var
  LIsNegative: Boolean;
  LTrimmed: string;
  LVal: Integer;
  P: PChar;
begin
  Result := False;
  LTrimmed := Trim(S);
  if LTrimmed = '' then
    Exit;
  LIsNegative := False;

  AValue.MakeSize(Length(S) div CStringMinLengths[ABase] + 1);
  AValue.FSize := 0;

  P := PChar(LTrimmed);
  if (P^ = '-') or (P^ = '+') then
  begin
    LIsNegative := (P^ = '-');
    Inc(P);
  end;
  if ABase = 10 then
    Result := InternalParseDecimal(P, AValue)
  else if ABase = 16 then
    Result := InternalParseHex(P, AValue)
  else
  begin
    while P^ <> #0 do
    begin
      if (P^ = '_') or (P^ = ' ') or (P^ = ',') then
      begin
        Inc(P);
        Continue;
      end;
      LVal := Ord(P^);
      Inc(P);
      if LVal in [Ord('0')..Ord('9')] then
        Dec(LVal, CNumBase)
      else if LVal >= CAlphaBase then
      begin
        if LVal >= Ord('a') then
          Dec(LVal, 32);
        Dec(LVal, CAlphaBase - 10);
      end
      else
        Exit;
      if LVal >= ABase then
        Exit;
      InternalMultiplyAndAdd16(PLimb(AValue.FData), ABase, LVal, AValue.FSize);
    end;
    Result := True;
  end;
  if not Result then
  begin
    AValue := BigInteger.Zero;
    Exit;
  end;
{$IFDEF RESETSIZE}
  AValue.Compact;     // FSize is already correct, but Compact also reallocates down, if RESETSIZE requires it.
{$ENDIF}
  if LIsNegative then
    AValue := -AValue;
end;

const
  CIntPowersOfTen: array[1..9] of Integer =
  (
                10,
               100,
              1000,
           10*1000,
          100*1000,
         1000*1000,
      10*1000*1000,
     100*1000*1000,
    1000*1000*1000
  );

class function BigInteger.InternalParseDecimal(P: PChar; var Value: BigInteger): Boolean;
var
  Cumulative: Cardinal;
  N: Integer;
begin
  Value := BigInteger.Zero;
  Result := False;
  while P^ <> #0 do
  begin
    N := 0;
    Cumulative := 0;
    while N < 9 do
    begin
      case P^ of
        '_', ' ', ',':
          begin
            Inc(P);     // Ignore!
            Continue;
          end;
        #0:
          Break;
        '0'..'9':
          Cumulative := Cumulative * 10 + Ord(P^) - Ord('0');
        else
          Exit;
      end;
      Inc(N);
      Inc(P);
    end;
    Value := Value * CIntPowersOfTen[N] + Cumulative;
  end;
  Result := True;
end;

class function BigInteger.InternalParseHex(P: PChar; var Value: BigInteger): Boolean;
var
  Cumulative: Cardinal;
  N: Integer;
begin
  while P^ <> #0 do
  begin
    N := 0;
    Cumulative := 0;
    while N <= 7 do
    begin
      case P^ of
        '_', ' ', ',':
          begin
            Inc(P);     // Ignore!
            Continue;
          end;
        #0:
          Break;
        '0'..'9':
          Cumulative := Cumulative shl 4 + Ord(P^) - Ord('0');
        'A'..'F':
          Cumulative := Cumulative shl 4 + Ord(P^) - Ord('A') + 10;
        'a'..'f':
          Cumulative := Cumulative shl 4 + Ord(P^) - Ord('a') + 10;
        else
          Exit(False);
      end;
      Inc(N);
      Inc(P);
    end;
    Value := Value shl (4 * N) + Cumulative;
  end;
  Result := True;
end;

class procedure BigInteger.Decimal;
begin
  FBase := 10;
end;

class function BigInteger.Divide(const Left: BigInteger; Right: UInt16): BigInteger;
var
  LSign: Integer;
begin
  if Right = 0 then
    Error(ecDivByZero, []);
  if Left.FData = nil then
  begin
    ShallowCopy(Zero, Result);
    Exit;
  end;
  LSign := Left.FSize and SignMask;
  Result.MakeSize(Left.FSize and SizeMask);
  InternalDivMod16(PLimb(Left.FData), Right, PLImb(Result.FData), nil, Left.FSize and SizeMask);
  Result.Compact;
  if Assigned(Result.FData) then
    Result.FSize := (Result.FSize and SizeMask) or LSign;
end;

class function BigInteger.Divide(const Left: BigInteger; Right: UInt32): BigInteger;
var
  LSign: Integer;
begin
  if Right = 0 then
    Error(ecDivByZero, []);
  if Left.FData = nil then
  begin
    ShallowCopy(Zero, Result);
    Exit;
  end;
  LSign := Left.FSize and SignMask;
  Result.MakeSize(Left.FSize and SizeMask);
  InternalDivMod32(PLimb(Left.FData), Right, PLimb(Result.FData), nil, Left.FSize and SizeMask);
  Result.Compact;
  if Assigned(Result.FData) then
    Result.FSize := (Result.FSize and SizeMask) or LSign;
end;

class function BigInteger.Divide(const Left, Right: BigInteger): BigInteger;
var
  Sign, LSize, RSize: Integer;
  Remainder: BigInteger;
begin
  if Right.FData = nil then
    Error(ecDivByZero, []);

  if Left.FData = nil then
    Exit(Zero);

  Sign := (Left.FSize and SignMask) xor (Right.FSize and SignMask);
  LSize := Left.FSize and SizeMask;
  RSize := Right.FSize and SizeMask;

  case InternalCompare(PLimb(Left.FData), PLimb(Right.FData), LSize, RSize) of
    -1:
      begin
        ShallowCopy(Zero, Result);
      end;
    0:
      begin
        if Sign = 0 then
          ShallowCopy(One, Result)
        else
          ShallowCopy(MinusOne, Result);
      end;
    else
      begin
        if ShouldUseBurnikelZiegler(LSize, RSize) then
          DivModBurnikelZiegler(Left, Right, Result, Remainder)
        else
          DivModKnuth(Left, Right, Result, Remainder);

        if Result.FSize <> 0 then
          Result.FSize := (Result.FSize and SizeMask) or Sign;
      end;
  end;
end;

{$IFNDEF BIGINTEGERIMMUTABLE}
function BigInteger.Divide(const Other: BigInteger): PBigInteger;
begin
  Result := @Self;
  Self := Self div Other;
end;
{$ENDIF}

class procedure BigInteger.DivMod(const Dividend, Divisor: BigInteger; var Quotient, Remainder: BigInteger);
var
  LSize, RSize: Integer;
begin
  if Divisor.FData = nil then
    Error(ecDivByZero, []);

  LSize := Dividend.FSize and SizeMask;
  RSize := Divisor.FSize and SizeMask;

  case InternalCompare(PLimb(Dividend.FData), PLimb(Divisor.FData), LSize, RSize) of
    -1:
      begin
        ShallowCopy(Dividend, Remainder);
        ShallowCopy(Zero, Quotient);
        Exit;
      end;
    0:
      begin
        if (Dividend.FSize xor Divisor.FSize) and SignMask = 0 then
          ShallowCopy(One, Quotient)
        else
          ShallowCopy(MinusOne, Quotient);
        ShallowCopy(Zero, Remainder);
        Exit;
      end
    else
      begin
        if ShouldUseBurnikelZiegler(LSize, RSize) then
          DivModBurnikelZiegler(Dividend, Divisor, Quotient, Remainder)
        else
          UncheckedDivModKnuth(Dividend, Divisor, Quotient, Remainder);
      end;
  end;
end;

class procedure BigInteger.UncheckedDivModKnuth(const Left, Right: BigInteger; var Quotient, Remainder: BigInteger);
var
  LSign, RSign: Integer;
  LSize, RSize: Integer;
  Q, R: BigInteger;
  Offset: Integer;              // Offset into left and right data when eliminating common trailing zero limbs.

  // Establish number of common trailing zero limbs.
  function CommonTrailingZeros(const Left, Right: PLimb; LSize, RSize: Integer): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to IntMin(LSize, RSize) - 1 do
      if (Left[I] or Right[I]) <> 0 then
        Exit(I);
  end;

begin
  if Right.FData = nil then
    Error(ecDivByZero, []);

  LSign := Left.FSize and SignMask;
  RSign := Right.FSize and SignMask;
  LSize := Left.FSize and SizeMask;
  RSize := Right.FSize and SizeMask;

  if (LSize and RSize) <> 0 then
  begin
    Offset := CommonTrailingZeros(PLimb(Left.FData), PLimb(Right.FData), LSize, RSize);
  end
  else
    Offset := 0;

  Q.MakeSize(LSize - RSize + 1);
  R.MakeSize(RSize + 1); // RSize should be enough, but apparently in 64 mode asm, it overwrites one extra limb.
  if not InternalDivMod(PLimb(Left.FData) + Offset, PLimb(Right.FData) + Offset, PLimb(Q.FData),
           PLimb(R.FData) + Offset, LSize - Offset, RSize - Offset) then
    Error(ecInvalidBase, []);
  Q.Compact;
  R.Compact;

  if Q.FSize <> 0 then
    Q.FSize := (Q.FSize and SizeMask) or (LSign xor RSign);
  if R.FSize <> 0 then
    R.FSize := (R.FSize and SizeMask) or LSign;
  ShallowCopy(Q, Quotient);
  ShallowCopy(R, Remainder);
end;

class procedure BigInteger.DivModKnuth(const Left, Right: BigInteger; var Quotient, Remainder: BigInteger);
var
  LSign, RSign: Integer;
  LSize, RSize: Integer;
  Q, R: BigInteger;
  Offset: Integer;              // Offset into left and right data when eliminating common trailing zero limbs.

  // Establish number of common trailing zero limbs.
  function CommonTrailingZeros(const Left, Right: PLimb; LSize, RSize: Integer): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to IntMin(LSize, RSize) - 1 do
      if (Left[I] or Right[I]) <> 0 then
        Exit(I);
  end;

begin
  if Right.FData = nil then
    Error(ecDivByZero, []);

  LSign := Left.FSize and SignMask;
  RSign := Right.FSize and SignMask;
  LSize := Left.FSize and SizeMask;
  RSize := Right.FSize and SizeMask;

  if (LSize and RSize) <> 0 then
  begin
    Offset := CommonTrailingZeros(PLimb(Left.FData), PLimb(Right.FData), LSize, RSize);
  end
  else
    Offset := 0;

  case InternalCompare(PLimb(Left.FData), PLimb(Right.FData), LSize, RSize) of
    -1:
      begin
        ShallowCopy(Left, Remainder);
        ShallowCopy(Zero, Quotient);
        Exit;
      end;
    0:
      begin
        ShallowCopy(Zero, Remainder);
        if LSign = RSign then
          ShallowCopy(One, Quotient)
        else
          ShallowCopy(MinusOne, Quotient);
        Exit;
      end
    else
      begin
        Q.MakeSize(LSize - RSize + 1);
        R.MakeSize(RSize + 1); // RSize should be enough, but apparently in 64 mode asm, it overwrites one extra limb.
        if not InternalDivMod(PLimb(Left.FData) + Offset, PLimb(Right.FData) + Offset, PLimb(Q.FData),
                 PLimb(R.FData) + Offset, LSize - Offset, RSize - Offset) then
          Error(ecInvalidBase, []);
        Q.Compact;
        R.Compact;
        if Q.FSize <> 0 then
          Q.FSize := (Q.FSize and SizeMask) or (LSign xor RSign);
        if R.FSize <> 0 then
          R.FSize := (R.FSize and SizeMask) or LSign;
        ShallowCopy(Q, Quotient);
        ShallowCopy(R, Remainder);
      end;
  end;
end;

class procedure BigInteger.InternalShiftLeft(Source, Dest: PLimb; Shift, Size: Integer);
{$IF DEFINED(PUREPASCAL)}
var
  I: Integer;
begin
  Shift := Shift and 31;
  if Shift = 0 then
    CopyLimbs(Source, Dest, Size)
  else
  begin
    Dest[Size] := Source[Size - 1] shr (CLimbBits - Shift);
    for I := Size - 1 downto 1 do
      Dest[I] := (Source[I] shl Shift) or (Source[I - 1] shr (CLimbBits - Shift));
    Dest[0] := Source[0] shl Shift;
  end;
end;
{$ELSEIF DEFINED(WIN32)}
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX

        MOV     ESI,EAX
        MOV     EDI,EDX
        XOR     EAX,EAX

        // No need to test for nil.
        MOV     EBX,Size

        DEC     EBX
        JS      @LoopEnd

@ShiftLoop:

        MOV     EDX,[ESI + CLimbSize*EBX]
        SHLD    EAX,EDX,CL
        MOV     [EDI + CLimbSize*EBX + CLimbSize],EAX
        MOV     EAX,EDX

@ShiftStart:

        DEC     EBX
        JNS     @ShiftLoop

@LoopEnd:

        SHL     EAX,CL
        MOV     [EDI],EAX

@Exit:

        POP     EBX
        POP     EDI
        POP     ESI
end;
{$ELSE}
asm
        XCHG    RCX,R8
        MOV     R10,RDX

        XOR     EAX,EAX
        DEC     R9D
        JS      @LoopEnd

@ShiftLoop:

        MOV     EDX,[R8 + CLimbSize*R9]
        SHLD    EAX,EDX,CL
        MOV     [R10 + CLimbSize*R9 + CLimbSize],EAX
        MOV     EAX,EDX

@ShiftStart:

        DEC     R9D
        JNS     @ShiftLoop

@LoopEnd:

        SHL     EAX,CL
        MOV     [R10],EAX

@Exit:
end;
{$IFEND}

class procedure BigInteger.InternalShiftRight(Source, Dest: PLimb; Shift, Size: Integer);
{$IF DEFINED(PUREPASCAL)}
var
  I: Integer;
begin
  Shift := Shift and 31;
  if Shift = 0 then
    CopyLimbs(Source, Dest, Size)
  else
  begin
    for I := 0 to Size - 1 do
      Dest[I] := (Source[I] shr Shift) or (Source[I + 1] shl (CLimbBits - Shift));
    Dest[Size - 1] := Source[Size - 1] shr Shift;
  end;
end;
{$ELSEIF DEFINED(WIN32)}
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX

        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EBX,Size
        MOV     EAX,[ESI]
        LEA     ESI,[ESI + CLimbSize]
        DEC     EBX
        JE      @EndLoop

@ShiftLoop:

        MOV     EDX,[ESI]
        SHRD    EAX,EDX,CL
        MOV     [EDI],EAX
        MOV     EAX,EDX
        LEA     ESI,[ESI + CLimbSize]
        LEA     EDI,[EDI + CLimbSize]
        DEC     EBX
        JNE     @ShiftLoop

@EndLoop:

        SHR     EAX,CL
        MOV     [EDI],EAX

@Exit:

        POP     EBX
        POP     EDI
        POP     ESI
end;
{$ELSE}
asm
        XCHG    RCX,R8                        // R8 = source, ECX = shift

        MOV     EAX,[R8]
        LEA     R8,[R8 + CLimbSize]
        DEC     R9D
        JE      @LoopEnd

@ShiftLoop:

        MOV     R10D,[R8]
        SHRD    EAX,R10D,CL
        MOV     [RDX],EAX
        MOV     EAX,R10D
        LEA     RDX,[RDX + CLimbSize]
        LEA     R8,[R8 + CLimbSize]
        DEC     R9D
        JNE     @ShiftLoop

@LoopEnd:

        SHR     EAX,CL
        MOV     [RDX],EAX

@Exit:

end;
{$IFEND}

type
{$IFDEF CPU64BITS}
  TDivLimb = UInt32;
  TDblLimb = UInt64;
{$ELSE}
  TDivLimb = UInt16;
  TDblLimb = UInt32;
{$ENDIF}
  PDivLimb = ^TDivLimb;
  PDblLimb = ^TDblLimb;

const
  CDivLimbBase = TDblLimb(High(TDivLimb)) + 1;
  CDivLimbBits = SizeOf(TDivLimb) * 8;
  CDblLimbBits = SizeOf(TDblLimb) * 8;

class function BigInteger.InternalDivMod16(Dividend: PLimb; Divisor: UInt16; Quotient, Remainder: PLimb;
  LSize: Integer): Boolean;
{$IFDEF PUREPASCAL}
// In PUREPASCAL, using 16-bit division with an intermediate 32-bit result turned out to be faster than
// 32-bit division with an intermediate 64-bit result.
type
  PUInt16 = ^UInt16;
var
  J: Integer;
  LRemainder: UInt16;
begin
  LSize := LSize + LSize;

  LRemainder := 0;
  for J := LSize - 1 downto 0 do
    System.Math.DivMod(Cardinal(LRemainder shl 16 + PUInt16(Dividend)[J]), Divisor, PUInt16(Quotient)[J], LRemainder);

  if Assigned(Remainder) then
    Remainder[0] := LRemainder;
  Exit(True);
end;
{$ELSE !PUREPASCAL}
// In assembler, 32 bit division is faster, so promote divisor to 32 bit and use InternalDivMod32.
begin
  Result := InternalDivMod32(Dividend, UInt32(Divisor), Quotient, Remainder, LSize);
end;
{$ENDIF !PUREPASCAL}

class function BigInteger.InternalDivMod32(Dividend: PLimb; Divisor: UInt32; Quotient, Remainder: PLimb;
  LSize: Integer): Boolean;
{$IFDEF PUREPASCAL}
{$IFDEF CPU32BITS}
begin
  // In 32PP, plain division using System.Math.DivMod(UInt64, ...) is much slower than this:
  Result := InternalDivMod(Dividend, @Divisor, Quotient, Remainder, LSize, 1);
end;
{$ELSE CPU64BITS}
var
  J: Integer;
  LQuotient, LRemainder: UInt64;
begin
  LRemainder := 0;
  for J := LSize - 1 downto 0 do
  begin
    // DivMod(UInt64, UInt64, var UInt64, var UInt64)
{$IFOPT R+}
{$DEFINE RCHECKS}
{$R-}
{$ENDIF}
    System.Math.DivMod((LRemainder shl 32) or Dividend[J], Divisor, LQuotient, LRemainder);
{$IFDEF RCHECKS}
{$R+}
{$UNDEF RCHECKS}
{$ENDIF}
    Quotient[J] := TLimb(LQuotient);
  end;
  if Assigned(Remainder) then
    Remainder[0] := TLimb(LRemainder);
  Exit(True);
end;
{$ENDIF CPU64BITS}
{$ELSE !PUREPASCAL}
{$IFDEF WIN32}
asm

// Note: in some versions of Delphi, DIV EBX generates the wrong opcode, while DIV EAX,EBX doesn't. The same for
//       MUL EBX and MUL EAX,EBX.

        PUSH    ESI
        PUSH    EDI
        PUSH    EBX

        MOV     EBX,EDX

        MOV     EDI,LSize
        LEA     ESI,[EAX + CLimbSize*EDI - CLimbSize]
        LEA     ECX,[ECX + CLimbSize*EDI - CLimbSize]
        XOR     EDX,EDX

        SHR     EDI,CUnrollShift
        JE      @Tail

@DivLoop:

        MOV     EAX,[ESI]
        DIV     EAX,EBX
        MOV     [ECX],EAX

        MOV     EAX,[ESI - CLimbSize]
        DIV     EAX,EBX
        MOV     [ECX - CLimbSize],EAX

        MOV     EAX,[ESI - 2 * CLimbSize]
        DIV     EAX,EBX
        MOV     [ECX - 2 * CLimbSize],EAX

        MOV     EAX,[ESI - 3 * CLimbSize]
        DIV     EAX,EBX
        MOV     [ECX - 3 * CLimbSize],EAX

        LEA     ESI,[ESI - 4 * CLimbSize]
        LEA     ECX,[ECX - 4 * CLimbSize]
        DEC     EDI
        JNE     @DivLoop

@Tail:

        MOV     EDI,LSize
        AND     EDI,CUnrollMask
        JE      @StoreRemainder

@TailLoop:

        MOV     EAX,[ESI]
        DIV     EAX,EBX
        MOV     [ECX],EAX
        LEA     ESI,[ESI - CLimbSize]
        LEA     ECX,[ECX - CLimbSize]
        DEC     EDI
        JNE     @TailLoop

@StoreRemainder:

        MOV     EBX,Remainder
        OR      EBX,EBX
        JE      @Exit

        MOV     [EBX],EDX

@Exit:

        MOV     EAX,1

        POP     EBX
        POP     EDI
        POP     ESI

end;
{$ELSE WIN64}
asm
        MOV     R10D,EDX

        MOV     R11D,LSize
        LEA     RCX,[RCX + R11*CLimbSize]
        LEA     R8,[R8 + R11*CLimbSize]
        XOR     EDX,EDX

        SHR     R11D,CUnrollShift
        JE      @Tail

@DivLoop:

        // Note: 64 bit division turned out to be considerably slower!

        MOV     EAX,[RCX - CLimbSize]
        DIV     EAX,R10D                        // Uses DIV EAX,R10D syntax because of bug in XE 64 bit assembler.
        MOV     [R8 - CLimbSize],EAX

        MOV     EAX,[RCX - 2 * CLimbSize]
        DIV     EAX,R10D
        MOV     [R8 - 2 * CLimbSize],EAX

        MOV     EAX,[RCX - 3 * CLimbSize]
        DIV     EAX,R10D
        MOV     [R8 - 3 * CLimbSize],EAX

        MOV     EAX,[RCX - 4 * CLimbSize]
        DIV     EAX,R10D
        MOV     [R8 - 4 * CLimbSize],EAX

        LEA     RCX,[RCX - 4 * CLimbSize]
        LEA     R8,[R8 - 4 * CLimbSize]
        DEC     R11D
        JNE     @DivLoop

@Tail:

        MOV     R11D,LSize
        AND     R11D,CUnrollMask
        JE      @StoreRemainder

@TailLoop:

        MOV     EAX,[RCX - ClimbSize]
        DIV     EAX,R10D
        MOV     [R8 - CLimbSize],EAX
        LEA     RCX,[RCX - CLimbSize]
        LEA     R8,[R8 - CLimbSize]
        DEC     R11D
        JNE     @TailLoop

@StoreRemainder:

        OR      R9,R9
        JE      @Exit
        MOV     [R9],EDX

@Exit:

        MOV     EAX,1

end;
{$ENDIF}
{$ENDIF PUREPASCAL}

class function BigInteger.InternalDivMod(Dividend, Divisor, Quotient, Remainder: PLimb; LSize, RSize: Integer): Boolean;

// Basecase division, see Knuth TAOCP, Vol. 2.

{$IF DEFINED(PUREPASCAL)}
var
  PDividend, PDivisor, PQuotient, PRemainder: PDivLimb;
  NormDividend, NormDivisor: TArray<TDivLimb>;          // Normalized dividend and divisor
  QHat: TDblLimb;                                       // Estimate quotient limb
  RHat: TDblLimb;                                       // Remainder after calculating QHat
  Product: TDblLimb;                                    // Product of limb and QHat
  Shift, RevShift, I, J: Integer;                       // Help variables
  NormDividendTop2, NormDivisorTop: TDblLimb;
{$IF SizeOf(TDivLimb) = SizeOf(TLimb)}
  Carry, Value: Int64;
{$ELSE}
  Carry, Value: Integer;
{$IFEND}
begin
  Assert(SizeOf(TDblLimb) = 2 * SizeOf(TDivLimb));
  PDividend := PDivLimb(Dividend);
  PDivisor := PDivLimb(Divisor);
  PQuotient := PDivLimb(Quotient);
  PRemainder := PDivLimb(Remainder);

{$IF SizeOf(TLimb) > SizeOf(TDivLimb)}
  LSize := LSize + LSize;
  RSize := RSize + RSize;

  if PDivisor[RSize - 1] = 0 then
    Dec(RSize);
{$IFEND}

  ///////////////////////////////////////////////////////////////////////////////////////////////////////
  ///  NOTE: In Win32, this uses 16-bit division (with 32-bit intermediate results) to avoid having   ///
  ///        to use 64-bit unsigned integers. This turned out to be (approx. 17%) faster than using   ///
  ///        32-bit limbs.                                                                            ///
  ///        In Win64, this uses 32-bit division with 64-bit intermediate results.                    ///
  ///////////////////////////////////////////////////////////////////////////////////////////////////////

  if (LSize < RSize) then
    Exit(False);

  while (RSize > 0) and (PDivisor[RSize - 1] = 0) do
    Dec(RSize);
  if RSize = 0 then
    Exit(False);

  while (LSize > 0) and (PDividend[LSize - 1] = 0) do
    Dec(LSize);

  ///////////////////////////////////////////////////////////////////////////////////////////////////////
  /// Perhaps it makes sense to shift away common trailing zero limbs, if divisor > certain size.     ///
  /// Shifting should be pretty simple: simply remove any common zeroes in both dividend and divisor, ///
  /// generate an offset to the lowest non-zero limb and shift accordingly (when normalizing).        ///
  /// Note that the remainder must be amended accordingly.                                            ///
  /// Note2: No need to amend the result: x / y == (x/n) / (y/n), when n is the trailing zero part.   ///
  ///////////////////////////////////////////////////////////////////////////////////////////////////////

  if RSize = 1 then
  begin
    // Handle single-digit divisor.

  {$IF SizeOf(TDivLimb) = SizeOf(TLimb)}
    Exit(InternalDivMod32(Dividend, PDivisor[0], Quotient, Remainder, LSize));
  {$ELSE}
    Exit(InternalDivMod16(Dividend, PDivisor[0], Quotient, Remainder, (LSize + 1) div 2));
  {$IFEND}
  end;
  // Normalize by shifting divisor left just enough so that its high-order bit is set, and shift
  // dividend left the same amount. A high-order digit is prepended to dividend unconditionally.

  // Get number of leading zeroes.
  Shift := Velthuis.Numerics.NumberOfLeadingZeros(PDivisor[RSize - 1]); // 0 <= Shift < Bits.
  RevShift := CDivLimbBits - Shift;

  // Normalize divisor and shift dividend left accordingly.
  SetLength(NormDivisor, RSize);
  SetLength(NormDividend, LSize + 1);
  if Shift > 0 then
  begin
    for I := RSize - 1 downto 1 do
      NormDivisor[I] := TDivLimb((TDblLimb(PDivisor[I]) shl Shift) or (TDblLimb(PDivisor[I - 1]) shr RevShift));
    NormDivisor[0] := TDivLimb(TDblLimb(PDivisor[0]) shl Shift);

    NormDividend[LSize] := PDividend[LSize - 1] shr RevShift;
    for I := LSize - 1 downto 1 do
      NormDividend[I] := TDivLimb((TDblLimb(PDividend[I]) shl Shift) or (TDblLimb(PDividend[I - 1]) shr RevShift));
    NormDividend[0] := TDivLimb(TDblLimb(PDividend[0]) shl Shift);
  end
  else
  begin
    // SizeOf(TDivLimb) is not always SizeOf(TLimb), so don't use MoveLimbs() here.
    Move(PDivisor[0], NormDivisor[0], RSize * SizeOf(TDivLimb));
    Move(PDividend[0], NormDividend[0], LSize * SizeOf(TDivLimb));
  end;

  // Knuth's basecase algorithm.

  // Main loop.
  for J := LSize - RSize downto 0 do
  begin
    NormDivisorTop := NormDivisor[RSize - 1];
    NormDividendTop2 := PDblLimb(@NormDividend[J + RSize - 1])^;

    // QHat -- q^ in TAOCP -- is (first) estimate of Quotient[J]
    QHat := NormDividendTop2 div NormDivisorTop;

    // RHat -- r^ in TAOCP -- is remainder belonging to q^.
    RHat := NormDividendTop2 - QHat * NormDivisorTop;

    while (QHat * NormDivisor[RSize - 2] > RHat shl CDivLimbBits + NormDividend[J + RSize - 2]) or
          (QHat >= CDivLimbBase) do
    begin
      Dec(QHat);
      Inc(RHat, NormDivisorTop);

      if RHat >= CDivLimbBase then
        Break;
    end;

    // Multiply and subtract.
    Carry := 0;
    for I := 0 to RSize - 1 do
    begin
      Product := QHat * NormDivisor[I];
      Value := NormDividend[I + J] - Carry - TDivLimb(Product);
      NormDividend[I + J] := TDivLimb(Value);
    {$IF SizeOf(TLimb) = SizeOf(TDivLimb)}
      // Integer cast to force sign-extension of 'Value shr Bits'
      Carry := Int64(Product) shr CDivLimbBits - Integer(Value shr CDivLimbBits);
    {$ELSE}
      // Smallint cast to force sign-extension of 'Value shr Bits'
      Carry := Integer(Product) shr CDivLimbBits - Smallint(Value shr CDivLimbBits);
    {$IFEND}
    end;
    Value := NormDividend[J + RSize] - Carry;
    NormDividend[J + RSize] := TDivLimb(Value);

    if Value < 0 then
    begin

      // If too much was subtracted, add back.
      Dec(QHat);
      Value := 0;
      for I := 0 to RSize - 1 do
      begin

        /////////////////////////////////////////////////////////////////////////////////////////////////
        ///  Note: the original code was:                                                             ///
        ///                                                                                           ///
        ///    Value := NormDividend[I + J] + NormDivisor[I] + Value shr CDivLimbBits;                ///
        ///                                                                                           ///
        ///  That caused bad results in 64 bit, probably because the first two operands were          ///
        ///  treated as 32 bit first, i.e.                                                            ///
        ///                                                                                           ///
        ///    UInt64 := UInt64(UInt32 + UInt32) + UInt64;                                            ///
        ///                                                                                           ///
        ///  instead of                                                                               ///
        ///                                                                                           ///
        ///    UInt64 := UInt64 + UInt64 + UInt64;                                                    ///
        /////////////////////////////////////////////////////////////////////////////////////////////////

        Value := Value shr CDivLimbBits + NormDividend[I + J] + NormDivisor[I];
        NormDividend[I + J] := TDivLimb(Value);
      end;
      Inc(NormDividend[J + RSize], Value shr CDivLimbBits);
    end;

    PQuotient[J] := QHat;
  end;

  // If the caller wants the remainder, unnormalize it and pass it back.
  if Assigned(PRemainder) then
    if Shift <> 0 then
      for I := 0 to RSize - 1 do
        PRemainder[I] := TDivLimb((TDblLimb(NormDividend[I]) shr Shift) or (TDblLimb(NormDividend[I + 1]) shl RevShift))
    else
      for I := 0 to RSize - 1 do
        PRemainder[I] := NormDividend[I];

  Result := True;
end;
{$ELSEIF DEFINED(WIN32)}
var
  LDividend, LDivisor, LQuotient: PLimb;                // Local copies of passed registers
  NormDividend, NormDivisor: PLimb;                     // Manually managed dynamic arrays
  QHat, RHat, Product: TUInt64;                         // 64 bit intermediate results
  Overflow: TLimb;                                      // "Carry" between multiplications
  Shift: Integer;                                       // Normalization shift
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX

        // To avoid reference count problems with Delphi's dynamic array types, we do our own,
        // "old school" dynarrays, using GetMem and FreeMem.

        XOR     EBX,EBX                         // Set "dynarrays" to nil, so the FreeMem calls won't fail.
        MOV     NormDividend,EBX
        MOV     NormDivisor,EBX

        MOV     LDividend,EAX
        MOV     LDivisor,EDX
        MOV     LQuotient,ECX

        MOV     ESI,LSize
        MOV     EDI,RSize
        CMP     ESI,EDI
        JL      @ExitFalse

        DEC     EDI
        JS      @ExitFalse
        JNE     @MultiLimbDivisor

/////////////////////////////////////////////////////////////////////////////////////////////////////////
///  Simple division                                                                                  ///
///    Divisor only contains one single limb: simple division and exit.                               ///
/////////////////////////////////////////////////////////////////////////////////////////////////////////

@SingleLimbDivisor:

        MOV     EBX,[EDX]
        DEC     ESI
        MOV     EDI,EAX
        XOR     EDX,EDX

@SingleDivLoop:

        MOV     EAX,[EDI + CLimbSize*ESI]
        DIV     EAX,EBX
        MOV     [ECX + CLimbSize*ESI],EAX
        DEC     ESI
        JNS     @SingleDivLoop
        MOV     EAX,Remainder
        TEST    EAX,EAX
        JZ      @ExitTrue
        MOV     [EAX],EDX
        JMP     @ExitTrue

/////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Multilimb division                                                                                ///
///   Divisor contains more than one limb: basecase division as described in Knuth's TAoCP.           ///
/////////////////////////////////////////////////////////////////////////////////////////////////////////

@MultiLimbDivisor:

        MOV     EAX,RSize               // GetMem(NormDivisor, (RSize + LSize + 1) * CLimbSize;
        ADD     EAX,LSize
        LEA     EAX,[EAX*CLimbSize + CLimbSize]
        CALL    System.AllocMem
        MOV     NormDivisor,EAX
        MOV     EDX,RSize
        LEA     EAX,[EAX + EDX*CLimbSize]
        MOV     NormDividend,EAX

// First: normalize Divisor by shifting left to eliminate leading zeroes
//        and shift Dividend left by same number of bits.

        // Get number of leading Divisor zeros (into ECX).

        MOV     ESI,LDivisor
        MOV     EBX,[ESI+CLimbSize*EDI]
        BSR     EBX,EBX
        MOV     ECX,31
        SUB     ECX,EBX
        MOV     Shift,ECX

        // Shift Divisor to NormDivisor by CL.

        MOV     EBX,EDI
        MOV     EDI,NormDivisor
        MOV     EAX,[ESI + CLimbSize*EBX]
        JMP     @ShiftDivisor

@ShiftDivisorLoop:

        MOV     EDX,[ESI + CLimbSize*EBX]
        SHLD    EAX,EDX,CL
        MOV     [EDI + CLimbSize*EBX + CLimbSize],EAX
        MOV     EAX,EDX

@ShiftDivisor:

        DEC     EBX
        JNS     @ShiftDivisorLoop

        // Handle lowest limb.

        SHL     EAX,CL
        MOV     [EDI],EAX

        // Shift Dividend to NormDividend by CL.

        MOV     EBX,LSize
        MOV     ESI,LDividend
        MOV     EDI,NormDividend
        XOR     EAX,EAX
        JMP     @ShiftDividend

@ShiftDividendLoop:

        MOV     EDX,[ESI + CLimbSize*EBX]
        SHLD    EAX,EDX,CL
        MOV     [EDI + CLimbSize*EBX + CLimbSize],EAX
        MOV     EAX,EDX

@ShiftDividend:

        DEC     EBX
        JNS     @ShiftDividendLoop

        // Handle lowest limb.

        SHL     EAX,CL
        MOV     [EDI],EAX

        MOV     EBX,LSize
        MOV     ECX,RSize

        MOV     ESI,NormDividend
        MOV     EDI,NormDivisor
        LEA     EDI,[EDI + CLimbSize*ECX - CLimbSize]

@MainLoop:

        XOR     EDX,EDX
        MOV     EAX,[ESI + CLimbSize*EBX]
        DIV     EAX,[EDI]
        MOV     QHat.Hi,EAX
        MOV     EAX,[ESI + CLimbSize*EBX - CLimbSize]
        DIV     EAX,[EDI]
        MOV     QHat.Lo,EAX
        MOV     RHat.Lo,EDX
        XOR     EDX,EDX
        MOV     RHat.Hi,EDX

@CheckAdjust:

        CMP     QHat.Hi,0
        JNE     @DoAdjust
        MOV     EAX,QHat.Lo
        MUL     EAX,[EDI - CLimbSize]

        CMP     EDX,RHat.Lo
        JA      @DoAdjust
        JB      @AdjustEnd
        CMP     EAX,[ESI + CLimbSize*EBX - 2*CLimbSize]
        JBE     @AdjustEnd

@DoAdjust:

        SUB     QHat.Lo,1
        SBB     QHat.Hi,0
        MOV     EAX,[EDI]
        ADD     RHat.Lo,EAX
        ADC     RHat.Hi,0
        JZ      @CheckAdjust

@AdjustEnd:

        // Now multiply NormDivisor by QHat and subtract the product from NormDividend[J].

        // Save a few registers.

        PUSH    EDI
        PUSH    EBX
        PUSH    ECX

        MOV     ECX,EBX
        SUB     ECX,RSize
        LEA     EDI,[ESI + CLimbSize*ECX]
        MOV     EAX,LQuotient
        MOV     EDX,QHat.Lo
        MOV     [EAX + CLimbSize*ECX],EDX
        XOR     EBX,EBX
        MOV     Overflow,EBX

@SubtractProduct:

        MOV     EAX,NormDivisor
        MOV     EAX,[EAX + CLimbSize*EBX]
        MUL     EAX,QHat.Lo
        MOV     Product.Lo,EAX
        MOV     Product.Hi,EDX
        XOR     EDX,EDX
        MOV     EAX,[EDI + CLimbSize*EBX]
        SUB     EAX,Overflow
        SBB     EDX,0
        SUB     EAX,Product.Lo
        SBB     EDX,0
        MOV     [EDI + CLimbSize*EBX],EAX
        MOV     EAX,Product.Hi
        SUB     EAX,EDX
        MOV     Overflow,EAX
        INC     EBX
        CMP     EBX,RSize
        JL      @SubtractProduct

@SubtractProductEnd:

        MOV     EBX,[ESP + 4]
        MOV     EDX,[ESI + CLimbSize*EBX]
        SUB     EDX,Overflow
        MOV     [ESI + CLimbSize*EBX],EDX
        JNC     @SkipAddBack

        // Add normalized divisor back, if necessary:

        MOV     EAX,LQuotient
        DEC     [EAX + CLimbSize*ECX]
        XOR     EBX,EBX
        MOV     Overflow,EBX

@AddBackLoop:

        CMP     EBX,RSize
        JE      @AddBackLoopEnd

        XOR     EDX,EDX
        MOV     EAX,NormDivisor
        MOV     EAX,[EAX + CLimbSize*EBX]
        ADD     EAX,Overflow

        ADC     EDX,0   // Note: forgetting this caused errors that only exhibited when I started testing ModPow.

        ADD     [EDI + CLimbSize*EBX],EAX
        ADC     EDX,0
        MOV     Overflow,EDX
        INC     EBX
        JMP     @AddBackLoop

@AddBackLoopEnd:

        ADD     [EDI + CLimbSize*EBX],EDX

@SkipAddBack:

        POP     ECX
        POP     EBX
        POP     EDI

        // End of main loop; loop if required.

        DEC     EBX
        CMP     EBX,ECX
        JGE      @MainLoop

        // NormDividend now contains remainder, scaled by Shift.
        // If Assigned(Remainder), then shift NormDividend down into Remainder.

        MOV     EAX,Remainder
        TEST    EAX,EAX
        JE      @ExitTrue
        XOR     EBX,EBX
        MOV     ESI,NormDividend
        MOV     EDI,EAX
        MOV     ECX,Shift
        MOV     EAX,[ESI + CLimbSize*EBX]

@RemainderLoop:

        MOV     EDX,[ESI + CLimbSize*EBX + CLimbSize]
        SHRD    EAX,EDX,CL
        MOV     [EDI + CLimbSize*EBX],EAX
        MOV     EAX,EDX
        INC     EBX
        CMP     EBX,RSize
        JL      @RemainderLoop
        SHR     EDX,CL
        MOV     [EDI + CLimbSize*EBX],EDX
        JMP     @ExitTrue

@ExitFalse:

        MOV     BL,0
        JMP     @Exit

@ExitTrue:

        MOV     BL,1

@Exit:

        // Clear dynamic arrays.

        MOV     EAX,NormDivisor
        CALL    System.@FreeMem

        MOV     EAX,EBX

        POP     EBX
        POP     EDI
        POP     ESI
end;
{$ELSE}
var
  LDividend, LDivisor, LQuotient, LRemainder: PLimb;
  NormDividend, NormDivisor: PLimb;
  QHat, RHat, Product: TUInt64;
  Overflow: TLimb;
  Shift: Integer;
  SaveRDI, SaveRBX, SaveRCX: NativeUInt;
asm
        .PUSHNV RSI
        .PUSHNV RDI
        .PUSHNV RBX

        // To avoid reference count problems with Delphi's dynamic array types, we do our own,
        // "old school" dynarrays, using GetMem and FreeMem.

        XOR     EBX,EBX                 // Set "dynarrays" to nil, so FreeMem calls won't fail.
        MOV     NormDividend,RBX
        MOV     NormDivisor,RBX

        MOV     LDividend,RCX
        MOV     LDivisor,RDX
        MOV     LQuotient,R8
        MOV     LRemainder,R9

        MOV     ESI,LSize
        MOV     EDI,RSize
        CMP     ESI,EDI
        JL      @ExitFalse

        DEC     EDI
        JS      @ExitFalse
        JNE     @MultiLimbDivisor

/////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Simple division                                                                                   ///
///   Divisor only contains one single limb: simple division and exit.                                ///
///                                                                                                   ///
///   NOTE: 32 bit division is easier and probably faster than 64 bit, even in 64 bit mode.           ///
///         This was tested for Decimals.pas.                                                         ///
/////////////////////////////////////////////////////////////////////////////////////////////////////////

@SingleLimbDivisor:

        MOV     EBX,[RDX]

        DEC     ESI
        MOV     RDI,RCX
        XOR     EDX,EDX

@SingleDivLoop:

        MOV     EAX,[RDI + CLimbSize*RSI]

        // ------------------------------------------------------------------------------------------- //
        // NOTE: In XE2, in 64 bit asm, "DIV <r/m32>" is generated as "DIV <r/m64>",                   //
        //       but "DIV EAX,<r/m32>" is generated correctly.                                         //
        //       The same applies to "MUL <r/m32>".                                                    //
        // ------------------------------------------------------------------------------------------- //

        DIV     EAX,EBX
        MOV     [R8 + CLimbSize*RSI],EAX
        DEC     ESI
        JNS     @SingleDivLoop
        MOV     RAX,LRemainder
        TEST    RAX,RAX
        JZ      @ExitTrue
        MOV     [RAX],EDX
        JMP     @ExitTrue

/////////////////////////////////////////////////////////////////////////////////////////////////////////
/// MultiLimb division                                                                                ///
///   Divisor contains more than one limb: basecase division as described in Knuth's TAoCP Vol. 2.    ///
/////////////////////////////////////////////////////////////////////////////////////////////////////////

@MultiLimbDivisor:

        MOV     ECX,RSize
        ADD     ECX,ECX
        ADD     ECX,ECX
        CALL    System.AllocMem
        MOV     NormDivisor,RAX

        MOV     ECX,LSize
        INC     ECX
        ADD     ECX,ECX
        ADD     ECX,ECX
        CALL    System.AllocMem
        MOV     NormDividend,RAX

// First: normalize Divisor by shifting left to eliminate leading zeroes
//        and shift Dividend left by same nubmer of bits.

        // Get number of leading Divisor zeroes (into ECX).

        MOV     RSI,LDivisor
        MOV     EBX,[RSI + CLimbSize*RDI]
        BSR     EBX,EBX
        MOV     ECX,31
        SUB     ECX,EBX
        MOV     Shift,ECX

        // Shift Divisor to NormDivisor by CL.

        MOV     EBX,EDI
        MOV     RDI,NormDivisor
        MOV     EAX,[RSI + CLimbSize*RBX]
        JMP     @ShiftDivisor

@ShiftDivisorLoop:

        MOV     EDX,[RSI + CLimbSize*RBX]
        SHLD    EAX,EDX,CL
        MOV     [RDI + CLimbSize*RBX + CLimbSize],EAX
        MOV     EAX,EDX

@ShiftDivisor:

        DEC     EBX
        JNS     @ShiftDivisorLoop

        // Handle lowest limb.

        SHL     EAX,CL
        MOV     [RDI],EAX

        // Shift Dividend to NormDividend by CL.

        MOV     EBX,LSize
        MOV     RSI,LDividend
        MOV     RDI,NormDividend
        XOR     EAX,EAX
        JMP     @ShiftDividend

@ShiftDividendLoop:

        MOV     EDX,[RSI + CLimbSize*RBX]
        SHLD    EAX,EDX,CL
        MOV     [RDI + CLimbSize*RBX + CLimbSize],EAX
        MOV     EAX,EDX

@ShiftDividend:

        DEC     EBX
        JNS     @ShiftDividendLoop

        // Handle lowest limb.

        SHL     EAX,CL
        MOV     [RDI],EAX

        MOV     EBX,LSize
        MOV     ECX,RSize

        MOV     RSI,NormDividend
        MOV     RDI,NormDivisor
        LEA     RDI,[RDI + CLimbSize*RCX - CLimbSize]

@MainLoop:

        XOR     EDX,EDX
        MOV     EAX,[RSI + CLimbSize*RBX]
        DIV     EAX,[RDI]
        MOV     QHat.Hi,EAX
        MOV     EAX,[RSI + CLimbSize*RBX - CLimbSize]
        DIV     EAX,[RDI]
        MOV     QHat.Lo,EAX
        MOV     RHat.Lo,EDX
        XOR     EDX,EDX
        MOV     RHat.Hi,EDX

@CheckAdjust:

        CMP     QHat.Hi,0
        JNE     @DoAdjust
        MOV     EAX,QHat.Lo
        MUL     EAX,[RDI - CLimbSize]

        CMP     EDX,RHat.Lo
        JA      @DoAdjust
        JB      @AdjustEnd
        CMP     EAX,[RSI + CLimbSize*RBX - 2*CLimbSize]
        JBE     @AdjustEnd

@DoAdjust:

        SUB     QHat.Lo,1
        SBB     QHat.Hi,0
        MOV     EAX,[RDI]
        ADD     RHat.Lo,EAX
        ADC     RHat.Hi,0
        JZ      @CheckAdjust

@AdjustEnd:

        MOV     SaveRDI,RDI
        MOV     SaveRBX,RBX
        MOV     SaveRCX,RCX

        MOV     ECX,EBX
        SUB     ECX,RSize
        LEA     RDI,[RSI + CLimbSize*RCX]
        MOV     RAX,LQuotient
        MOV     EDX,QHat.Lo
        MOV     [RAX + CLimbSize*RCX],EDX
        XOR     EBX,EBX
        MOV     Overflow,EBX

@SubtractProduct:

        MOV     RAX,NormDivisor
        MOV     EAX,[RAX + CLimbSize*RBX]
        MUL     EAX,QHat.Lo
        MOV     Product.Lo,EAX
        MOV     Product.Hi,EDX
        XOR     EDX,EDX
        MOV     EAX,[RDI + CLimbSize*RBX]
        SUB     EAX,Overflow
        SBB     EDX,0
        SUB     EAX,Product.Lo
        SBB     EDX,0
        MOV     [RDI + CLimbSize*RBX],EAX
        MOV     EAX,Product.Hi
        SUB     EAX,EDX
        MOV     Overflow,EAX
        INC     EBX
        CMP     EBX,RSize
        JL      @SubtractProduct

@SubtractProductEnd:

        MOV     RBX,SaveRBX
        MOV     EDX,[RSI + CLimbSize*RBX]
        SUB     EDX,Overflow
        MOV     [RSI + CLimbSize*RBX],EDX
        JNC     @SkipAddBack

        // Add normalized divisor back, if necessary:

        MOV     RAX,LQuotient
        DEC     DWORD PTR [RAX + ClimbSize*RCX]
        XOR     EBX,EBX
        MOV     Overflow,EBX

@AddBackLoop:

        CMP     EBX,RSize
        JE      @AddBackLoopEnd
        XOR     EDX,EDX
        MOV     RAX,NormDivisor
        MOV     EAX,[RAX + CLimbSize*RBX]
        ADD     EAX,Overflow
        ADC     EDX,0
        ADD     [RDI + CLimbSize*RBX],EAX
        ADC     EDX,0
        MOV     Overflow,EDX
        INC     EBX
        JMP     @AddBackLoop

@AddBackLoopEnd:

        ADD     [RDI + CLimbSize*RBX],EDX

@SkipAddBack:

        MOV     RCX,SaveRCX
        MOV     RBX,SaveRBX
        MOV     RDI,SaveRDI

        // End of main loop; loop if required

        DEC     EBX
        CMP     EBX,ECX
        JGE     @MainLoop

        // NormDividend now contains remainder, scaled by Shift.
        // If Assigned(Remainder), then shift NormDividend down into Remainder

        MOV     RAX,LRemainder
        TEST    RAX,RAX
        JE      @ExitTrue
        XOR     EBX,EBX
        MOV     RSI,NormDividend
        MOV     RDI,RAX
        MOV     ECX,Shift
        MOV     EAX,[RSI + CLimbSize*RBX]

@RemainderLoop:

        MOV     EDX,[RSI + CLimbSize*RBX + CLimbSize]
        SHRD    EAX,EDX,CL
        MOV     [RDI + CLimbSize*RBX],EAX
        MOV     EAX,EDX
        INC     EBX
        CMP     EBX,RSize
        JL      @RemainderLoop
        SHR     EDX,CL
        MOV     [RDI + CLimbSize*RBX],EDX
        JMP     @ExitTrue

@ExitFalse:

        MOV     BL,False
        JMP     @Exit

@ExitTrue:

        MOV     BL,True

@Exit:

        // Clear dynamic arrays.

        MOV     RCX,NormDividend
        CALL    System.@FreeMem

        MOV     RCX,NormDivisor
        CALL    System.@FreeMem

        MOV     EAX,EBX

end;
{$IFEND}

// Note: only handles Abs(Self) > 0.
class procedure BigInteger.InternalIncrement(Limbs: PLimb; Size: Integer);
{$IFDEF PUREPASCAL}
var
  N: TLimb;
begin
  N := MaxInt;
  while Size > 0 do
  begin
    N := Limbs^;
    Inc(N);
    Limbs^ := N;
    if N <> 0 then
      Break;
    Inc(Limbs);
    Dec(Size);
  end;
  if N = 0 then
  begin
    Limbs^ := 1;
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF WIN32}
asm

        TEST    EDX,EDX
        JE      @Exit

@Loop:

        MOV     ECX,[EAX]
        INC     ECX
        MOV     [EAX],ECX
        TEST    ECX,ECX
        JNE     @Exit
        LEA     EAX,[EAX + CLimbSize]
        DEC     EDX
        JNE     @Loop

@Last:

        TEST    ECX,ECX
        JNE     @Exit
        MOV     TLimb PTR [EAX],1

@Exit:

end;
{$ELSE !WIN32}
asm

        TEST    EDX,EDX
        JE      @Exit

@Loop:

        MOV     EAX,[RCX]
        INC     EAX
        MOV     [RCX],EAX
        TEST    EAX,EAX
        JNE     @Exit
        LEA     RCX,[RCX + CLimbSize]
        DEC     EDX
        JNE     @Loop

@Last:

        TEST    EAX,EAX
        JNE     @Exit
        MOV     TLimb PTR [RCX],1

@Exit:

end;
{$ENDIF !WIN32}
{$ENDIF !PUREPASCAL}

// Note: only handles Abs(Self) > 1
class procedure BigInteger.InternalDecrement(Limbs: PLimb; Size: Integer);
{$IFDEF PUREPASCAL}
begin
  repeat
    Dec(Limbs^);
    if Limbs^ <> TLimb(-1) then
      Break;
    Inc(Limbs);
    Dec(Size);
  until Size = 0;
end;
{$ELSE !PUREPASCAL}
{$IFDEF WIN32}
asm

@Loop:

        MOV     ECX,[EAX]
        DEC     ECX
        MOV     [EAX],ECX
        CMP     ECX,-1
        JNE     @Exit
        LEA     EAX,[EAX + CLimbSize]
        DEC     EDX
        JNE     @Loop

@Exit:

end;
{$ELSE !WIN32}
asm

@Loop:

        MOV     EAX,[RCX]
        DEC     EAX
        MOV     [RCX],EAX
        CMP     EAX,-1
        JNE     @Exit
        LEA     RCX,[RCX + CLimbSize]
        DEC     EDX
        JNE     @Loop

@Exit:

end;
{$ENDIF !WIN32}
{$ENDIF !PUREPASCAL}

// Divides a magnitude (usually the FData of a TBigInteger) by Base and returns the remainder.
class function BigInteger.InternalDivideByBase(Mag: PLimb; Base: Integer; var Size: Integer): UInt32;
{$IF DEFINED(PUREPASCAL)}

// This routine uses DivMod(Cardinal, Word, Word, Word).
// In Win32, that is 14 times faster than the previous version using the DivMod with UInt64 parameters.
// In Win64, it is only a little bit slower.

type
  UInt32Rec = record
    Lo, Hi: UInt16;
  end;
  PUInt16 = ^UInt16;

var
  P, PMag: PUInt16;
  Remainder: UInt16;
  CurrentWord: UInt32;
begin
  Result := 0;
  if Size = 0 then
    Exit;
  PMag := PUInt16(Mag);
  P := PMag + Size * 2;
  Remainder := 0;
  while P > PMag do
  begin
    Dec(P);
    UInt32Rec(CurrentWord).Lo := P^;
    UInt32Rec(CurrentWord).Hi := Remainder;
    System.Math.DivMod(CurrentWord, Base, P^, Remainder);
  end;
  Result := Remainder;
  if Mag[Size - 1] = 0 then
    Dec(Size);
end;
{$ELSEIF DEFINED(WIN32)}
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     EBX,ECX                         // var Size
        MOV     ECX,EDX
        MOV     ESI,EAX                         // PBase (= Mag)
        MOV     EDX,[EBX]
        XOR     EAX,EAX                         // Result
        TEST    EDX,EDX
        JE      @Exit
        LEA     EDI,[ESI + CLimbSize*EDX]       // P
        XOR     EDX,EDX                         // Remainder := 0;
        CMP     EDI,ESI                         // while P > PBase do
        JBE     @CheckSize
@Loop:
        SUB     EDI,4                           // Dec(P);
        MOV     EAX,[EDI]                       // DivMod(P^ or (Remainder shl 32), 10, P^, Remainder);
        DIV     EAX,ECX
        MOV     [EDI],EAX
        CMP     EDI,ESI                         // while P > PBase do
        JA      @Loop
@CheckSize:
        MOV     EAX,EDX                         // if (PBase + Size - 1)^ = 0 then
        MOV     EDX,[EBX]
        LEA     ESI,[ESI + CLimbSize*EDX - CLimbSize]
        CMP     [ESI],0
        JNE     @Exit
        DEC     DWORD PTR [EBX]                 //   Dec(Size);
@Exit:
        POP     EBX
        POP     EDI
        POP     ESI
end;
{$ELSE}
asm
        .NOFRAME

        MOV     R11,R8                          // var Size
        MOV     R9,RCX                          // PBase := Mag;
        MOV     ECX,EDX
        XOR     EAX,EAX                         // Result := 0;
        MOV     EDX,[R11]                       // if Size = 0 then Exit;
        OR      EDX,EDX
        JE      @Exit
        LEA     R10,[R9 + CLimbSize*RDX]        // P
        XOR     EDX,EDX                         // Remainder := 0;
        CMP     R10,R9                          // while P > PBase do
        JBE     @CheckSize
@Loop:
        SUB     R10,4                           // Dec(P)
        MOV     EAX,[R10]                       // DivMod(P^ or (Remainder shl 32), 10, P^, Remainder);
        DIV     EAX,ECX
        MOV     [R10],EAX
        CMP     R10,R9                          // while P > PBase do
        JA      @Loop
@CheckSize:
        MOV     EAX,EDX
        MOV     EDX,[R11]
        CMP     [R9 + CLimbSize*RDX - CLimbSize],0   // if (PBase + Size - 1)^ = 0 then
        JNE     @Exit
        DEC     DWORD PTR [R11]                 //   Dec(Size);
@Exit:
end;
{$IFEND}

class operator BigInteger.Equal(const Left, Right: BigInteger): Boolean;
begin
  Result := Compare(Left, Right) = 0;
end;

class procedure BigInteger.Error(ErrorCode: TErrorCode; const ErrorInfo: array of const);
begin
  case ErrorCode of
    ecParse:
      raise EConvertError.CreateFmt(SErrorParsingFmt, ErrorInfo);
    ecDivbyZero:
      raise EZeroDivide.Create(SDivisionByZero);
    ecConversion:
      raise EConvertError.CreateFmt(SConversionFailedFmt, ErrorInfo);
    ecOverflow:
      raise EOverflow.Create(SOverflow);
    ecInvalidArgFloat:
      raise EInvalidArgument.CreateFmt(SInvalidArgumentFloatFmt, ErrorInfo);
    ecInvalidBase:
      raise EInvalidArgument.Create(SInvalidArgumentBase);
    ecInvalidArg:
      raise EInvalidArgument.CreateFmt(SInvalidArgumentFmt, ErrorInfo);
    ecNoInverse:
      raise EInvalidArgument.Create(SNoInverse);
    ecNegativeExponent:
      raise EInvalidArgument.CreateFmt(SNegativeExponent, ErrorInfo);
    ecNegativeRadicand:
      raise EInvalidArgument.CreateFmt(SNegativeRadicand, ErrorInfo);
  else
    raise EInvalidOp.Create(SInvalidOperation);
  end;
end;

class operator BigInteger.Explicit(const Value: BigInteger): Int32;
begin
  if Value.FData = nil then
    Result := 0
  else
  begin
    Result := Int32(Value.FData[0]);
    if Value.FSize < 0 then
      Result := -Result;
  end;
end;

class operator BigInteger.Explicit(const Value: BigInteger): UInt32;
begin
  if Value.FData = nil then
    Result := 0
  else
    Result := Value.FData[0];
  if Value.FSize < 0 then
    Result := UInt32(-Int32(Result));
end;

class operator BigInteger.Explicit(const Value: BigInteger): Int64;
begin
  if Value.FData = nil then
    Result := 0
  else
  begin
    TUInt64(Result).Lo := Value.FData[0];
    if (Value.FSize and SizeMask) > 1 then
      TUInt64(Result).Hi := Value.FData[1]
    else
      TUInt64(Result).Hi := 0;
    if Value.FSize < 0 then
      Result := -Result;
  end;
end;

class operator BigInteger.Explicit(const Value: BigInteger): UInt64;
begin
  if Value.FData = nil then
    Result := 0
  else
  begin
    TUInt64(Result).Lo := Value.FData[0];
    if (Value.FSize and SizeMask) > 1 then
      TUInt64(Result).Hi := Value.FData[1]
    else
      TUInt64(Result).Hi := 0;
  end;
  if Value.FSize < 0 then
    Result := UInt64(-Int64(Result));
end;

function BigInteger.AsCardinal: Cardinal;
begin
  Result := 0;
  if not IsNegative and (BitLength <= CCardinalBits) then
    Result := Cardinal(Self)
  else
    Error(ecConversion, ['BigInteger', 'Cardinal']);
end;

function GetBitAt(FData: PLimb; BitNum: Integer): Boolean;
begin
  Result := (FData[BitNum div 32] and (1 shl (BitNum and 31))) <> 0
end;

class procedure BigInteger.ConvertToFloatComponents(const Value: BigInteger; SignificandSize: Integer;
  var Sign: Integer; var Significand: UInt64; var Exponent: Integer);
var
  LRemainder, LLowBit, LSignificand: BigInteger;
  LBitLen: Integer;
begin
  if Value.IsNegative then
    Sign := -1
  else
    Sign := 1;

  Exponent := 0;
  LSignificand := BigInteger.Abs(Value);

  LBitLen := LSignificand.BitLength;
  if LBitLen > SignificandSize then
  begin
    // --- Shift down and adjust exponent.

    // Get lowest bit.
    LLowBit := BigInteger.One shl (LBitLen - SignificandSize);

    // Mask out bits being shifted out and save them for later.
    LRemainder := (LSignificand and (LLowBit - BigInteger.One)) shl 1;

    // Shift significand until it fits in SignificandSize (in bits).
    LSignificand := LSignificand shr (LBitLen - SignificandSize);
    Inc(Exponent, LBitLen - 1);

    // --- Round
    if (LRemainder > LLowBit) or ((LRemainder = LLowBit) and not LSignificand.IsEven) then
    begin
      LSignificand := LSignificand + BigInteger.One;
      if LSignificand.BitLength > SignificandSize then
      begin
        LSignificand := LSignificand shr 1;
        Inc(Exponent);
      end;
    end;
  end
  else
  begin
    LSignificand := LSignificand shl (SignificandSize - LBitLen);
    Inc(Exponent, LBitLen - 1);
  end;
  Significand := LSignificand.AsUInt64;
end;

const
  // Number of bits in full significand (including hidden bit, if any)
  // of IEEE-754 floating point types.
  KSingleSignificandBits          = 24;
  KDoubleSignificandBits          = 53;
  KExtendedSignificandBits        = 64;

  // Maximum possible exponents for IEEE-754 floating point types.
  KSingleMaxExponent              = 127;
  KDoubleMaxExponent              = 1023;
  KExtendedMaxExponent            = 16383;

function BigInteger.AsSingle: Single;
var
  LSign, LExponent: Integer;
  LMantissa: UInt64;
begin
  if Self.IsZero then
    Exit(0.0);

  ConvertToFloatComponents(Self, KSingleSignificandBits, LSign, LMantissa, LExponent);

  // Handle overflow.
  if LExponent > KSingleMaxExponent then
    if LSign < 0 then
      Result := NegInfinity
    else
      Result := Infinity
    // No need to check for denormals.
  else
    Result := {Velthuis.FloatUtils.}MakeSingle(LSign, LMantissa, LExponent);
end;

function BigInteger.AsDouble: Double;
var
  LSign, LExponent: Integer;
  LMantissa: UInt64;
begin
  if Self.IsZero then
    Exit(0.0);

  ConvertToFloatComponents(Self, KDoubleSignificandBits, LSign, LMantissa, LExponent);

  // Handle overflow.
  if LExponent > KDoubleMaxExponent then
    if LSign < 0 then
      Result := NegInfinity
    else
      Result := Infinity
    // No need to check for denormals.
  else
    Result := {Velthuis.FloatUtils.}MakeDouble(LSign, LMantissa, LExponent);
end;

{$IFDEF HasExtended}
function BigInteger.AsExtended: Extended;
var
  LSign, LExponent: Integer;
  LMantissa: UInt64;
begin
  if Self.IsZero then
    Exit(0.0);

  ConvertToFloatComponents(Self, KExtendedSignificandBits, LSign, LMantissa, LExponent);

  // Handle overflow.
  if LExponent > KExtendedMaxExponent then
    if LSign < 0 then
      Result := NegInfinity
    else
      Result := Infinity
    // No need to check for denormals.
  else
    Result := Velthuis.FloatUtils.MakeExtended(LSign, LMantissa, LExponent);
end;
{$ENDIF}

function BigInteger.AsInt64: Int64;
begin
  Result := 0;
  if BitLength <= CInt64Bits then
    Result := Int64(Self)
  else
    Error(ecConversion, ['BigInteger', 'Int64']);
end;

function BigInteger.AsInteger: Integer;
begin
  Result := 0;
  if BitLength <= CIntegerBits then
    Result := Integer(Self)
  else
    Error(ecConversion, ['BigInteger', 'Integer']);
end;

function BigInteger.AsUInt64: UInt64;
begin
  Result := 0;
  if not IsNegative and (BitLength <= CUInt64Bits) then
    Result := UInt64(Self)
  else
    Error(ecConversion, ['BigInteger', 'UInt64']);
end;

class function BigInteger.InternalCompare(Left, Right: PLimb; LSize, RSize: Integer): Integer;
{$IFDEF PUREPASCAL}
var
  L, R: PLimb;
begin
  if (LSize or RSize) = 0 then
    Exit(0);
  if LSize > RSize then
    Result := 1
  else if LSize < RSize then
    Result := -1
  else

  // Same size, so compare limbs. Start at the "top" (most significant limb).
  begin
    L := Left + LSize - 1;
    R := Right + LSize - 1;
    while L >= Left do
    begin
      if L^ <> R^ then
      begin
        if L^ > R^ then
          Exit(1)
        else if L^ < R^ then
          Exit(-1);
      end;
      Dec(L);
      Dec(R);
    end;
    Exit(0);
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF WIN32}
asm
        PUSH    ESI

        TEST    EAX,EAX
        JNE     @LeftNotNil
        TEST    EDX,EDX
        JZ      @ExitZero
        JMP     @ExitNeg

@LeftNotNil:

        TEST    EDX,EDX
        JZ      @ExitPos

        CMP     ECX,RSize
        JA      @ExitPos
        JB      @ExitNeg

        MOV     ESI,EAX

@Loop:

        MOV     EAX,[ESI + ECX*CLimbSize - CLimbSize]
        CMP     EAX,[EDX + ECX*CLimbSize - CLimbSize]
        JA      @ExitPos
        JB      @ExitNeg
        DEC     ECX
        JNE     @Loop

@ExitZero:

        XOR     EAX,EAX
        JMP     @Exit

@ExitPos:

        MOV     EAX,1
        JMP     @Exit

@ExitNeg:

        MOV     EAX,-1

@Exit:

        POP     ESI
end;
{$ELSE WIN64}
asm
        TEST    RCX,RCX
        JNZ     @LeftNotNil

        // Left is nil
        TEST    RDX,RDX
        JZ      @ExitZero                       // if Right nil too, then equal
        JMP     @ExitNeg                        // Otherwise, Left < Right

@LeftNotNil:

        TEST    RDX,RDX
        JZ      @ExitPos

        CMP     R8D,R9D
        JA      @ExitPos
        JB      @ExitNeg

        // R8D and R9D are same.

        LEA     RCX,[RCX + R8*CLimbSize]
        LEA     RDX,[RDX + R8*CLimbSize]

        TEST    R8D,1
        JZ      @NotOdd

        LEA     RCX,[RCX - CLimbSize]
        LEA     RDX,[RDX - CLimbSize]
        MOV     EAX,[RCX]
        CMP     EAX,[RDX]
        JA      @ExitPos
        JB      @ExitNeg
        DEC     R8D

@NotOdd:

        SHR     R8D,1
        JZ      @ExitZero

@Loop:

        LEA     RCX,[RCX - DLimbSize]
        LEA     RDX,[RDX - DLimbSize]
        MOV     RAX,[RCX]
        CMP     RAX,[RDX]
        JA      @ExitPos
        JB      @ExitNeg
        DEC     R8D
        JNE     @Loop

@ExitZero:

        XOR     EAX,EAX
        JMP     @Exit

@ExitPos:

        MOV     EAX,1
        JMP     @Exit

@ExitNeg:

        MOV     EAX,-1

@Exit:

end;
{$ENDIF WIN64}
{$ENDIF !PUREPASCAL}

{$IFNDEF PUREPASCAL}
class procedure BigInteger.InternalSubtractModified(Larger, Smaller, Result: PLimb; LSize, SSize: Integer);
{$IFDEF WIN32}
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX

        MOV     ESI,EAX                         // Left
        MOV     EDI,EDX                         // Right
        MOV     EBX,ECX                         // Result

        MOV     ECX,SSize
        MOV     EDX,LSize

        SUB     EDX,ECX
        PUSH    EDX
        XOR     EDX,EDX

        XOR     EAX,EAX

        MOV     EDX,ECX
        AND     EDX,CUnrollMask
        SHR     ECX,CUnrollShift

        CLC
        JE      @MainTail

@MainLoop:

        MOV     EAX,[ESI]
        SBB     EAX,[EDI]
        MOV     [EBX],EAX

        MOV     EAX,[ESI + CLimbSize]
        SBB     EAX,[EDI + CLimbSize]
        MOV     [EBX + CLimbSize],EAX

        MOV     EAX,[ESI + 2*CLimbSize]
        SBB     EAX,[EDI + 2*CLimbSize]
        MOV     [EBX + 2*CLimbSize],EAX

        MOV     EAX,[ESI + 3*CLimbSize]
        SBB     EAX,[EDI + 3*CLimbSize]
        MOV     [EBX + 3*CLimbSize],EAX

        LEA     ESI,[ESI + 4*CLimbSize]
        LEA     EDI,[EDI + 4*CLimbSize]
        LEA     EBX,[EBX + 4*CLimbSize]

        LEA     ECX,[ECX - 1]
        JECXZ   @MainTail
        JMP     @Mainloop

@MainTail:

        LEA     ESI,[ESI + EDX*CLimbSize]
        LEA     EDI,[EDI + EDX*CLimbSize]
        LEA     EBX,[EBX + EDX*CLimbSize]

        LEA     ECX,[@JumpsMain]
        JMP     [ECX + EDX*TYPE Pointer]

        .ALIGN  16

@JumpsMain:

        DD      @DoRestLoop
        DD      @Main1
        DD      @Main2
        DD      @Main3

@Main3:

        MOV     EAX,[ESI - 3*CLimbSize]
        SBB     EAX,[EDI - 3*CLimbSize]
        MOV     [EBX - 3*CLimbSize],EAX

@Main2:

        MOV     EAX,[ESI - 2*CLimbSize]
        SBB     EAX,[EDI - 2*CLimbSize]
        MOV     [EBX - 2*CLimbSize],EAX

@Main1:

        MOV     EAX,[ESI - CLimbSize]
        SBB     EAX,[EDI - CLimbSize]
        MOV     [EBX - CLimbSize],EAX

@DoRestLoop:

        SETC    AL                      // Save Carry Flag

        XOR     EDI,EDI

        POP     ECX
        MOV     EDX,ECX
        AND     EDX,CUnrollMask
        SHR     ECX,CUnrollShift

        ADD     AL,255                  // Restore Carry Flag.

        JECXZ   @RestLast3

@RestLoop:

        MOV     EAX,[ESI]
        SBB     EAX,EDI
        MOV     [EBX],EAX

        MOV     EAX,[ESI + CLimbSize]
        SBB     EAX,EDI
        MOV     [EBX + CLimbSize],EAX

        MOV     EAX,[ESI + 2*CLimbSize]
        SBB     EAX,EDI
        MOV     [EBX + 2*CLimbSize],EAX

        MOV     EAX,[ESI + 3*CLimbSize]
        SBB     EAX,EDI
        MOV     [EBX + 3*CLimbSize],EAX

        LEA     ESI,[ESI + 4*CLimbSize]
        LEA     EBX,[EBX + 4*CLimbSize]

        LEA     ECX,[ECX - 1]
        JECXZ   @RestLast3
        JMP     @RestLoop

@RestLast3:

        LEA     ESI,[ESI + EDX*CLimbSize]
        LEA     EBX,[EBX + EDX*CLimbSize]

        LEA     ECX,[@RestJumps]
        JMP     [ECX + EDX*TYPE Pointer]

        .ALIGN  16

@RestJumps:

        DD      @Exit
        DD      @Rest1
        DD      @Rest2
        DD      @Rest3

@Rest3:

        MOV     EAX,[ESI - 3*CLimbSize]
        SBB     EAX,EDI
        MOV     [EBX - 3*CLimbSize],EAX

@Rest2:

        MOV     EAX,[ESI - 2*CLimbSize]
        SBB     EAX,EDI
        MOV     [EBX - 2*CLimbSize],EAX

@Rest1:

        MOV     EAX,[ESI - CLimbSize]
        SBB     EAX,EDI
        MOV     [EBX - CLimbSize],EAX

@Exit:

        POP     EBX
        POP     EDI
        POP     ESI
end;
{$ELSE WIN32/WIN64}
asm
        MOV     R10,RCX
        MOV     ECX,SSize

        // R10 = Left, RDX = Right, R8 = Result, R9D = LSize, ECX = SSize

        CMP     R9D,ECX
        JAE     @SkipSwap
        XCHG    ECX,R9D
        XCHG    R10,RDX

@SkipSwap:

        SUB     R9D,ECX
        PUSH    R9

        MOV     R9D,ECX
        AND     R9D,CUnrollMask
        SHR     ECX,CUnrollShift

        CLC
        JE      @MainTail

@MainLoop:

        MOV     RAX,[R10]
        SBB     RAX,[RDX]
        MOV     [R8],RAX

        MOV     RAX,[R10 + DLimbSize]
        SBB     RAX,[RDX + DLimbSize]
        MOV     [R8 + DLimbSize],RAX

        LEA     R10,[R10 + 2*DLimbSize]
        LEA     RDX,[RDX + 2*DLimbSize]
        LEA     R8,[R8 + 2*DLimbSize]

        LEA     RCX,[RCX - 1]
        JRCXZ   @MainTail
        JMP     @MainLoop

@MainTail:

// Here, code does not add index*CLimbSize and then use negative offsets, because that would take away
// the advantage of using 64 bit registers.
// Each block is separate, no fall through.

        LEA     RCX,[@MainJumps]
        JMP     [RCX + R9*TYPE Pointer]

        // Align jump table. Update if necessary!

        DB      $90,$90,$90,$90,$90

@MainJumps:

        DQ      @DoRestLoop
        DQ      @Main1
        DQ      @Main2
        DQ      @Main3

@Main3:

        MOV     RAX,[R10]
        SBB     RAX,[RDX]
        MOV     [R8],RAX

        MOV     EAX,[R10 + 2*CLimbSize]
        SBB     EAX,[RDX + 2*CLimbSize]
        MOV     [R8 + 2*CLimbSize],EAX

        LEA     R10,[R10 + 3*CLimbSize]
        LEA     RDX,[RDX + 3*CLimbSize]
        LEA     R8,[R8 + 3*CLimbSize]

        JMP     @DoRestLoop

@Main2:

        MOV     RAX,[R10]
        SBB     RAX,[RDX]
        MOV     [R8],RAX

        LEA     R10,[R10 + 2*CLimbSize]
        LEA     RDX,[RDX + 2*CLimbSize]
        LEA     R8,[R8 + 2*CLimbSize]

        JMP     @DoRestLoop

@Main1:

        MOV     EAX,[R10]
        SBB     EAX,[RDX]
        MOV     [R8],EAX

        LEA     R10,[R10 + CLimbSize]
        LEA     RDX,[RDX + CLimbSize]
        LEA     R8,[R8 + CLimbSize]

@DoRestLoop:

        SETC    AL                      // Save Carry Flag

        XOR     EDX,EDX

        POP     RCX
        MOV     R9D,ECX
        AND     R9D,CUnrollMask
        SHR     ECX,CUnrollShift

        ADD     AL,255                  // Restore Carry Flag.

        JECXZ   @RestLast3

@RestLoop:

        MOV     RAX,[R10]
        SBB     RAX,RDX
        MOV     [R8],RAX

        MOV     RAX,[R10 + DLimbSize]
        SBB     RAX,RDX
        MOV     [R8 + DLimbSize],RAX

        LEA     R10,[R10 + 2*DLimbSize]
        LEA     R8,[R8 + 2*DLimbSize]

        LEA     RCX,[RCX - 1]
        JRCXZ   @RestLast3
        JMP     @RestLoop

@RestLast3:

        LEA     RCX,[@RestJumps]
        JMP     [RCX + R9*TYPE Pointer]

        // If necessary, align second jump table with NOPs

        DB      $90,$90,$90,$90,$90,$90,$90

@RestJumps:

        DQ      @Exit
        DQ      @Rest1
        DQ      @Rest2
        DQ      @Rest3

@Rest3:

        MOV     RAX,[R10]
        SBB     RAX,RDX
        MOV     [R8],RAX

        MOV     EAX,[R10 + DLimbSize]
        SBB     EAX,EDX
        MOV     [R8 + DLimbSize],EAX

        JMP     @Exit

@Rest2:

        MOV     RAX,[R10]
        SBB     RAX,RDX
        MOV     [R8],RAX

        JMP     @Exit

@Rest1:

        MOV     EAX,[R10]
        SBB     EAX,EDX
        MOV     [R8],EAX

@Exit:

end;
{$ENDIF}

class procedure BigInteger.InternalSubtractPlain(Larger, Smaller, Result: PLimb; LSize, SSize: Integer);
{$IFDEF WIN32}
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX

        MOV     ESI,EAX                         // Left
        MOV     EDI,EDX                         // Right
        MOV     EBX,ECX                         // Result

        MOV     ECX,SSize
        MOV     EDX,LSize

        SUB     EDX,ECX
        PUSH    EDX
        XOR     EDX,EDX

        XOR     EAX,EAX

        MOV     EDX,ECX
        AND     EDX,CUnrollMask
        SHR     ECX,CUnrollShift

        CLC
        JE      @MainTail

@MainLoop:

        // Unrolled 4 times. More times will not improve speed anymore.

        MOV     EAX,[ESI]
        SBB     EAX,[EDI]
        MOV     [EBX],EAX

        MOV     EAX,[ESI + CLimbSize]
        SBB     EAX,[EDI + CLimbSize]
        MOV     [EBX + CLimbSize],EAX

        MOV     EAX,[ESI + 2*CLimbSize]
        SBB     EAX,[EDI + 2*CLimbSize]
        MOV     [EBX + 2*CLimbSize],EAX

        MOV     EAX,[ESI + 3*CLimbSize]
        SBB     EAX,[EDI + 3*CLimbSize]
        MOV     [EBX + 3*CLimbSize],EAX

        // Update pointers.

        LEA     ESI,[ESI + 4*CLimbSize]
        LEA     EDI,[EDI + 4*CLimbSize]
        LEA     EBX,[EBX + 4*CLimbSize]

        // Update counter and loop if required.

        DEC     ECX                             // Note: if INC/DEC must be emulated:
        JNE     @MainLoop                       //         LEA ECX,[ECX - 1]; JECXZ @MainTail; JMP @MainLoop

@MainTail:

        // Add index*CLimbSize so @MainX branches can fall through.

        LEA     ESI,[ESI + EDX*CLimbSize]
        LEA     EDI,[EDI + EDX*CLimbSize]
        LEA     EBX,[EBX + EDX*CLimbSize]

        // Indexed jump.

        LEA     ECX,[@JumpsMain]
        JMP     [ECX + EDX*TYPE Pointer]

        .ALIGN  16


@JumpsMain:

        DD      @DoRestLoop
        DD      @Main1
        DD      @Main2
        DD      @Main3

@Main3:

        MOV     EAX,[ESI - 3*CLimbSize]         // negative offset, because index*CLimbSize was already added.
        SBB     EAX,[EDI - 3*CLimbSize]
        MOV     [EBX - 3*CLimbSize],EAX

@Main2:

        MOV     EAX,[ESI - 2*CLimbSize]
        SBB     EAX,[EDI - 2*CLimbSize]
        MOV     [EBX - 2*CLimbSize],EAX

@Main1:

        MOV     EAX,[ESI - CLimbSize]
        SBB     EAX,[EDI - CLimbSize]
        MOV     [EBX - CLimbSize],EAX

@DoRestLoop:

        SETC    AL                      // Save Carry Flag

        XOR     EDI,EDI

        POP     ECX
        MOV     EDX,ECX
        AND     EDX,CUnrollMask
        SHR     ECX,CUnrollShift

        ADD     AL,255                  // Restore Carry Flag.

        INC     ECX
        DEC     ECX
        JE      @RestLast3              // JECXZ is slower than INC/DEC/JE

@RestLoop:

        MOV     EAX,[ESI]
        SBB     EAX,EDI
        MOV     [EBX],EAX

        MOV     EAX,[ESI + CLimbSize]
        SBB     EAX,EDI
        MOV     [EBX + CLimbSize],EAX

        MOV     EAX,[ESI + 2*CLimbSize]
        SBB     EAX,EDI
        MOV     [EBX + 2*CLimbSize],EAX

        MOV     EAX,[ESI + 3*CLimbSize]
        SBB     EAX,EDI
        MOV     [EBX + 3*CLimbSize],EAX

        LEA     ESI,[ESI + 4*CLimbSize] // LEA does not affect the flags, so carry will not be changed.
        LEA     EBX,[EBX + 4*CLimbSize]

        DEC     ECX                     // DEC does not affect carry flag, but causes partial-flags stall
        JNE     @RestLoop               //   (e.g. when using SBB) on older CPUs.

@RestLast3:

        LEA     ESI,[ESI + EDX*CLimbSize]
        LEA     EBX,[EBX + EDX*CLimbSize]

        LEA     ECX,[@RestJumps]
        JMP     [ECX + EDX*TYPE Pointer]

        .ALIGN  16

@RestJumps:

        DD      @Exit
        DD      @Rest1
        DD      @Rest2
        DD      @Rest3

@Rest3:

        MOV     EAX,[ESI - 3*CLimbSize]
        SBB     EAX,EDI
        MOV     [EBX - 3*CLimbSize],EAX

@Rest2:

        MOV     EAX,[ESI - 2*CLimbSize]
        SBB     EAX,EDI
        MOV     [EBX - 2*CLimbSize],EAX

@Rest1:

        MOV     EAX,[ESI - CLimbSize]
        SBB     EAX,EDI
        MOV     [EBX - CLimbSize],EAX

@Exit:

        POP     EBX
        POP     EDI
        POP     ESI
end;
{$ELSE WIN32/WIN64}
asm
        MOV     R10,RCX         // in emulating code, ECX must be used as loop counter! So do not exchange
        MOV     ECX,SSize       //   RCX and R10 in the editor.

        // R10 = Left, RDX = Right, R8 = Result, R9D = LSize, ECX = SSize

        CMP     R9D,ECX
        JAE     @SkipSwap
        XCHG    ECX,R9D
        XCHG    R10,RDX

@SkipSwap:

        SUB     R9D,ECX
        PUSH    R9

        MOV     R9D,ECX
        AND     R9D,CUnrollMask
        SHR     ECX,CUnrollShift

        CLC
        JE      @MainTail               // ECX = 0, so fewer than 3 limbs to be processed in main

@MainLoop:

        MOV     RAX,[R10]               // Add two limbs at once, taking advantage of 64 bit registers.
        SBB     RAX,[RDX]
        MOV     [R8],RAX

        MOV     RAX,[R10 + DLimbSize]   // And next two limbs too.
        SBB     RAX,[RDX + DLimbSize]
        MOV     [R8 + DLimbSize],RAX

        LEA     R10,[R10 + 2*DLimbSize]
        LEA     RDX,[RDX + 2*DLimbSize]
        LEA     R8,[R8 + 2*DLimbSize]

        DEC     ECX                     // if INC/DEC must be emulated:
                                        //   LEA ECX,[ECX - 1]; JECXZ @MainTail; JMP @MainLoop
        JNE     @MainLoop

@MainTail:

// Here, code does not add index*CLimbSize and then use negative offsets, because that would take away the
// advantage of using 64 bit registers.
// Each block is separate, no fall through.

        LEA     RCX,[@MainJumps]
        JMP     [RCX + R9*TYPE Pointer]

        .ALIGN  16

@MainJumps:

        DQ      @DoRestLoop
        DQ      @Main1
        DQ      @Main2
        DQ      @Main3

@Main3:

        MOV     RAX,[R10]
        SBB     RAX,[RDX]
        MOV     [R8],RAX

        MOV     EAX,[R10 + DLimbSize]
        SBB     EAX,[RDX + DLimbSize]
        MOV     [R8 + 2*CLimbSize],EAX

        LEA     R10,[R10 + 3*CLimbSize]
        LEA     RDX,[RDX + 3*CLimbSize]
        LEA     R8,[R8 + 3*CLimbSize]

        JMP     @DoRestLoop

@Main2:

        MOV     RAX,[R10]
        SBB     RAX,[RDX]
        MOV     [R8],RAX

        LEA     R10,[R10 + DLimbSize]
        LEA     RDX,[RDX + DLimbSize]
        LEA     R8,[R8 + DLimbSize]

        JMP     @DoRestLoop

@Main1:

        MOV     EAX,[R10]
        SBB     EAX,[RDX]
        MOV     [R8],EAX

        LEA     R10,[R10 + CLimbSize]
        LEA     RDX,[RDX + CLimbSize]
        LEA     R8,[R8 + CLimbSize]

@DoRestLoop:

        SETC    AL                      // Save Carry Flag

        XOR     EDX,EDX

        POP     RCX
        MOV     R9D,ECX
        AND     R9D,CUnrollMask
        SHR     ECX,CUnrollShift

        ADD     AL,255                  // Restore Carry Flag.

        INC     ECX
        DEC     ECX
        JE      @RestLast3              // JECXZ is slower than INC/DEC/JE

@RestLoop:

        MOV     RAX,[R10]               // Do two limbs at once.
        SBB     RAX,RDX
        MOV     [R8],RAX

        MOV     RAX,[R10 + DLimbSize] // And the next two limbs.
        SBB     RAX,RDX
        MOV     [R8 + DLimbSize],RAX

        LEA     R10,[R10 + 2*DLimbSize]
        LEA     R8,[R8 + 2*DLimbSize]

        DEC     ECX
        JNE     @RestLoop

@RestLast3:

        LEA     RCX,[@RestJumps]
        JMP     [RCX + R9*TYPE Pointer]

        // If necessary, align second jump table with NOPs

@RestJumps:

        DQ      @Exit
        DQ      @Rest1
        DQ      @Rest2
        DQ      @Rest3

@Rest3:

        MOV     RAX,[R10]
        SBB     RAX,RDX
        MOV     [R8],RAX

        MOV     EAX,[R10 + 2*CLimbSize]
        SBB     EAX,EDX
        MOV     [R8 + 2*CLimbSize],EAX

        LEA     R8,[R8 + 3*CLimbSize]

        JMP     @Exit

@Rest2:

        MOV     RAX,[R10]
        SBB     RAX,RDX
        MOV     [R8],RAX

        LEA     R8,[R8 + 2*CLimbSize]

        JMP     @Exit

@Rest1:

        MOV     EAX,[R10]
        SBB     EAX,EDX
        MOV     [R8],EAX

        LEA     R8,[R8 + CLimbSize]

@Exit:

end;
{$ENDIF !WIN32}
{$ENDIF !PUREPASCAL}

{$IFDEF PUREPASCAL}
class procedure BigInteger.InternalSubtractPurePascal(Larger, Smaller, Result: PLimb; LSize, SSize: Integer);
{$IFDEF CPU64BITS}
var
  LDiff: NativeInt;
  LTail, LCount: Integer;
begin
  Dec(LSize, SSize);

  LTail := SSize and CUnrollMask;
  LCount := SSize shr CUnrollShift;
  LDiff := 0;

  while LCount > 0 do
  begin
    LDiff := Int64(Larger[0]) - Smaller[0] + Int32(LDiff shr 32);
    Result[0] := TLimb(LDiff);

    LDiff := Int64(Larger[1]) - Smaller[1] + Int32(LDiff shr 32);
    Result[1] := TLimb(LDiff);

    LDiff := Int64(Larger[2]) - Smaller[2] + Int32(LDiff shr 32);
    Result[2] := TLimb(LDiff);

    LDiff := Int64(Larger[3]) - Smaller[3] + Int32(LDiff shr 32);
    Result[3] := TLimb(LDiff);

    Inc(Larger, CUnrollIncrement);
    Inc(Smaller, CUnrollIncrement);
    Inc(Result, CUnrollIncrement);
    Dec(LCount);
  end;

  while LTail > 0 do
  begin
    LDiff := Int64(Larger[0]) - Smaller[0] + Int32(LDiff shr 32);
    Result[0] := TLimb(LDiff);

    Inc(Larger);
    Inc(Smaller);
    Inc(Result);
    Dec(LTail);
  end;

  LTail := LSize and CUnrollMask;
  LCount := LSize shr CUnrollShift;

  while LCount > 0 do
  begin
    LDiff := Int64(Larger[0]) + Int32(LDiff shr 32);
    Result[0] := TLimb(LDiff);

    LDiff := Int64(Larger[1]) + Int32(LDiff shr 32);
    Result[1] := TLimb(LDiff);

    LDiff := Int64(Larger[2]) + Int32(LDiff shr 32);
    Result[2] := TLimb(LDiff);

    LDiff := Int64(Larger[3]) + Int32(LDiff shr 32);
    Result[3] := TLimb(LDiff);

    Inc(Larger, CUnrollIncrement);
    Inc(Result, CUnrollIncrement);
    Dec(LCount);
  end;

  while LTail > 0 do
  begin
    LDiff := Int64(Larger[0]) + Int32(LDiff shr 32);
    Result[0] := TLimb(LDiff);

    Inc(Larger);
    Inc(Result);
    Dec(LTail);
  end;
end;
{$ELSE}
var
  LDiff: NativeInt;
  LCount, LTail: Integer;
begin
  Dec(LSize, SSize);
  LDiff := 0;

  LTail := SSize and CUnrollMask;
  LCount := SSize shr CUnrollShift;

  while LCount > 0 do
  begin
    LDiff := Int32(PUInt16(Larger)[0]) - PUInt16(Smaller)[0] + Int16(LDiff shr 16);
    PUInt16(Result)[0] := UInt16(LDiff);

    LDiff := Int32(PUInt16(Larger)[1]) - PUInt16(Smaller)[1] + Int16(LDiff shr 16);
    PUInt16(Result)[1] := UInt16(LDiff);

    LDiff := Int32(PUInt16(Larger)[2]) - PUInt16(Smaller)[2] + Int16(LDiff shr 16);
    PUInt16(Result)[2] := UInt16(LDiff);

    LDiff := Int32(PUInt16(Larger)[3]) - PUInt16(Smaller)[3] + Int16(LDiff shr 16);
    PUInt16(Result)[3] := UInt16(LDiff);

    LDiff := Int32(PUInt16(Larger)[4]) - PUInt16(Smaller)[4] + Int16(LDiff shr 16);
    PUInt16(Result)[4] := UInt16(LDiff);

    LDiff := Int32(PUInt16(Larger)[5]) - PUInt16(Smaller)[5] + Int16(LDiff shr 16);
    PUInt16(Result)[5] := UInt16(LDiff);

    LDiff := Int32(PUInt16(Larger)[6]) - PUInt16(Smaller)[6] + Int16(LDiff shr 16);
    PUInt16(Result)[6] := UInt16(LDiff);

    LDiff := Int32(PUInt16(Larger)[7]) - PUInt16(Smaller)[7] + Int16(LDiff shr 16);
    PUInt16(Result)[7] := UInt16(LDiff);

    Inc(Larger, CUnrollIncrement);
    Inc(Smaller, CUnrollIncrement);
    Inc(Result, CUnrollIncrement);
    Dec(LCount);
  end;

  while LTail > 0 do
  begin
    LDiff := Int32(PUInt16(Larger)[0]) - PUInt16(Smaller)[0] + Int16(LDiff shr 16);
    PUInt16(Result)[0] := UInt16(LDiff);

    LDiff := Int32(PUInt16(Larger)[1]) - PUInt16(Smaller)[1] + Int16(LDiff shr 16);
    PUInt16(Result)[1] := UInt16(LDiff);

    Inc(Larger);
    Inc(Smaller);
    Inc(Result);
    Dec(LTail);
  end;

  LTail := LSize and CUnrollMask;
  LCount := LSize shr CUnrollShift;

  while LCount > 0 do
  begin
    LDiff := Int32(PUInt16(Larger)[0]) + Int16(LDiff shr 16);
    PUInt16(Result)[0] := UInt16(LDiff);

    LDiff := Int32(PUInt16(Larger)[1]) + Int16(LDiff shr 16);
    PUInt16(Result)[1] := UInt16(LDiff);

    LDiff := Int32(PUInt16(Larger)[2]) + Int16(LDiff shr 16);
    PUInt16(Result)[2] := UInt16(LDiff);

    LDiff := Int32(PUInt16(Larger)[3]) + Int16(LDiff shr 16);
    PUInt16(Result)[3] := UInt16(LDiff);

    LDiff := Int32(PUInt16(Larger)[4]) + Int16(LDiff shr 16);
    PUInt16(Result)[4] := UInt16(LDiff);

    LDiff := Int32(PUInt16(Larger)[5]) + Int16(LDiff shr 16);
    PUInt16(Result)[5] := UInt16(LDiff);

    LDiff := Int32(PUInt16(Larger)[6]) + Int16(LDiff shr 16);
    PUInt16(Result)[6] := UInt16(LDiff);

    LDiff := Int32(PUInt16(Larger)[7]) + Int16(LDiff shr 16);
    PUInt16(Result)[7] := UInt16(LDiff);

    Inc(Larger, CUnrollIncrement);
    Inc(Result, CUnrollIncrement);
    Dec(LCount);
  end;

  while LTail > 0 do
  begin
    LDiff := Int32(PUInt16(Larger)[0]) + Int16(LDiff shr 16);
    PUInt16(Result)[0] := UInt16(LDiff);

    LDiff := Int32(PUInt16(Larger)[1]) + Int16(LDiff shr 16);
    PUInt16(Result)[1] := UInt16(LDiff);

    Inc(Larger);
    Inc(Result);
    Dec(LTail);
  end;
end;
{$ENDIF}
{$ENDIF}

function BigInteger.IsZero: Boolean;
begin
  Result := FData = nil;
end;

class procedure BigInteger.ShiftLeft(const Value: BigInteger; Shift: Integer; var Result: BigInteger);
var
  LimbShift: Integer;
  LSign: Integer;
begin
  if Value.FData = nil then
  begin
    Result := Zero;
    Exit;
  end;
  LSign := SignBitOf(Value.FSize);
  LimbShift := Shift div CLimbBits;
  Shift := Shift mod CLimbBits;
  if Shift > 0 then
  begin
    Result.MakeSize((Value.FSize and SizeMask) + LimbShift + 1);
    InternalShiftLeft(PLimb(Value.FData), PLimb(Result.FData) + LimbShift, Shift, (Value.FSize and SizeMask));
  end
  else
  begin
    Result.MakeSize((Value.FSize and SizeMask) + LimbShift);
    CopyLimbs(PLimb(Value.FData), PLimb(Result.FData) + LimbShift, (Value.FSize and SizeMask));
  end;
  Result.FSize := (Result.FSize and SizeMask) or Integer(LSign);
  Result.Compact;
end;

class function BigInteger.ShiftLeft(const Value: BigInteger; Shift: Integer): BigInteger;
begin
  ShiftLeft(Value, Shift, Result);
end;

class operator BigInteger.LeftShift(const Value: BigInteger; Shift: Integer): BigInteger;
begin
  ShiftLeft(Value, Shift, Result);
end;

class operator BigInteger.LessThan(const Left, Right: BigInteger): Boolean;
begin
  Result := Compare(Left, Right) < 0;
end;

class operator BigInteger.LessThanOrEqual(const Left, Right: BigInteger): Boolean;
begin
  Result := Compare(left, Right) <= 0;
end;

function BigInteger.Ln: Double;
begin
  Result := Ln(Self);
end;

function BigInteger.BitLength: Integer;
begin
  if Self.FData = nil then
    Result := 0
  else
  begin
    Result := CLimbBits * (Size - 1) + {Velthuis.Numerics.}VBitLength(FData[Size - 1]);

    // IsPowerOfTwo is expensive, but probably less expensive than a copy and
    // subsequent decrement, like in BitCount.
    if (FSize < 0) and Self.IsPowerOfTwo then
      Dec(Result);
  end;
end;

function BigInteger.BitCount: Integer;
var
  Mag: TMagnitude;
  I: Integer;
begin
  if FData = nil then
    Exit(0);

  if FSize > 0 then
    Mag := FData
  else
  begin
    Mag := Copy(FData);
    InternalDecrement(PLimb(Mag), FSize and SizeMask);
  end;

  Result := 0;
  for I := 0 to Size - 1 do
    Result := Result + {Velthuis.Numerics.}VBitCount(Mag[I]);
end;

// http://stackoverflow.com/a/7982137/95954
// Or: ln(a) = ln(a / 2^k) + k * ln(2)
class function BigInteger.Ln(const Value: BigInteger): Double;
var
  ExtraBits: Integer;
  NewInt: BigInteger;
begin
  if Value.IsNegative then
    Exit(System.Math.NaN)
  else if Value.IsZero then
    Exit(System.Math.NegInfinity);
  ExtraBits := Value.BitLength - 1022;
  if ExtraBits > 0 then
    NewInt := Value shr ExtraBits
  else
    NewInt := Value;
  Result := System.Ln(NewInt.AsDouble);
  if ExtraBits > 0 then
    Result := Result + ExtraBits * FLog2;
end;

class function BigInteger.Log(const Value: BigInteger; Base: Double): Double;
begin
  Result := BigInteger.Ln(Value) / System.Ln(Base);
end;

class function BigInteger.Log10(const Value: BigInteger): Double;
begin
  Result := Log(Value, 10.0);
end;

class function BigInteger.Log2(const Value: BigInteger): Double;
begin
  Result := Log(Value, 2.0);
end;

function BigInteger.Log(Base: Double): Double;
begin
  Result := Log(Self, Base);
end;

function BigInteger.Log10: Double;
begin
  Result := Log(Self, 10.0);
end;

function BigInteger.Log2: Double;
begin
  Result := Log(Self, 2.0);
end;

// https://stackoverflow.com/a/7982137/95954
class function BigInteger.Exp(const b: Double): BigInteger;
var
  bc, b2, c: Double;
  t: Integer;
begin
  if IsNan(b) or IsInfinite(b) then
    Error(ecInvalidArgFloat, ['Double']);
  bc := 680.0;
  if b < bc then
    Exit(BigInteger(System.Exp(b)));
  // I think this can be simplified:
  c := b - bc;
  t := System.Math.Ceil(c / FLog2);
  c := t * FLog2;
  b2 := b - c;
  Result := BigInteger(System.Exp(b2)) shl t;
end;

class operator BigInteger.LogicalNot(const Value: BigInteger): BigInteger;
begin
  Result := Value;
  Inc(Result);
  if Result.FSize <> 0 then
    Result.FSize := Result.FSize xor SignMask;
  Result.Compact;
end;

function BigInteger.LowestSetBit: Integer;
var
  I: Integer;
begin
  if FData = nil then
    Exit(-1);
  I := 0;
  Result := 0;
  while FData[I] = 0 do
  begin
    Inc(Result, CLimbBits);
    Inc(I);
  end;
  Inc(Result, VNumberOfTrailingZeros(FData[I]));
end;

class function BigInteger.Max(const Left, Right: BigInteger): BigInteger;
begin
  if Left > Right then
    ShallowCopy(Left, Result)
  else
    ShallowCopy(Right, Result);
end;

class function BigInteger.Min(const Left, Right: BigInteger): BigInteger;
begin
  if Left < Right then
    ShallowCopy(Left, Result)
  else
    ShallowCopy(Right, Result);
end;

// https://www.di-mgt.com.au/euclidean.html#code-modinv
class function BigInteger.ModInverse(const Value, Modulus: BigInteger): BigInteger;
var
  u1, u3, v1, v3, temp1, temp3, q: BigInteger;
  iter: Integer;
begin
  // Step X1. Initialise
  u1 := One;
  u3 := Abs(Value);
  v1 := Zero;
  v3 := Abs(Modulus);
  // X mod 1 is nonsense (always 0), but it might still be passed.
  if (Compare(v3, One) = 0) or Modulus.IsZero then
    Error(ecNoInverse, []);
  // Remember odd/even iterations
  iter := 0;
  // Step X2. Loop while v3 <> 0
  while not v3.IsZero do
  begin
    // Step X3. Divide and Subtract
    DivMod(u3, v3, q, temp3);
    temp1 := Add(u1, BigInteger.Multiply(q, v1));
    // Swap
    u1 := v1;
    v1 := temp1;
    u3 := v3;
    v3 := temp3;
    Inc(iter);
  end;
  // Ensure u3, i.e. gcd(Value, Modulus) = 1
  if u3 <> One then
    Error(ecNoInverse, []);
  if Odd(iter) then
    Result := Subtract(Abs(Modulus), u1)
  else
    Result := u1;
  if Value < 0 then
    Result := -Result;
end;

// http://stackoverflow.com/questions/8496182/calculating-powa-b-mod-n
class function BigInteger.ModPow(const ABase, AExponent, AModulus: BigInteger): BigInteger;
var
  Base: BigInteger;
  Exp: BigInteger;
begin
  if not AModulus.IsPositive  then
    Error(ecDivByZero, []);
  if AModulus.IsOne then
    Exit(BigInteger.Zero);
  Result := BigInteger.One;
  Exp := AExponent;
  Base := ABase mod AModulus;
  while Exp > Zero do
  begin
    if not Exp.IsEven then
      Result := (Result * Base) mod AModulus;
    Exp := Exp shr 1;
    Base := Sqr(Base) mod AModulus;
  end;
end;

class operator BigInteger.Modulus(const Left, Right: BigInteger): BigInteger;
begin
  Result := Remainder(Left, Right);
end;

class operator BigInteger.Modulus(const Left: BigInteger; Right: UInt32): BigInteger;
begin
  Result := Remainder(Left, Right);
end;

class operator BigInteger.Modulus(const Left: BigInteger; Right: UInt16): BigInteger;
begin
  Result := Remainder(Left, Right);
end;

// Note: this can only be used to multiply by a base and add a digit, i.e. ADigit must be < ABase!
class procedure BigInteger.InternalMultiplyAndAdd16(Value: PLimb; ABase, ADigit: Word; var Size: Integer);
{$IFDEF PUREPASCAL}
type
  TUInt32 = packed record
    Lo, Hi: UInt16;
  end;
var
  I: Integer;
  LProduct: UInt32;
  LSize: Integer;
begin
  LSize := Size shl 1;
  I := 0;
  LProduct := 0;
  while I < LSize do
  begin
    LProduct := UInt32(PUInt16(Value)[I]) * ABase + TUInt32(LProduct).Hi;
    PUInt16(Value)[I] := TUInt32(LProduct).Lo;
    Inc(I);
  end;
  if TUInt32(LProduct).Hi <> 0 then
  begin
    PUInt16(Value)[I] := TUInt32(LProduct).Hi;
    Inc(Size);
  end;
  if ADigit > 0 then
  begin
    Inc(Value[0], ADigit);
    if Size = 0 then
      Size := 1;
  end;
end;
{$ELSEIF DEFINED(Win32)}
var
  LValue: PLimb;
  LDigit: UInt16;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX

        MOV     ESI,EAX
        MOV     LValue,EAX
        MOVZX   EDI,DX
        MOV     LDigit,CX
        MOV     ECX,Size
        MOV     ECX,[ECX]
        JECXZ   @DoAdd
        XOR     EBX,EBX

@MultLoop:

        MOV     EAX,[ESI]
        MUL     EAX,EDI
        ADD     EAX,EBX
        ADC     EDX,0
        MOV     [ESI],EAX
        MOV     EBX,EDX
        LEA     ESI,[ESI + CLimbSize]
        LEA     ECX,[ECX - 1]
        JECXZ   @CheckLastLimb
        JMP     @MultLoop

@CheckLastLimb:

        OR      EBX,EBX
        JE      @DoAdd
        MOV     [ESI],EBX               // Carry not zero, so increment size and store carry
        MOV     ECX,Size
        INC     DWORD PTR [ECX]

@DoAdd:

        MOVZX   EAX,LDigit
        OR      EAX,EAX
        JZ      @Exit                   // Skip if ADigit is 0 anyway.
        MOV     ECX,Size
        CMP     DWORD PTR [ECX],0       // If Size = 0 and ADigit <> 0, must add 1 to size
        JNZ     @SkipInc
        INC     DWORD PTR [ECX]

@SkipInc:

        MOV     ESI,LValue
        ADD     [ESI],EAX               // Note that allocated size is always > 1.

@Exit:

        POP     EBX
        POP     EDI
        POP     ESI
end;
{$ELSE WIN64}
asm
        .PUSHNV RSI

        PUSH    RCX                     // Save Value
        MOV     RSI,RCX                 // RSI = Value
        MOV     R10D,EDX                // R10D = ABase
        XOR     R11D,R11D               // Multiplication "carry"
        MOV     RCX,R9                  // Size
        MOV     ECX,[RCX]               // Size^
        JECXZ   @DoAdd

@MultLoop:

        MOV     EAX,[RSI]
        MUL     EAX,R10D
        ADD     EAX,R11D
        ADC     EDX,0
        MOV     [RSI],EAX
        MOV     R11D,EDX
        LEA     RSI,[RSI + CLimbSize]
        LEA     ECX,[ECX - 1]
        JECXZ   @CheckLastLimb
        JMP     @MultLoop

@CheckLastLimb:

        OR      EDX,EDX
        JE      @DoAdd
        MOV     [RSI],EDX
        INC     DWORD PTR [R9]

@DoAdd:

        POP     RCX                     // Restore Value
        OR      R8D,R8D                 // If ADigit is 0 then we are finished
        JZ      @Exit
        CMP     [R9],0                  // If Size = 0, and ADigit isn't, then increment size
        JNE     @SkipInc
        INC     DWORD PTR [R9]

@SkipInc:

        ADD     [RCX],R8D               // Add ADigit

@Exit:

end;
{$IFEND}


{ TODO: It dawned to me that if you multiply by Base, and then add a number that is < Base, there can *never* be a
        carry. Even $FFFFFFFF x 36, the lowest limb will be $FFFFFFFF - 35 ($FFFFFFDC), so adding 35 ($23) can not
        cause a carry. Tried that with other multiplicators too.

        This can be applied to MultiplyAndAdd32 too.

        So just call it InternalMultiplyBaseAndAdd. Addend must be < Base, and there will never be a carry.
        This means it is possible to pre-allocate and pass the size. This routine must update size if it sets the top
        limb. So there can be a Size and it must be a var parameter. Just add the addend to the lowest limb. No need
        to carry.
}
class procedure BigInteger.InternalMultiply16(const Left: TMagnitude; var Result: TMagnitude; LSize: Integer; Right: Word);
{$IF DEFINED(PUREPASCAL)}
type
  TUInt32 = packed record
    Lo, Hi: UInt16;
  end;
var
  I: Integer;
  LProduct: UInt32;
  LHi16: UInt16;
begin
  LSize := LSize * 2;
  LHi16 := 0;
  I := 0;
  while I < LSize do
  begin
    LProduct := UInt32(PUInt16(Left)[I]) * Right + LHi16;
    PUInt16(Result)[I] := TUInt32(LProduct).Lo;
    LHi16 := TUInt32(LProduct).Hi;
    Inc(I);
  end;
  if LHi16 <> 0 then
  begin
    PUInt16(Result)[I] := LHi16;
    // var parameter Size := I;
    // Size should be the fifth parameter, so it can easily be set from 64 bit code.
  end;
end;
{$ELSEIF DEFINED(WIN32)}
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        JECXZ   @Exit
        MOV     ESI,EAX
        MOV     EDI,[EDX]
        XOR     EBX,EBX
        CMP     Right,0
        JE      @Exit

@MultLoop:

        MOV     EAX,[ESI]
        MOVZX   EDX,Right
        MUL     EDX
        ADD     EAX,EBX
        ADC     EDX,0
        MOV     EBX,EDX
        MOV     [EDI],EAX
        LEA     ESI,[ESI + CLimbSize]
        LEA     EDI,[EDI + CLimbSize]
        LEA     ECX,[ECX - 1]
        JECXZ   @EndMultLoop
        JMP     @MultLoop

@EndMultLoop:

        MOV     [EDI],EBX

@Exit:

        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ELSE WIN64}
asm
        .PUSHNV RBX
        .PUSHNV RDI
        .PUSHNV RSI

        OR      R8D,R8D
        JE      @Exit
        OR      R9D,R9D
        JE      @Exit
        MOV     R11,[RDX]               // R11 = SaveResult
        MOV     RDI,R11                 // RDI = Result
        MOV     RSI,RCX                 // RSI = Left
        MOV     ECX,R8D                 // ECX = MSize
        XOR     EBX,EBX                 // EBX = Carry

@MultLoop:

        MOV     EAX,[RSI]
        MUL     EAX,R9D                 // Unusual syntax, but XE2 otherwise generates MUL R9 instead of MUL R9D
        ADD     EAX,EBX
        ADC     EDX,0
        MOV     [RDI],EAX
        MOV     EBX,EDX
        LEA     RSI,[RSI + CLimbSize]
        LEA     RDI,[RDI + CLimbSize]
        LEA     ECX,[ECX - 1]
        JECXZ   @EndMultLoop
        JMP     @MultLoop

@EndMultLoop:

        MOV     [RDI],EBX

@Exit:
end;
{$IFEND}

class operator BigInteger.Multiply(const Left: BigInteger; Right: Word): BigInteger;
var
  ResData: TMagnitude;
  ResSize: Integer;
begin
  if (Right = 0) or ((Left.FSize and SizeMask) = 0) then
    Exit(Zero);
  ResSize := (Left.FSize and SizeMask) + 2;
  SetLength(ResData, ResSize);
  InternalMultiply16(Left.FData, ResData, (Left.FSize and SizeMask), Right);
  Assert(Result.FData <> ResData);
  Result.FData := ResData;
  Result.FSize := (Left.FSize and SignMask) or ResSize;
  Result.Compact;
end;

class operator BigInteger.Multiply(Left: Word; const Right: BigInteger): BigInteger;
begin
  Result := Multiply(Right, Left);
end;

class procedure BigInteger.MultiplyKaratsuba(const Left, Right: BigInteger; var Result: BigInteger);
var
  k, LSign: Integer;
  z0, z1, z2: BigInteger;
  x, y: TArray<BigInteger>;
  Shift: Integer;
begin
  if ((Left.FSize and SizeMask) < KaratsubaThreshold) or ((Right.FSize and SizeMask) < KaratsubaThreshold) then
  begin
    MultiplyBaseCase(Left, Right, Result);
    Exit;
  end;

  //////////////////////////////////////////////////////////////////////////////////////////////////
  ///  This is a so called divide and conquer algorithm, solving a big task by dividing it up    ///
  ///  into easier (and hopefully faster, in total) smaller tasks.                               ///
  ///                                                                                            ///
  ///  Let's treat a BigInteger as a polynomial, i.e. x = x1 * B + x0, where B is chosen thus,   ///
  ///  that the top and the low part of the BigInteger are almost the same in size.              ///
  ///  The result R of the multiplication of two such polynomials can be seen as:                ///
  ///                                                                                            ///
  ///  R = (x1 * B + x0) * (y1 * B + y0) = x1 * y1 * B^2 + (x1 * y0 + x0 * y1) * B + x0 * y0     ///
  ///                                                                                            ///
  ///  say, z0 = x0 * y0                                                                         ///
  ///       z1 = x1 * y0 + x0 * y1                                                               ///
  ///       z2 = x1 * y1                                                                         ///
  ///                                                                                            ///
  ///  then                                                                                      ///
  ///  R = z2 * B^2 + z1 * B + z0                                                                ///
  ///                                                                                            ///
  ///  Karatsuba noted that:                                                                     ///
  ///                                                                                            ///
  ///  (x1 + x0) * (y1 + y0) = z2 + z1 + z0, so z1 = (x1 + x0) * (y1 + y0) - (z2 + z0)           ///
  ///                                                                                            ///
  ///  That reduced four multiplications and a few additions to three multiplications, a few     ///
  ///  additions and a subtraction. Surely the parts will be multilimb, but this is called       ///
  ///  recursively down until the sizes are under a threshold, and then simple base case         ///
  ///  (a.k.a. "schoolbook") multiplication is performed.                                        ///
  //////////////////////////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////////////////////////
  ///  Note: it may look tempting to use pointers into the original operands, to use one large   ///
  ///  buffer for all results, and to use InternalMultiply directly, but remember that           ///
  ///  InternalMultiply performs a basecase multiplication and it does NOT resurse into a        ///
  ///  deeper level of MultiplyKaratsuba, so after one level, the advantage gained by reducing   ///
  ///  the number of multiplications would be minimal.                                           ///
  ///                                                                                            ///
  ///  There is an overhead caused by using complete BigIntegers, but it is not as high as it    ///
  ///  may look.                                                                                 ///
  //////////////////////////////////////////////////////////////////////////////////////////////////

  LSign := (Left.FSize xor Right.FSize) and SignMask;

  k := (IntMax(Left.FSize and SizeMask, Right.FSize and SizeMask) + 1) shr 1;

  x := Left.Split(k, 2);
  y := Right.Split(k, 2);

  // Recursion further reduces the number of multiplications!
  MultiplyKaratsuba(x[1], y[1], z2);
  MultiplyKaratsuba(x[0], y[0], z0);
  MultiplyKaratsuba(x[1] - x[0], y[0] - y[1], z1);
  Add(z1, z2 + z0, z1);

  Shift := k * CLimbBits;

  Result := z0 + ((z1 + z2 shl Shift) shl Shift);
  Result.FSize := (Result.FSize and SizeMask) or LSign;
end;

// Used by Karatsuba, Toom-Cook and Burnikel-Ziegler algorithms.
// Splits Self into BlockCount pieces of (at most) BlockSize limbs, starting with the least significant part.
function BigInteger.Split(BlockSize, BlockCount: Integer): TArray<BigInteger>;
var
  I: Integer;
begin
  SetLength(Result, BlockCount);
  for I := 0 to BlockCount - 1 do
  begin
    if (Self.FSize and BigInteger.SizeMask) > I * BlockSize then
    begin
      Result[I].MakeSize(IntMin(BlockSize, (Self.FSize and SizeMask) - I * BlockSize));
      CopyLimbs(PLimb(Self.FData) + I * BlockSize, PLimb(Result[I].FData), IntMin(BlockSize, (Self.FSize and SizeMask) - I * BlockSize));
      Result[I].Compact;
    end
    else
      ShallowCopy(Zero, Result[I]);
  end;
end;

{$IFNDEF PUREPASCAL}
class procedure BigInteger.InternalDivideBy3(Value, Result: PLimb; ASize: Integer);
const
  MultConst = $AAAAAAAB;
  MultConst2 = $55555556;
{$IFDEF WIN32}
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX

        MOV     ESI,EAX                 // Value
        MOV     EDI,EDX                 // Result
        XOR     EBX,EBX                 // borrow

@Loop:

        MOV     EAX,[ESI]
        SUB     EAX,EBX
        SETC    BL

        MOV     EDX,MultConst
        MUL     EAX,EDX
        MOV     [EDI],EAX

        CMP     EAX,MultConst2
        JB      @SkipInc
        INC     EBX
        CMP     EAX,MultConst
        JB      @SkipInc
        INC     EBX

@SkipInc:

        LEA     ESI,[ESI + CLimbSize]
        LEA     EDI,[EDI + CLimbSize]
        DEC     ECX
        JNE     @Loop

@Exit:

        POP     EBX
        POP     EDI
        POP     ESI
end;
{$ELSE WIN64}
asm
        XOR     R9D,R9D                 // borrow
        MOV     R10,RDX                 // Result
        MOV     R11D,MultConst

@Loop:

        MOV     EAX,[RCX]
        SUB     EAX,R9D
        SETC    R9B

        MUL     EAX,R11D                // MultConst
        MOV     [R10],EAX

        CMP     EAX,MultConst2
        JB      @SkipInc
        INC     R9D
        CMP     EAX,R11D                // MultConst
        JB      @SkipInc
        INC     R9D

@SkipInc:

        LEA     RCX,[RCX + CLimbSize]
        LEA     R10,[R10 + CLimbSize]
        DEC     R8D
        JNE     @Loop
end;
{$ENDIF WIN64}
{$ENDIF !PUREPASCAL}

// Only works if it is known that there is no remainder and A is positive.
class function BigInteger.DivideBy3Exactly(const A: BigInteger): BigInteger;
const
  ModInverse3 = $AAAAAAAB; // Modular inverse of 3 modulo $100000000.
  ModInverse3t2 = $55555556; // ModInverse3 / 2
{$IFDEF PUREPASCAL}
var
  i: Integer;
  ai, w, qi, borrow: Int64;
begin
  if A.FData = nil then
    Exit(Zero);

  Result.MakeSize(A.FSize and SizeMask);
  borrow := 0;
  for i := 0 to (A.FSize and SizeMask) - 1 do
  begin
    ai := A.FData[i];
    w := ai - borrow;
    if borrow > ai then
      borrow := 1
    else
      borrow := 0;

    qi := (w * ModInverse3) and $FFFFFFFF;
    Result.FData[i] := UInt32(qi);

    if qi >= ModInverse3t2 then
    begin
      Inc(borrow);
      if qi >= ModInverse3 then
        Inc(borrow);
    end;
  end;

  Result.Compact;
end;
{$ELSE !PUREPASCAL}
begin
  if A.FData = nil then
  begin
    ShallowCopy(Zero, Result);
    Exit;
  end;

  Result.MakeSize(A.FSize and SizeMask);
  InternalDivideBy3(PLimb(A.FData), PLimb(Result.FData), A.FSize and SizeMask);
  Result.Compact;
end;
{$ENDIF !PUREPASCAL}

class function BigInteger.MultiplyToomCook3(const Left, Right: BigInteger): BigInteger;
var
  k, Shift: Integer;
  a, b: TArray<BigInteger>;
  a02, b02: BigInteger;
  v0, v1, vm1, v2, vinf: BigInteger;
  t1, t2: BigInteger;
  Sign: Integer;
begin
  // Step 1: if n < threshold then return MultiplyKaratsuba(A, B)
  if ((Left.FSize and SizeMask) < ToomCook3Threshold) and ((Right.FSize and SizeMask) < ToomCook3Threshold) then
  begin
    MultiplyKaratsuba(Left, Right, Result);
    Exit;
  end;

  Sign := (Left.FSize xor Right.FSize) and SignMask;

  // Richard P. Brent and Paul Zimmermann,
  // "Modern Computer Arithmetic", version 0.5.1 of April 28, 2010
  // http://arxiv.org/pdf/1004.4710v1.pdf
  // Algorithm 1.4

  // Step 2: write A = a0 + a1*x + a2*x^2, B = b0 + b1*x + b2*x^2, with x = ß^k.
  k := (IntMax(Left.FSize and SizeMask, Right.FSize and SizeMask) + 2) div 3; // = Ceil(IntMax(...) div 3)

  a := Left.Split(k, 3);
  b := Right.Split(k, 3);

  // Evaluation at x = -1, 0, 1, 2 and +inf.

  // Step 3: v0 <- ToomCook3(a0, b0)
  v0 := MultiplyToomCook3(a[0], b[0]);

  // Step 4a: a02 <- a0 + a2, b02 <- b0 + b2
  a02 := a[0] + a[2];
  b02 := b[0] + b[2];

  // Step 5: v(-1) <- ToomCook3(a02 - a1, b02 - b1) = ToomCook3(a0 + a2 - a1, b0 + b2 - b1)
  vm1 := MultiplyToomCook3(a02 - a[1], b02 - b[1]);

  // Intermediate step: a'02 = a02 + a1, b'02 = b02 + b1
  a02 := a02 + a[1];
  b02 := b02 + b[1];

  // Step 4b: v1 <- ToomCook3(a02 + a1, b02 + b1) = ToomCook3(a'02, b'02)
  v1 := MultiplyToomCook3(a02, b02);

  // Step 6: v2 <- ToomCook3(a0 + 2*a1 + 4*a2, b0 + 2*b1 + 4*b2)
  // Note: first operand is a0 + a1 + a1 + a2 + a2 + a2 + a2 = 2*(a0 + a1 + a2 + a2) - a0 = 2*(a'02 + a2) - a0
  v2 := MultiplyToomCook3((a02 + a[2]) shl 1 - a[0], (b02 + b[2]) shl 1 - b[0]);

  // Step 7: v(inf) <- ToomCook3(a2, b2)
  vinf := MultiplyToomCook3(a[2], b[2]);

  // Step 8: t1 <- (3*v0 + 2*v(−1) + v2)/6 − 2 * v(inf), t2 <- (v1 + v(−1))/2
  t1 := DivideBy3Exactly(((v0 + vm1) shl 1 + v0 + v2) shr 1) - (vinf shl 1);
  t2 := (v1 + vm1) shr 1;

  // Step 9: c0 <- v0, c1 <- v1 - t1, c2 <- t2 - v0 - vinf, c3 <- t1 - t2, c4 <- vinf
  Shift := k * CLimbBits;

  Result := (((((((vinf shl Shift) + t1 - t2) shl Shift) + t2 - v0 - vinf) shl Shift) + v1 - t1) shl Shift) + v0;
  Result.FSize := (Result.FSize and SizeMask) or Sign;
end;

class function BigInteger.SqrKaratsuba(const Value: BigInteger): BigInteger;
var
  NDiv2Shift, NDiv2: Integer;
  ValueUpper: BigInteger;
  ValueLower: BigInteger;
  Upper, Middle, Lower: BigInteger;
  LSize: Integer;
begin
  LSize := (Value.FSize and SizeMask);
  NDiv2Shift := (LSize and $FFFFFFFE) shl 4; // := LSize div 2 * SizeOf(TLimb);
  NDiv2 := LSize div 2;

  ValueLower.MakeSize(NDiv2);
  CopyLimbs(PLimb(Value.FData), PLimb(ValueLower.FData), NDiv2);
  ValueUpper.MakeSize((Value.FSize and SizeMask) - NDiv2);
  CopyLimbs(PLimb(Value.FData) + NDiv2, PLimb(ValueUpper.FData), (Value.FSize and SizeMask) - NDiv2);
  ValueLower.Compact;

  Upper := Sqr(ValueUpper);
  Lower := Sqr(ValueLower);
  Middle := (ValueUpper * ValueLower) shl 1;

  // Can't simply move these values into place, because they still overlap when shifted.
  Result := Upper shl (NDiv2Shift + NDiv2Shift) + Middle shl NDiv2Shift + Lower;
  Result.FSize := Result.FSize and SizeMask;
end;

class function BigInteger.Multiply(const Left, Right: BigInteger): BigInteger;
begin
  Multiply(Left, Right, Result);
end;

class procedure BigInteger.Multiply(const Left, Right: BigInteger; var Result: BigInteger);
var
  LResult: BigInteger; // Avoid prematurely overwriting result when it is same as one of the operands.
begin
  if (Left.FData = nil) or (Right.FData = nil) then
  begin
    ShallowCopy(BigInteger.Zero, Result);
    Exit;
  end;

  if ((Left.FSize and SizeMask) < KaratsubaThreshold) or ((Right.FSize and SizeMask) < KaratsubaThreshold) then
  begin
    // The following block is "Result := MultiplyBaseCase(Left, Right);" written out in full.
    LResult.MakeSize((Left.FSize and SizeMask) + (Right.FSize and SizeMask) + 1);
    InternalMultiply(PLimb(Left.FData), PLimb(Right.FData), PLimb(LResult.FData), Left.FSize and SizeMask,
      Right.FSize and SizeMask);
    LResult.Compact;
    LResult.FSize := (LResult.FSize and SizeMask) or ((Left.FSize xor Right.FSize) and SignMask);
    ShallowCopy(LResult, Result);
  end
  else
  begin
    if ((Left.FSize and SizeMask) < ToomCook3Threshold) and ((Right.FSize and SizeMask) < ToomCook3Threshold) then
      MultiplyKaratsuba(Left, Right, Result)
    else
      Result := MultiplyToomCook3(Left, Right);
  end;
end;

class procedure BigInteger.MultiplyBaseCase(const Left, Right: BigInteger; var Result: BigInteger);
var
  LResult: BigInteger; // Avoid prematurely overwriting result when it is same as one of the operands.
begin
  if (Left.FData = nil) or (Right.FData = nil) then
  begin
    ShallowCopy(Zero, Result);
    Exit;
  end;

//$$RV  LResult.MakeSize((Left.FSize and SizeMask) + (Right.FSize and SizeMask) + 1);
  LResult.MakeSize((Left.FSize and SizeMask) + (Right.FSize and SizeMask) + 256);
  InternalMultiply(PLimb(Left.FData), PLimb(Right.FData), PLimb(LResult.FData), Left.FSize and SizeMask,
    Right.FSize and SizeMask);
  LResult.Compact;
  LResult.SetSign(SignBitOf(Left.FSize) xor SignBitOf(Right.FSize));
  ShallowCopy(LResult, Result);
end;

class operator BigInteger.Multiply(const Left, Right: BigInteger): BigInteger;
begin
  Result := Multiply(Left, Right);
end;

class procedure BigInteger.SetBase(const Value: TNumberBase);
begin
  FBase := Value;
end;

procedure BigInteger.SetSign(Value: Integer);
begin
  FSize := (FSize and SizeMask) or (Ord(Value < 0) * SignMask);
end;

{$IFNDEF BIGINTEGERIMMUTABLE}
function BigInteger.Subtract(const Other: BigInteger): PBigInteger;
var
  MinusOther: BigInteger;
begin
  ShallowCopy(Other, MinusOther);
  MinusOther.FSize := MinusOther.FSize xor SignMask;
  Result := Add(MinusOther);
end;
{$ENDIF}

class function BigInteger.Subtract(const Left, Right: BigInteger): BigInteger;
begin
  Subtract(Left, Right, Result);
end;

class procedure BigInteger.Subtract(const Left, Right: BigInteger; var Result: BigInteger);
const
  BoolMasks: array[Boolean] of Integer = (SignMask, 0);
var
  Largest, Smallest: PBigInteger;
  LSize, SSize: Integer;
  ResData: TMagnitude;
  ResSize: Integer;
  NewSize: Integer;
  Comparison: Integer;
begin
  if Left.FData = nil then
  begin
    Result := Right;
    if Result.FSize <> 0 then
      Result.FSize := Result.FSize xor SignMask;
    Exit;
  end;
  if Right.FData = nil then
  begin
    Result := Left;
    Exit;
  end;

  Comparison := InternalCompare(PLimb(Left.FData), PLimb(Right.FData), Left.FSize and SizeMask,
                  Right.FSize and SizeMask);
  if (Comparison = 0) and (Left.Sign = Right.Sign) then
  begin
    Result := Zero;
    Exit;
  end;

  if Comparison > 0 then
  begin
    Largest := @Left;
    Smallest := @Right;
  end
  else
  begin
    Largest := @Right;
    Smallest := @Left;
  end;

  SSize := Smallest^.FSize and SizeMask;
  LSize := Largest^.FSize and SizeMask;
  ResSize := LSize + 1;
  SetLength(ResData, (ResSize + 3) and CapacityMask);

  if Largest^.Sign = Smallest^.Sign then
    // Same sign: subtract magnitudes.
    FInternalSubtract(PLimb(Largest^.FData), PLimb(Smallest^.FData), PLimb(ResData), LSize, SSize)
  else
    // Different sign: add magnitudes.
    FInternalAdd(PLimb(Largest^.FData), PLimb(Smallest^.FData), PLimb(ResData), LSize, SSize);

  // Compact and set sign.
  NewSize := ActualSize(PLimb(ResData), ResSize);
  if NewSize = 0 then
  begin
    Result := Zero;
    Exit;
  end
  else
  begin
    {$IFDEF RESETSIZE}
    if NewSize < (ResSize + 3) and CapacityMask then
      SetLength(ResData, (NewSize + 3) and CapacityMask);
    {$ENDIF}
    // Set sign and size.
    Result.FSize := NewSize or BoolMasks[(Largest^.FSize < 0) xor (Largest = @Left)];
    Result.FData := ResData;
  end;
end;

class operator BigInteger.Subtract(const Left, Right: BigInteger): BigInteger;
begin
  Subtract(Left, Right, Result);
end;

procedure BigInteger.EnsureSize(RequiredSize: Integer);
begin
  RequiredSize := RequiredSize and SizeMask;
  if RequiredSize > Length(FData) then
    SetLength(FData, (RequiredSize + 4) and CapacityMask);
  FSize := (FSize and SignMask) or RequiredSize;
end;

procedure BigInteger.MakeSize(RequiredSize: Integer);
begin
  FData := nil;
  AllocNewMagnitude(FData, RequiredSize);
  FSize := RequiredSize;
end;

// In Win32, we keep what we have. In Win64, we switch, depending on Size. At 25 limbs or above,
// the unrolled loop version is faster.
class procedure BigInteger.InternalNegate(Source, Dest: PLimb; Size: Integer);
{$IFDEF PUREPASCAL}
var
  R: TLimb;
begin
  repeat
    R := (not Source^) + 1;
    Dest^ := R;
    Inc(Source);
    Inc(Dest);
    Dec(Size);
    if Size = 0 then
      Exit;
  until R <> 0;
  while Size > 0 do
  begin
    Dest^ := not Source^;
    Inc(Source);
    Inc(Dest);
    Dec(Size);
  end;
end;
{$ELSE}
{$IFDEF WIN32}

// This is faster than an unrolled loop with NOT and ADC, especially for smaller BigIntegers.

asm
        PUSH    ESI

@Loop:

        MOV     ESI,[EAX]
        NOT     ESI
        INC     ESI
        MOV     [EDX],ESI
        LEA     EAX,[EAX + CLimbSize]
        LEA     EDX,[EDX + CLimbSize]
        DEC     ECX
        JE      @Exit
        TEST    ESI,ESI                 // Only if ESI is 0, a carry occurred.
        JE      @Loop

@RestLoop:                              // No more carry. We can stop incrementing.

        MOV     ESI,[EAX]
        NOT     ESI
        MOV     [EDX],ESI
        LEA     EAX,[EAX + CLimbSize]
        LEA     EDX,[EDX + CLimbSize]
        DEC     ECX
        JNE     @RestLoop

@Exit:

        POP     ESI
end;
{$ELSE WIN64}
asm

        CMP     R8D,25
        JA      @Unrolled

// Plain version. Faster for small BigIntegers (<= 25 limbs).

@Loop:

        MOV     EAX,[RCX]
        NOT     EAX
        INC     EAX
        MOV     [RDX],EAX
        LEA     RCX,[RCX + CLimbSize]
        LEA     RDX,[RDX + CLimbSize]
        DEC     R8D
        JE      @Exit
        TEST    EAX,EAX
        JE      @Loop

@RestLoop:

        MOV     EAX,[RCX]
        NOT     EAX
        MOV     [RDX],EAX
        LEA     RCX,[RCX + CLimbSize]
        LEA     RDX,[RDX + CLimbSize]
        DEC     R8D
        JNE     @RestLoop
        JMP     @Exit

// Unrolled version. Faster for larger BigIntegers.

@Unrolled:

        TEST    RCX,RCX
        JE      @Exit
        XCHG    R8,RCX
        MOV     R9,RDX
        XOR     EDX,EDX
        MOV     R10D,ECX
        AND     R10D,CUnrollMask
        SHR     ECX,CUnrollShift
        STC
        JE      @Rest

@LoopU:

        MOV     RAX,[R8]
        NOT     RAX
        ADC     RAX,RDX
        MOV     [R9],RAX

        MOV     RAX,[R8 + DLimbSize]
        NOT     RAX
        ADC     RAX,RDX
        MOV     [R9 + DLimbSize],RAX

        LEA     R8,[R8 + 2*DLimbSize]
        LEA     R9,[R9 + 2*DLimbSize]
        LEA     ECX,[ECX - 1]
        JECXZ   @Rest
        JMP     @LoopU

@Rest:

        LEA     RAX,[@JumpTable]
        JMP     [RAX + R10*TYPE Pointer]

        .ALIGN  16

@JumpTable:

        DQ      @Exit
        DQ      @Rest1
        DQ      @Rest2
        DQ      @Rest3

@Rest3:

        MOV     RAX,[R8]
        NOT     RAX
        ADC     RAX,RDX
        MOV     [R9],RAX

        MOV     EAX,[R8 + DLimbSize]
        NOT     EAX
        ADC     EAX,EDX
        MOV     [R9 + DLimbSize],EAX

        JMP     @Exit

@Rest2:

        MOV     RAX,[R8]
        NOT     RAX
        ADC     RAX,RDX
        MOV     [R9],RAX

        JMP     @Exit

@Rest1:

        MOV     EAX,[R8]
        NOT     EAX
        ADC     EAX,EDX
        MOV     [R9],EAX

@Exit:
end;
{$ENDIF WIN64}
{$ENDIF !PUREPASCAL}

class procedure BigInteger.InternalBitwise(const Left, Right: BigInteger;
  var Result: BigInteger; PlainOp, OppositeOp, InversionOp: TBinaryOperator);

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///  The code for the bitwise operators AND, OR and XOR does not differ much.                                     ///
///  Since the results should be the results for two's complement, two's complement semantics are emulated.       ///
///  Originally, this meant that the magnitudes of negative bigintegers were negated, then the                    ///
///  operation was performed and if the result had to be negative, the magnitude of the result was negated.       ///
///  These negation steps were slow, so now this code uses some logical shortcuts.                                ///
///                                                                                                               ///
///  The rules used are like follows.                                                                             ///
///                                                                                                               ///
///  In the following, A and B represent positive integer values, so -A and -B represent negative values.         ///
///  Note that, to keep this simple, 0 -- i.e. FData = nil -- is not handled at all. That is handled              ///
///  by the caller and then this routine is not called.                                                           ///
///                                                                                                               ///
///  Relation between negation and inversion of an integer/magnitude:                                             ///
///  -A = not A + 1    => not A = -A - 1                                                                          ///
///  -A = not (A - 1)                                                                                             ///
///                                                                                                               ///
///  Note: A and B are magnitudes here. Negating a BigInteger is as simple as flipping the sign bit. That         ///
///  does not work for magnitudes.                                                                                ///
///                                                                                                               ///
///  Boolean (and bitwise) rules followed:                                                                        ///
///  not not A       = A                                                                                          ///
///  not (A and B)   = not A or not B                                                                             ///
///  not (A or B)    = not A and not B                                                                            ///
///  not (A xor B)   = not A xor B = A xor not B                                                                  ///
///  not A xor not B = A xor B                                                                                    ///
///                                                                                                               ///
///  Expressions used here:                                                                                       ///
///                                                                                                               ///
///  A and B      = A and B                               ; both positive, plain operation                        ///
///  A and -B     = A and not (B - 1)                     ; one positive, one negative, result positive           ///
///  -(-A and -B) = -(not (A - 1) and not (B - 1))        ; both negative, result is negative too                 ///
///               = - not ((A - 1) or (B - 1)))                                                                   ///
///               = (A - 1) or (B - 1) + 1                                                                        ///
///                                                                                                               ///
///  A or B       = A or B                                ; both positive                                         ///
///  -(A or -B)   = -(A or not (B - 1))                   ; one positive, one negative, result is negative too    ///
///               = - not (not A and (B - 1))                                                                     ///
///               = ((B - 1) and not A) + 1                                                                       ///
///  -(-A or -B)  = -(not (A - 1) or not (B - 1))         ; both negative, result is negative too                 ///
///               = not (not (A - 1) or not (B - 1) + 1                                                           ///
///               = (A - 1) and (B - 1) + 1                                                                       ///
///                                                                                                               ///
///  A xor B      = A xor B                               ; both positive                                         ///
///  -(A xor -B)  = -(A xor not (B - 1))                  ; one positive, one negative, result is negative too    ///
///               = not (A xor not (B - 1)) + 1                                                                   ///
///               = A xor (B - 1) + 1                                                                             ///
///  -A xor -B    = not (A - 1) xor not (B - 1)           ; both negative, result is positive                     ///
///               = (A - 1) xor (B - 1)                                                                           ///
///                                                                                                               ///
///  So the only "primitives" required are routines for AND, OR, XOR and AND NOT. The latter is not really        ///
///  a primitive, but it is so easy to implement, that it can be considered one. NOT is cheap, does not require   ///
///  complicated carry handling.                                                                                  ///
///  Routines like Inc and Dec are cheap too: you only loop as long as there is a carry (or borrow). Often, that  ///
///  is only over very few limbs.                                                                                 ///
///                                                                                                               ///
///  Primitives (InternalAnd(), etc.) routines were optimized too. Loops were unrolled, 64 bit registers used     ///
///  where possible, both sizes are passed, so the operations can be done on the original data. The latter        ///
///  reduces the need for copying into internal buffers.                                                          ///
///                                                                                                               ///
///  These optimizations made bitwise operators 2-3 times as fast as with the simple implementations before.      ///
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

var
  LSize, RSize, MinSize, MaxSize: Integer;
  LPtr, RPtr: PLimb;
begin
  LSize := Left.FSize and SizeMask;
  RSize := Right.FSize and SizeMask;
  MinSize := IntMin(LSize, RSize);
  MaxSize := IntMax(LSize, RSize);

  if ((Left.FSize xor Right.FSize) and SignMask) = 0 then
  begin
    if (Left.FSize > 0) then
    begin
      if Addr(PlainOp) = Addr(InternalAnd) then
        Result.MakeSize(MinSize)
      else
        Result.MakeSize(MaxSize);
      PlainOp(PLimb(Left.FData), PLimb(Right.FData), PLimb(Result.FData), LSize, RSize);
    end
    else
    begin
      LPtr := AllocLimbs(LSize + RSize);                        // LPtr := Copy(Left);
      RPtr := LPtr + LSize;                                     // RPtr := Coyp(Right);
      CopyLimbs(PLimb(Left.FData), LPtr, LSize);
      CopyLimbs(PLimb(Right.FData), RPtr, RSize);
      InternalDecrement(LPtr, LSize);                           // LPtr^ := LPtr^ - 1
      InternalDecrement(RPtr, RSize);                           // RPtr^ := RPtr^ - 1
      Result.FSize := 0;
      Result.MakeSize(MaxSize);
      OppositeOp(LPtr, RPtr, PLimb(Result.FData), LSize, RSize); // Opposite op: AND --> OR, OR --> AND, XOR --> XOR
      if Addr(PlainOp) = Addr(InternalXor) then
        Result.FSize := Result.FSize and SizeMask               // Make positive.
      else
      begin
        InternalIncrement(PLimb(Result.FData), MaxSize);        // Result := Result + 1
        Result.FSize := Result.FSize or SignMask;               // Make negative.
      end;
      FreeMem(LPtr);
    end;
  end
  else
  begin
    if (Left.FSize > 0) then
    begin
      RPtr := AllocLimbs(RSize);
      CopyLimbs(PLimb(Right.FData), RPtr, RSize);
      InternalDecrement(RPtr, RSize);
      Result.FSize := 0;
      if Addr(PlainOp) = Addr(InternalOr) then
        Result.MakeSize(RSize)
      else
        Result.MakeSize(MaxSize);
      // Inversion op: AND --> AND NOT, OR --> NOT AND, XOR --> XOR
      InversionOp(PLimb(Left.FData), RPtr, PLimb(Result.FData), LSize, RSize);
      if Addr(PlainOp) = Addr(InternalAnd) then
        Result.FSize := Result.FSize and SizeMask               // Make positive.
      else
      begin
         InternalIncrement(PLimb(Result.FData), (Result.FSize and SizeMask));
         Result.FSize := Result.FSize or SignMask;              // Make negative.
      end;
      FreeMem(RPtr);
    end
    else
    begin
      LPtr := AllocLimbs(LSize);
      CopyLimbs(PLimb(Left.FData), LPtr, LSize);
      InternalDecrement(LPtr, LSize);
      Result.FSize := 0;
      if Addr(PlainOp) = Addr(InternalOr) then
        Result.MakeSize(LSize)
      else
        Result.MakeSize(MaxSize);
      InversionOp(PLimb(Right.FData), LPtr, PLimb(Result.FData), RSize, LSize);
      if Addr(PlainOp) = Addr(InternalAnd) then
        Result.FSize := Result.FSize and SizeMask
      else
      begin
         InternalIncrement(PLimb(Result.FData), (Result.FSize and SizeMask));
         Result.FSize := Result.FSize or SignMask;
      end;
      FreeMem(LPtr);
    end;
  end;
  Result.Compact;
end;

class function BigInteger.Negate(const Value: BigInteger): BigInteger;
begin
  Result.FData := Value.FData;
  Result.FSize := Value.FSize xor SignMask;
end;

class operator BigInteger.Negative(const Value: BigInteger): BigInteger;
begin
  // Magnitude is not modified, so a shallow copy is enough!
  ShallowCopy(Value, Result);
  if Result.FSize <> 0 then
    Result.FSize := Result.FSize xor SignMask;
end;

class function BigInteger.Parse(const S: string): BigInteger;
var
  TryResult: BigInteger;
begin
  if TryParse(S, TryResult) then
    Result := TryResult
  else
    Error(ecParse, [S, 'BigInteger']);
end;


class function BigInteger.Pow(const ABase: BigInteger; AExponent: Integer): BigInteger;
begin
  Pow(ABase, AExponent, Result);
end;

class procedure BigInteger.Pow(const ABase: BigInteger; AExponent: Integer; var Result: BigInteger);
var
  LBase: BigInteger;
  LBaseBitLength: Integer;
  LScaleFactor: Int64;
  LBigResult: BigInteger;
  LTrailingZeros: Integer;
  LShift: Int64;
  LNewSign: Integer;
  LIntResult: Int64;
  IntBase: Int64;
  LExponent: Integer;
  LResultIsNegative: Boolean;
begin
  if AExponent < 0 then
    Error(ecNegativeExponent, ['AExponent']);

  if ABase.IsZero then
    if AExponent = 0 then
    begin
      ShallowCopy(BigInteger.One, Result);
      Exit;
    end
    else
    begin
      ShallowCopy(ABase, Result);
      Exit;
    end;

  LResultIsNegative := ABase.IsNegative and Odd(AExponent);

  LBase := BigInteger.Abs(ABase);

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ///  Speed things up by removing any common trailing zero bits. The resulting values will probably be smaller,  ///
  ///  so exponentation is done with smaller values, and thus probably faster. The zero bits are added back in    ///
  ///  (multiplied by the exponent, of course) at the very end.                                                   ///
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  LTrailingZeros := LBase.LowestSetBit;
  LShift := Int64(LTrailingZeros) * AExponent;
  if LShift > High(Integer) then
    Error(ecOverflow, []);

  if LTrailingZeros <> 0 then
  begin
    LBase := LBase shr LTrailingZeros;
    LBaseBitLength := LBase.BitLength;
    if LBaseBitLength = 1 then
    begin
      if LResultIsNegative then
      begin
        ShallowCopy(BigInteger.MinusOne shl (LTrailingZeros * AExponent), Result);
        Exit;
      end
      else
      begin
        ShallowCopy(BigInteger.One shl (LTrailingZeros * AExponent), Result);
        Exit;
      end;
    end;
  end
  else
  begin
    LBaseBitLength := LBase.BitLength;
    if LBaseBitLength = 1 then
      if LResultIsNegative then
      begin
        ShallowCopy(BigInteger.MinusOne, Result);
        Exit;
      end
      else
      begin
        ShallowCopy(BigInteger.One, Result);
        Exit;
      end;
  end;

  LScaleFactor := Int64(LBaseBitLength) * AExponent;

  if (LBase.Size = 1) and (LScaleFactor < 31) then
  begin
    // Small values.
    LNewSign := 1;
    if LResultIsNegative then
      LNewSign := -1;
    LIntResult := 1;
    IntBase := LBase.Magnitude[0];

    //////////////////////////////////////////////////////////////////////////////////////////////////////////
    ///  The exponentiation proper:                                                                        ///
    ///                                                                                                    ///
    ///  1. Square the power for each iteration. So you get Base^1, Base^2, Base^4, Base^8, Base^16, etc.  ///
    ///  2. For each bit in the exponent, multiply with the corresponding (i.e. current value of) power    ///
    ///                                                                                                    ///
    ///  Example: 7^11 = 7 ^ (8 + 2 + 1) = 7^1 * 7^2 * 7^8.                                                ///
    //////////////////////////////////////////////////////////////////////////////////////////////////////////
    LExponent := AExponent;
    while LExponent <> 0 do
    begin
      if Odd(LExponent) then
        LIntResult := LIntResult * IntBase;
      LExponent := LExponent shr 1;
      if LExponent <> 0 then
        IntBase := IntBase * IntBase;
    end;

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
    ///  Append the trailing zeroes (times exponent) back in, to get the real result.                       ///
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
    if LTrailingZeros > 0 then
    begin
      if LShift + LScaleFactor < 31 then
        Result := BigInteger(LIntResult shl LShift)  // LIntResult shl Shift is in range of Integer.
      else
        Result := BigInteger(LIntResult) shl LShift; // slightly slower: BigInteger is shifted, not the integer.
      if LResultIsNegative then
      begin
        ShallowCopy(-Result, Result);
        Exit;
      end
      else
        Exit;
    end
    else
    begin
      ShallowCopy(BigInteger(LIntResult * LNewSign), Result);
      Exit;
    end;
  end
  else
  begin
    // True BigIntegers.
    LBigResult := BigInteger.One;
    LExponent := AExponent;

    // The exponentiation proper. See explanation above.
    while LExponent <> 0 do
    begin
      if Odd(LExponent) then
        BigInteger.Multiply(LBigResult, LBase, LBigResult);
      LExponent := LExponent shr 1;
      if LExponent <> 0 then
        LBase := Sqr(LBase);
    end;

    // Append the trailing zeroes (times exponent) back in, to get the real result.
    if LTrailingZeros > 0 then
      LBigResult := LBigResult shl (LTrailingZeros * AExponent);

    if LResultIsNegative then
    begin
      ShallowCopy(-LBigResult, Result);
      Exit;
    end
    else
    begin
      ShallowCopy(LBigResult, Result);
      Exit;
    end;
  end;
end;

class operator BigInteger.NotEqual(const Left, Right: BigInteger): Boolean;
begin
  Result := Compare(Left, Right) <> 0;
end;

class procedure BigInteger.Octal;
begin
  FBase := 8;
end;

class function BigInteger.Remainder(const Left: BigInteger; Right: UInt16): BigInteger;
var
  LQuotient: TMagnitude;
begin
  if Right = 0 then
    Error(ecDivByZero, []);
  Result.MakeSize(1);
  SetLength(LQuotient, (Left.FSize and SizeMask));
  InternalDivMod32(PLimb(Left.FData), Right, PLimb(LQuotient), PLimb(Result.FData), (Left.FSize and SizeMask));
  Result.Compact;
  if Result.FSize <> 0 then
    Result.FSize := (Result.FSize and SizeMask) or SignBitOf(Left.FSize);
end;

class function BigInteger.Remainder(const Left: BigInteger; Right: UInt32): BigInteger;
var
  LQuotient: TMagnitude;
begin
  if Right = 0 then
    Error(ecDivByZero, []);
  Result.MakeSize(1);
  SetLength(LQuotient, (Left.FSize and SizeMask));
  InternalDivMod32(PLimb(Left.FData), Right, PLimb(LQuotient), PLimb(Result.FData), (Left.FSize and SizeMask));
  Result.Compact;
  if Result.FSize <> 0 then
    Result.FSize := (Result.FSize and SizeMask) or SignBitOf(Left.FSize);
end;

class function BigInteger.Remainder(const Left, Right: BigInteger): BigInteger;
var
  Quotient: BigInteger;
  LSize, RSize: Integer;
begin
  if Right.FData = nil then
    Error(ecDivByZero, []);

  LSize := Left.FSize and SizeMask;
  RSize := Right.FSize and SizeMask;

  case InternalCompare(PLimb(Left.FData), PLimb(Right.FData), LSize, RSize) of
    -1:
      begin
        ShallowCopy(Left, Result);
        Exit;
      end;
    0:
      begin
        ShallowCopy(Zero, Result);
        Exit;
      end;
    else
      begin
        if ShouldUseBurnikelZiegler(LSize, RSize) then
          DivModBurnikelZiegler(Left, Right, Quotient, Result)
        else
          DivModKnuth(Left, Right, Quotient, Result);

        // In Delphi, sign of remainder is sign of dividend.
        if Result.FSize <> 0 then
          Result.FSize := (Result.FSize and SizeMask) or SignBitOf(Left.FSize);
      end;
  end;
end;

{$IFNDEF BIGINTEGERIMMUTABLE}
function BigInteger.Remainder(const Other: BigInteger): PBigInteger;
begin
  Result := @Self;
  Self := Self mod Other;
end;
{$ENDIF}


class procedure BigInteger.ShiftRight(const Value: BigInteger; Shift: Integer; var Result: BigInteger);

// Note: this emulates two's complement, more or less like the bitwise operators.

/////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                                     //
//  If Value is a negative BigInteger, then the following applies:                                     //                                                                        //
//                                                                                                     //
//  - shift magnitude as for positive value                                                            //
//  - if (Value < 0) and (Shift > Abs(Value).LowestSetBit()) then                                      //
//      Inc(Result.Magnitude);                                                                         //
//  - Make Result negative if necessary.                                                               //
//                                                                                                     //
//  This greatly simplifies the previous code for negative results.                                    //
//                                                                                                     //
/////////////////////////////////////////////////////////////////////////////////////////////////////////

var
  LSize, LShift: Integer;
  ShiftOffset: Integer;
  RSize: Integer;
  Lowest: Integer;
//  P: PLimb;
begin
  if Value.FData = nil then
  begin
    ShallowCopy(Zero, Result);
    Exit;
  end;

  LSize := (Value.FSize and SizeMask);
  ShiftOffset := Shift shr 5;
  RSize := LSize - ShiftOffset;

  if RSize <= 0 then

  // Shift results in 0. But for negative values, result might be -1.

  begin
    if (Value.FSize < 0) and (Shift > Value.LowestSetBit) then
      ShallowCopy(MinusOne, Result)
    else
      ShallowCopy(Zero, Result);
    Exit;
  end;

  LShift := Shift and $1F;
  Result.MakeSize(RSize);
  if LShift > 0 then
    InternalShiftRight(PLimb(Value.FData) + ShiftOffset, PLimb(Result.FData), LShift, RSize)
  else
    CopyLimbs(PLimb(Value.FData) + ShiftOffset, PLimb(Result.FData), RSize);

  // See comment box above. Handle negative values, if necessary.

  if Value.FSize < 0 then
  begin

    // Simulate two's complement.

    Lowest := Value.LowestSetBit;
    if Shift > Lowest then
      InternalIncrement(PLimb(Result.FData), RSize);
    Result.FSize := Result.FSize or SignMask;
  end;

  Result.Compact;
end;

class function BigInteger.ShiftRight(const Value: BigInteger; Shift: Integer): BigInteger;
begin
  ShiftRight(Value, Shift, Result);
end;

class operator BigInteger.RightShift(const Value: BigInteger; Shift: Integer): BigInteger;
begin
  ShiftRight(Value, Shift, Result);
end;

class operator BigInteger.Implicit(const Value: string): BigInteger;
begin
  if not TryParse(Value, Result) then
    Error(ecParse, [Value, 'BigInteger']);
end;

{$IFNDEF NoAnsi}
class operator BigInteger.Implicit(const Value: PAnsiChar): BigInteger;
begin
  if not TryParse(string(AnsiString(Value)), Result) then
    Error(ecParse, [string(AnsiString(Value)), 'BigInteger']);
end;
{$ENDIF}

class operator BigInteger.Implicit(const Value: PWideChar): BigInteger;
begin
  if not TryParse(Value, Result) then
    Error(ecParse, [Value, 'BigInteger']);
end;

{$IFDEF HasExtended}
class operator BigInteger.Explicit(const Value: BigInteger): Extended;
begin
  Result := Value.AsExtended;
end;
{$ENDIF}

class operator BigInteger.Explicit(const Value: BigInteger): Double;
begin
  Result := Value.AsDouble;
end;

class operator BigInteger.Explicit(const Value: BigInteger): Single;
begin
  Result := Value.AsSingle;
end;

class operator BigInteger.Explicit(const Value: Double): BigInteger;
begin
  Result.Create(Value);
end;

class operator BigInteger.Explicit(const Value: BigInteger): string;
begin
  Result := Value.ToString;
end;

class operator BigInteger.Inc(const Value: BigInteger): BigInteger;
begin
  if Value.FData = nil then
  begin
    ShallowCopy(One, Result);
    Exit;
  end;
  Result.FData := Copy(Value.FData);
  Result.FSize := Value.FSize;
  if Result.FSize > 0 then
  begin
    Result.EnsureSize((Result.FSize and SizeMask) + 1);
    InternalIncrement(PLimb(Result.FData), (Result.FSize and SizeMask));
  end
  else
    InternalDecrement(PLimb(Result.FData), (Result.FSize and SizeMask));
  Result.Compact;
end;

class operator BigInteger.Dec(const Value: BigInteger): BigInteger;
begin
  if Value.FData = nil then
  begin
    ShallowCopy(MinusOne, Result);
    Exit;
  end;
  Result.FData := Copy(Value.FData);
  Result.FSize := Value.FSize;
  if Result.FSize < 0 then
  begin
    Result.EnsureSize((Result.FSize and SizeMask) + 1);
    InternalIncrement(PLimb(Result.FData), (Result.FSize and SizeMask));
  end
  else
    InternalDecrement(PLimb(Result.FData), (Result.FSize and SizeMask));
  Result.Compact;
end;

{$IFNDEF BIGINTEGERIMMUTABLE}
function BigInteger.Add(const Other: BigInteger): PBigInteger;
var
  SelfSize, OtherSize: Integer;
  Comparison: Integer;
begin
  Result := @Self;
  if Other.IsZero then
    Exit;
  if Self.IsZero then
  begin
    Self := Other;
    Exit;
  end;
  FData := Copy(FData);
  SelfSize := FSize and SizeMask;
  OtherSize := Other.FSize and SizeMask;
  if Self.IsNegative = Other.IsNegative then
  begin
    EnsureSize(IntMax(SelfSize, OtherSize) + 1);
    FInternalAdd(PLimb(Self.FData), PLimb(Other.FData), PLimb(Self.FData), SelfSize, OtherSize);
  end
  else
  begin
    // Different signs, so subtract.
    EnsureSize(IntMax(SelfSize, OtherSize));
    Comparison := InternalCompare(PLimb(Self.FData), PLimb(Other.FData), (Self.FSize and SizeMask),
                    (Other.FSize and SizeMask));
    if Comparison = 0 then
    begin
      Self := Zero;
      Exit;
    end;

    if Comparison > 0 then
    begin
      FInternalSubtract(PLimb(Self.FData), PLimb(Other.FData), PLimb(Self.FData), SelfSize, OtherSize);
    end
    else
    begin
      FInternalSubtract(PLimb(Other.FData), PLimb(Self.FData), PLimb(Self.FData), OtherSize, SelfSize);
      Self.FSize := Self.FSize xor SignMask;
    end;
  end;
  Compact;
end;
{$ENDIF}

class procedure BigInteger.AvoidPartialFlagsStall(Value: Boolean);
{$IFDEF PUREPASCAL}
begin
  FInternalAdd := InternalAddPurePascal;
  FInternalSubtract := InternalSubtractPurePascal;
end;
{$ELSE}
begin
  FAvoidStall := Value;
  if Value then
  begin
    FInternalAdd := InternalAddModified;
    FInternalSubtract := InternalSubtractModified;
  end
  else
  begin
    FInternalAdd := InternalAddPlain;
    FInternalSubtract := InternalSubtractPlain;
  end;
end;
{$ENDIF}

{$IFNDEF BIGINTEGERIMMUTABLE}
function BigInteger.Multiply(const Other: BigInteger): PBigInteger;
begin
  Result := @Self;
  Self := Self * Other;
end;
{$ENDIF}

procedure FlipBigIntegerBit(var B: BigInteger; Index: Integer); inline;
begin
  B.FData := Copy(B.FData);
  B.EnsureSize(IntMax(Index shr 5 + 1, B.FSize and BigInteger.SizeMask));
  B.FData[Index shr 5] := B.FData[Index shr 5] xor (1 shl (Index and $1F));
  B.Compact;
end;

function BigInteger.TestBit(Index: Integer): Boolean;

///////////////////////////////////////////////////////////////////////
///  Two's complement semantics are required.                       ///
///                                                                 ///
///  Note: -A = not (A - 1) = not A + 1                             ///
///                                                                 ///
///  Example, assuming 16 bit limbs, negating goes like follows:    ///
///                                                                 ///
///    -$1234 5678 9ABC 0000 0000 -> $EDCB A987 6544 0000 0000      ///
///  0:                      0000 ->                      FFFF + 1  ///
///  1:                 0000      ->                 FFFF + 1       ///
///  2:            9ABC           ->            6543 + 1            ///
///  3:       5678                ->       A987                     ///
///  4:  1234                     ->  EDCB                          ///
///                                                                 ///
///  So accessing limb 4 or 3:    Data := not Data                  ///
///     accessing limb 2, 1 or 0: Data := not Data + 1              ///
///////////////////////////////////////////////////////////////////////

var
  I: Integer;
  Mask: TLimb;
  Data: TLimb;
begin
  if FData = nil then

    // Zero, so no bit set. Return False.
    Result := False
  else if Index >= BitLength then

    // Beyond bit length, so return sign
    Result := (FSize and SignMask) <> 0
  else
  begin
    Mask := 1 shl (Index and $1F);
    Index := Index shr 5;
    Data := FData[Index];

    // Emulate negation if this BigInteger is negative.
    // Not necessary if BigInteger is positive.
    if (FSize and SignMask) <> 0 then
    begin

      // -A = not A + 1.
      Data := not Data; // Wait with the + 1, see below.
      I := 0;

      // See if carry propagates from lowest limb to limb containing the bit. If so, increment Data.
      while (I <= Index) and (FData[I] = 0) do
        Inc(I);
      if Index <= I then
        Inc(Data);
    end;

    // Get the bit.
    Result := (Data and Mask) <> 0;
  end;
end;

function BigInteger.SetBit(Index: Integer): BigInteger;
var
  LimbIndex: Integer;
  BitMask, Borrow, Data: TLimb;
begin
  Result := Self.Clone;
  LimbIndex := Index shr 5;
  BitMask := 1 shl (Index and 31);

  if Self.IsNegative then
  begin
    // If negative, every bit beyond the bit length is supposed to be set already (assuming two's complement), so
    // no change.
    if Index > Self.BitLength then
      Exit;

    // No need to change the limbs below the index, so start at LimbIndex.

    // Negate this limb, set the bit, negate it again and store it back.
    Data := Result.FData[LimbIndex];
    Result.FData[LimbIndex] := -(-Data or BitMask);
    Inc(LimbIndex);

    // If there was a borrow, it must be propagated.
    Borrow := Ord(Data = 0);
    if Borrow <> 0 then
      while LimbIndex < (Result.FSize and SizeMask) do
      begin
        Data := Result.FData[LimbIndex];
        Result.FData[LimbIndex] := Data - 1;

        // We can stop if the limb *wasn't* 0, since then there will be no borrow anymore.
        if Data <> 0 then
          Break
        else
          Inc(LimbIndex);
      end;
  end
  else
  begin
    // If the bit is beyond the bit length, the size must be expanded.
    if (Index > Self.BitLength) or Self.IsZero then
      Result.EnsureSize(LimbIndex + 1);

    // Set the bit.
    Result.FData[LimbIndex] := Result.FData[LimbIndex] or BitMask;
  end;
  Result.Compact;
end;

function BigInteger.ClearBit(Index: Integer): BigInteger;
var
  LimbIndex: Integer;
  BitMask, Borrow, Data: TLimb;
begin
  Result := Self.Clone;
  LimbIndex := Index shr 5;
  BitMask := 1 shl (Index and 31);

  if Self.IsNegative then
  begin
    if Index > Self.BitLength then
    begin
      Result.EnsureSize(LimbIndex + 1);
      Result.FData[LimbIndex] := Result.FData[LimbIndex] or BitMask;
    end
    else
    begin
      Data := Result.FData[LimbIndex];
      Result.FData[LimbIndex] := TLimb(-(-Data and not BitMask));
      Inc(LimbIndex);

      // Propagate borrow
      Borrow := Ord(Data = 0);
      if Borrow > 0 then
        while LimbIndex < Result.FSize and SizeMask do
        begin
          Data := Result.FData[LimbIndex];
          Dec(Result.FData[LimbIndex]);
          if Data <> 0 then
            Break
          else
            Inc(LimbIndex);
        end;
    end;
  end
  else
  begin
    if Index > BitLength then
      Exit;
    Result.FData[LimbIndex] := Result.FData[LimbIndex] and not BitMask;
  end;
  Result.Compact;
end;

function BigInteger.FlipBit(Index: Integer): BigInteger;
var
  LimbIndex: Integer;
  BitMask, Borrow, Data: TLimb;
begin
  Result := Self.Clone;
  LimbIndex := Index shr 5;
  BitMask := 1 shl (Index and 31);

  if Self.IsNegative then
  begin
    if Index > Self.BitLength then
    begin
      Result.EnsureSize(LimbIndex + 1);
      Result.FData[LimbIndex] := Result.FData[LimbIndex] xor BitMask;
    end
    else
    begin
      Data := Result.FData[LimbIndex];
      Result.FData[LimbIndex] := -(-Data xor BitMask);
      Inc(LimbIndex);

      // Propagate borrow
      Borrow := Ord(Data = 0);
      if Borrow > 0 then
        while LimbIndex < Result.FSize and SizeMask do
        begin
          Data := Result.FData[LimbIndex];
          Dec(Result.FData[LimbIndex]);
          if Data <> 0 then
            Break
          else
            Inc(LimbIndex);
        end;
    end;
  end
  else
  begin
    if (Index > BitLength) or Self.IsZero then
      Result.EnsureSize(LimbIndex + 1);
    Result.FData[LimbIndex] := Result.FData[LimbIndex] xor BitMask;
  end;
  Result.Compact;
end;

class function BigInteger.NthRoot(const Radicand: BigInteger; Index: Integer): BigInteger;
var
  PredIndex: Integer;
  BigIndex, BigPredIndex: Integer;
  Newestimate, PrevEstimate: BigInteger;
begin
  if Radicand.IsZero or Radicand.IsOne then
    Exit(Radicand);
  if Radicand.IsNegative then
    Error(ecNegativeRadicand, ['NthRoot']);
  case Index of
    0: Exit(BigInteger.Zero);
    1: Exit(Radicand);
    2: Exit(BaseCaseSqrt(Radicand));
  end;
  if Index < 0 then
    Error(ecNegativeExponent, ['NthRoot']);
  PredIndex := System.Pred(Index);
  Result := BigInteger.Zero.SetBit(Radicand.BitLength div Index);
  PrevEstimate := Result;

  // Loop invariants
  BigIndex := Index;
  BigPredIndex := PredIndex;

  // Newton-Raphson approximation loop, similar to code in Sqrt().
  repeat
    NewEstimate := (Result * BigPredIndex + Radicand div BigInteger.Pow(Result, PredIndex)) div BigIndex;
    // Loop until no difference with previous value or detect end of a cycle.

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    /// Unfortunately, the true root is only detected when the cycle starts repeating, i.e. at the end of the cycle. ///
    /// That means that this routine can be slower if there is a cycle. Otherwise, it is fast.                 ///
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    if (Result = NewEstimate) or ((Result < NewEstimate) and (Result < PrevEstimate)) then
      Exit(Result);
    PrevEstimate := Result;
    Result := NewEstimate;
  until False;
end;

class procedure BigInteger.NthRootRemainder(const Radicand: BigInteger; Index: Integer; var Root, Remainder: BigInteger);
begin
  Root := NthRoot(Radicand, Index);
  Remainder := Radicand - Pow(Root, Index);
end;

class function BigInteger.Sqr(const Value: BigInteger): BigInteger;
begin
  if (Value.FSize and SizeMask) < KaratsubaSqrThreshold then
    Result := Value * Value
  else
    Result := SqrKaratsuba(Value);
end;

//////////////////////////////////////////////////////////////////////////////////////////////////
/// A Newton-Raphson algorithm is *much* faster than the previous binary-search like           ///
/// algorithm.                                                                                 ///
///                                                                                            ///
/// This new N-R algorithm is even faster than the previous and correct. The previous one      ///
/// could go into an endless loop when the estimate flipped continuously between the root and  ///
/// root+1, which is something that doesn't happen often, but can occur, e.g. for a radicand   ///
/// with the value 8.                                                                          ///
///                                                                                            ///
/// https://stackoverflow.com/questions/4407839#16804098                                       ///
//////////////////////////////////////////////////////////////////////////////////////////////////
class function BigInteger.BaseCaseSqrt(const Radicand: BigInteger): BigInteger;
var
  PrevEstimate, NewEstimate: BigInteger;
begin
  if Radicand.IsOne or Radicand.IsZero then
    Exit(Radicand);
  if Radicand.IsNegative then
    Error(ecNegativeRadicand, ['Sqrt']); // Do not translate!
  Result := Radicand shr (Radicand.BitLength shr 1);
  PrevEstimate := Result;
  // Loop until we hit the same value twice in a row, or wind up alternating.
  repeat
    NewEstimate := (Result + Radicand div Result) shr 1;
    if (NewEstimate = Result) or              // normal case
       (NewEstimate = PrevEstimate) then        // alternating case
      Exit(BigInteger.Min(Result, NewEstimate));
    PrevEstimate := Result;
    Result := NewEstimate;
  until False;
end;

class procedure BigInteger.BaseCaseSqrtRemainder(const Radicand: BigInteger; var Root, Remainder: BigInteger);
begin
  Root := BaseCaseSqrt(Radicand);
  Remainder := Radicand - Sqr(Root);
end;

class function BigInteger.Sqrt(const Radicand: BigInteger): BigInteger;
var
  Rem: BigInteger;
begin
  SqrtRemainder(Radicand, Result, Rem);
end;

// Richard P. Brent and Paul Zimmermann, "Modern Computer Arithmetic", Algorithm 1.12
// Produces square root and square root remainder in one go.
// Extremely fast, much faster than Newton-Raphson (as used in BaseCaseSqrtRemainder), even for relatively
// small sizes.
class procedure BigInteger.SqrtRemainder(const Radicand: BigInteger; var Root, Remainder: BigInteger);
var
  RadCopy: BigInteger;
  Limbs: Integer;
  BaseToL, BaseMask: BigInteger;
  A3, A2, A1, A0: BigInteger;
  RootQ, RemQ: BigInteger;
  Quot, Rem: BigInteger;
begin
  // Note: if the threshold is too small, a stack overflow will occur.
  if Radicand.Size < 10 then
  begin
    BaseCaseSqrtRemainder(Radicand, Root, Remainder);
    Exit;
  end;

  // l = trunc((n - 1) / 4)
  Limbs := (Radicand.Size - 1) div 4;

  // if l = 0 then return BaseCaseSqrtRem(m) <-- See above: there is a threshold > 0

  BaseToL := BigInteger.One shl (CLimbBits * Limbs);
  BaseMask := BaseToL - 1;

  // Write m = a3*beta^3*l + a2*beta^2*l + a1*beta^l + a0 with 0 <= a2, a1, a0 < beta^l
  A0 := Radicand and BaseMask;
  RadCopy := Radicand shr (CLimbBits * Limbs);
  A1 := RadCopy and BaseMask;
  RadCopy := RadCopy shr (CLimbBits * Limbs);
  A2 := RadCopy and BaseMask;
  A3 := RadCopy shr (CLimbBits * Limbs);

  // (s^', r') <-- SqrtRem(a3*beta^l + a2)
  BigInteger.SqrtRemainder(A3 * BaseToL + A2, RootQ, RemQ);

  // (q, u) <-- DivRem(r'*beta^l + a1, 2*s')
  BigInteger.DivMod(RemQ * BaseToL + A1, RootQ shl 1, Quot, Rem);

  // s <-- s'*beta^l + q
  Root := RootQ * BaseToL + Quot;

  // r <-- u*beta^l + a0 - q^2
  Remainder := Rem * BaseToL + A0 - BigInteger.Sqr(Quot);

  // if r < 0 then
  if Remainder < 0 then
  begin
    // r <-- r + 2*s - 1
    Remainder := Remainder + 2 * Root - 1;

    // s <-- s - 1
    Root := Root - 1;
  end;

  // return (s, r)
end;

class procedure BigInteger.DivThreeHalvesByTwo(const LeftUpperMid, LeftLower, Right, RightUpper: BigInteger;
  const RightLower: BigInteger; N: Integer;
  var Quotient, Remainder: BigInteger);
var
  Q, R: BigInteger;
begin
  if RightLower.FData <> nil then
    ;
  Q := BigInteger.Zero;
  R := BigInteger.Zero;
  if (LeftUpperMid shr N) = RightUpper then
  begin
    Q := (BigInteger.One shl N) - BigInteger.One;
    R := LeftUpperMid - (RightUpper shl N) + RightUpper;
  end
  else
    DivTwoDigitsByOne(LeftUpperMid, RightUpper, N, Q, R);

  Quotient := Q;
  Remainder := ((R shl N) or LeftLower) - Q * RightLower;
  while Remainder < 0 do
  begin
    Dec(Quotient);
    Remainder := Remainder + Right;
  end;
end;


class procedure BigInteger.DivTwoDigitsByOne(const Left, Right: BigInteger; N: Integer;
  var Quotient, Remainder: BigInteger);
var
  NIsOdd: Boolean;
  LeftCopy, RightCopy: BigInteger;
  HalfN: Integer;
  HalfMask: BigInteger;
  RightUpper, RightLower: BigInteger;
  QuotientUpper, QuotientLower: BigInteger;
  Quot, Rem: BigInteger;
begin
  Quot := BigInteger.Zero;
  Rem := BigInteger.Zero;
  if N <= BigInteger.BurnikelZieglerThreshold * CLimbBits then
  begin
    BigInteger.DivModKnuth(Left, Right, Quot, Rem);
    Quotient := Quot;
    Remainder := Rem;
    Exit;
  end;

  NIsOdd := Odd(N);
  if NIsOdd then
  begin
    LeftCopy := Left shl 1;
    RightCopy := Right shl 1;
    Inc(N);
  end
  else
  begin
    LeftCopy := Left;
    RightCopy := Right;
  end;
  HalfN := N shr 1;
  HalfMask := (BigInteger.One shl HalfN) - BigInteger.One;

  RightUpper := RightCopy shr HalfN;
  RightLower := RightCopy and HalfMask;

  DivThreeHalvesByTwo(LeftCopy shr N, (LeftCopy shr HalfN) and HalfMask, RightCopy, RightUpper,
    RightLower, HalfN, QuotientUpper, Rem);
  DivThreeHalvesByTwo(Rem, LeftCopy and HalfMask, RightCopy, RightUpper,
    RightLower, HalfN, QuotientLower, Rem);

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ///                                                                                                               ///
  ///  Grade school division, but with (very) large digits, dividing [a1,a2,a3,a4] by [b1,b2]:                      ///
  ///                                                                                                               ///
  ///    +----+----+----+----+     +----+----+   +----+                                                             ///
  ///    | a1 | a2 | a3 | a4 |  /  | b1 | b2 | = | q1 |        DivideThreeHalvesByTwo(a1a2, a3, b1b2, n, q1, r1r2)  ///
  ///    +----+----+----+----+     +----+----+   +----+                                                             ///
  ///    +--------------+  |                       |                                                                ///
  ///    |   b1b2 * q1  |  |                       |                                                                ///
  ///    +--------------+  |                       |                                                                ///
  ///  - ================  v                       |                                                                ///
  ///         +----+----+----+     +----+----+     |  +----+                                                        ///
  ///         | r1 | r2 | a4 |  /  | b1 | b2 | =   |  | q2 |   DivideThreeHalvesByTwo(r1r2, a4, b1b2, n, q1, r1r2)  ///
  ///         +----+----+----+     +----+----+     |  +----+                                                        ///
  ///         +--------------+                     |    |                                                           ///
  ///         |   b1b2 * q2  |                     |    |                                                           ///
  ///         +--------------+                     |    |                                                           ///
  ///       - ================                     v    v                                                           ///
  ///              +----+----+                   +----+----+                                                        ///
  ///              | r1 | r2 |                   | q1 | q2 |   r1r2 = a1a2a3a4 mod b1b2, q1q2 = a1a2a3a4 div b1b2   ///
  ///              +----+----+                   +----+----+ ,                                                      ///
  ///                                                                                                               ///
  ///  Note: in the diagram above, a1, b1, q1, r1 etc. are the most significant "digits" of their numbers.          ///
  ///                                                                                                               ///
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  if NIsOdd then
    Rem := Rem shr 1;
  Remainder := Rem;
  Quotient := (QuotientUpper shl HalfN) or QuotientLower;
end;

class procedure BigInteger.InternalDivModBurnikelZiegler(const Left, Right: BigInteger;
  var Quotient, Remainder: BigInteger);
var
  LCopy: BigInteger;
  N: Integer;
  DigitMask: BigInteger;
  LeftDigits: TArray<BigInteger>;
  NumDigits: Integer;
  QuotientDigit: BigInteger;
  DigitIndex: Integer;
begin
  LCopy := Left;
  N := Right.BitLength;

  // A digit has the same bit length as right has, so this is the number of digits that must be allocated.
  NumDigits := (Left.BitLength + N - 1) div N;
  SetLength(LeftDigits, NumDigits);

  // Split Left into a number of digits of the same bitsize as Right, and collect them in LeftDigits.
  DigitIndex := 0;
  DigitMask := (BigInteger.One shl N) - BigInteger.One;
  while not LCopy.IsZero do
  begin
    LeftDigits[DigitIndex] := LCopy and DigitMask;
    LCopy := LCopy shr N;
    Inc(DigitIndex);
  end;
  // Make DigitIndex point to top of "stack"
  Dec(DigitIndex);

  // Remainder is the top digit of the "two digits" that are to be divided by "one".
  if LeftDigits[DigitIndex] >= Right then
    Remainder := BigInteger.Zero
  else
  begin
    Remainder := LeftDigits[DigitIndex];
    Dec(DigitIndex);
  end;

  // Clear QuotientDigit and Quotient.
  QuotientDigit := BigInteger.Zero;
  Quotient := BigInteger.Zero;

  // Repeatedly divide two digits by the right digit and shift the resulting quotient digit into the final quotient.
  while DigitIndex >= 0 do
  begin
    DivTwoDigitsByOne((Remainder shl N) + LeftDigits[DigitIndex], Right, N, QuotientDigit, Remainder);
    Dec(DigitIndex);
    Quotient := (Quotient shl N) + QuotientDigit;
  end;
end;

class procedure BigInteger.DivModBurnikelZiegler(const Left, Right: BigInteger; var Quotient, Remainder: BigInteger);
var
  Q, R: BigInteger;
begin

  if Right.IsZero then
    raise Exception.Create('Division by zero')
  else if Right.IsNegative then
  begin
    DivModBurnikelZiegler(-Left, -Right, Q, R);
    Quotient := Q;
    Remainder := -R;
    Exit;
  end
  else if Left.IsNegative then
  begin
    DivModBurnikelZiegler(not Left, Right, Q, R);
    Quotient := not Q;
    Remainder := Right + not R;
    Exit;
  end
  else if Left.IsZero then
  begin
    Quotient := BigInteger.Zero;
    Remainder := BigInteger.Zero;
    Exit;
  end
  else
  begin
    InternalDivModBurnikelZiegler(Left, Right, Q, R);
    Quotient := Q;
    Remainder := R;
    Exit;
  end;
end;

// end of BigInteger

// from unit IPTypesX;

const
  SInvalidIPv4Value = '''%s'' is not a valid IPv4 address';
  SInvalidIPv4FormatType = 'Invalid format type for IPv4';
  SInvalidIPv6Value = '''%s'' is not a valid IPv6 address';
  SInvalidIPv6FormatType = 'Invalid format type for IPv6';

procedure IPv4Error(const Message: String);
begin
  raise EIPv4Error.Create(Message);
end;

procedure IPv4ErrorFmt(const Message, IPv4AsString: String);
begin
  raise EIPv4Error.Create(Format(Message, [IPv4AsString]));
end;

procedure IPv6Error(const Message: String);
begin
  raise EIPv6Error.Create(Message);
end;

procedure IPv6ErrorFmt(const Message, IPv6AsString: String);
begin
  raise EIPv6Error.Create(Format(Message, [IPv6AsString]));
end;

{ Utility routines }

procedure CheckCase(var S: String);
begin
  if IPCharCase = ccLowerCase then
    S := LowerCase(S);
end;

function TryStrToIPv4(const S: String; ARaiseErrors: Boolean; out AValidIP: Boolean): TIPv4; //Vitaly 22.10.2019
var
  SIP: String;
  Start: Integer;
  I: T4;
  Index: Integer;
  Count: Integer;
  SGroup: String;
  G: Integer;
begin
  AValidIP := True;
  SIP := S + '.';
  Start := 1;
  for I := High(T4) downto Low(T4) do
  begin
    Index := PosEx('.', SIP, Start);
    if Index = 0 then
    begin
      AValidIP := False;
      if ARaiseErrors then
        IPv4ErrorFmt(SInvalidIPv4Value, S);
      Exit;
    end;
    Count := Index - Start + 1;
    SGroup := Copy(SIP, Start, Count - 1);
    if TryStrToInt(SGroup, G) and (G >= Low(Word)) and (G <= High(Word)) then
        Result.Groups[I] := G
      else
        Result.Groups[I] := 0;
    Inc(Start, Count);
  end;
end;

function IsValidIPv4(const S: String): Boolean; //Vitaly 22.10.2019
begin
  TryStrToIPv4(S, False, Result);
end;

function StrToIPv4(const S: String): TIPv4;
  //Vitaly 22.10.2019
var
  LValidIP: Boolean;
begin
  Result := TryStrToIPv4(S, True, LValidIP);
end;

function IPv4Compare(const AIPv41, AIPv42: TIPv4): Integer;
begin
  if AIPv41.Value = AIPv42.Value then
    Result := 0
  else if AIPv41.Value < AIPv42.Value then
    Result := -1
  else
    Result := 1;
end;

procedure IPv4ToBits(const AIPv4: TIPv4; ABits: TBits);
var
  I: Integer;
begin
  if ABits <> nil then
  begin
    ABits.Size := IPv4BitSize;
    for I := 0 to IPv4BitSize - 1 do
      ABits[IPv4BitSize - I - 1] := AIPv4.Value and (1 shl I) <> 0;
  end;
end;

function IPv4ToIPv6(const AIPv4: TIPv4): TIPv6;
begin
  FillChar(Result.E, 5 * SizeOf(Word), 0);
  Result.F := $FFFF;
  Result.G := AIPv4.A shl 8 + AIPv4.B;
  Result.H := AIPv4.C shl 8 + AIPv4.D;
end;

function IPv4ToStr(const AIPv4: TIPv4): String;
begin
  with AIPv4 do
    Result := Format('%d.%d.%d.%d', [A, B, C, D]);
end;

function IPv4ToStrHex(const AIPv4: TIPv4): String;
begin
  with AIPv4 do
    Result := Format('%.2x.%.2x.%.2x.%.2x', [A, B, C, D]);
  CheckCase(Result);
end;

function IPv4ToStrOutwr(const AIPv4: TIPv4): String;
begin
  with AIPv4 do
    Result := Format('%.3d.%.3d.%.3d.%.3d', [A, B, C, D]);
end;

function IPv4ToURL(const AIPv4: TIPv4; const Protocol: String = DefProtocol;
  const PortNumber: Word = DefPortNumber): String;
begin
  Result := IPv4ToStr(AIPv4);
  if (PortNumber <> DefPortNumber) or (Protocol <> DefProtocol) then
    Result := Protocol + '://' + Result + ':' + IntToStr(PortNumber) + '/'
  else
    Result := Protocol + '://' + Result + '/';
end;

function TryStrToIPv6(const S: String; ARaiseErrors: Boolean; out AValidIP: Boolean): TIPv6; //Vitaly 22.10.2019
{ Valid examples for S:
  2001:0db8:85a3:0000:0000:8a2e:0370:7334
  2001:db8:85a3:0:0:8a2e:370:7334
  2001:db8:85a3::8a2e:370:7334
  ::8a2e:370:7334
  2001:db8:85a3::
  ::1
  ::
  ::ffff:c000:280
  ::ffff:192.0.2.128 }
var
  ZeroPos: Integer;
  DotPos: Integer;
  SIP: String;
  Start: Integer;
  Index: Integer;
  Count: Integer;
  SGroup: String;
  G: Integer;

  procedure NormalNotation;
  var
    I: T8;
  begin
    SIP := S + ':';
    Start := 1;
    for I := High(T8) downto Low(T8) do
    begin
      Index := PosEx(':', SIP, Start);
      if Index = 0 then
      begin
        AValidIP := False;
        if ARaiseErrors then
          IPv6ErrorFmt(SInvalidIPv6Value, S);
        Exit;
      end;
      Count := Index - Start + 1;
      SGroup := '$' + Copy(SIP, Start, Count - 1);
      if not TryStrToInt(SGroup, G) or (G > High(Word)) or (G < 0) then
      begin
        AValidIP := False;
        if ARaiseErrors then
          IPv6ErrorFmt(SInvalidIPv6Value, S);
        Exit;
      end;
      Result.Groups[I] := G;
      Inc(Start, Count);
    end;
  end;

  procedure CompressedNotation;
  var
    I: T8;
    A: array of Word;
  begin
    SIP := S + ':';
    Start := 1;
    I := High(T8);
    while Start < ZeroPos do
    begin
      Index := PosEx(':', SIP, Start);
      if Index = 0 then
      begin
        AValidIP := False;
        if ARaiseErrors then
          IPv6ErrorFmt(SInvalidIPv6Value, S);
        Exit;
      end;
      Count := Index - Start + 1;
      SGroup := '$' + Copy(SIP, Start, Count - 1);
      if not TryStrToInt(SGroup, G) or (G > High(Word)) or (G < 0) then
      begin
        AValidIP := False;
        if ARaiseErrors then
          IPv6ErrorFmt(SInvalidIPv6Value, S);
        Exit;
      end;
      Result.Groups[I] := G;
      Inc(Start, Count);
      Dec(I);
    end;
    FillChar(Result.H, (I + 1) * SizeOf(Word), 0);
    if ZeroPos < (Length(S) - 1) then
    begin
      SetLength(A, I + 1);
      Start := ZeroPos + 2;
      repeat
        Index := PosEx(':', SIP, Start);
        if Index > 0 then
        begin
          Count := Index - Start + 1;
          SGroup := '$' + Copy(SIP, Start, Count - 1);
          if not TryStrToInt(SGroup, G) or (G > High(Word)) or (G < 0) then
          begin
            AValidIP := False;
            if ARaiseErrors then
              IPv6ErrorFmt(SInvalidIPv6Value, S);
            Exit;
          end;
          A[I] := G;
          Inc(Start, Count);
          Dec(I);
        end;
      until Index = 0;
      Inc(I);
      Count := Length(A) - I;
      Move(A[I], Result.H, Count * SizeOf(Word));
    end;
  end;

  procedure DottedQuadNotation;
  var
    I: T4;
  begin
    if UpperCase(Copy(S, ZeroPos + 2, 4)) <> 'FFFF' then
    begin
      AValidIP := False;
      if ARaiseErrors then
        IPv6ErrorFmt(SInvalidIPv6Value, S);
      Exit;
    end;
    FillChar(Result.E, 5 * SizeOf(Word), 0);
    Result.F := $FFFF;
    SIP := S + '.';
    Start := ZeroPos + 7;
    for I := Low(T4) to High(T4) do
    begin
      Index := PosEx('.', SIP, Start);
      if Index = 0 then
      begin
        AValidIP := False;
        if ARaiseErrors then
          IPv6ErrorFmt(SInvalidIPv6Value, S);
        Exit;
      end;
      Count := Index - Start + 1;
      SGroup := Copy(SIP, Start, Count - 1);
      if not TryStrToInt(SGroup, G) or (G > High(Byte)) or (G < 0) then
      begin
        AValidIP := False;
        if ARaiseErrors then
          IPv6ErrorFmt(SInvalidIPv6Value, S);
        Exit;
      end;
      case I of
        0: Result.G := G shl 8;
        1: Inc(Result.G, G);
        2: Result.H := G shl 8;
        3: Inc(Result.H, G);
      end;
      Inc(Start, Count);
    end;
  end;

begin
  AValidIP := True;
  ZeroPos := Pos('::', S);
  if ZeroPos = 0 then
    NormalNotation
  else
  begin
    DotPos := Pos('.', S);
    if DotPos = 0 then
      CompressedNotation
    else
      DottedQuadNotation;
  end;
end;

function IsValidIPv6(const S: String): Boolean; //Vitaly 22.10.2019
begin
  TryStrToIPv6(S, False, Result);
end;

function StrToIPv6(const S: String): TIPv6;
  //Vitaly 22.10.2019
var
  LValidIP: Boolean;
begin
  Result := TryStrToIPv6(S, True, LValidIP);
end;

function IPv6Compare(const AIPv61, AIPv62: TIPv6): Integer;
var
  I: T8;
begin
  Result := 0;
  for I := High(T8) downto Low(T8) do
    if AIPv61.Groups[I] <> AIPv62.Groups[I] then
    begin
      if AIPv61.Groups[I] < AIPv62.Groups[I] then
        Result := -1
      else
        Result := 1;
      Break;
    end;
end;

procedure IPv6ToBits(const AIPv6: TIPv6; ABits: TBits);
var
  I: Integer;
  GroupBitSize: Integer;
begin
  if ABits <> nil then
  begin
    ABits.Size := IPv6BitSize;
    GroupBitSize := IPv6BitSize div Length(AIPv6.Groups);
    for I := 0 to IPv6BitSize - 1 do
      ABits[IPv6BitSize - I - 1] :=
        AIPv6.Groups[I div GroupBitSize] and (1 shl (I mod GroupBitSize)) <> 0;
  end;
end;

function IPv6ToIPv4(const AIPv6: TIPv6): TIPv4;
begin
  with AIPv6 do
    if (A > 0) or (B > 0) or (C > 0) or (D > 0) or (E > 0) or (F < $FFFF) then
      IPv6Error(SInvalidIPv6FormatType);
  Move(AIPv6.G, Result, 2 * SizeOf(Word));
end;

function IPv6ToStr(const AIPv6: TIPv6): String;
begin
  with AIPv6 do
    Result := Format('%x:%x:%x:%x:%x:%x:%x:%x', [A, B, C, D, E, F, G, H]);
  CheckCase(Result);
end;

function IPv6ToStrCompr(const AIPv6: TIPv6): String;
var
  Zeroed: Boolean;
  I: T8;
begin
  Result := '';
  Zeroed := False;
  for I := High(T8) downto Low(T8) do
  begin
    if AIPv6.Groups[I] = 0 then
    begin
      if (I = Low(T8)) then
        Result := Result + ':';
      if not Zeroed then
      begin
        Result := Result + ':';
        Zeroed := True;
      end;
    end
    else
      if (I = High(T8)) and not Zeroed then
        Result := Result + Format('%x', [AIPv6.Groups[I]])
      else
        Result := Result + Format(':%x', [AIPv6.Groups[I]]);
  end;
  CheckCase(Result);
end;

function IPv6ToStrOutwr(const AIPv6: TIPv6): String;
begin
  with AIPv6 do
    Result := Format('%.4x:%.4x:%.4x:%.4x:%.4x:%.4x:%.4x:%.4x', [A, B, C, D,
      E, F, G, H]);
  CheckCase(Result);
end;

function IPv6ToURL(const AIPv6: TIPv6; const Protocol: String = DefProtocol;
  const PortNumber: Word = DefPortNumber): String;
begin
  Result := IPv6ToStr(AIPv6);
  CheckCase(Result);
  if (PortNumber <> DefPortNumber) or (Protocol <> DefProtocol) then
    Result := Protocol + '://[' + Result + ']:' + IntToStr(PortNumber) + '/'
  else
    Result := Protocol + '://[' + Result + ']/';
end;

function IPv6AddOp(const Left, Right: TIPv6): TIPv6;
var
  I: T8;
  Sum: Integer;
  Remain: Word;
begin
  Remain :=  0;
  for I := Low(T8) to High(T8) do
  begin
    Sum := Remain + Left.Groups[I] + Right.Groups[I];
    Result.Groups[I] := Sum mod (High(Word) + 1);
    Remain := Sum div (High(Word) + 1);
  end;
end;

function IPv6AndOp(const Left, Right: TIPv6): TIPv6;
var
  I: T8;
begin
  for I := Low(T8) to High(T8) do
    Result.Groups[I] := Left.Groups[I] and Right.Groups[I];
end;

function IPv6OrOp(const Left, Right: TIPv6): TIPv6;
var
  I: T8;
begin
  for I := Low(T8) to High(T8) do
    Result.Groups[I] := Left.Groups[I] or Right.Groups[I];
end;

function IPv6SubtractOp(const Left, Right: TIPv6): TIPv6;
var
  I: T8;
  Sum: Integer;
  Lost: Word;
begin
  Lost := 0;
  for I := Low(T8) to High(T8) do
  begin
    Sum := Left.Groups[I] - Right.Groups[I] - Lost;
    if Sum < 0 then
    begin
      Inc(Sum, High(Word) + 1);
      Lost := 1;
    end
    else
      Lost := 0;
    Result.Groups[I] := Sum;
  end;
  if Lost > 0 then
    Result := ZeroIPv6;
end;

function IPv6XorOp(const Left, Right: TIPv6): TIPv6;
var
  I: T8;
begin
  for I := Low(T8) to High(T8) do
    Result.Groups[I] := Left.Groups[I] xor Right.Groups[I];
end;

// from unit uMMDBIPAddress;

const
  SBadAddressSize = 'Bad address size: %d.';
  SUnableConvertStringToIP = 'Unable to convert string "%s" to IP address.';

{ TMMDBIPAddress }

constructor TMMDBIPAddress.Create(ipv4: TIPv4);
begin
  _v6 := False;
  Move(ipv4, _ipv4, sizeof(TIPv4));
end;

constructor TMMDBIPAddress.Create(ipv6: TIPv6);
begin
  _v6 := True;
  Move(ipv6, _ipv6, sizeof(TIPv6));
end;

constructor TMMDBIPAddress.Create(const address: TBytes);
var
  I: Integer;
begin
  case Length(address) of
    4:
      begin
        _v6 := False;
        //convert network to host bytes order
        for I := 0 to 3 do
          _ipv4.Groups[3-I] := address[I];
      end;
    16:
      begin
        _v6 := True;
        //convert network to host bytes order
        for I := 0 to 7 do
          _ipv6.Groups[7-I] := (Word(address[I*2]) shl 8) or Word(address[I*2+1]);
      end;
  else
    raise Exception.Create(Format(SBadAddressSize, [Length(address)]));
  end;
end;

function TMMDBIPAddress.GetAddressBytes: TBytes;
var
  I: Integer;
begin
  if _v6 then
  begin
    SetLength(Result, 16);
    //convert host to network bytes order
    for I := 0 to 7 do
    begin
      Result[I*2]   := (_ipv6.Groups[7-I] and $ff00) shr 8;
      Result[I*2+1] := (_ipv6.Groups[7-I] and $ff);
    end;
  end else
  //if not _v6 then
  begin
    SetLength(Result, 4);
    //convert host to network bytes order
    for I := 0 to 3 do
      Result[I] := _ipv4.Groups[3-I];
  end;
end;

class function TMMDBIPAddress.Parse(const ipString: String): TMMDBIPAddress;
begin
  if not TryParse(ipString, Result) then
    raise Exception.Create(Format(SUnableConvertStringToIP, [ipString]));
end;

function TMMDBIPAddress.ToString: String;
begin
  if _v6 then
    Exit(LowerCase(IPv6ToStrCompr(_ipv6)));
  Result := IPv4ToStr(_ipv4);
end;

class function TMMDBIPAddress.TryParse(const ipString: String;
  out address: TMMDBIPAddress): Boolean;
var
  ipv4: TIPv4;
  ipv6: TIPv6;
begin
  ipv4 := TryStrToIPv4(ipString, False, Result);
  if Result then
  begin
    address := TMMDBIPAddress.Create(ipv4);
    Exit;
  end;
  ipv6 := TryStrToIPv6(ipString, False, Result);
  if Result then
  begin
    address := TMMDBIPAddress.Create(ipv6);
    Exit;
  end;
  Result := False;
end;

// from unit uMMDBReader;

type
  TMMDBArrayBuffer = class(TMMDBBuffer)
  private
    _fileBytes: TBytes;
  public
    constructor Create(const filename: String); overload;
    constructor Create(stream: TStream); overload;
    function Read(offset: Int64; count: Integer): TBytes; override;
    function ReadString(offset: Int64; count: Integer): String; override;
    function ReadOne(offset: Int64): Byte; override;
    procedure Copy(offset: Int64; arr: TBytes); override;
  end;

{ TMMDBBuffer }

constructor TMMDBBuffer.Create(length: Integer);
begin
  _Length := length;
end;

procedure ReverseBytes(Bytes: TBytes);
var
  L, R: Integer;
  tmp: Byte;
begin
  L := 0;
  R := System.Length(Bytes)-1;
  while L < R do
  begin
    tmp := Bytes[L];
    Bytes[L] := Bytes[R];
    Bytes[R] := tmp;
    Inc(L);
    Dec(R);
  end;
end;

function TMMDBBuffer.ReadBigInteger(offset: Int64; size: Integer): BigInteger;
var
  buffer: TBytes;
begin
  // This could be optimized if it ever matters
  buffer := Read(offset, size);
  ReverseBytes(buffer);
  // The integer will always be positive. We need to make sure
  // the last bit is 0.
  if (System.Length(buffer) > 0) and ((buffer[System.Length(buffer) - 1] and $80) > 0) then
    System.SetLength(buffer, System.Length(buffer) + 1);
  Result := BigInteger.Create(buffer);
end;

function TMMDBBuffer.ReadDouble(offset: Int64): Double;
var
  buffer: TBytes;
begin
  buffer := Read(offset, SizeOf(Result));
  ReverseBytes(buffer);
  Move(buffer[0], Result, SizeOf(Result));
end;

function TMMDBBuffer.ReadFloat(offset: Int64): Single;
var
  buffer: TBytes;
begin
  buffer := Read(offset, SizeOf(Result));
  ReverseBytes(buffer);
  Move(buffer[0], Result, SizeOf(Result));
end;

function TMMDBBuffer.ReadInteger(val: Integer; offset: Int64;
  size: Integer): Integer;
var
  i: Integer;
begin
  Result := val;
  for i := 0 to size-1 do
    Result := (Result shl 8) or Integer(ReadOne(offset + i));
end;

function TMMDBBuffer.ReadLong(offset: Int64; size: Integer): Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to size-1 do
    Result := (Result shl 8) or Integer(ReadOne(offset + i));
end;

function TMMDBBuffer.ReadULong(offset: Int64; size: Integer): UInt64;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to size-1 do
    Result := (Result shl 8) or Integer(ReadOne(offset + i));
end;

{ TMMDBArrayBuffer }

constructor TMMDBArrayBuffer.Create(const filename: String);
var
  LStream: TStream;
begin
  LStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    Create(LStream);
  finally
    FreeAndNil(LStream);
  end;
end;

procedure TMMDBArrayBuffer.Copy(offset: Int64; arr: TBytes);
begin
  System.Move(_fileBytes[offset], arr[0], System.Length(arr));
end;

constructor TMMDBArrayBuffer.Create(stream: TStream);
begin
  inherited Create(stream.Size);
  if stream.Size > 0 then
  begin
    System.SetLength(_fileBytes, stream.Size);
    stream.Position := 0;
    Stream.ReadBuffer(_fileBytes, stream.Size);
  end;
end;

function TMMDBArrayBuffer.Read(offset: Int64; count: Integer): TBytes;
begin
  System.SetLength(Result, count);
  Copy(offset, Result);
end;

function TMMDBArrayBuffer.ReadOne(offset: Int64): Byte;
begin
  Result := _fileBytes[offset];
end;

function TMMDBArrayBuffer.ReadString(offset: Int64; count: Integer): String;
begin
  Result := TEncoding.UTF8.GetString(_fileBytes, offset, count);
end;

{ TMMDBDecoder }

procedure TMMDBDecoder.CheckType(expected, from: PTypeInfo);
begin
  if expected <> from then
  begin
    if expected = TypeInfo(TValue) then Exit;
    raise EMMDBException.Create(Format(
      'Could not convert ''%s'' to ''%s''.',
      [from.Name, expected.Name]));
  end;
end;

constructor TMMDBDecoder.Create(ownerObjects: TObjectList;
  database: TMMDBBuffer; pointerBase: Int64; followPointers: Boolean);
begin
  _pointerBase := pointerBase;
  _database := database;
  _followPointers := followPointers;
  FOwnerObjects := ownerObjects;
end;

{$IFDEF DEBUG_OUT}
class constructor TMMDBDecoder.Create;
begin
  FDebugOutCS := TCriticalSection.Create;
{$IFDEF DEBUG_OUT_FILE}
  FDebugFileStream := TFileStream.Create(ExtractFileDir(ParamStr(0)) + '\' + DebugOutFile, fmCreate or fmShareDenyWrite);
{$ELSE}
  FDebugFileStream := nil;
{$ENDIF}
{$IFDEF DEBUG_OUT_FILTER}
  FDebugFilterRegEx := TRegEx.Create(DebugOutFilter);
{$ENDIF}
end;
{$ENDIF}

function TMMDBDecoder.CtrlData(offset: Int64; out size: Integer;
  out outOffset: Int64): ObjectType;
var
  ctrlByte: Byte;
  _type: ObjectType;
  nextByte: Integer;
  typeNum: Integer;
  bytesToRead: Integer;
  i: Integer;
begin
  if offset >= _database.Length then
    raise EMMDBException.Create(
      'The MaxMind DB file''s data section contains bad data: '
      + 'pointer larger than the database.');
  ctrlByte := _database.ReadOne(offset);
  Inc(offset);
  _type := ObjectType(ctrlByte shr 5);
  if _type = ObjectType.otExtended then
  begin
    nextByte := _database.ReadOne(offset);
    typeNum := nextByte + 7;
    if typeNum < 8 then
      raise EMMDBException.Create(
        'Something went horribly wrong in the decoder. An extended type '
        + 'resolved to a type number < 8 (' + IntToStr(typeNum)
        + ')');
    _type := ObjectType(typeNum);
    Inc(offset);
  end;
  // The size calculation is inlined as it is hot code
  size := ctrlByte and $1f;
  if size >= 29 then
  begin
    bytesToRead := size - 28;
    i := _database.ReadInteger(0, offset, bytesToRead);
    offset := offset + bytesToRead;
    case size of
      29: size := 29 + i;
      30: size := 285 + i;
    else
          size := 65821 + (i and ($0FFFFFFF shr (32 - 8 * bytesToRead)));
    end;
  end;
  outOffset := offset;
  Result := _type;
end;

{$IFDEF DEBUG_OUT}
procedure TMMDBDecoder.DebugOutput(const S: String);
{$IFDEF DEBUG_OUT_FILE}
var
  B: TBytes;
{$ENDIF}
begin
  if Length(S) < 1 then Exit;
{$IFDEF DEBUG_OUT_FILTER}
  if not FDebugFilterRegEx.IsMatch(S) then Exit;
{$ENDIF}
{$IFDEF DEBUG_OUT_FILE}
   FDebugOutCS.Acquire;
   try
     B := TEncoding.UTF8.GetBytes(S + sLineBreak);
     FDebugFileStream.Write(B, Length(B));
   finally
     FDebugOutCS.Release;
   end;
{$ELSE}
  OutputDebugString(PChar(S));
{$ENDIF}
end;
{$ENDIF}

procedure TMMDBDecoder.Decode(expectedType: PTypeInfo; offset: Int64;
  out outOffset: Int64; var valResult: TValue{$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF});
var
  _type: ObjectType;
  size: Integer;
begin
  _type := CtrlData(offset, size, offset);
  DecodeByType(expectedType, _type, offset, size, outOffset, valResult{$IFDEF DEBUG_OUT}, keyPath{$ENDIF});
end;

procedure TMMDBDecoder.Decode<T>(offset: Int64; out outOffset: Int64;
  var tResult: T);
var
  val: TValue;
begin
  val := TValue.From<T>(tResult);
  Decode(TypeInfo(T), offset, outOffset, val{$IFDEF DEBUG_OUT}, ''{$ENDIF});
end;

function TMMDBDecoder.Decode(expectedType: PTypeInfo; offset: Int64;
  out outOffset: Int64{$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF}): TValue;
var
  _type: ObjectType;
  size: Integer;
  expectedRttiType: TRttiType;
  et: TRttiInstanceType;
begin
  _type := CtrlData(offset, size, offset);
  if expectedType.Kind = tkClass then
  begin
    expectedRttiType := FContext.GetType(expectedType);
    et := expectedRttiType.AsInstance;
    Result := et.GetMethod('Create').Invoke(et.MetaclassType, []);
    try
      DecodeByType(expectedType, _type, offset, size, outOffset, Result{$IFDEF DEBUG_OUT}, keyPath{$ENDIF});
    except
      raise;
    end;
  end else
    Result := DecodeByType(expectedType, _type, offset, size, outOffset{$IFDEF DEBUG_OUT}, keyPath{$ENDIF});
end;

function TMMDBDecoder.Decode<T>(offset: Int64; out outOffset: Int64): T;
var
  _type: ObjectType;
  size: Integer;
  val: TValue;
begin
  _type := CtrlData(offset, size, offset);
  val := DecodeByType(TypeInfo(T), _type, offset, size, outOffset{$IFDEF DEBUG_OUT}, ''{$ENDIF});
  Result := val.AsType<T>;
end;

function TMMDBDecoder.DecodeArray(expectedType: PTypeInfo; size: Integer;
  offset: Int64; out outOffset: Int64{$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF}): TObject;
var
  expectedRttiType: TRttiType;
  t: TRttiInstanceType;
begin
  expectedRttiType := FContext.GetType(expectedType);
  t := expectedRttiType.AsInstance;
  Result := t.GetMethod('Create').Invoke(t.MetaclassType, []).AsObject;
  if Assigned(FOwnerObjects) then FOwnerObjects.Add(Result);
  DecodeArray(expectedType, size, offset, outOffset, Result{$IFDEF DEBUG_OUT}, keyPath{$ENDIF});
end;

procedure TMMDBDecoder.DecodeArray(expectedType: PTypeInfo; size: Integer;
  offset: Int64; out outOffset: Int64; objResult: TObject{$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF});
var
  expectedRttiType: TRttiType;
  t: TRttiInstanceType;
  clearMethod: TRttiMethod;
  addMethod: TRttiMethod;
  addParams: TArray<TRttiParameter>;
  paramValue: TRttiParameter;
  valueType: PTypeInfo;
  val: TValue;
  i: Integer;
begin
  expectedRttiType := FContext.GetType(expectedType);
  t := expectedRttiType.AsInstance;
  if not GetInstanceTypeMethod(t, 'Clear', clearMethod, 0)
    then Exit;
  clearMethod.Invoke(objResult, []);
  if not GetInstanceTypeMethod(t, 'Add', addMethod, 1)
    then Exit;
  addParams := addMethod.GetParameters;
  paramValue := addParams[0];
  valueType := paramValue.ParamType.Handle;
  for i := 0 to size-1 do
  begin
    val := Decode(valueType, offset, offset{$IFDEF DEBUG_OUT}, keyPath + '[' + IntToStr(i) + ']'{$ENDIF});
    addMethod.Invoke(objResult, [val]);
  end;
  outOffset := offset;
end;

function TMMDBDecoder.DecodeBigInteger(expectedType: PTypeInfo; offset: Int64;
  size: Integer): BigInteger;
begin
  raise EMMDBException.Create('Unsupported method: DecodeBigInteger');
end;

function TMMDBDecoder.DecodeBoolean(expectedType: PTypeInfo;
  size: Integer): Boolean;
begin
  CheckType(expectedType, TypeInfo(Boolean));
  case size of
    0: Exit(False);
    1: Exit(True);
  else
    raise EMMDBException.Create('The MaxMind DB file''s data section contains bad data: '
      + 'invalid size of boolean.');
  end;
end;

function TMMDBDecoder.DecodeBytes(expectedType: PTypeInfo; offset: Int64;
  size: Integer): TBytes;
begin
  CheckType(expectedType, TypeInfo(TBytes));
  Result := _database.Read(offset, size);
end;

function TMMDBDecoder.DecodeByType(expectedType: PTypeInfo; _type: ObjectType;
  offset: Int64; size: Integer; out outOffset: Int64{$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF}): TValue;
begin
  Result := TValue.Empty;
  DecodeByType(expectedType, _type, offset, size, outOffset, Result{$IFDEF DEBUG_OUT}, keyPath{$ENDIF});
end;

procedure TMMDBDecoder.DecodeByType(expectedType: PTypeInfo; _type: ObjectType;
  offset: Int64; size: Integer; out outOffset: Int64; var valResult: TValue
  {$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF});
var
  pointer: Int64;
  _: Int64;
begin
  outOffset := offset + size;
  case _type of
    ObjectType.otPointer:
      begin
        pointer := DecodePointer(offset, size, offset);
        outOffset := offset;
        if (not _followPointers) then
        begin
          TValue.Make(pointer, expectedType, valResult);
          Exit;
        end;
        Decode(expectedType, Integer(pointer), _, valResult{$IFDEF DEBUG_OUT}, keyPath{$ENDIF});
      end;
    ObjectType.otMap:
      if not valResult.IsEmpty then
        DecodeMap(expectedType, offset, size, outOffset, valResult.AsObject{$IFDEF DEBUG_OUT}, keyPath{$ENDIF})
      else
        valResult := DecodeMap(expectedType, offset, size, outOffset{$IFDEF DEBUG_OUT}, keyPath{$ENDIF});

    ObjectType.otArray:
      if not valResult.IsEmpty then
        DecodeArray(expectedType, size, offset, outOffset, valResult.AsObject{$IFDEF DEBUG_OUT}, keyPath{$ENDIF})
      else
        valResult := DecodeArray(expectedType, size, offset, outOffset{$IFDEF DEBUG_OUT}, keyPath{$ENDIF});

    ObjectType.otBoolean:
      begin
        outOffset := offset;
        valResult := DecodeBoolean(expectedType, size);
      end;

    ObjectType.otUtf8String:
      valResult := DecodeString(expectedType, offset, size);

    ObjectType.otDouble:
      valResult := DecodeDouble(expectedType, offset, size);

    ObjectType.otFloat:
      valResult := DecodeFloat(expectedType, offset, size);

    ObjectType.otBytes:
      valResult := TValue.From<TBytes>(DecodeBytes(expectedType, offset, size));

    ObjectType.otUint16:
      valResult := DecodeInteger(expectedType, offset, size);

    ObjectType.otUint32:
      valResult := DecodeLong(expectedType, offset, size);

    ObjectType.otInt32:
      valResult := DecodeInteger(expectedType, offset, size);

    ObjectType.otUint64:
      valResult := DecodeUInt64(expectedType, offset, size);

    ObjectType.otUint128:
      valResult := TValue.From<BigInteger>(DecodeBigInteger(expectedType, offset, size));

  else
     raise EMMDBException.Create('Unable to handle type:' + IntToStr(Ord(_type)));
  end;
end;

function TMMDBDecoder.DecodeDouble(expectedType: PTypeInfo; offset: Int64;
  size: Integer): Double;
begin
  CheckType(expectedType, TypeInfo(Double));
  if (size <> 8) then
    raise EMMDBException.Create('The MaxMind DB file''s data section contains bad data: '
      + 'invalid size of double.');
  Result := _database.ReadDouble(offset);
end;

function TMMDBDecoder.DecodeFloat(expectedType: PTypeInfo; offset: Int64;
  size: Integer): Single;
begin
  CheckType(expectedType, TypeInfo(Single));
  if (size <> 4) then
    raise EMMDBException.Create('The MaxMind DB file''s data section contains bad data: '
      + 'invalid size of float.');
  Result := _database.ReadFloat(offset);
end;

function TMMDBDecoder.DecodeInteger(expectedType: PTypeInfo; offset: Int64;
  size: Integer): Integer;
begin
  CheckType(expectedType, TypeInfo(Integer));
  Result := _database.ReadInteger(0, offset, size);
end;

function TMMDBDecoder.DecodeKey(offset: Int64; out outOffset: Int64): TBytes;
var
  _type: ObjectType;
  size: Integer;
begin
  _type := CtrlData(offset, size, offset);
  case _type of
    ObjectType.otPointer:
      begin
        offset := DecodePointer(offset, size, outOffset);
        Exit(DecodeKey(offset, offset));
      end;

    ObjectType.otUtf8String:
      begin
        outOffset := offset + size;
        Exit(_database.Read(offset, size));
      end

    else
      raise EMMDBException.Create('Database contains a non-string as map key: '+IntToStr(Ord(_type)));
  end;
end;

function TMMDBDecoder.DecodeLong(expectedType: PTypeInfo; offset: Int64;
  size: Integer): Int64;
begin
  CheckType(expectedType, TypeInfo(Int64));
  Result := _database.ReadLong(offset, size);
end;

procedure TMMDBDecoder.DecodeMap(expectedType: PTypeInfo; offset: Int64;
  size: Integer; out outOffset: Int64; objResult: TObject
  {$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF});
begin
  if IsDictType(expectedType) then
    DecodeMapToDictionary(expectedType, offset, size, outOffset, objResult{$IFDEF DEBUG_OUT}, keyPath{$ENDIF})
  else
    DecodeMapToType(expectedType, offset, size, outOffset, objResult{$IFDEF DEBUG_OUT}, keyPath{$ENDIF});
end;

function TMMDBDecoder.DecodeMap(expectedType: PTypeInfo; offset: Int64;
  size: Integer; out outOffset: Int64
  {$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF}): TObject;
begin
  if IsDictType(expectedType) then
    Exit(DecodeMapToDictionary(expectedType, offset, size, outOffset{$IFDEF DEBUG_OUT}, keyPath{$ENDIF}));
  Result := DecodeMapToType(expectedType, offset, size, outOffset{$IFDEF DEBUG_OUT}, keyPath{$ENDIF});
end;

function TMMDBDecoder.DecodeMapToDictionary(expectedType: PTypeInfo;
  offset: Int64; size: Integer; out outOffset: Int64
  {$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF}): TObject;
var
  expectedRttiType: TRttiType;
  t: TRttiInstanceType;
begin
  expectedRttiType := FContext.GetType(expectedType);
  t := expectedRttiType.AsInstance;
  Result := t.GetMethod('Create').Invoke(t.MetaclassType, [0{ACapacity=0}]).AsObject;
  if Assigned(FOwnerObjects) then FOwnerObjects.Add(Result);
  DecodeMapToDictionary(expectedType, offset, size, outOffset, Result{$IFDEF DEBUG_OUT}, keyPath{$ENDIF});
end;

procedure TMMDBDecoder.DecodeMapToDictionary(expectedType: PTypeInfo;
  offset: Int64; size: Integer; out outOffset: Int64; objResult: TObject
  {$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF});
var
  expectedRttiType: TRttiType;
  t: TRttiInstanceType;
  clearMethod: TRttiMethod;
  addMethod: TRttiMethod;
  addParams: TArray<TRttiParameter>;
  paramKey: TRttiParameter;
  paramValue: TRttiParameter;
  keyType: PTypeInfo;
  valueType: PTypeInfo;
  key: TValue;
  val: TValue;
  i: Integer;
begin
  expectedRttiType := FContext.GetType(expectedType);
  t := expectedRttiType.AsInstance;
  if not GetInstanceTypeMethod(t, 'Clear', clearMethod, 0)
    then Exit;
  clearMethod.Invoke(objResult, []);
  if not GetInstanceTypeMethod(t, 'Add', addMethod, 2)
    then Exit;
  addParams := addMethod.GetParameters;
  paramKey := addParams[0];
  paramValue := addParams[1];
  keyType := paramKey.ParamType.Handle;
  valueType := paramValue.ParamType.Handle;
  for i := 0 to size-1 do
  begin
    key := Decode(keyType, offset, offset{$IFDEF DEBUG_OUT}, keyPath + '[' + IntToStr(i) + '].key'{$ENDIF});
    val := Decode(valueType, offset, offset{$IFDEF DEBUG_OUT}, keyPath + '[' + IntToStr(i) + '].value'{$ENDIF});
{$IFDEF DEBUG_OUT}
    DebugOutput(expectedRttiType.Name + '.' + key.ToString + ':' + paramValue.ParamType.Name + ' = ' + val.ToString);
{$ENDIF}
    addMethod.Invoke(objResult, [key, val]);
  end;
  outOffset := offset;
end;

procedure TMMDBDecoder.DecodeMapToType(expectedType: PTypeInfo; offset: Int64;
  size: Integer; out outOffset: Int64; objResult: TObject{$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF});
var
  key: String; //TBytes;
  expectedRttiType: TRttiType;
  p, prop: TRttiProperty;
  ca: TCustomAttribute;
  //attr: TMMDBAttribute;
  val: TValue;
  i: Integer;
begin
  expectedRttiType := FContext.GetType(expectedType);
  for i := 0 to size-1 do
  begin
    key := TEncoding.UTF8.GetString(DecodeKey(offset, offset));
    //prop := expectedRttiType.GetProperty(key);
    prop := nil;
    //attr := nil;
    for p in expectedRttiType.GetProperties do
    begin
      for ca in p.GetAttributes do
      begin
        if (ca is TMMDBAttribute) and
           SameText(TMMDBAttribute(ca).Name, key) then
        begin
          //attr := TMMDBAttribute(ca);
          prop := p;
          Break;
        end;
      end;
      if prop <> nil then Break;
    end;
    if prop <> nil then
    begin
      if prop.IsReadable and
        (prop.PropertyType.TypeKind = tkClass) then
      begin
        val := prop.GetValue(objResult);
        if not val.IsEmpty then
        begin
{$IFDEF DEBUG_OUT}
          DebugOutput(expectedRttiType.Name + '.' + prop.Name + ':' + prop.PropertyType.Name + ' = ' + val.ToString);
{$ENDIF}
          Decode(prop.PropertyType.Handle, offset, offset, val{$IFDEF DEBUG_OUT}, keyPath + '.' + key{$ENDIF});
          Continue;
        end;
      end;
      if prop.IsWritable then
      begin
        val := Decode(prop.PropertyType.Handle, offset, offset{$IFDEF DEBUG_OUT}, keyPath + '.' + key{$ENDIF});
{$IFDEF DEBUG_OUT}
        DebugOutput(expectedRttiType.Name + '.' + prop.Name + ':' + prop.PropertyType.Name + ' = ' + val.ToString);
{$ENDIF}
        prop.SetValue(objResult, val);
        Continue;
      end;
{$IFDEF DEBUG_OUT}
      DebugOutput('WARNING: ' + expectedRttiType.Name + '.' + prop.Name + ' NOT WRITABLE');
{$ENDIF}
    end else
    begin
{$IFDEF DEBUG_OUT}
      DebugOutput('WARNING: ' + expectedRttiType.Name + ' NOT FOUND (' + keyPath + '.' + key +')');
{$ENDIF}
    end;
    offset := NextValueOffset(offset, 1);
  end;
  outOffset := offset;
end;

function TMMDBDecoder.DecodeMapToType(expectedType: PTypeInfo; offset: Int64;
  size: Integer; out outOffset: Int64{$IFDEF DEBUG_OUT}; const keyPath: String{$ENDIF}): TObject;
var
  expectedRttiType: TRttiType;
  t: TRttiInstanceType;
begin
  expectedRttiType := FContext.GetType(expectedType);
  t := expectedRttiType.AsInstance;
  Result := t.GetMethod('Create').Invoke(t.MetaclassType, []).AsObject;
  if Assigned(FOwnerObjects) then FOwnerObjects.Add(Result);
  DecodeMapToType(expectedType, offset, size, outOffset, Result{$IFDEF DEBUG_OUT}, keyPath{$ENDIF});
end;

function TMMDBDecoder.DecodePointer(offset: Int64; size: Integer;
  out outOffset: Int64): Int64;
const
  _pointerValueOffset: TArray<Integer> = [ 0, 0, 1 shl 11, (1 shl 19) + (1 shl 11), 0 ];
var
  pointerSize: Integer;
  b: Integer;
  _packed: Integer;
begin
  pointerSize := ((size shr 3) and $03) + 1;
  b := IfThen(pointerSize = 4, 0, size and $07);
  _packed := _database.ReadInteger(b, offset, pointerSize);
  outOffset := offset + pointerSize;
  Result := _packed + _pointerBase + _pointerValueOffset[pointerSize];
end;

function TMMDBDecoder.DecodeString(expectedType: PTypeInfo; offset: Int64;
  size: Integer): String;
begin
  CheckType(expectedType, TypeInfo(String));
  Result := _database.ReadString(offset, size);
end;

function TMMDBDecoder.DecodeUInt64(expectedType: PTypeInfo; offset: Int64;
  size: Integer): UInt64;
begin
  CheckType(expectedType, TypeInfo(UInt64));
  Result := _database.ReadULong(offset, size);
end;

{$IFDEF DEBUG_OUT}
class destructor TMMDBDecoder.Destroy;
begin
  if Assigned(FDebugFileStream) then
    FDebugFileStream.Free;
  FDebugOutCS.Free;
end;
{$ENDIF}

function TMMDBDecoder.GetInstanceTypeMethod(instance: TRttiInstanceType;
  const methodName: String; out method: TRttiMethod;
  paramCount: Integer): Boolean;
var
  params: TArray<TRttiParameter>;
begin
  method := instance.GetMethod(methodName);
  if not Assigned(method) then
  begin
{$IFDEF DEBUG_OUT}
    DebugOutput(instance.Name+'.'+methodName+' method does not exist.');
{$ENDIF}
    //raise EMMDBException.Create(instance.Name+'.'+methodName+' method does not exist.');
    Exit(False);
  end;
  if paramCount >= 0 then
  begin
    params := method.GetParameters;
    if Length(params) <> paramCount then
    begin
{$IFDEF DEBUG_OUT}
      DebugOutput('Bad '+instance.Name+'.'+methodName+' parameters count.');
{$ENDIF}
      //raise EMMDBException.Create('Bad '+instance.Name+'.'+methodName+' parameters count.');
      Exit(False);
    end;
  end;
  Result := True;
end;

function TMMDBDecoder.IsDictType(expected: PTypeInfo): Boolean;
var
  expectedRttiType: TRttiType;
begin
//  Result :=
//    (expected = TypeInfo(TDictionary<string, string>)) or
//    (expected = TypeInfo(TDictionary<string, TObject>));
  expectedRttiType := FContext.GetType(expected);
  Result := expectedRttiType.Name.StartsWith('TDictionary<');
end;

function TMMDBDecoder.NextValueOffset(offset: Int64;
  numberToSkip: Integer): Int64;
var
  _type: ObjectType;
  size: Integer;
begin
  while true do
  begin
    if numberToSkip = 0 then
    begin
      Exit(offset);
    end;

    _type := CtrlData(offset, size, offset);

    case _type of
      ObjectType.otPointer:
        DecodePointer(offset, size, offset);

      ObjectType.otMap:
        Inc(numberToSkip, 2 * size);

      ObjectType.otArray:
        Inc(numberToSkip, size);

      ObjectType.otBoolean: ;

      else
        Inc(offset, size);
    end;

    numberToSkip := numberToSkip - 1;
  end;
end;

{ TMMDBAttribute }

constructor TMMDBAttribute.Create(const AName: String);
begin
  FName := AName;
end;

{ TMMDBMetadata }

constructor TMMDBMetadata.Create;
begin
  _Description := TDictionary<string, string>.Create;
  _Languages := TList<string>.Create;
end;

destructor TMMDBMetadata.Destroy;
begin
  _Languages.Free;
  _Description.Free;
  inherited;
end;

function TMMDBMetadata.GetBuildDate: TDateTime;
begin
  Result := IncSecond(EncodeDate(1970, 1, 1), _BuildEpoch);
end;

function TMMDBMetadata.GetNodeByteSize: Int64;
begin
  Result := _RecordSize div 4;
end;

function TMMDBMetadata.GetSearchTreeSize: Int64;
begin
  Result := _NodeCount * NodeByteSize;
end;

{ TMMDBNetNode }

constructor TMMDBNetNode.Create(byteCount, Pointer: Integer);
begin
  SetLength(_IPBytes, byteCount);
  FillChar(_IPBytes[0], Length(_IPBytes), 0);
  _Bit := 0;
  _Pointer := Pointer;
end;

constructor TMMDBNetNode.Create(IPBytes: TBytes; Bit, Pointer: Integer);
begin
  SetLength(_IPBytes, Length(IPBytes));
  Move(IPBytes[0], _IPBytes[0], Length(_IPBytes));
  _Bit := Bit;
  _Pointer := Pointer;
end;

destructor TMMDBNetNode.Destroy;
begin

  inherited;
end;

{ TMMDBIteratorNode<T> }

constructor TMMDBIteratorNode<T>.Create(data: T);
begin
  _Data := data;
end;

destructor TMMDBIteratorNode<T>.Destroy;
begin

  inherited;
end;

function TMMDBIteratorNode<T>.GetNode: TMMDBIteratorNode<T>;
begin
  Result := Self;
end;

{ TMMDBEnumerator<T> }

constructor TMMDBEnumerator<T>.Create(reader: TMMDBReader; IPv4Only: Boolean;
  iterator: IMMDBIterator<T>; cacheSize: Integer);
var
  root: TMMDBNetNode;
begin
  inherited Create;
  _reader := reader;
  _iterator := iterator;
  if (_reader.Metadata.IPVersion = 6) and (not IPv4Only) then
    byteCount := 16
  else
    byteCount := 4;
  nodes := TList<TMMDBNetNode>.Create;
  root := TMMDBNetNode.Create(byteCount, _reader.StartNode(byteCount * 8));
  nodes.Add(root);
  node := nil;
  _dataCache := TDictionary<Integer, T>.Create;
end;

destructor TMMDBEnumerator<T>.Destroy;
var
  tmp: TMMDBNetNode;
begin
  _dataCache.Free;
  if node <> nil then
     FreeAndNil(node);
  while nodes.Count > 0 do
  begin
    tmp := nodes[nodes.Count-1];
    nodes.Delete(nodes.Count-1);
    tmp.Free;
  end;
  nodes.Free;
  _iterator := nil;
  inherited;
end;

function TMMDBEnumerator<T>.getCurrent: TObject;
begin
  Result := nil;
end;

function TMMDBEnumerator<T>.GetCurrentGeneric: IMMDBIterator<T>;
begin
  if (node <> nil) and (node.Pointer > _reader.Metadata.NodeCount) then
  begin
    Exit(_iterator);
  end;
  Result := nil;
end;

function TMMDBEnumerator<T>.MoveNext: Boolean;
var
  ipRight: TBytes;
  rightPointer: Integer;
  rightNode: TMMDBNetNode;
  isIPV4: Boolean;
  i: Integer;
begin
  if node <> nil then
    FreeAndNil(node);
  while nodes.Count > 0 do
  begin
    node := nodes[nodes.Count-1];
    nodes.Delete(nodes.Count-1);
    while True do
    begin
      if node.Pointer < _reader.Metadata.NodeCount then
      begin
        SetLength(ipRight, byteCount);
        Move(node.IPBytes[0], ipRight[0], Length(ipRight));
        if Length(ipRight) <= (node.Bit shr 3) then
          raise EMMDBException.Create(Format(
            'Invalid search tree, bad bit %d', [node.Bit]));
        ipRight[node.Bit shr 3] := ipRight[node.Bit shr 3] or
          Byte(1 shl (7 - (node.Bit mod 8)));
        rightPointer := _reader.ReadNode(node.Pointer, 1);
        node.Bit := node.Bit + 1;
        rightNode := TMMDBNetNode.Create(ipRight, node.Bit, rightPointer);
        nodes.Add(rightNode);
        node.Pointer := _reader.ReadNode(node.Pointer, 0);
      end else
      begin
        if node.Pointer > _reader.Metadata.NodeCount then
        begin
          // data node, we are done with this branch
          if _iterator = nil then
          begin
            _iterator := TMMDBIteratorNode<T>.Create;
            _iterator.Node._Data := _reader.ResolveDataPointer<T>(node.Pointer);
          end else
            _reader.ResolveDataPointer<T>(node.Pointer, _iterator.Node._Data);
          isIPV4 := true;
          for i := 0 to Length(node.IPBytes) - 4 do
          begin
            if node.IPBytes[i] = 0 then Continue;
            isIPV4 := False;
            break;
          end;
          if ((not isIPV4) or (Length(node.IPBytes) = 4)) then
          begin
            _iterator.Node._Start := TMMDBIPAddress.Create(node.IPBytes);
            _iterator.Node._Prefix := node._Bit;
          end else
          begin
            _iterator.Node._Start := TMMDBIPAddress.Create(Copy(node.IPBytes, 12, 4));
            _iterator.Node._Prefix := node._Bit - 16;
          end;
          Exit(True);
        end;
        // else node is an empty node (terminator node), we are done with this branch
        Break;
      end;
    end;
    FreeAndNil(node);
  end;
  Result := False;
end;

procedure TMMDBEnumerator<T>.Reset;
var
  tmp, root: TMMDBNetNode;
begin
  if node <> nil then
    FreeAndNil(node);
  while nodes.Count > 0 do
  begin
    tmp := nodes[nodes.Count-1];
    nodes.Delete(nodes.Count-1);
    tmp.Free;
  end;
  root := TMMDBNetNode.Create(byteCount, _reader.StartNode(byteCount * 8));
  nodes.Add(root);
end;

{ TMMDBEnumerable<T> }

constructor TMMDBEnumerable<T>.Create(reader: TMMDBReader; IPv4Only: Boolean;
  iterator: IMMDBIterator<T>; cacheSize: Integer);
begin
  inherited Create;
  _reader := reader;
  _IPv4Only := IPv4Only;
  _iterator := iterator;
  _cacheSize := cacheSize;
end;

destructor TMMDBEnumerable<T>.Destroy;
begin

  inherited;
end;

function TMMDBEnumerable<T>.GetEnumerator: IEnumerator;
begin
  Result := nil;
end;

function TMMDBEnumerable<T>.GetEnumeratorGeneric: IEnumerator<IMMDBIterator<T>>;
begin
  Result := TMMDBEnumerator<T>.Create(_reader, _IPv4Only, _iterator, _cacheSize);
end;

{ TMMDBReader }

constructor TMMDBReader.Create(const Filename: String);
var
  start: Integer;
  metaDecode: TMMDBDecoder;
  _: Int64;
begin
  _fileName := Filename;
  _database := TMMDBArrayBuffer.Create(Filename);
  start := FindMetadataStart;
  FFindOwnerObjects := TObjectList.Create(True);
  FMetaOwnerObjects := TObjectList.Create(True);
  metaDecode := TMMDBDecoder.Create(FMetaOwnerObjects, _database, start);
  try
    //FMetadata := metaDecode.Decode<TMMDBMetadata>(start, _);
    FMetadata := TMMDBMetadata.Create;
    metaDecode.Decode<TMMDBMetadata>(start, _, FMetadata);
    FDecoder := TMMDBDecoder.Create(FFindOwnerObjects, _database, Metadata.SearchTreeSize + DataSectionSeparatorSize);
  finally
    metaDecode.Free;
  end;
end;

constructor TMMDBReader.Create(Stream: TStream);           { V9.5 }
var
  start: Integer;
  metaDecode: TMMDBDecoder;
  _: Int64;
begin
  _fileName := 'Streamed';
  _database := TMMDBArrayBuffer.Create(Stream);
  start := FindMetadataStart;
  FFindOwnerObjects := TObjectList.Create(True);
  FMetaOwnerObjects := TObjectList.Create(True);
  metaDecode := TMMDBDecoder.Create(FMetaOwnerObjects, _database, start);
  try
    //FMetadata := metaDecode.Decode<TMMDBMetadata>(start, _);
    FMetadata := TMMDBMetadata.Create;
    metaDecode.Decode<TMMDBMetadata>(start, _, FMetadata);
    FDecoder := TMMDBDecoder.Create(FFindOwnerObjects, _database, Metadata.SearchTreeSize + DataSectionSeparatorSize);
  finally
    metaDecode.Free;
  end;
end;

destructor TMMDBReader.Destroy;
begin
  if Assigned(FDecoder) then
    FDecoder.Free;
  if Assigned(FMetadata) then
    FMetadata.Free;
  FMetaOwnerObjects.Free;
  FFindOwnerObjects.Free;
  _database.Free;
  inherited;
end;

function TMMDBReader.Find<T>(const ipAddress: TMMDBIPAddress;
  out prefixLength: Integer): T;
var
  pointer: Integer;
begin
  pointer := FindAddressInTree(ipAddress, prefixLength);
  if pointer = 0 then
    raise EMMDBNotFoundException.Create('Not Found');
  Result := ResolveDataPointer<T>(pointer);
end;

function TMMDBReader.Find<T>(const ipAddress: TMMDBIPAddress;
  out prefixLength: Integer; var tResult: T): Boolean;
var
  pointer: Integer;
begin
  pointer := FindAddressInTree(ipAddress, prefixLength);
  if pointer = 0 then Exit(False);
  ResolveDataPointer<T>(pointer, tResult);
  Result := True;
end;

function TMMDBReader.FindAddressInTree(const address: TMMDBIPAddress;
  out prefixLength: Integer): Integer;
var
  rawAddress: TBytes;
  bitLength: Integer;
  _record: Int64;
  nodeCount: Int64;
  i: Integer;
  bit: Byte;
begin
  rawAddress := address.GetAddressBytes;
  bitLength := Length(rawAddress) * 8;
  _record := StartNode(bitLength);
  nodeCount := Metadata.NodeCount;
  for i := 0 to bitLength-1 do
  begin
    if _record >= nodeCount then Break;
    bit := 1 and (rawAddress[i shr 3] shr (7 - (i mod 8)));
    _record := ReadNode(_record, bit);
  end;
  prefixLength := i;
  if _record = Metadata.NodeCount then
  begin
    //record is empty
    Exit(0);
  end;
  if _record > Metadata.NodeCount then
  begin
    //record is a data pointer
    Exit(_record);
  end;
  raise EMMDBException.Create('Something bad happened');
end;

function TMMDBReader.FindAll<T>(data: T; IPv4Only: Boolean;
  cacheSize: Integer): IEnumerable<IMMDBIterator<T>>;
begin
  Result := TMMDBEnumerable<T>.Create(Self, IPv4Only,
    TMMDBIteratorNode<T>.Create(data), cacheSize);
end;

function TMMDBReader.FindAll<T>(IPv4Only: Boolean;
  cacheSize: Integer): IEnumerable<IMMDBIterator<T>>;
begin
  Result := TMMDBEnumerable<T>.Create(Self, IPv4Only, nil, cacheSize);
end;

function TMMDBReader.FindMetadataStart: Integer;
const
  _metadataStartMarker: TBytes =
    [ $AB, $CD, $EF, 77, 97, 120, 77, 105, 110, 100, 46, 99, 111, 109 ];
var
  I: Integer;
  buffer: TBytes;
begin
  System.SetLength(buffer, System.Length(_metadataStartMarker));
  I := _database.Length - System.Length(_metadataStartMarker);
  while I >= 0 do
  begin
    _database.Copy(I, buffer);
    if CompareMem(buffer, _metadataStartMarker, System.Length(_metadataStartMarker)) then
    begin
      Exit(I + System.Length(_metadataStartMarker));
    end;
    Dec(I);
  end;
  raise EMMDBException.Create(
    Format('Could not find a MaxMind Db metadata marker in this file (%s). Is this a valid MaxMind Db file?',
      [_fileName]));
end;

function TMMDBReader.IPv4Start: Integer;
var
  i, node: Integer;
begin
  if (_IPv4Start <> 0) or (Metadata.IPVersion = 4) then
    Exit(_IPv4Start);
  node := 0;
  for i := 0 to 95 do
  begin
    if node >= Metadata.NodeCount
      then Break;
    node := ReadNode(node, 0);
  end;
  _IPv4Start := node;
  Result := node;
end;

function TMMDBReader.ReadNode(nodeNumber, index: Integer): Integer;
var
  baseOffset, offset: Int64;
  size: Integer;
begin
  baseOffset := nodeNumber * Metadata.NodeByteSize;
  size := Metadata.RecordSize;
  case size of
    24:
      begin
        offset := baseOffset + index * 3;
        Exit((Integer(_database.ReadOne(offset)) shl 16) or
          (Integer(_database.ReadOne(offset + 1)) shl 8) or
          Integer(_database.ReadOne(offset + 2)));
      end;
    28:
      begin
        if (index = 0) then
          Exit((Integer(_database.ReadOne(baseOffset + 3) and $F0) shl 20) or
            (Integer(_database.ReadOne(baseOffset)) shl 16) or
            (Integer(_database.ReadOne(baseOffset + 1)) shl 8) or
            Integer(_database.ReadOne(baseOffset + 2)));
        Exit((Integer(_database.ReadOne(baseOffset + 3) and $0F) shl 24) or
          (Integer(_database.ReadOne(baseOffset + 4)) shl 16) or
          (Integer(_database.ReadOne(baseOffset + 5)) shl 8) or
          Integer(_database.ReadOne(baseOffset + 6)));
      end;
    32:
      begin
        offset := baseOffset + index * 4;
        Exit((Integer(_database.ReadOne(offset) shl 24)) or
          (Integer(_database.ReadOne(offset + 1)) shl 16) or
          (Integer(_database.ReadOne(offset + 2)) shl 8) or
          Integer(_database.ReadOne(offset + 3)));
      end;
  end;
  raise EMMDBException.Create(Format('Unknown record size: %d', [size]));
end;

procedure TMMDBReader.ResolveDataPointer<T>(pointer: Integer; var tResult: T);
var
  resolved, _: Int64;
begin
  resolved := pointer - Metadata.NodeCount + Metadata.SearchTreeSize;
  if resolved >= _database.Length then
    raise EMMDBException.Create('The MaxMind Db file''s search tree is corrupt: '
      + 'contains pointer larger than the database.');
  Decoder.Decode<T>(resolved, _, tResult);
end;

function TMMDBReader.ResolveDataPointer<T>(pointer: Integer): T;
var
  resolved, _: Int64;
begin
  resolved := pointer - Metadata.NodeCount + Metadata.SearchTreeSize;
  if resolved >= _database.Length then
    raise EMMDBException.Create('The MaxMind Db file''s search tree is corrupt: '
      + 'contains pointer larger than the database.');
  Exit(Decoder.Decode<T>(resolved, _));
end;

function TMMDBReader.StartNode(bitLength: Integer): Integer;
begin
  if (Metadata.IPVersion = 6) and (bitLength = 32)  then
    Exit(IPv4Start);
  Result := 0;
end;


{ TMMDBIPCountryInfo }

constructor TMMDBIPCountryInfo.Create;
begin
  _continent := TMMDBContinentInfo.Create;
  _country := TMMDBCountryInfo.Create;
  _registered_country := TMMDBCountryInfo.Create;
  _represented_country := TMMDBRepresentedCountryInfo.Create;
  _traits := TMMDBTraitsInfo.Create;
end;

destructor TMMDBIPCountryInfo.Destroy;
begin
  _traits.Free;
  _represented_country.Free;
  _registered_country.Free;
  _country.Free;
  _continent.Free;
  inherited;
end;

{ TMMDBContinentInfoEx }

constructor TMMDBContinentInfoEx.Create;
begin
  inherited;
  _names := TDictionary<string, string>.Create;
end;

destructor TMMDBContinentInfoEx.Destroy;
begin
  _names.Free;
  inherited;
end;

{ TMMDBCountryInfoEx }

constructor TMMDBCountryInfoEx.Create;
begin
  inherited;
  _names := TDictionary<string, string>.Create;
end;

destructor TMMDBCountryInfoEx.Destroy;
begin
  _names.Free;
  inherited;
end;

{ TMMDBIPCountryInfoEx }

constructor TMMDBIPCountryInfoEx.Create;
begin
  _continent := TMMDBContinentInfoEx.Create;
  _country := TMMDBCountryInfoEx.Create;
  _registered_country := TMMDBCountryInfoEx.Create;
  _represented_country := TMMDBRepresentedCountryInfoEx.Create;
  _traits := TMMDBTraitsInfo.Create;
end;

destructor TMMDBIPCountryInfoEx.Destroy;
begin
  _traits.Free;
  _represented_country.Free;
  _registered_country.Free;
  _country.Free;
  _continent.Free;
  inherited;
end;

{ TMMDBCityInfo }

constructor TMMDBCityInfo.Create;
begin

end;

destructor TMMDBCityInfo.Destroy;
begin

  inherited;
end;

{ TMMDBIPCountryCityInfo }

{$IFDEF DEBUG_TMMDBIPCountryCityInfo}
var
  TMMDBIPCountryCityInfo_Count: Integer = 0;
{$ENDIF}

constructor TMMDBIPCountryCityInfo.Create;
begin
  inherited;
  _city := TMMDBCityInfo.Create;
  _location := TMMDBLocation.Create;
  _postal := TMMDBPostal.Create;
  _subdivisions := TList<TMMDBSubdivision>.Create;
{$IFDEF DEBUG_TMMDBIPCountryCityInfo}
  OutputDebugString(PChar('TMMDBIPCountryCityInfo_Count: '+IntToStr(
    InterlockedIncrement(TMMDBIPCountryCityInfo_Count))));
{$ENDIF}
end;

destructor TMMDBIPCountryCityInfo.Destroy;
begin
{$IFDEF DEBUG_TMMDBIPCountryCityInfo}
  OutputDebugString(PChar('TMMDBIPCountryCityInfo_Count: '+IntToStr(
    InterlockedDecrement(TMMDBIPCountryCityInfo_Count))));
{$ENDIF}
  _subdivisions.Free;
  _postal.Free;
  _location.Free;
  _city.Free;
  inherited;
end;

{ TMMDBCityInfoEx }

constructor TMMDBCityInfoEx.Create;
begin
  inherited;
  _names := TDictionary<string, string>.Create;
end;

destructor TMMDBCityInfoEx.Destroy;
begin
  _names.Free;
  inherited;
end;

{ TMMDBIPCountryCityInfoEx }

{$IFDEF DEBUG_TMMDBIPCountryCityInfoEx}
var
  TMMDBIPCountryCityInfoEx_Count: Integer = 0;
{$ENDIF}

constructor TMMDBIPCountryCityInfoEx.Create;
begin
  inherited;
  _city := TMMDBCityInfoEx.Create;
  _location := TMMDBLocation.Create;
  _postal := TMMDBPostal.Create;
  _subdivisions := TObjectList<TMMDBSubdivisionEx>.Create(True);
{$IFDEF DEBUG_TMMDBIPCountryCityInfoEx}
  OutputDebugString(PChar('TMMDBIPCountryCityInfoEx_Count: '+IntToStr(
    InterlockedIncrement(TMMDBIPCountryCityInfoEx_Count))));
{$ENDIF}
end;

destructor TMMDBIPCountryCityInfoEx.Destroy;
begin
{$IFDEF DEBUG_TMMDBIPCountryCityInfoEx}
  OutputDebugString(PChar('TMMDBIPCountryCityInfoEx_Count: '+IntToStr(
    InterlockedDecrement(TMMDBIPCountryCityInfoEx_Count))));
{$ENDIF}
  _subdivisions.Free;
  _postal.Free;
  _location.Free;
  _city.Free;
  inherited;
end;

// ICS component

constructor TIcsGeoTools.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FLoadIPDB := False;
    FLoadCnty := False;
    FCountryFName := 'ICS-Countries.csv';   // always available as a RES file
    FCountryFile := 'C:\ProgramData\ICS-OpenSSL\ICS-Geodb\dbip-country-lite.mmdb';
    FASNFile := 'C:\ProgramData\ICS-OpenSSL\ICS-Geodb\dbip-asn-lite.mmdb';
    FCityFile := 'C:\ProgramData\ICS-OpenSSL\ICS-Geodb\dbip-city-lite.mmdb';
    FMMDBCountry  := Nil;
    FMMDBASN := Nil;
    FMMDBCity := Nil;
    FUseResources := True;
end;


destructor TIcsGeoTools.Destroy;
begin
    Unload;
    inherited Destroy;
end;

// called by TFindList for sort and find comparison, note may be two or three characters
// look in TIcsCountry database }
function CompareISOA2(Item1, Item2: Pointer): Integer;
// Compare returns < 0 if Item1 is less than Item2, 0 if they are equal and > 0 if Item1 is greater than Item2.
begin
    result := CompareText(PTIcsCountry(Item1).ISOA2, PTIcsCountry(Item2).ISOA2)
end ;


procedure TIcsGeoTools.AppLog(const Msg: string);      // application logging event
begin
    if Assigned(FOnAppLog) then
        FOnAppLog(Self, Msg);
end;


procedure TIcsGeoTools.UnLoad;      // unload databases
begin
    FLoadIPDB := False;
    FLoadCnty := False;
    FLoadAsn := False;
    FLoadCity := False;
    FreeAndNil(FMMDBCountry);
    FreeAndNil(FMMDBASN);
    FreeAndNil(FMMDBCity);
    FreeAndNil(FCityInfo);
    FreeAndNil(FCountryInfo);
    FreeAndNil(FASNInfo);
    FreeAndNil(fISOA2Idx);
//    SetLength(FCountries, 0);
end;


function TIcsGeoTools.IsLoaded: Boolean;
begin
    Result := FLoadIPDB and FLoadCnty;
end;


function TIcsGeoTools.Load: Boolean;
begin
    Result := LoadDBCountry;
    if Result then
        Result := LoadIcsCountries;
end;


function TIcsGeoTools.LoadDBCountry: Boolean;
var
    ResStream : TResourceStream;
begin
    Result := FLoadIPDB;
    if Result then
        Exit;

 // load MaxMind GEO database into memory buffer
    if NOT Assigned (FMMDBCountry) then begin
        FCountryInfo := TMMDBIPCountryInfo.Create;
        ResStream := Nil;
        if FUseResources then
            try
                ResStream := TResourceStream.Create(HInstance, 'ICSDBIPCOUNTRTYMMDB', RT_RCDATA);
            except
              // ignore error, we'll load from file instead
            end;
        try
            try
                if Assigned(ResStream) then begin
                    FMMDBCountry := TMMDBReader.Create(ResStream);
                    FCountryFile := 'dbip-country-lite';  // cosmetic
                end
                else begin
                    if (FCountryFile = '') or (NOT FileExists(FCountryFile)) then begin
                        AppLog('Can Not Load MaxMind Database File: ' + FCountryFile);
                        Exit;
                    end;
                    FMMDBCountry := TMMDBReader.Create(FCountryFile);
                end;
            except
                AppLog('Failed to Load MaxMind Database File: ' + FCountryFile + '- ' + IcsGetExceptMess (ExceptObject)) ;
                Exit;
            end;
        finally
            ResStream.Free;
        end;
        DBTot := FMMDBCountry.Metadata.NodeCount;
        DBType := FMMDBCountry.Metadata.DatabaseType;
        DBDate := UnixToDateTime(FMMDBCountry.Metadata.BuildEpoch, False);   // real time not UTC
        AppLog('Loaded MaxMind Database OK: ' + FCountryFile + ', Built: ' + DateToStr(DBDate) +
                                                       ', Nodes: ' + IcsIntToKbyte(DBTot) + ', Type: ' + DBType);
        Result := True;
        FLoadIPDB := True;
    end;
end;

function TIcsGeoTools.LoadDBASN: Boolean;
var
    ResStream : TResourceStream;
begin
    Result := FLoadASN;
    if Result then
        Exit;

 // load MaxMind GEO database into memory buffer
    if NOT Assigned (FMMDBASN) then begin
        FASNInfo := TMMDBASN.Create;
        ResStream := Nil;
        if FUseResources then
            try
                ResStream := TResourceStream.Create(HInstance, 'ICSDBASNMMDB', RT_RCDATA);
            except
              // ignore error, we'll load from file instead
            end;
        try
            try
                if Assigned(ResStream) then begin
                    FMMDBASN := TMMDBReader.Create(ResStream);
                    FASNFile := 'dbip-asn-lite';  // cosmetic
                end
                else begin
                    if (FAsnFile = '') or (NOT FileExists(FAsnFile)) then begin
                        AppLog('Can Not Load MaxMind Database File: ' + FAsnFile);
                        Exit;
                    end;
                    FMMDBASN := TMMDBReader.Create(FASNFile);
                end;
            except
                AppLog('Failed to Load MaxMind Database File: ' + FASNFile + ' - ' + IcsGetExceptMess (ExceptObject)) ;
                Exit;
            end;
        finally
            ResStream.Free;
        end;
//        DBTot := FMMDBASN.Metadata.NodeCount;
//        DBType := FMMDBASN.Metadata.DatabaseType;
//        DBDate := UnixToDateTime(FMMDBASN.Metadata.BuildEpoch);
        AppLog('Loaded MaxMind Database OK: ' + FASNFile + ', Built: ' + DateToStr(UnixToDateTime(FMMDBASN.Metadata.BuildEpoch, False)) +
                              ', Nodes: ' + IcsIntToKbyte(FMMDBASN.Metadata.NodeCount) + ', Type: ' + FMMDBASN.Metadata.DatabaseType);
        Result := True;
        FLoadASN := True;
    end;
end;


function TIcsGeoTools.LoadDBCity: Boolean;
begin
    Result := FLoadCity;
    if Result then
        Exit;

 // load MaxMind GEO database into memory buffer
    if NOT Assigned (FMMDBCity) then begin
        FCityInfo := TMMDBIPCountryCityInfoEx.Create;
        try
            if (FCityFile = '') or (NOT FileExists(FCityFile)) then begin
                AppLog('Can Not Load MaxMind Database File: ' + FCityFile);
                Exit;
            end;
            FMMDBCity := TMMDBReader.Create(FCityFile);
        except
            AppLog('Failed to Load MaxMind Database File: ' + FCityFile + ' - ' + IcsGetExceptMess (ExceptObject)) ;
            Exit;
        end;
//        DBTot := FMMDBCity.Metadata.NodeCount;
//        DBType := FMMDBCity.Metadata.DatabaseType;
//        DBDate := UnixToDateTime(FMMDBCity.Metadata.BuildEpoch);
        AppLog('Loaded MaxMind Database OK: ' + FCityFile + ', Built: ' + DateToStr(UnixToDateTime(FMMDBCity.Metadata.BuildEpoch, False)) +
                              ', Nodes: ' + IcsIntToKbyte(FMMDBCity.Metadata.NodeCount) + ', Type: ' + FMMDBCity.Metadata.DatabaseType);
        Result := True;
        FLoadCity := True;
    end;
end;


function TIcsGeoTools.LoadIcsCountries: Boolean;
var
    CountryTB: TBytes;
    CountryList: TStringList;
    OneCountry: TStringList;
    I, Ctot: Integer;
begin
    Result := FLoadCnty;
    if Result then
        Exit;

// ISO country names and codes
// GB,United Kingdom,44,,GBR,826,UK,150,154
// US,United States,1,United States of America,USA,840,US,019,021
// KR,"Korea, Republic of ",82,South,KOR,410,KR,142,030

    FTotCountries := 0;
    CountryList := TStringList.Create;
    OneCountry := TStringList.Create;
    OneCountry.StrictDelimiter := True;  // ensure space is not treated as a delimiter
    try
        try
            if NOT ASsigned(FISOA2Idx) then
                FISOA2Idx := TIcsFindList.Create;
            if FUseResources then
                CountryTB := IcsResourceGetTB('ICSCOUNTRIESTXT', RT_RCDATA);   { see if list linked into application }
            if Length(CountryTB) > 0 then begin
                CountryList.Text := IcsTBytesToString(CountryTB);
                FCountryFName := 'ICS-Countries';
            end
            else if FileExists(FCountryFName) then
                CountryList.LoadFromFile(FCountryFName);
            Ctot := CountryList.Count;
            FISOA2Idx.Clear;
            if Ctot > 0 then begin
                OneCountry.CommaText := CountryList[0];
                if (OneCountry.Count < 9) or (OneCountry[0] <> 'ISO-A2') then begin
                    AppLog('Invalid ICS-Countries file, Header Line: ' + CountryList[0]);
                    Exit;
                end;
                SetLength(FCountries, Ctot);
                for I := 1 to Ctot - 1 do begin
                    OneCountry.CommaText := CountryList[I];
                    if (OneCountry.Count < 2) then begin
                        AppLog('Too few fields in ICS-Countries: ' + CountryList[I]);
                        Continue;
                    end;
                    FCountries[FTotCountries] := Default(TIcsCountry);
                    with FCountries[FTotCountries] do begin
                        ISOA2 := Trim(OneCountry[0]);
                        Country := Trim(OneCountry[1]);
                        if (OneCountry.Count > 2) and (Trim(OneCountry[2]) <> '') then begin
                            if (OneCountry.Count < 9) then begin
                                AppLog('Too few fields in ICS-Countries: ' + CountryList[I]);
                                Continue;
                            end;
                            DialCode := Trim(OneCountry[2]);
                            AkaCountry := Trim(OneCountry[3]);
                            ISOA3 := Trim(OneCountry[4]);
                            ISONUM := Trim(OneCountry[5]);
                            Internet := Trim(OneCountry[6]);
                            Region := atoi(Trim(OneCountry[7]));
                            SubRegion := atoi(Trim(OneCountry[8]));
                            NumISO := atoi(ISONUM);
                            if (ISONUM <> '') and (ISONUM <> '?') and (NumISO = 0) then begin
                                AppLog('Invalid ISONum: ' + CountryList[I]);
                                Continue;
                            end;
                        end;
                    end;
                    FISOA2Idx.AddSorted(@FCountries[FTotCountries], CompareISOA2);
                    FTotCountries := FTotCountries + 1;
                end;
                AppLog('Loaded ICS-Countries OK, Total: ' + IntToStr(FTotCountries));
                Result := True;
                FLoadCnty := True;
            end
            else
                AppLog('Failed to load ICS-Countries: ' + FCountryFName);
        except
            AppLog('Failed to load ICS-Countries - ' + IcsGetExceptMess (ExceptObject)) ;
        end;
    finally
       CountryList.Free;
       OneCountry.Free;
    end;
end;

// lookup ISOA2 country code from IP address, either SocAddr or IP string, or both
function TIcsGeoTools.FindISOA2Code(ASockAddr: TSockAddrIn6; const IpStr: String): string;
var
    MyIPAddress: TMMDBIPAddress;
    prefixLength: Integer;
    MyIpStr: String;
    Success: Boolean;
    SocFamily: TSocketFamily;
begin
    Result := '';
    MyIPStr := IPStr;
    if ASockAddr.sin6_family = 0 then begin
        if MyIpStr = '' then
            Exit;
        ASockAddr := WSocketIPStrToSocAddr(MyIpStr, Success);
        if NOT Success then
           Exit;
    end
    else begin
        if MyIpStr = '' then
            MyIpStr := WSocketSockAddrToStr(ASockAddr);
    end;
    SocFamily := IcsFamilyFromSocAddr(ASockAddr);

// check LAN
    if (SocFamily = sfIPv4) then begin
       if IcsIsLocalIPv4(IcsIPv4AddrFromSocAddr(ASockAddr)) then begin
           Result := 'LAN';
           Exit;
       end;
    end
    else if (SocFamily = sfIPv6) then begin
       if IcsIsLocalIPv6(IcsIPv6AddrFromSocAddr(ASockAddr)) then begin
           Result := 'LAN';
           Exit;
       end;
    end
    else
       Exit;

// check database loaded
    if NOT IsLoaded then
        Exit;

// finally look up country information in MaxMind Database
    try
        MyIPAddress := TMMDBIPAddress.Parse(MyIpStr);
        if FMMDBCountry.Find<TMMDBIPCountryInfo>(MyIPAddress, prefixLength, FCountryInfo) then begin
            Result := FCountryInfo.Country.ISOCode;
            if Result = '' then
               Result := IcsUnknownISOA2;
        end;
    except
        AppLog('Exception Finding IP Address: ' + IpStr + ' - ' + IcsGetExceptMess (ExceptObject)) ;
    end;
end;


// lookup ISOA2 country code from IP string
function TIcsGeoTools.FindISOA2Code(const IpStr: String): string;
begin
    Result := FindISOA2Code(BlankSockAddr, IpStr);
end;


// lookup ISOA2 country code from SocAddr
function TIcsGeoTools.FindISOA2Code(ASockAddr: TSockAddrIn6): string;
begin
    Result := FindISOA2Code(ASockAddr, '');
end;

function TIcsGeoTools.FindASNCode(const IpStr: String; var OrgNum: Int64): string;
var
    MyIPAddress: TMMDBIPAddress;
    ASockAddr: TSockAddrIn6;
    prefixLength: Integer;
    Success: Boolean;
    SocFamily: TSocketFamily;
    AddrLong: LongWord;
begin
// check database loaded
    Result := '';
    OrgNum := 0;
    if NOT FLoadASN then
        Exit;

// not all IP ranges have an ASN, London Internet Exchange (LINX)
    ASockAddr := WSocketIPStrToSocAddr(IpStr, Success);
    if NOT Success then
       Exit;
    SocFamily := IcsFamilyFromSocAddr(ASockAddr);

// check LAN and LINX
    if (SocFamily = sfIPv4) then begin
       if IcsIsLocalIPv4(IcsIPv4AddrFromSocAddr(ASockAddr)) then begin
           Result := 'LAN';
           Exit;
       end;
//       AddrLong := LongWord(WSocket_ntohl(IcsIPv4AddrFromSocAddr(ASockAddr))); // must swap bytes to compare range
       AddrLong := VReverseBytes(IcsIPv4AddrFromSocAddr(ASockAddr)); // must swap bytes to compare range
       if ((AddrLong >= IP195_66_224_0) and (AddrLong <= IP195_66_255_255)) then begin
            OrgNum := 5459;
            Result := 'London Internet Exchange Ltd';  // LINX
            Exit;
       end;
    end;

// finally look up ASN information in MaxMind Database
// note need Find<TMMDBASN> or Find<TMMDBIPCountryCityInfo> or ASN or City databaes
    try
        MyIPAddress := TMMDBIPAddress.Parse(IpStr);
        if FMMDBASN.Find<TMMDBASN>(MyIPAddress, prefixLength, FASNInfo) then begin
            Result := FASNInfo.AutonomousSystemOrganization;
            OrgNum := FASNInfo.AutonomousSystemNumber;
        end;
    except
        AppLog('Exception Finding IP Address: ' + IpStr + ' - ' + IcsGetExceptMess (ExceptObject)) ;
    end;

end;


function TIcsGeoTools.FindISO2ACity(const IpStr: String): TDBCityInfo;
var
    MyIPAddress: TMMDBIPAddress;
    prefixLength: Integer;
    CityName, CountryName, SubName: string;
begin
// check database loaded
//    Default(Result);
    Result.IpStr := '';
    Result.IS0A2 := '';
    if NOT FLoadASN then
        Exit;

// finally look up ASN information in MaxMind Database
    try
        MyIPAddress := TMMDBIPAddress.Parse(IpStr);
        if FMMDBASN.Find<TMMDBIPCountryCityInfoEx>(MyIPAddress, prefixLength, FCityInfo) then begin
            Result.IpStr := IpStr;
            if NOT FCityInfo.Country.Names.TryGetValue('en', CountryName) then
                CountryName := '';
            if NOT FCityInfo.City.Names.TryGetValue('en', CityName) then
                CityName := '';
      //      if NOT FCityInfo.Subdivisions.Names.TryGetValue('en', SubName) then
                SubName := '';
            Result.IS0A2 := FCityInfo.Country.ISOCode;
            Result.CountryName := CountryName;
            Result.CountryID := FCityInfo.Country.GeonameId;
            Result.Continent := FCityInfo.Continent.Code;
            Result.StateProv := SubName;
            Result.City := CityName;
            Result.GeoLat := FCityInfo.Location.latitude;
            Result.GeoLong := FCityInfo.Location.longitude;
            Result.Accuracy := FCityInfo.Location.Accuracy;
        end;
    except
        AppLog('Exception Finding IP Address: ' + IpStr + ' - ' + IcsGetExceptMess (ExceptObject)) ;
    end;
end;


// lookup TIcsCountry record from ISOA2 country code, contains name, ISO region codes and other stuff, blank for none
// BO,Bolivia,591,,BOL,068,BO,019,419
function TIcsGeoTools.FindNameRec(const ISOA2: String): TIcsCountry;
var
    RecNr: Integer;
begin
    Result := Default(TIcsCountry);
    if (ISOA2 = 'LAN') or (ISOA2 = 'XX') then begin
        Result.ISOA2 := ISOA2;
        Result.Country := 'Local Area Network';
        Exit;
    end;
    if Length(ISOA2) <> 2 then
        Exit;
    if ISOA2 = IcsUnknownISOA2 then
        Exit;

 // check database loaded
    if NOT IsLoaded then
        Exit;
    if FISOA2Idx.Count = 0 then
        Exit;

    Result.ISOA2 := ISOA2;
    if NOT FISOA2Idx.Find(@Result, CompareISOA2, RecNr) then begin
    //  AppLog('Failed to Find Name for: ' + ISOA2);    // TEMP
        Result.ISOA2 := '';
        Exit;
    end;
//   AppLog('Found Name: ' +  FCountries[RecNr].Country); // TEMP
    Result := FCountries[RecNr];
end;


// lookup Country name ISOA2 country code, blank for none
function TIcsGeoTools.FindCountry(const ISOA2: String): string ;
begin
    Result := FindNameRec(ISOA2).Country;
end;


// lookup region or subregion name from numeric ISO region ID, ie IcsRegionSouthEurope constant (39)
function TIcsGeoTools.FindRegion(ID: Integer): string ;
var
    I: Integer;
begin
    Result := '';
    if ID = 0 then
        Exit;
    for I := 1 to TotRegionNames do begin
        if IcsRegionNames[I].ID = ID then begin
            Result := IcsRegionNames[I].Name;
            Exit;
        end;
    end;
end;



end.
