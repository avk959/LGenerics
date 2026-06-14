{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Helpers for some basic types.                                           *
*                                                                           *
*   Copyright(c) 2018-2026 A.Koverdyaev(avk)                                *
*                                                                           *
*   This code is free software; you can redistribute it and/or modify it    *
*   under the terms of the Apache License, Version 2.0;                     *
*   You may obtain a copy of the License at                                 *
*     http://www.apache.org/licenses/LICENSE-2.0.                           *
*                                                                           *
*  Unless required by applicable law or agreed to in writing, software      *
*  distributed under the License is distributed on an "AS IS" BASIS,        *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *
*  See the License for the specific language governing permissions and      *
*  limitations under the License.                                           *
*                                                                           *
*****************************************************************************}
unit lgHelpers;

{$MODE OBJFPC}{$H+}
{$MODESWITCH TYPEHELPERS}
{$MODESWITCH ADVANCEDRECORDS}
{$INLINE ON}

interface

uses
  Classes, SysUtils, DateUtils, TypInfo, Variants,
  lgUtils,
  lgHash;

type

  TGGuidHelper = record helper(TGuidHelper) for TGUID
    class function HashCode(const aValue: TGUID): SizeInt; static; inline;
    class function Equal(const L, R: TGUID): Boolean; static; inline;
    class function Less(const L, R: TGUID): Boolean; static; inline;
    class function CreateV7(d: TDateTime): TGUID; static;
    class function CreateV7: TGUID; static; inline;
    class function CreateV7Ctr(d: TDateTime; aResetCtr: Boolean = False): TGUID; static;
    class function CreateV7Ctr(aResetCtr: Boolean = False): TGUID; static; inline;
    class function TryParse(p: PAnsiChar; aCount: SizeInt; out aValue: TGUID): Boolean; static;
    class function TryParse(const s: string; out aValue: TGUID): Boolean; static; inline;
    function ToString(aSkipBrackets: Boolean = False): string;
  end;

  TAStrHelper = type helper(TStringHelper) for ansistring
    class function HashCode(const aValue: ansistring): SizeInt; static; inline;
    class function Equal(const L, R: ansistring): Boolean; static; inline;
    class function Less(const L, R: ansistring): Boolean; static;
  end;

  TWStrHelper = type helper{$IF FPC_FULLVERSION>30300}(TWideStringHelper){$ENDIF} for widestring
    class function HashCode(const aValue: widestring): SizeInt; static; inline;
    class function Equal(const L, R: widestring): Boolean; static; inline;
    class function Less(const L, R: widestring): Boolean; static;
  end;

  TUStrHelper = type helper{$IF FPC_FULLVERSION>30300}(TUnicodeStringHelper){$ENDIF} for unicodestring
    class function HashCode(const aValue: unicodestring): SizeInt; static; inline;
    class function Equal(const L, R: unicodestring): Boolean; static; inline;
    class function Less(const L, R: unicodestring): Boolean; static;
  end;

  TShortStrHelper = type helper{$IF FPC_FULLVERSION>30300}(TShortStringHelper){$ENDIF} for shortstring
    class function HashCode(const aValue: shortstring): SizeInt; static; inline;
    class function Equal(const L, R: shortstring): Boolean; static; inline;
    class function Less(const L, R: shortstring): Boolean; static; inline;
  end;

  TGACharHelper = type helper for AnsiChar
    class function HashCode(aValue: AnsiChar): SizeInt; static; inline;
    class function Equal(L, R: AnsiChar): Boolean; static; inline;
    class function Less(L, R: AnsiChar): Boolean; static; inline;
  end;

  TGWCharHelper = type helper for WideChar
    class function HashCode(aValue: WideChar): SizeInt; static; inline;
    class function Equal(L, R: WideChar): Boolean; static; inline;
    class function Less(L, R: WideChar): Boolean; static; inline;
  end;

  TGByteHelper = type helper(TByteHelper) for Byte
    class function HashCode(aValue: Byte): SizeInt; static; inline;
    class function Equal(L, R: Byte): Boolean; static; inline;
    class function Less(L, R: Byte): Boolean; static; inline;
  end;

  TGShortIntHelper = type helper(TShortIntHelper) for ShortInt
    class function HashCode(aValue: ShortInt): SizeInt; static; inline;
    class function Equal(L, R: ShortInt): Boolean; static; inline;
    class function Less(L, R: ShortInt): Boolean; static; inline;
  end;

  TGWordHelper = type helper(TWordHelper) for Word
    class function HashCode(aValue: Word): SizeInt; static; inline;
    class function Equal(L, R: Word): Boolean; static; inline;
    class function Less(L, R: Word): Boolean; static; inline;
  end;

  TGSmallIntHelper = type helper(TSmallIntHelper) for SmallInt
    class function HashCode(aValue: SmallInt): SizeInt; static; inline;
    class function Equal(L, R: SmallInt): Boolean; static; inline;
    class function Less(L, R: SmallInt): Boolean; static; inline;
  end;

  TGDWordHelper = type helper(TCardinalHelper) for DWord
    class function HashCode(aValue: DWord): SizeInt; static; inline;
    class function Equal(L, R: DWord): Boolean; static; inline;
    class function Less(L, R: DWord): Boolean; static; inline;
  end;

  TGLongIntHelper = type helper(TIntegerHelper) for LongInt
    class function HashCode(aValue: LongInt): SizeInt; static; inline;
    class function Equal(L, R: LongInt): Boolean; static; inline;
    class function Less(L, R: LongInt): Boolean; static; inline;
  end;

  TGQWordHelper = type helper(TQWordHelper) for QWord
    class function HashCode(aValue: QWord): SizeInt; static; inline;
    class function Equal(L, R: QWord): Boolean; static; inline;
    class function Less(L, R: QWord): Boolean; static; inline;
  end;

  TGInt64Helper = type helper(TInt64Helper) for Int64
    class function HashCode(aValue: Int64): SizeInt; static; inline;
    class function Equal(L, R: Int64): Boolean; static; inline;
    class function Less(L, R: Int64): Boolean; static; inline;
  end;

  TGNativeUIntHelper = type helper(TNativeUIntHelper) for NativeUInt
    class function HashCode(aValue: NativeUInt): SizeInt; static; inline;
    class function Equal(L, R: NativeUInt): Boolean; static; inline;
    class function Less(L, R: NativeUInt): Boolean; static; inline;
  end;

  TGNativeIntHelper = type helper(TNativeIntHelper) for NativeInt
    class function HashCode(aValue: NativeInt): SizeInt; static; inline;
    class function Equal(L, R: NativeInt): Boolean; static; inline;
    class function Less(L, R: NativeInt): Boolean; static; inline;
  end;

  TGPointerHelper = type helper for Pointer
    class function HashCode(aValue: Pointer): SizeInt; static; inline;
    class function Equal(L, R: Pointer): Boolean; static; inline;
    class function Less(L, R: Pointer): Boolean; static; inline;
  end;

  TGSingleHelper = type helper(TSingleHelper) for Single
  private
  const
    SIGN_FLAG  = DWord($80000000);
    EXP_MASK   = DWord($7f800000);
    QUIET_FLAG = DWord($00400000);
  public
  const
    MAX_EXACT_INT = Single($ffffff); //2^24 - 1
    MAX_PAYLOAD   = DWord($3fffff);
  public
    class function  IsZero(const aValue: Single): Boolean; inline; static;
    class function  HashCode(const aValue: Single): SizeInt; inline; static;
    class function  Equal(const L, R: Single): Boolean; inline; static;
    class function  Less(const L, R: Single): Boolean; inline; static;
    class function  IsFinite(const aValue: Single): Boolean; inline; static;
    class function  IsExactInt(const aValue: Single): Boolean; inline; static;
    class function  IsExactInt(const aValue: Single; out aIntValue: Int32): Boolean; inline; static;
    class procedure Negate(var aValue: Single); inline; static;
  { returns value with the magnitude of aMagn and the sign of aSgn }
    class function CopySign(const aMagn, aSgn: Single): Single; inline; static;
  { returns True and in the aQuiet parameter whether it is quiet if aValue is NaN,
    otherwise returns False }
    class function IsNan(const aValue: Single; out aQuiet: Boolean): Boolean; overload; inline; static;
  { if aValue is a non-negative integer not exceeding MAX_PAYLOAD, returns a quiet NaN
    with that payload; otherwize returns 0.0 }
    class function SetPayload(const aValue: Single): Single; static;
    class function SetPayload(const aValue: Int32): Single; static;
  { returns True if aPayload is a nonnegative integer greater than zero and not exceeding
    MAX_PAYLOAD, and a signaling NaN containing this payload in the aSNaN parameter;
    otherwise, it returns False and 0.0 }
    class function SetSignalPayload(const aPayload: Single; out aSNaN: Single): Boolean; static;
    class function SetSignalPayload(const aPayload: Int32; out aSNaN: Single): Boolean; static;
  { if aValue is NaN then returns its payload, otherwise returns -1 }
    class function GetPayload(const aValue: Single): Single; inline; static;
    class function GetPayloadI(const aValue: Single): Int32; inline; static;
  { returns True if L and R are unordered, that is, at least one of them is NaN }
    class function Unordered(const L, R: Single): Boolean; inline; static;
  { returns True if L > R; returns False if L <= R or unordered }
    class function IsGreater(const L, R: Single): Boolean; inline; static;
  { returns True if L >= R; returns False if L < R or unordered }
    class function IsGreaterEqual(const L, R: Single): Boolean; inline; static;
  { returns True if L < R; returns False if L >= R or unordered }
    class function IsLess(const L, R: Single): Boolean; inline; static;
  { returns True if L <= R; returns False if L > R or unordered }
    class function IsLessEqual(const L, R: Single): Boolean; inline; static;
  { returns True if L = R; returns False if L <> R or unordered }
    class function IsEqual(const L, R: Single): Boolean; inline; static;
  { returns True if L <> R; returns False if L = R or unordered }
    class function IsNotEqual(const L, R: Single): Boolean; inline; static;
  { TotalOrder predicate }
    class function TotalOrder(const L, R: Single): Boolean; static;
  { returns quiet NaN if L and R are unordered, otherwise
    returns L if L <= R or R if R < L (-0 < +0) }
    class function Minimum(const L, R: Single): Single; static;
  { returns quiet NaN if L and R are unordered, otherwise
    returns L if L >= R or R if R > L (+0 > -0) }
    class function Maximum(const L, R: Single): Single; static;
  { returns the next representable value after aNum in the direction of aTo }
    class function NextAfter(const aNum, aTo: Single): Single; static;
  { returns the Quantum(IEEE 754) value of aNum if aNum is a finite number,
    otherwise returns Abs(aNum) }
    class function Quantum(const aNum: Single): Single; static;
  { returns the value of a unit in the last place(JH version) if aNum is a finite number,
    otherwise returns Abs(aNum) }
    class function Ulp(const aNum: Single): Single; static;
  { converts the value aNum to its shortest possible decimal representation s;
    returns the length of s; uses Schubfach float-to-string conversion algorithm }
    class function ToDecString(const aNum: Single; out s: shortstring; aDecSeparator: AnsiChar = '.';
                               aForceShowFrac: Boolean = False): Integer; static;
    class function ToDecString(const aNum: Single; aDecSeparator: AnsiChar = '.';
                               aForceShowFrac: Boolean = False): string; static;
  { uses the current locale's decimal separator }
    class function ToDecStringDef(const aNum: Single; aForceShowFrac: Boolean = False): string; static;
    function  IsZero: Boolean; inline;
    function  IsFinite: Boolean; inline;
    function  IsExactInt: Boolean; inline;
    function  IsExactInt(out aValue: Int32): Boolean; inline;
    procedure Negate; inline;
    function  CopySign(const aSgn: Single): Single; inline;
    function  IsNan(out aQuit: Boolean): Boolean; overload; inline;
    function  SetPayload: Single; inline;
    function  SetSignalPayload(out aSNaN: Single): Boolean; inline;
    function  GetPayload: Single; inline;
    function  GetPayloadI: Int32; inline;
    function  NextToward(const aValue: Single): Single; inline;
    function  Quantum: Single; inline;
    function  Ulp: Single; inline;
    function  ToDecString(aDecSep: AnsiChar = '.'; aForceShowFrac: Boolean = False): string;
    function  ToDecStringDef(aForceShowFrac: Boolean = False): string;
  end;

  TGDoubleHelper = type helper(TDoubleHelper) for Double
  private
  const
    SIGN_FLAG  = QWord($8000000000000000);
    EXP_MASK   = QWord($7ff0000000000000);
    QUIET_FLAG = QWord($0008000000000000);
  public
  const
    MAX_EXACT_INT = Double($1fffffffffffff); //2^53 - 1
    MAX_PAYLOAD   = QWord($7ffffffffffff);
  public
    class function  IsZero(const aValue: Double): Boolean; inline; static;
    class function  HashCode(const aValue: Double): SizeInt; inline; static;
    class function  Equal(const L, R: Double): Boolean; inline; static;
    class function  Less(const L, R: Double): Boolean; inline; static;
    class function  IsFinite(const aValue: Double): Boolean; inline; static;
    class function  IsExactInt(const aValue: Double): Boolean; inline; static;
    class function  IsExactInt(const aValue: Double; out aIntValue: Int64): Boolean; inline; static;
    class procedure Negate(var aValue: Double); inline; static;
  { returns value with the magnitude of aMagn and the sign of aSgn }
    class function CopySign(const aMagn, aSgn: Double): Double; inline; static;
  { returns True and in the aQuiet parameter whether it is quiet if aValue is NaN,
    otherwise returns False }
    class function IsNan(const aValue: Double; out aQuiet: Boolean): Boolean; overload; inline; static;
  { if aValue is a non-negative integer not exceeding MAX_PAYLOAD, returns a quiet NaN
    with that payload; otherwize returns 0.0 }
    class function SetPayload(const aValue: Double): Double; static;
    class function SetPayload(const aValue: Int64): Double; static;
  { returns True if aPayload is a nonnegative integer greater than zero and not exceeding
    MAX_PAYLOAD, and a signaling NaN containing this payload in the aSNaN parameter;
    otherwise, it returns False and 0.0 }
    class function SetSignalPayload(const aPayload: Double; out aSNaN: Double): Boolean; static;
    class function SetSignalPayload(const aPayload: Int64; out aSNaN: Double): Boolean; static;
  { if aValue is NaN then returns its payload, otherwise returns -1 }
    class function GetPayload(const aValue: Double): Double; inline; static;
    class function GetPayloadI(const aValue: Double): Int64; inline; static;
  { returns True if L and R are unordered, that is, at least one of them is NaN }
    class function Unordered(const L, R: Double): Boolean; inline; static;
  { returns True if L > R; returns False if L <= R or unordered }
    class function IsGreater(const L, R: Double): Boolean; inline; static;
  { returns True if L >= R; returns False if L < R or unordered }
    class function IsGreaterEqual(const L, R: Double): Boolean; inline; static;
  { returns True if L < R; returns False if L >= R or unordered }
    class function IsLess(const L, R: Double): Boolean; inline; static;
  { returns True if L <= R; returns False if L > R or unordered }
    class function IsLessEqual(const L, R: Double): Boolean; inline; static;
  { returns True if L = R; returns False if L <> R or unordered }
    class function IsEqual(const L, R: Double): Boolean; inline; static;
  { returns True if L <> R; returns False if L = R or unordered }
    class function IsNotEqual(const L, R: Double): Boolean; inline; static;
  { TotalOrder predicate }
    class function TotalOrder(const L, R: Double): Boolean; static;
  { returns quiet NaN if L and R are unordered, otherwise
    returns L if L <= R or R if R < L (-0 < +0) }
    class function Minimum(const L, R: Double): Double; static;
  { returns quiet NaN if L and R are unordered, otherwise
    returns L if L >= R or R if R > L (+0 > -0) }
    class function Maximum(const L, R: Double): Double; static;
  { returns the next representable value after aNum in the direction of aTo }
    class function NextAfter(const aNum, aTo: Double): Double; static;
  { returns the Quantum(IEEE 754) value of aNum if aNum is a finite number,
    otherwise returns Abs(aNum) }
    class function Quantum(const aNum: Double): Double; static;
  { returns the value of a unit in the last place(JH version) if aNum is a finite number,
    otherwise returns Abs(aNum) }
    class function Ulp(const aNum: Double): Double; static;
    function  IsZero: Boolean; inline;
    function  IsFinite: Boolean; inline;
    function  IsExactInt: Boolean; inline;
    function  IsExactInt(out aValue: Int64): Boolean; inline;
    procedure Negate; inline;
    function  CopySign(const aSgn: Double): Double; inline;
    function  IsNan(out aQuit: Boolean): Boolean; overload; inline;
    function  SetPayload: Double; inline;
    function  SetSignalPayload(out aSNaN: Double): Boolean; inline;
    function  GetPayload: Double; inline;
    function  GetPayloadI: Int64; inline;
    function  NextToward(const aValue: Double): Double; inline;
    function  Quantum: Double; inline;
    function  Ulp: Double; inline;
  end;

{$ifdef FPC_HAS_TYPE_EXTENDED}
  TGExtendedHelper = type helper(TExtendedHelper) for Extended
  private
  type
    TPWord = packed record
      Mantis: QWord;
      PExp: Word;
    end;
  const
    SIGN_FLAG  = Word($8000);
    EXP_MASK   = Word($7fff);
    INT_FLAG   = QWord($8000000000000000);
    QUIET_MASK = QWord($c000000000000000);
  public
  const
    MAX_PAYLOAD = QWord($3fffffffffffffff);
  public
    class function  IsZero(const aValue: Extended): Boolean; inline; static;
    class function  HashCode(const aValue: Extended): SizeInt; inline; static;
    class function  Equal(const L, R: Extended): Boolean; inline; static;
    class function  Less(const L, R: Extended): Boolean; inline; static;
    class function  IsFinite(const aValue: Extended): Boolean; inline; static;
    class function  IsExactInt(const aValue: Extended): Boolean; inline; static;
    class function  IsExactInt(const aValue: Extended; out aIntValue: Int64): Boolean; static;
    class procedure Negate(var aValue: Extended); inline; static;
  { returns value with the magnitude of aMagn and the sign of aSgn }
    class function CopySign(const aMagn, aSgn: Extended): Extended; inline; static;
  { returns True and in the aQuiet parameter whether it is quiet if aValue is NaN,
    otherwise returns False }
    class function IsNan(const aValue: Extended; out aQuiet: Boolean): Boolean; overload; static;
  { if aValue is a non-negative integer not exceeding MAX_PAYLOAD, returns a quiet NaN
    with that payload; otherwize returns 0.0 }
    class function SetPayload(const aValue: Extended): Extended; static;
    class function SetPayload(const aValue: Int64): Extended; static;
  { returns True if aPayload is a nonnegative integer not exceeding MAX_PAYLOAD,
    and a signaling NaN containing this payload in the aSNaN parameter;
    otherwise, it returns False and 0.0 }
    class function SetSignalPayload(const aPayload: Extended; out aSNaN: Extended): Boolean; static;
    class function SetSignalPayload(const aPayload: Int64; out aSNaN: Extended): Boolean; static;
  { if aValue is NaN then returns its payload, otherwise returns -1 }
    class function GetPayload(const aValue: Extended): Extended; static;
    class function GetPayloadI(const aValue: Extended): Int64; static;
  { returns True if L and R are unordered, that is, at least one of them is NaN }
    class function Unordered(const L, R: Extended): Boolean; inline; static;
  { returns True if L > R; returns False if L <= R or unordered }
    class function IsGreater(const L, R: Extended): Boolean; inline; static;
  { returns True if L >= R; returns False if L < R or unordered }
    class function IsGreaterEqual(const L, R: Extended): Boolean; inline; static;
  { returns True if L < R; returns False if L >= R or unordered }
    class function IsLess(const L, R: Extended): Boolean; inline; static;
  { returns True if L <= R; returns False if L > R or unordered }
    class function IsLessEqual(const L, R: Extended): Boolean; inline; static;
  { returns True if L = R; returns False if L <> R or unordered }
    class function IsEqual(const L, R: Extended): Boolean; inline; static;
  { returns True if L <> R; returns False if L = R or unordered }
    class function IsNotEqual(const L, R: Extended): Boolean; inline; static;
  { TotalOrder predicate }
    class function TotalOrder(const L, R: Extended): Boolean; static;
  { returns quiet NaN if L and R are unordered, otherwise
    returns L if L <= R or R if R < L (-0 < +0) }
    class function Minimum(const L, R: Extended): Extended; static;
  { returns quiet NaN if L and R are unordered, otherwise
    returns L if L >= R or R if R > L (+0 > -0) }
    class function Maximum(const L, R: Extended): Extended; static;
  { returns the next representable value after aNum in the direction of aTo }
    class function NextAfter(const aNum, aTo: Extended): Extended; static;
  { returns the Quantum(IEEE 754) value of aNum if aNum is a finite number,
    otherwise returns Abs(aNum) }
    class function Quantum(const aNum: Extended): Extended; static;
  { returns the value of a unit in the last place(JH version) if aNum is a finite number,
    otherwise returns Abs(aNum) }
    class function Ulp(const aNum: Extended): Extended; static;
    function  IsZero: Boolean; inline;
    function  IsFinite: Boolean; inline;
    function  IsExactInt: Boolean; inline;
    function  IsExactInt(out aValue: Int64): Boolean; inline;
    procedure Negate; inline;
    function  CopySign(const aSgn: Extended): Extended; inline;
    function  IsNan(out aQuit: Boolean): Boolean; overload; inline;
    function  SetPayload: Extended; inline;
    function  SetSignalPayload(out aSNaN: Extended): Boolean; inline;
    function  GetPayload: Extended; inline;
    function  GetPayloadI: Int64; inline;
    function  NextToward(const aValue: Extended): Extended; inline;
    function  Quantum: Extended; inline;
    function  Ulp: Extended; inline;
  end;
{$ENDIF}

{$IF DECLARED(Comp)}
  TCompHelper = type helper for Comp
    class function HashCode(aValue: Comp): SizeInt; static; inline;
    class function Equal(L, R: Comp): Boolean; static; inline;
    class function Less(L, R: Comp): Boolean; static; inline;
  end;
{$ENDIF}

  TGDateTimeHelper = type helper{$IF FPC_FULLVERSION>30202}(TDateTimeHelper){$ENDIF} for TDateTime
    class function HashCode(aValue: TDateTime): SizeInt; static; inline;
    class function Equal(L, R: TDateTime): Boolean; static; inline;
    class function Less(L, R: TDateTime): Boolean; static; inline;
  end;

  TGDateHelper = type helper for TDate
    class function HashCode(aValue: TDate): SizeInt; static; inline;
    class function Equal(L, R: TDate): Boolean; static; inline;
    class function Less(L, R: TDate): Boolean; static; inline;
  end;

  TGTimeHelper = type helper for TTime
    class function HashCode(aValue: TTime): SizeInt; static; inline;
    class function Equal(L, R: TTime): Boolean; static; inline;
    class function Less(L, R: TTime): Boolean; static; inline;
  end;

  TGCurrencyHelper = type helper{$IF FPC_FULLVERSION>30300}(TCurrencyHelper){$ENDIF} for Currency
  private
    class function GetMaxValue: Currency; static; inline;
    class function GetMinValue: Currency; static; inline;
  public
    class function HashCode(const aValue: Currency): SizeInt; static; inline;
    class function Equal(const L, R: Currency): Boolean; static; inline;
    class function Less(const L, R: Currency): Boolean; static; inline;
{$IF FPC_FULLVERSION<=30300}
    function ToString: string; inline;
{$ENDIF}
    class property MaxValue: Currency read GetMaxValue;
    class property MinValue: Currency read GetMinValue;
  end;

  TGObjectHelper = class helper for TObject
    class function HashCode(aValue: TObject): SizeInt; static; inline;
    class function Equal(L, R: TObject): Boolean; static; inline;
    class function Less(L, R: TObject): Boolean; static; inline;
    function GetSelfRef: TObject;
  end;

  TVariantHelper = type helper for Variant
    class function HashCode(const aValue: Variant): SizeInt; static; inline;
    class function Equal(const L, R: Variant): Boolean; static; inline;
    class function Less(const L, R: Variant): Boolean; static; inline;
  end;

  TStringArrayHelper = type helper for TStringArray
  private
    function  GetLength: SizeInt;
    procedure SetLen(aValue: SizeInt);
  public
    function  IsEmpty: Boolean;
    function  NonEmpty: Boolean;
    procedure Add(const aValue: string);
    property  Length: SizeInt read GetLength write SetLen;
  end;

  TPointHelper = type helper for TPoint
    class function HashCode(const aValue: TPoint): SizeInt; static; inline;
    class function Equal(const L, R: TPoint): Boolean; static; inline;
  end;

  TPrioTaskHelper = type helper for IPriorityTask
    class function Less(L, R: IPriorityTask): Boolean; static; inline;
  end;

  PTypeInfo = TypInfo.PTypeInfo;
  PTypeData = TypInfo.PTypeData;

  generic TGDefaults<T> = class
  public
  type
    TLess           = specialize TGLessCompare<T>;
    TOnLess         = specialize TGOnLessCompare<T>;
    TEqualCompare   = specialize TGEqualCompare<T>;
    TOnEqualCompare = specialize TGOnEqualCompare<T>;

    TComparer = class
      class function Less(const L, R: T): Boolean; static; inline;
    end;

    TEqualityComparer = class
      class function Equal(const L, R: T): Boolean; static; inline;
      class function HashCode(const aValue: T): SizeInt; static; inline;
    end;

  private
  type
    THashCode = function(constref aValue: T): SizeInt;

    TComparator = object
      function LessCompare(const L, R: T): Boolean;
      function EqualCompare(const L, R: T): Boolean;
    end;

  class var
    CFLess: TLess;
    CFEqualCompare: TEqualCompare;
    CFHashCode: THashCode;
    CFComparator: TComparator;
    class constructor Init;
    class procedure InitInt(aData: PTypeData); static;
    class procedure InitFloat(aData: PTypeData); static;
    class function CompareBin(const L, R: T): Boolean; static;
    class function EqualBin(const L, R: T): Boolean; static;
    class function HashBin(const aValue: T): SizeInt; static;
    class function GetOnLess: TOnLess; static; inline;
    class function GetOnEqualCompare: TOnEqualCompare; static; inline;
  public
    class property Less: TLess read CFLess;
    class property EqualCompare: TEqualCompare read CFEqualCompare;
    class property OnLess: TOnLess read GetOnLess;
    class property OnEqualCompare: TOnEqualCompare read GetOnEqualCompare;
  end;

  function CompareShortInt(const L, R: ShortInt): Boolean;
  function CompareUByte(const L, R: Byte): Boolean;
  function CompareSmallInt(const L, R: SmallInt): Boolean;
  function CompareWord(const L, R: Word): Boolean;
  function CompareLongInt(const L, R: LongInt): Boolean;
  function CompareDWord(const L, R: DWord): Boolean;
  function CompareInt64(const L, R: Int64): Boolean;
  function CompareQWord(const L, R: QWord): Boolean;
  function CompareChar(const L, R: AnsiChar): Boolean;
  function CompareSingle(const L, R: Single): Boolean;
  function CompareDouble(const L, R: Double): Boolean;
  function CompareExtended(const L, R: Extended): Boolean;
  {$IF DECLARED(Comp)}
  function CompareComp(const L, R: Comp): Boolean;
  {$ENDIF}
  function CompareCurrency(const L, R: Currency): Boolean;
  function CompareShortStr(const L, R: shortstring): Boolean;
  function CompareLStr(const L, R: string): Boolean;
  function CompareAStr(const L, R: ansistring): Boolean;
  function CompareWStr(const L, R: widestring): Boolean;
  function CompareVariant(const L, R: Variant): Boolean;
  function CompareObj(const L, R: TObject): Boolean;
  function CompareWChar(const L, R: WideChar): Boolean;
  function CompareUStr(const L, R: unicodestring): Boolean;
  function ComparePointer(const L, R: Pointer): Boolean;

  function ShortIntEqual(const L, R: ShortInt): Boolean;
  function UByteEqual(const L, R: Byte): Boolean;
  function SmallIntEqual(const L, R: SmallInt): Boolean;
  function WordEqual(const L, R: Word): Boolean;
  function LongIntEqual(const L, R: LongInt): Boolean;
  function DWordEqual(const L, R: DWord): Boolean;
  function Int64Equal(const L, R: Int64): Boolean;
  function QWordEqual(const L, R: QWord): Boolean;
  function CharEqual(const L, R: AnsiChar): Boolean;
  function SingleEqual(const L, R: Single): Boolean;
  function DoubleEqual(const L, R: Double): Boolean;
  function ExtendedEqual(const L, R: Extended): Boolean;
  function CompEqual(const L, R: Comp): Boolean;
  function CurrencyEqual(const L, R: Currency): Boolean;
  function ShortStrEqual(const L, R: shortstring): Boolean;
  function LStrEqual(const L, R: string): Boolean;
  function AStrEqual(const L, R: ansistring): Boolean;
  function WStrEqual(const L, R: widestring): Boolean;
  function VariantEqual(const L, R: Variant): Boolean;
  function ObjEqual(const L, R: TObject): Boolean;
  function WCharEqual(const L, R: WideChar): Boolean;
  function UStrEqual(const L, R: unicodestring): Boolean;
  function PointerEqual(const L, R: Pointer): Boolean;

  function HashShortInt(const aValue: ShortInt): SizeInt;
  function HashUByte(const aValue: Byte): SizeInt;
  function HashSmallInt(const aValue: SmallInt): SizeInt;
  function HashWord(const aValue: Word): SizeInt;
  function HashLongInt(const aValue: LongInt): SizeInt;
  function HashDWord(const aValue: DWord): SizeInt;
  function HashInt64(const aValue: Int64): SizeInt;
  function HashQWord(const aValue: QWord): SizeInt;
  function HashChar(const aValue: AnsiChar): SizeInt;
  function HashSingle(const aValue: Single): SizeInt;
  function HashDouble(const aValue: Double): SizeInt;
  function HashExtended(const aValue: Extended): SizeInt;
  function HashComp(const aValue: Comp): SizeInt;
  function HashCurrency(const aValue: Currency): SizeInt;
  function HashShortStr(const aValue: shortstring): SizeInt;
  function HashLStr(const aValue: string): SizeInt;
  function HashAStr(const aValue: ansistring): SizeInt;
  function HashWStr(const aValue: widestring): SizeInt;
  function HashVariant(const aValue: Variant): SizeInt;
  function HashObj(const aValue: TObject): SizeInt;
  function HashWChar(const aValue: WideChar): SizeInt;
  function HashUStr(const aValue: unicodestring): SizeInt;
  function HashPointer(const aValue: Pointer): SizeInt;

implementation
{$WARN 4110 OFF : Range check error while evaluating constants ($1 must be between $2 and $3)}
{$Q-}{$R-}{$B-}{$COPERATORS ON}{$MACRO ON}
{$DEFINE HashFunc := TxxHash32LE}

class function TGGuidHelper.HashCode(const aValue: TGUID): SizeInt;
begin
  Result := HashFunc.HashGuid(aValue);
end;

class function TGGuidHelper.Equal(const L, R: TGUID): Boolean;
type
  TDWords4 = packed record
    D1, D2, D3, D4: DWord;
  end;
var
  dL: TDWords4 absolute L;
  dR: TDWords4 absolute R;
begin
  if @L = @R then
    exit(True);
  Result := (dL.D1 = dR.D1) and (dL.D2 = dR.D2) and (dL.D3 = dR.D3) and (dL.D4 = dR.D4);
end;

class function TGGuidHelper.Less(const L, R: TGUID): Boolean;
begin
  Result := CompareMemRange(@L, @R, SizeOf(TGUID)) < 0;
end;

function FillBufRandom(out aBuffer; aSize: SizeInt): SizeInt;
{$PUSH}{$J+}
const
  InitRequired: Boolean = True;
{$POP}
var
  p: PByte;
  cnt: SizeInt;
begin
  if InitRequired then
    begin
      Jsf64Randomize;
      InitRequired := False;
    end;
  p := @aBuffer;
  Result := 0;
  while aSize > 0 do
    begin
      if aSize > SizeOf(QWord) then
        cnt := SizeOf(QWord)
      else
        cnt := aSize;
      System.Move(Jsf64Next, p^, cnt);
      Result += cnt;
      p += cnt;
      aSize -= cnt;
    end;
end;

class function TGGuidHelper.CreateV7(d: TDateTime): TGUID;
var
  ts: QWord;
  rnd: array[0..9] of Byte;
begin
  ts := QWord(MillisecondsBetween(d, UnixEpoch));
  FillBufRandom(rnd, SizeOf(rnd));
  Result.D1 := (ts shr 16) and $ffffffff;
  Result.D2 := (ts shr 4) and $0fff or $7000;
  Result.D3 := (ts and $0f) shl 12 or (rnd[0]) shl 4 or rnd[1] shr 4;
  Result.D4[0] := rnd[2] and $3f or $80;
  System.Move(rnd[3], Result.D4[1], 7);
end;

class function TGGuidHelper.CreateV7: TGUID;
begin
{$IF FPC_FULLVERSION < 30204}
  Result := CreateV7(DateUtils.LocalTimeToUniversal(SysUtils.Now));
{$ELSE}
  Result := CreateV7(SysUtils.Now);
{$ENDIF}
end;

class function TGGuidHelper.CreateV7Ctr(d: TDateTime; aResetCtr: Boolean): TGUID;
{$PUSH}{$J+}
const
  Ctr: DWord = 0;
{$POP}
var
  ts: QWord;
begin
  if aResetCtr then Ctr := 0;
  ts := QWord(MillisecondsBetween(d, UnixEpoch));
  Result.D1 := (ts shr 16) and $ffffffff;
  Result.D2 := (ts shr 4) and $0fff or $7000;
  Result.D3 := (ts and $0f) shl 12 or Ctr shr 6;
  Result.D4[0] := Ctr and $3f or $80;
  FillBufRandom(Result.D4[1], SizeOf(Result.D4) - 1);
  Ctr := (Ctr + 1) and $03ffff;
end;

class function TGGuidHelper.CreateV7Ctr(aResetCtr: Boolean): TGUID;
begin
{$IF FPC_FULLVERSION < 30204}
  Result := CreateV7Ctr(DateUtils.LocalTimeToUniversal(SysUtils.Now), aResetCtr);
{$ELSE}
  Result := CreateV7Ctr(SysUtils.NowUtc, aResetCtr);
{$ENDIF}
end;

class function TGGuidHelper.TryParse(p: PAnsiChar; aCount: SizeInt; out aValue: TGUID): Boolean;
const
{$PUSH}{$J-}
  HT: array['0'..'f'] of Byte = (
   0, 1, 2, 3, 4, 5, 6, 7, 8, 9,15,15,15,15,15,15,
  15,10,11,12,13,14,15,15,15,15,15,15,15,15,15,15,
  15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,
  15,10,11,12,13,14,15);
{$POP}
  HxC = ['0'..'9','A'..'F','a'..'f'];
begin
  if p = nil then exit(False);
  if aCount = 38 then begin
    if not((p^ = '{') and (p[37] = '}')) then exit(False);
    Inc(p);
  end else
    if aCount <> 36 then exit(False);

  if not((p[0] in HxC) and (p[1] in HxC) and (p[2] in HxC) and (p[3] in HxC) and
         (p[4] in HxC) and (p[5] in HxC) and (p[6] in HxC) and (p[7] in HxC)) then
    exit(False);
  aValue.D1 :=
    DWord(HT[p[0]]) shl 28 or HT[p[1]] shl 24 or HT[p[2]] shl 20 or HT[p[3]] shl 16 or
          HT[p[4]]  shl 12 or HT[p[5]] shl  8 or HT[p[6]] shl  4 or HT[p[7]];

  if not((p[8]='-')and(p[9] in HxC)and(p[10] in HxC)and(p[11] in HxC)and(p[12] in HxC))then
    exit(False);
  aValue.D2 := Word(HT[p[9]]) shl 12 or HT[p[10]] shl 8 or HT[p[11]] shl 4 or HT[p[12]];

  if not((p[13]='-')and(p[14] in HxC)and(p[15] in HxC)and(p[16] in HxC)and(p[17] in HxC))then
    exit(False);
  aValue.D3 := Word(HT[p[14]]) shl 12 or HT[p[15]] shl 8 or HT[p[16]] shl 4 or HT[p[17]];

  if not((p[18]='-')and(p[19] in HxC)and(p[20] in HxC)and(p[21] in HxC)and(p[22] in HxC))then
    exit(False);
  aValue.D4[0] := HT[p[19]] shl 4 or HT[p[20]];
  aValue.D4[1] := HT[p[21]] shl 4 or HT[p[22]];

  if not((p[23]='-')and(p[24] in HxC)and(p[25] in HxC)and(p[26] in HxC)and(p[27] in HxC) and
         (p[28] in HxC) and (p[29] in HxC) and (p[30] in HxC) and (p[31] in HxC) and
         (p[32] in HxC) and (p[33] in HxC) and (p[34] in HxC) and(p[35] in HxC)) then
    exit(False);
  aValue.D4[2] := HT[p[24]] shl 4 or HT[p[25]];
  aValue.D4[3] := HT[p[26]] shl 4 or HT[p[27]];
  aValue.D4[4] := HT[p[28]] shl 4 or HT[p[29]];
  aValue.D4[5] := HT[p[30]] shl 4 or HT[p[31]];
  aValue.D4[6] := HT[p[32]] shl 4 or HT[p[33]];
  aValue.D4[7] := HT[p[34]] shl 4 or HT[p[35]];
  Result := True;
end;

class function TGGuidHelper.TryParse(const s: string; out aValue: TGUID): Boolean;
begin
  Result := TryParse(Pointer(s), System.Length(s), aValue);
end;

function TGGuidHelper.ToString(aSkipBrackets: Boolean): string;
{$PUSH}{$J-}
const
  HexChar: array[0..$f] of AnsiChar = (
    '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
  );
{$POP}
var
  s: string;
  p: PAnsiChar;
begin
  if aSkipBrackets then
    begin
      System.SetLength(s, 36);
      p := Pointer(s);
    end
  else
    begin
      System.SetLength(s, 38);
      p := Pointer(s);
      p[0] := '{';
      p[37] := '}';
      Inc(p);
    end;

  p[ 0] := HexChar[D1 shr 28];
  p[ 1] := HexChar[(D1 shr 24) and $0f];
  p[ 2] := HexChar[(D1 shr 20) and $0f];
  p[ 3] := HexChar[(D1 shr 16) and $0f];
  p[ 4] := HexChar[(D1 shr 12) and $0f];
  p[ 5] := HexChar[(D1 shr  8) and $0f];
  p[ 6] := HexChar[(D1 shr  4) and $0f];
  p[ 7] := HexChar[D1 and $0f];
  p[ 8] := '-';

  p[ 9] := HexChar[D2 shr 12];
  p[10] := HexChar[(D2 shr 8) and $0f];
  p[11] := HexChar[(D2 shr 4) and $0f];
  p[12] := HexChar[D2 and $0f];
  p[13] := '-';

  p[14] := HexChar[D3 shr 12];
  p[15] := HexChar[(D3 shr 8) and $0f];
  p[16] := HexChar[(D3 shr 4) and $0f];
  p[17] := HexChar[D3 and $0f];
  p[18] := '-';

  p[19] := HexChar[D4[0] shr 4];
  p[20] := HexChar[D4[0] and $0f];
  p[21] := HexChar[D4[1] shr 4];
  p[22] := HexChar[D4[1] and $0f];
  p[23] := '-';

  p[24] := HexChar[D4[2] shr 4];
  p[25] := HexChar[D4[2] and $0f];
  p[26] := HexChar[D4[3] shr 4];
  p[27] := HexChar[D4[3] and $0f];
  p[28] := HexChar[D4[4] shr 4];
  p[29] := HexChar[D4[4] and $0f];
  p[30] := HexChar[D4[5] shr 4];
  p[31] := HexChar[D4[5] and $0f];
  p[32] := HexChar[D4[6] shr 4];
  p[33] := HexChar[D4[6] and $0f];
  p[34] := HexChar[D4[7] shr 4];
  p[35] := HexChar[D4[7] and $0f];

  Result := s;
end;

class function TAStrHelper.HashCode(const aValue: ansistring): SizeInt;
begin
  Result := HashFunc.HashStr(aValue);
end;

class function TAStrHelper.Equal(const L, R: ansistring): Boolean;
begin
  Result := L = R;
end;

class function TAStrHelper.Less(const L, R: ansistring): Boolean;
begin
  Result := AnsiCompareStr(L, R) < 0;
end;

class function TWStrHelper.HashCode(const aValue: widestring): SizeInt;
begin
  Result := HashFunc.HashBuf(PWideChar(aValue), System.Length(aValue) * SizeOf(System.WideChar));
end;

class function TWStrHelper.Equal(const L, R: widestring): Boolean;
begin
  Result := L = R;
end;

class function TWStrHelper.Less(const L, R: widestring): Boolean;
begin
  Result := WideCompareStr(L, R) < 0;
end;

class function TUStrHelper.HashCode(const aValue: unicodestring): SizeInt;
begin
  Result := HashFunc.HashBuf(PUnicodeChar(aValue), System.Length(aValue) * SizeOf(System.UnicodeChar));
end;

class function TUStrHelper.Equal(const L, R: unicodestring): Boolean;
begin
  Result := L = R;
end;

class function TUStrHelper.Less(const L, R: unicodestring): Boolean;
begin
  Result := UnicodeCompareStr(L, R) < 0;
end;

class function TShortStrHelper.HashCode(const aValue: shortstring): SizeInt;
begin
  Result := HashFunc.HashBuf(@aValue[1], System.Length(aValue));
end;

class function TShortStrHelper.Equal(const L, R: shortstring): Boolean;
begin
  Result := L = R;
end;

class function TShortStrHelper.Less(const L, R: shortstring): Boolean;
begin
  Result := L < R;
end;

class function TGACharHelper.HashCode(aValue: AnsiChar): SizeInt;
begin
  Result := Ord(aValue) xor Ord(aValue) shr 5;
end;

class function TGACharHelper.Equal(L, R: AnsiChar): Boolean;
begin
  Result := L = R;
end;

class function TGACharHelper.Less(L, R: AnsiChar): Boolean;
begin
  Result := L < R;
end;

class function TGWCharHelper.HashCode(aValue: WideChar): SizeInt;
begin
  Result := HashFunc.HashWord(Ord(aValue));
end;

class function TGWCharHelper.Equal(L, R: WideChar): Boolean;
begin
  Result := L = R;
end;

class function TGWCharHelper.Less(L, R: WideChar): Boolean;
begin
  Result := L < R;
end;

class function TGByteHelper.HashCode(aValue: Byte): SizeInt;
begin
  Result := aValue xor aValue shr 5;
end;

class function TGByteHelper.Equal(L, R: Byte): Boolean;
begin
  Result := L = R;
end;

class function TGByteHelper.Less(L, R: Byte): Boolean;
begin
  Result := L < R;
end;

class function TGShortIntHelper.HashCode(aValue: ShortInt): SizeInt;
begin
  Result := Byte.HashCode(aValue);
end;

class function TGShortIntHelper.Equal(L, R: ShortInt): Boolean;
begin
  Result := L = R;
end;

class function TGShortIntHelper.Less(L, R: ShortInt): Boolean;
begin
  Result := L < R;
end;

class function TGWordHelper.HashCode(aValue: Word): SizeInt;
begin
  Result := HashFunc.HashWord(aValue);
end;

class function TGWordHelper.Equal(L, R: Word): Boolean;
begin
  Result := L = R;
end;

class function TGWordHelper.Less(L, R: Word): Boolean;
begin
  Result := L < R;
end;

class function TGSmallIntHelper.HashCode(aValue: SmallInt): SizeInt;
begin
  Result := HashFunc.HashWord(aValue);
end;

class function TGSmallIntHelper.Equal(L, R: SmallInt): Boolean;
begin
  Result := L = R;
end;

class function TGSmallIntHelper.Less(L, R: SmallInt): Boolean;
begin
  Result := L < R;
end;

class function TGDWordHelper.HashCode(aValue: DWord): SizeInt;
begin
  Result := HashFunc.HashDWord(aValue);
end;

class function TGDWordHelper.Equal(L, R: DWord): Boolean;
begin
  Result := L = R;
end;

class function TGDWordHelper.Less(L, R: DWord): Boolean;
begin
  Result := L < R;
end;

class function TGLongIntHelper.HashCode(aValue: LongInt): SizeInt;
begin
  Result := HashFunc.HashDWord(aValue);
end;

class function TGLongIntHelper.Equal(L, R: LongInt): Boolean;
begin
  Result := L = R;
end;

class function TGLongIntHelper.Less(L, R: LongInt): Boolean;
begin
  Result := L < R;
end;

class function TGQWordHelper.HashCode(aValue: QWord): SizeInt;
begin
  Result := HashFunc.HashQWord(aValue);
end;

class function TGQWordHelper.Equal(L, R: QWord): Boolean;
begin
  Result := L = R;
end;

class function TGQWordHelper.Less(L, R: QWord): Boolean;
begin
  Result := L < R;
end;

class function TGInt64Helper.HashCode(aValue: Int64): SizeInt;
begin
  Result := HashFunc.HashQWord(aValue);
end;

class function TGInt64Helper.Equal(L, R: Int64): Boolean;
begin
  Result := L = R;
end;

class function TGInt64Helper.Less(L, R: Int64): Boolean;
begin
  Result := L < R;
end;

class function TGNativeUIntHelper.HashCode(aValue: NativeUInt): SizeInt;
begin
{$IF DEFINED(CPU64)}
  Result := HashFunc.HashQWord(aValue);
{$ELSEIF DEFINED(CPU32)}
  Result := HashFunc.HashDWord(aValue);
{$ELSE}
  Result := HashFunc.HashWord(aValue);
{$ENDIF}
end;

class function TGNativeUIntHelper.Equal(L, R: NativeUInt): Boolean;
begin
  Result := L = R;
end;

class function TGNativeUIntHelper.Less(L, R: NativeUInt): Boolean;
begin
  Result := L < R;
end;

class function TGNativeIntHelper.HashCode(aValue: NativeInt): SizeInt;
begin
{$IF DEFINED(CPU64)}
  Result := HashFunc.HashQWord(aValue);
{$ELSEIF DEFINED(CPU32)}
  Result := HashFunc.HashDWord(aValue);
{$ELSE}
  Result := HashFunc.HashWord(aValue);
{$ENDIF}
end;

class function TGNativeIntHelper.Equal(L, R: NativeInt): Boolean;
begin
  Result := L = R;
end;

class function TGNativeIntHelper.Less(L, R: NativeInt): Boolean;
begin
  Result := L < R;
end;

{$PUSH}{$WARN 4055 OFF : Conversion between ordinals and pointers is not portable}
class function TGPointerHelper.HashCode(aValue: Pointer): SizeInt;
begin
  Result := SizeUInt.HashCode(SizeUInt(aValue));
end;
{$POP}

class function TGPointerHelper.Equal(L, R: Pointer): Boolean;
begin
  Result := L = R;
end;

class function TGPointerHelper.Less(L, R: Pointer): Boolean;
begin
  Result := L < R;
end;

class function TGSingleHelper.IsZero(const aValue: Single): Boolean;
begin
  Result:= DWord(aValue) and not SIGN_FLAG = 0;
end;

class function TGSingleHelper.HashCode(const aValue: Single): SizeInt;
begin
  if IsZero(aValue) then
    Result := HashFunc.HashDWord(DWord(Single(0.0)))
  else
    Result := HashFunc.HashDWord(DWord(aValue));
end;

class function TGSingleHelper.Equal(const L, R: Single): Boolean;
begin
  Result := L = R;
end;

class function TGSingleHelper.Less(const L, R: Single): Boolean;
begin
  Result := L < R;
end;

class function TGSingleHelper.IsFinite(const aValue: Single): Boolean;
begin
  Result := (DWord(aValue) and not SIGN_FLAG) < EXP_MASK;
end;

class function TGSingleHelper.IsExactInt(const aValue: Single): Boolean;
begin
  if not IsFinite(aValue) then exit(False);
  Result := (System.Frac(aValue) = 0) and (System.Abs(aValue) <= MAX_EXACT_INT);
end;

class function TGSingleHelper.IsExactInt(const aValue: Single; out aIntValue: Int32): Boolean;
begin
  Result := IsExactInt(aValue);
  if Result then aIntValue := System.Trunc(aValue);
end;

class procedure TGSingleHelper.Negate(var aValue: Single);
begin
  DWord(aValue) := DWord(aValue) xor SIGN_FLAG;
end;

class function TGSingleHelper.CopySign(const aMagn, aSgn: Single): Single;
begin
  DWord(Result) := (DWord(aMagn) and not SIGN_FLAG) or (DWord(aSgn) and SIGN_FLAG);
end;

class function TGSingleHelper.IsNan(const aValue: Single; out aQuiet: Boolean): Boolean;
begin
  Result := (DWord(aValue) and not SIGN_FLAG) > EXP_MASK;
  if Result then aQuiet := DWord(aValue) and QUIET_FLAG <> 0;
end;

{$PUSH}{$WARN 5036 off : Local variable "$1" does not seem to be initialized}
class function TGSingleHelper.SetPayload(const aValue: Single): Single;
var
  I: Int32;
begin
  if IsExactInt(aValue, I) and (DWord(I) <= MAX_PAYLOAD) then
    DWord(Result) := EXP_MASK or QUIET_FLAG or DWord(I)
  else
    Result := 0;
end;

class function TGSingleHelper.SetPayload(const aValue: Int32): Single;
begin
  if DWord(aValue) <= MAX_PAYLOAD then
    DWord(Result) := EXP_MASK or QUIET_FLAG or DWord(aValue)
  else
    Result := 0;
end;

class function TGSingleHelper.SetSignalPayload(const aPayload: Single; out aSNaN: Single): Boolean;
var
  I: Int32;
begin
  Result := IsExactInt(aPayload, I) and (I <> 0) and (DWord(I) <= MAX_PAYLOAD);
  if Result then
    DWord(aSNaN) := EXP_MASK or DWord(I)
  else
    aSNaN := 0;
end;
{$POP}

class function TGSingleHelper.SetSignalPayload(const aPayload: Int32; out aSNaN: Single): Boolean;
begin
  Result := (aPayload <> 0) and (DWord(aPayload) <= MAX_PAYLOAD);
  if Result then
    DWord(aSNaN) := EXP_MASK or DWord(aPayload)
  else
    aSNaN := 0;
end;

class function TGSingleHelper.GetPayload(const aValue: Single): Single;
begin
  if (DWord(aValue) and not SIGN_FLAG) > EXP_MASK then
    Result := Int32(DWord(aValue) and MAX_PAYLOAD)
  else
    Result := -1;
end;

class function TGSingleHelper.GetPayloadI(const aValue: Single): Int32;
begin
  if (DWord(aValue) and not SIGN_FLAG) > EXP_MASK then
    Result := Int32(DWord(aValue) and MAX_PAYLOAD)
  else
    Result := -1;
end;

class function TGSingleHelper.Unordered(const L, R: Single): Boolean;
begin
  Result := ((DWord(L) and not SIGN_FLAG) > EXP_MASK)or((DWord(R) and not SIGN_FLAG) > EXP_MASK);
end;

class function TGSingleHelper.IsGreater(const L, R: Single): Boolean;
begin
  if Unordered(L, R) then exit(False);
  Result := L > R;
end;

class function TGSingleHelper.IsGreaterEqual(const L, R: Single): Boolean;
begin
  if Unordered(L, R) then exit(False);
  Result := L >= R;
end;

class function TGSingleHelper.IsLess(const L, R: Single): Boolean;
begin
  if Unordered(L, R) then exit(False);
  Result := L < R;
end;

class function TGSingleHelper.IsLessEqual(const L, R: Single): Boolean;
begin
  if Unordered(L, R) then exit(False);
  Result := L <= R;
end;

class function TGSingleHelper.IsEqual(const L, R: Single): Boolean;
begin
  if Unordered(L, R) then exit(False);
  Result := L = R;
end;

class function TGSingleHelper.IsNotEqual(const L, R: Single): Boolean;
begin
  if Unordered(L, R) then exit(False);
  Result := L <> R;
end;

class function TGSingleHelper.TotalOrder(const L, R: Single): Boolean;
var
  LNeg, RNeg: Boolean;
begin
  LNeg := Boolean(DWord(L) shr 31);
  RNeg := Boolean(DWord(R) shr 31);
  if LNeg xor RNeg then exit(LNeg);
  if LNeg then
    Result := (DWord(L) and not SIGN_FLAG) > (DWord(R) and not SIGN_FLAG)
  else
    Result := (DWord(L) and not SIGN_FLAG) < (DWord(R) and not SIGN_FLAG);
end;

class function TGSingleHelper.Minimum(const L, R: Single): Single;
var
  Quiet: Boolean;
begin
  if IsNan(L, Quiet) then
    if Quiet then
      exit(L)
    else
      exit(NaN)
  else
    if IsNan(R, Quiet) then
      if Quiet then
        exit(R)
      else
        exit(NaN);
  if L < R then
    Result := L
  else
    if R < L then
      Result := R
    else
      if IsZero(L) and (L.Sign xor R.Sign) then
        if L.Sign then
          Result := L
        else
          Result := R
      else
        Result := L;
end;

class function TGSingleHelper.Maximum(const L, R: Single): Single;
var
  Quiet: Boolean;
begin
  if IsNan(L, Quiet) then
    if Quiet then
      exit(L)
    else
      exit(NaN)
  else
    if IsNan(R, Quiet) then
      if Quiet then
        exit(R)
      else
        exit(NaN);
  if L > R then
    Result := L
  else
    if R > L then
      Result := R
    else
      if IsZero(L) and (L.Sign xor R.Sign) then
        if R.Sign then
          Result := L
        else
          Result := R
      else
        Result := L;
end;

class function TGSingleHelper.NextAfter(const aNum, aTo: Single): Single;
var
  n, t: DWord;
begin
  n := DWord(aNum) and not SIGN_FLAG;
  if n > EXP_MASK then exit(aNum);
  t := DWord(aTo) and not SIGN_FLAG;
  if (t > EXP_MASK) or (aNum = aTo) then exit(aTo);
  if n <> 0 then
    if(Boolean(DWord(aNum) shr 31) xor Boolean(DWord(aTo) shr 31)) or (n > t)then
      //to zero
      DWord(Result) := DWord(aNum) - 1
    else
      DWord(Result) := DWord(aNum) + 1
  else
    DWord(Result) := DWord(1) or DWord(aTo) and SIGN_FLAG;
end;

class function TGSingleHelper.Quantum(const aNum: Single): Single;
var
  n: DWord;
  nExp: Int32;
begin
  n := DWord(aNum) and not SIGN_FLAG;
  if n < EXP_MASK then
    begin
      nExp := Int32(n shr 23);
      case nExp of
        0, 1:  DWord(Result) := DWord(1);
        2..23: DWord(Result) := DWord(1) shl (nExp - 1);
      else
        DWord(Result) := DWord(nExp - 23) shl 23;
      end;
    end
  else
    DWord(Result) := n;
end;

class function TGSingleHelper.Ulp(const aNum: Single): Single;
var
  n: DWord absolute Result;
begin
  n := DWord(aNum) and not SIGN_FLAG;
  if n < EXP_MASK then
    Result -= NextAfter(Result, -1);
end;

{ Pascal port of the Alexander Bolz's implementation of the Schubfach algorithm for
  single-precision floating-point numbers (https://github.com/abolz/Drachennest) }
class function TGSingleHelper.ToDecString(const aNum: Single; out s: shortstring; aDecSeparator: AnsiChar;
  aForceShowFrac: Boolean): Integer;
type
  TSingleRepr = record
    digits: UInt32;
    exponent: Int32;
  end;
const
  SIGNIFICAND_SIZE  = 24;          // p (includes hidden bit)
  EXPONENT_BIAS     = 150;         // max_exp-1 + (p-1) = 128-1+23
  HIDDEN_BIT        = UInt32(1) shl 23;
  SIGNIFICAND_MASK  = HIDDEN_BIT - 1;
  MAX_IEEE_EXPONENT = $FF;
  { floor division by power of two (arithmetic right shift) }
  function FloorDivPow2(x: Int32; n: Int32): Int32; inline;
  begin
    Result := SarLongint(x, n);
  end;
  { floor(log2(10^e)) }
  function FloorLog2Pow10(e: Int32): Int32; inline;
  begin
    Assert(e >= -1233); Assert(e <= 1233);
    Result := FloorDivPow2(e * 1741647, 19);
  end;
  function ComputePow10(k: Int32): UInt64; inline;
  const
    K_MIN = -31;
    K_MAX =  45;
    g: array[K_MIN..K_MAX] of UInt64 = (
      UInt64($81CEB32C4B43FCF5), // -31
      UInt64($A2425FF75E14FC32), // -30
      UInt64($CAD2F7F5359A3B3F), // -29
      UInt64($FD87B5F28300CA0E), // -28
      UInt64($9E74D1B791E07E49), // -27
      UInt64($C612062576589DDB), // -26
      UInt64($F79687AED3EEC552), // -25
      UInt64($9ABE14CD44753B53), // -24
      UInt64($C16D9A0095928A28), // -23
      UInt64($F1C90080BAF72CB2), // -22
      UInt64($971DA05074DA7BEF), // -21
      UInt64($BCE5086492111AEB), // -20
      UInt64($EC1E4A7DB69561A6), // -19
      UInt64($9392EE8E921D5D08), // -18
      UInt64($B877AA3236A4B44A), // -17
      UInt64($E69594BEC44DE15C), // -16
      UInt64($901D7CF73AB0ACDA), // -15
      UInt64($B424DC35095CD810), // -14
      UInt64($E12E13424BB40E14), // -13
      UInt64($8CBCCC096F5088CC), // -12
      UInt64($AFEBFF0BCB24AAFF), // -11
      UInt64($DBE6FECEBDEDD5BF), // -10
      UInt64($89705F4136B4A598), // -9
      UInt64($ABCC77118461CEFD), // -8
      UInt64($D6BF94D5E57A42BD), // -7
      UInt64($8637BD05AF6C69B6), // -6
      UInt64($A7C5AC471B478424), // -5
      UInt64($D1B71758E219652C), // -4
      UInt64($83126E978D4FDF3C), // -3
      UInt64($A3D70A3D70A3D70B), // -2
      UInt64($CCCCCCCCCCCCCCCD), // -1
      UInt64($8000000000000000), //  0
      UInt64($A000000000000000), //  1
      UInt64($C800000000000000), //  2
      UInt64($FA00000000000000), //  3
      UInt64($9C40000000000000), //  4
      UInt64($C350000000000000), //  5
      UInt64($F424000000000000), //  6
      UInt64($9896800000000000), //  7
      UInt64($BEBC200000000000), //  8
      UInt64($EE6B280000000000), //  9
      UInt64($9502F90000000000), // 10
      UInt64($BA43B74000000000), // 11
      UInt64($E8D4A51000000000), // 12
      UInt64($9184E72A00000000), // 13
      UInt64($B5E620F480000000), // 14
      UInt64($E35FA931A0000000), // 15
      UInt64($8E1BC9BF04000000), // 16
      UInt64($B1A2BC2EC5000000), // 17
      UInt64($DE0B6B3A76400000), // 18
      UInt64($8AC7230489E80000), // 19
      UInt64($AD78EBC5AC620000), // 20
      UInt64($D8D726B7177A8000), // 21
      UInt64($878678326EAC9000), // 22
      UInt64($A968163F0A57B400), // 23
      UInt64($D3C21BCECCEDA100), // 24
      UInt64($84595161401484A0), // 25
      UInt64($A56FA5B99019A5C8), // 26
      UInt64($CECB8F27F4200F3A), // 27
      UInt64($813F3978F8940985), // 28
      UInt64($A18F07D736B90BE6), // 29
      UInt64($C9F2C9CD04674EDF), // 30
      UInt64($FC6F7C4045812297), // 31
      UInt64($9DC5ADA82B70B59E), // 32
      UInt64($C5371912364CE306), // 33
      UInt64($F684DF56C3E01BC7), // 34
      UInt64($9A130B963A6C115D), // 35
      UInt64($C097CE7BC90715B4), // 36
      UInt64($F0BDC21ABB48DB21), // 37
      UInt64($96769950B50D88F5), // 38
      UInt64($BC143FA4E250EB32), // 39
      UInt64($EB194F8E1AE525FE), // 40
      UInt64($92EFD1B8D0CF37BF), // 41
      UInt64($B7ABC627050305AE), // 42
      UInt64($E596B7B0C643C71A), // 43
      UInt64($8F7E32CE7BEA5C70), // 44
      UInt64($B35DBF821AE4F38C)  // 45
    );
  begin
    Result := g[k];
  end;
  { RoundToOdd: returns (y1 | (y0 > 1)) where y1:y0 are bits 32..95 of g * cp }
  function RoundToOdd(g: UInt64; cp: UInt32): UInt32;
  var
    b01, b11, hi: UInt64;
    lo_g, hi_g: UInt32;
  begin
    lo_g := UInt32(g);
    hi_g := UInt32(g shr 32);
    b01 := UInt64(lo_g) * cp;
    b11 := UInt64(hi_g) * cp;
    hi := b11 + (b01 shr 32);
    Result := UInt32(hi shr 32) or (Ord((hi and $FFFFFFFF) > 1));
  end;
  { check if value is divisible by 2^e2 }
  function MultipleOfPow2(value: UInt32; e2: Int32): Boolean; inline;
  begin
    Result := (value and ((UInt32(1) shl e2) - 1)) = 0;
  end;
  { core conversion algorithm }
  function ToDecimalRepr(ieee_significand, ieee_exponent: UInt32): TSingleRepr;
  var
    pow10: UInt64;
    c, cbl, cb, cbr, vbl, vb, vbr, lower, upper, s, sp, mid: UInt32;
    q, k, h: Int32;
    is_even, accept_lower, accept_upper, lower_boundary_is_closer, up_inside,
    wp_inside, u_inside, w_inside, round_up: Boolean;
  begin
    if ieee_exponent <> 0 then begin
      c := HIDDEN_BIT or ieee_significand;
      q := Int32(ieee_exponent) - EXPONENT_BIAS;
      if (0 <= -q) and (-q < SIGNIFICAND_SIZE) and MultipleOfPow2(c, -q) then begin
        Result.digits := c shr (-q);
        Result.exponent := 0;
        exit;
      end;
    end else begin
      c := ieee_significand;
      q := 1 - EXPONENT_BIAS;
    end;

    is_even := not System.Odd(c);
    accept_lower := is_even;
    accept_upper := is_even;
    lower_boundary_is_closer := (ieee_significand = 0) and (ieee_exponent > 1);

    cbl := 4 * c - 2 + Ord(lower_boundary_is_closer);
    cb  := 4 * c;
    cbr := 4 * c + 2;

    k := FloorDivPow2(q * 1262611 - (Ord(lower_boundary_is_closer) * 524031), 22);
    h := q + FloorLog2Pow10(-k) + 1;
    Assert(h >= 1); Assert(h <= 4);

    pow10 := ComputePow10(-k);
    vbl := RoundToOdd(pow10, cbl shl h);
    vb  := RoundToOdd(pow10, cb  shl h);
    vbr := RoundToOdd(pow10, cbr shl h);

    lower := vbl + Ord(not accept_lower);
    upper := vbr - Ord(not accept_upper);

    s := vb shr 2;

    if s >= 10 then begin
      sp := s div 10;
      up_inside := lower <= 40 * sp;
      wp_inside := 40 * sp + 40 <= upper;
      if up_inside <> wp_inside then begin
        if wp_inside then
          Result.digits := sp + 1
        else
          Result.digits := sp;
        Result.exponent := k + 1;
        exit;
      end;
    end;

    u_inside := lower <= 4 * s;
    w_inside := 4 * s + 4 <= upper;
    if u_inside <> w_inside then begin
      if w_inside then
        Result.digits := s + 1
      else
        Result.digits := s;
      Result.exponent := k;
      exit;
    end;

    mid := 4 * s + 2;
    round_up := (vb > mid) or ((vb = mid) and ((s and 1) <> 0));
    if round_up then
      Result.digits := s + 1
    else
      Result.digits := s;
    Result.exponent := k;
  end;
  { helper for two digit conversion }
  procedure Print2Digits(aBuffer: PAnsiChar; aIndex: UInt32);
  const
    Digits100: array[0..199] of Char = (
      '0','0','0','1','0','2','0','3','0','4','0','5','0','6','0','7','0','8','0','9',
      '1','0','1','1','1','2','1','3','1','4','1','5','1','6','1','7','1','8','1','9',
      '2','0','2','1','2','2','2','3','2','4','2','5','2','6','2','7','2','8','2','9',
      '3','0','3','1','3','2','3','3','3','4','3','5','3','6','3','7','3','8','3','9',
      '4','0','4','1','4','2','4','3','4','4','4','5','4','6','4','7','4','8','4','9',
      '5','0','5','1','5','2','5','3','5','4','5','5','5','6','5','7','5','8','5','9',
      '6','0','6','1','6','2','6','3','6','4','6','5','6','6','6','7','6','8','6','9',
      '7','0','7','1','7','2','7','3','7','4','7','5','7','6','7','7','7','8','7','9',
      '8','0','8','1','8','2','8','3','8','4','8','5','8','6','8','7','8','8','8','9',
      '9','0','9','1','9','2','9','3','9','4','9','5','9','6','9','7','9','8','9','9'
    );
  begin
    aBuffer[0] := Digits100[aIndex shl 1];
    aBuffer[1] := Digits100[Succ(aIndex shl 1)];
  end;
  function TrailingZeros2Digits(digits: UInt32): Int32; inline;
  const
    TrailingZeros: array[0..99] of Int8 = (
      2,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,
      1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0
    );
  begin
    Result := TrailingZeros[digits];
  end;
  { Print decimal digits backwards into the buffer, return number of trailing zeros }
  function PrintDigitsBackwards(buf: PChar; output: UInt32): Int32;
  var
    p: PAnsiChar;
    q, r, rH, rL: UInt32;
    tz, nd: Int32;
  begin
    p := buf;
    tz := 0;
    nd := 0;

    if output >= 10000 then begin
      q := output div 10000;
      r := output - q*10000;
      output := q;
      Dec(p, 4);
      if r <> 0 then begin
        rH := r div 100;
        rL := r - rH*100;
        Print2Digits(p, rH);
        Print2Digits(p + 2, rL);
        if rL = 0 then
          tz := TrailingZeros2Digits(rH) + 2
        else
          tz := TrailingZeros2Digits(rL);
      end else
        tz := 4;
      nd := 4;
    end;

    if output >= 100 then begin
      q := output div 100;
      r := output - q*100;
      output := q;
      Dec(p, 2);
      Print2Digits(p, r);
      if tz = nd then
        Inc(tz, TrailingZeros2Digits(r));
      Inc(nd, 2);

      if output >= 100 then begin
        q := output div 100;
        r := output - q*100;
        output := q;
        Dec(p, 2);
        Print2Digits(p, r);
        if tz = nd then
          Inc(tz, TrailingZeros2Digits(r));
        Inc(nd, 2);
      end;
    end;

    // remaining 1 or 2 digits
    if output >= 10 then begin
      Dec(p, 2);
      Print2Digits(p, output);
      if tz = nd then
        Inc(tz, TrailingZeros2Digits(output));
    end else begin
      Dec(p);
      p^ := Char(Ord('0') + output);
    end;

    Result := tz;
  end;
  function DecimalLen(v: UInt32): Int32; inline;
  begin
    if v < 10000 then
      if v < 100 then
        Result := 2 - Ord(v < 10)
      else
        Result := 4 - Ord(v < 1000)
    else
      if v < 100000000 then
        if v < 1000000 then
          Result := 6 - Ord(v < 100000)
        else
          Result := 8 - Ord(v < 10000000)
      else
        Result := 9;
  end;
  { format decimal digits into final string representation;
    returns pointer after last written character }
  function FormatDigits(aBuf: PAnsiChar; aDigs: UInt32; aExp: Int32; aDecSep: AnsiChar;
    aForceShowFrac: Boolean): PAnsiChar;
  const
    MIN_FIXED = -4;
    MAX_FIXED = 9;
  var
    //tmp: array[0..31] of AnsiChar;
    DigitsEnd: PAnsiChar;
    NumDigits, DecPoint, tz, DecDigitsPos, ScientificExp: Int32;
    k: UInt32;
    UseFixed: Boolean;
  begin
    NumDigits := DecimalLen(aDigs);
    DecPoint := NumDigits + aExp;
    UseFixed := (MIN_FIXED <= DecPoint) and (DecPoint <= MAX_FIXED);

    if UseFixed then
      if DecPoint <= 0 then
        DecDigitsPos := 2 - DecPoint   // "0.[000]aDigs"
      else
        DecDigitsPos := 0              // "dig.its" or "aDigs[000]"
    else
      DecDigitsPos := 1;               // "dE+123" or "d.igitsE+123"

    DigitsEnd := aBuf + DecDigitsPos + NumDigits;
    tz := PrintDigitsBackwards(DigitsEnd, aDigs);
    Dec(DigitsEnd, tz);
    Dec(NumDigits, tz);

    if UseFixed then begin
      if DecPoint <= 0 then begin
        // "0.[000]aDigs"
        aBuf[1] := aDecSep;
        Result := DigitsEnd;
      end else
        if DecPoint < NumDigits then begin
          // "dig.its"
          Move(aBuf[DecPoint], aBuf[DecPoint + 1], NumDigits - DecPoint);
          aBuf[DecPoint] := aDecSep;
          Result := DigitsEnd + 1;
        end else begin
          // "aDigs[000]"
          Result := aBuf + DecPoint;
          if aForceShowFrac then
          begin
            Result^ := aDecSep;
            Result[1] := '0';
            Inc(Result, 2);
          end;
        end;
    end else begin
      // scientific notation
      aBuf[0] := aBuf[1];   // move first digit one left
      if NumDigits = 1 then begin
        // "dE+123"
        Result := aBuf + 1;
      end else begin
        // "d.igitsE+123"
        aBuf[1] := aDecSep;
        Result := DigitsEnd;
      end;

      ScientificExp := DecPoint - 1;
      if ScientificExp < 0 then begin
        Result^ := 'E';
        Result[1] := '-';
        k := UInt32(-ScientificExp);
      end else begin
        Result^ := 'E';
        Result[1] := '+';
        k := UInt32(ScientificExp);
      end;
      Inc(Result, 2);

      if k < 10 then begin
        Result^ := AnsiChar(Ord('0') + k);
        Inc(Result);
      end else begin
        Print2Digits(Result, k);
        Inc(Result, 2);
      end;
    end;
  end;
  function ToDecimal(aBuffer: PAnsiChar; const aValue: Single; aDecSep: AnsiChar;
    aForceShowFrac: Boolean): PAnsiChar;
  var
    bits: UInt32 absolute aValue;
    repr: TSingleRepr;
    significand, exponent: UInt32;
  const
    sInf  = 'Infinity';
    sQNan = 'qNaN';
    sSNan = 'sNaN';
  begin
    significand := bits and SIGNIFICAND_MASK;
    exponent := (bits and EXP_MASK) shr (SIGNIFICAND_SIZE - 1);

    // Infinity or NaN
    if exponent = MAX_IEEE_EXPONENT then
      if significand = 0 then begin
        if (bits and SIGN_FLAG) <> 0 then begin
          aBuffer[0] := '-';
          Inc(aBuffer);
        end;
        Move(sInf[1], aBuffer^, System.Length(sInf));
        Result := aBuffer + System.Length(sInf);
        exit;
      end else begin
        if bits and QUIET_FLAG <> 0 then begin
          Move(sQNan[1], aBuffer^, System.Length(sQNan));
          Result := aBuffer + System.Length(sQNan);
        end else begin
          Move(sSNan[1], aBuffer^, System.Length(sSNan));
          Result := aBuffer + System.Length(sSNan);
        end;
        exit;
      end;

    // Finite number
    if (bits and SIGN_FLAG) <> 0 then begin
      aBuffer[0] := '-';
      Inc(aBuffer);
    end;

    if (exponent <> 0) or (significand <> 0) then begin
      repr := ToDecimalRepr(significand, exponent);
      Result := FormatDigits(aBuffer, repr.digits, repr.exponent, aDecSep, aForceShowFrac);
    end else begin
      // Zero
      aBuffer[0] := '0';
      Inc(aBuffer);
      if aForceShowFrac then begin
        aBuffer^ := aDecSep;
        aBuffer[1] := '0';
        Inc(aBuffer, 2);
      end;
      Result := aBuffer;
    end;
  end;
begin
  Result := ToDecimal(@s[1], aNum, aDecSeparator, aForceShowFrac) - PAnsiChar(@s[1]);
  System.SetLength(s, Result);
end;

class function TGSingleHelper.ToDecString(const aNum: Single; aDecSeparator: AnsiChar; aForceShowFrac: Boolean): string;
var
  ss: shortstring;
begin
  System.SetLength(Result, ToDecString(aNum, ss, aDecSeparator, aForceShowFrac));
  System.Move(ss[1], Pointer(Result)^, System.Length(Result));
end;

class function TGSingleHelper.ToDecStringDef(const aNum: Single; aForceShowFrac: Boolean): string;
begin
  Result := ToDecString(aNum, FormatSettings.DecimalSeparator, aForceShowFrac);
end;

function TGSingleHelper.IsZero: Boolean;
begin
  Result:= IsZero(Self);
end;

function TGSingleHelper.IsFinite: Boolean;
begin
  Result := IsFinite(Self);
end;

function TGSingleHelper.IsExactInt: Boolean;
begin
  Result := IsExactInt(Self);
end;

function TGSingleHelper.IsExactInt(out aValue: Int32): Boolean;
begin
  Result := IsExactInt(Self, aValue);
end;

procedure TGSingleHelper.Negate;
begin
  Negate(Self);
end;

function TGSingleHelper.CopySign(const aSgn: Single): Single;
begin
  Result := CopySign(Self, aSgn);
end;

function TGSingleHelper.IsNan(out aQuit: Boolean): Boolean;
begin
  Result := IsNan(Self, aQuit);
end;

function TGSingleHelper.SetPayload: Single;
begin
  Result := SetPayload(Self);
end;

function TGSingleHelper.SetSignalPayload(out aSNaN: Single): Boolean;
begin
  Result := SetSignalPayload(Self, aSNaN);
end;

function TGSingleHelper.GetPayload: Single;
begin
  Result := GetPayload(Self);
end;

function TGSingleHelper.GetPayloadI: Int32;
begin
  Result := GetPayloadI(Self);
end;

function TGSingleHelper.NextToward(const aValue: Single): Single;
begin
  Result := NextAfter(Self, aValue);
end;

function TGSingleHelper.Quantum: Single;
begin
  Result := Quantum(Self);
end;

function TGSingleHelper.Ulp: Single;
begin
  Result := Ulp(Self);
end;

function TGSingleHelper.ToDecString(aDecSep: AnsiChar; aForceShowFrac: Boolean): string;
begin
  Result := ToDecString(Self, aDecSep, aForceShowFrac);
end;

function TGSingleHelper.ToDecStringDef(aForceShowFrac: Boolean): string;
begin
  Result := ToDecStringDef(Self, aForceShowFrac);
end;

class function TGDoubleHelper.IsZero(const aValue: Double): Boolean;
begin
  Result := QWord(aValue) and not SIGN_FLAG = 0;
end;

class function TGDoubleHelper.HashCode(const aValue: Double): SizeInt;
begin
  if IsZero(aValue) then
    Result := HashFunc.HashQWord(QWord(Double(0.0)))
  else
    Result := HashFunc.HashQWord(QWord(aValue));
end;

class function TGDoubleHelper.Equal(const L, R: Double): Boolean;
begin
  Result := L = R;
end;

class function TGDoubleHelper.Less(const L, R: Double): Boolean;
begin
  Result := L < R;
end;

class function TGDoubleHelper.IsFinite(const aValue: Double): Boolean;
begin
  Result := (QWord(aValue) and not SIGN_FLAG) < EXP_MASK;
end;

class function TGDoubleHelper.IsExactInt(const aValue: Double): Boolean;
begin
  if not IsFinite(aValue) then exit(False);
  Result := (System.Frac(aValue) = 0) and (System.Abs(aValue) <= MAX_EXACT_INT);
end;

class function TGDoubleHelper.IsExactInt(const aValue: Double; out aIntValue: Int64): Boolean;
begin
  Result := IsExactInt(aValue);
  if Result then aIntValue := System.Trunc(aValue);
end;

class procedure TGDoubleHelper.Negate(var aValue: Double);
begin
  QWord(aValue) := QWord(aValue) xor SIGN_FLAG;
end;

class function TGDoubleHelper.CopySign(const aMagn, aSgn: Double): Double;
begin
  QWord(Result) := (QWord(aMagn) and not SIGN_FLAG) or (QWord(aSgn) and SIGN_FLAG);
end;

class function TGDoubleHelper.IsNan(const aValue: Double; out aQuiet: Boolean): Boolean;
begin
  Result := (QWord(aValue) and not SIGN_FLAG) > EXP_MASK;
  if Result then aQuiet := QWord(aValue) and QUIET_FLAG <> 0;
end;

{$PUSH}{$WARN 5036 OFF : Local variable "$1" does not seem to be initialized }
class function TGDoubleHelper.SetPayload(const aValue: Double): Double;
var
  I: Int64;
begin
  if IsExactInt(aValue, I) and (QWord(I) <= MAX_PAYLOAD) then
    QWord(Result) := EXP_MASK or QUIET_FLAG or QWord(I)
  else
    Result := 0;
end;

class function TGDoubleHelper.SetPayload(const aValue: Int64): Double;
begin
  if QWord(aValue) <= MAX_PAYLOAD then
    QWord(Result) := EXP_MASK or QUIET_FLAG or QWord(aValue)
  else
    Result := 0;
end;

class function TGDoubleHelper.SetSignalPayload(const aPayload: Double; out aSNaN: Double): Boolean;
var
  I: Int64;
begin
  Result := IsExactInt(aPayload, I) and (I <> 0) and (QWord(I) <= MAX_PAYLOAD);
  if Result then
    QWord(aSNaN) := EXP_MASK or QWord(I)
  else
    aSNaN := 0;
end;
{$POP}

class function TGDoubleHelper.SetSignalPayload(const aPayload: Int64; out aSNaN: Double): Boolean;
begin
  Result := (aPayload <> 0) and (QWord(aPayload) <= MAX_PAYLOAD);
  if Result then
    QWord(aSNaN) := EXP_MASK or QWord(aPayload)
  else
    aSNaN := 0;
end;


class function TGDoubleHelper.GetPayload(const aValue: Double): Double;
begin
  if (QWord(aValue) and not SIGN_FLAG) > EXP_MASK then
    Result := Int64(QWord(aValue) and MAX_PAYLOAD)
  else
    Result := -1;
end;

class function TGDoubleHelper.GetPayloadI(const aValue: Double): Int64;
begin
  if (QWord(aValue) and not SIGN_FLAG) > EXP_MASK then
    Result := Int64(QWord(aValue) and MAX_PAYLOAD)
  else
    Result := -1;
end;

class function TGDoubleHelper.Unordered(const L, R: Double): Boolean;
begin
  Result := ((QWord(L) and not SIGN_FLAG) > EXP_MASK)or((QWord(R) and not SIGN_FLAG) > EXP_MASK);
end;

class function TGDoubleHelper.IsGreater(const L, R: Double): Boolean;
begin
  if Unordered(L, R) then exit(False);
  Result := L > R;
end;

class function TGDoubleHelper.IsGreaterEqual(const L, R: Double): Boolean;
begin
  if Unordered(L, R) then exit(False);
  Result := L >= R;
end;

class function TGDoubleHelper.IsLess(const L, R: Double): Boolean;
begin
  if Unordered(L, R) then exit(False);
  Result := L < R;
end;

class function TGDoubleHelper.IsLessEqual(const L, R: Double): Boolean;
begin
  if Unordered(L, R) then exit(False);
  Result := L <= R;
end;

class function TGDoubleHelper.IsEqual(const L, R: Double): Boolean;
begin
  if Unordered(L, R) then exit(False);
  Result := L = R;
end;

class function TGDoubleHelper.IsNotEqual(const L, R: Double): Boolean;
begin
  if Unordered(L, R) then exit(False);
  Result := L <> R;
end;

class function TGDoubleHelper.TotalOrder(const L, R: Double): Boolean;
var
  LNeg, RNeg: Boolean;
begin
  LNeg := Boolean(QWord(L) shr 63);
  RNeg := Boolean(QWord(R) shr 63);
  if LNeg xor RNeg then exit(LNeg);
  if LNeg then
    Result := (QWord(L) and not SIGN_FLAG) > (QWord(R) and not SIGN_FLAG)
  else
    Result := (QWord(L) and not SIGN_FLAG) < (QWord(R) and not SIGN_FLAG);
end;

class function TGDoubleHelper.Minimum(const L, R: Double): Double;
var
  Quiet: Boolean;
begin
  if IsNan(L, Quiet) then
    if Quiet then
      exit(L)
    else
      exit(NaN)
  else
    if IsNan(R, Quiet) then
      if Quiet then
        exit(R)
      else
        exit(NaN);
  if L < R then
    Result := L
  else
    if R < L then
      Result := R
    else
      if IsZero(L) and (L.Sign xor R.Sign) then
        if L.Sign then
          Result := L
        else
          Result := R
      else
        Result := L;
end;

class function TGDoubleHelper.Maximum(const L, R: Double): Double;
var
  Quiet: Boolean;
begin
  if IsNan(L, Quiet) then
    if Quiet then
      exit(L)
    else
      exit(NaN)
  else
    if IsNan(R, Quiet) then
      if Quiet then
        exit(R)
      else
        exit(NaN);
  if L > R then
    Result := L
  else
    if R > L then
      Result := R
    else
      if IsZero(L) and (L.Sign xor R.Sign) then
        if R.Sign then
          Result := L
        else
          Result := R
      else
        Result := L;
end;

class function TGDoubleHelper.NextAfter(const aNum, aTo: Double): Double;
var
  n, t: QWord;
begin
  n := QWord(aNum) and not SIGN_FLAG;
  if n > EXP_MASK then exit(aNum);
  t := QWord(aTo) and not SIGN_FLAG;
  if (t > EXP_MASK) or (aNum = aTo) then exit(aTo);
  if n <> 0 then
    if(Boolean(QWord(aNum) shr 63) xor Boolean(QWord(aTo) shr 63)) or (n > t)then
       //to zero
       QWord(Result) := QWord(aNum) - 1
     else
       QWord(Result) := QWord(aNum) + 1
  else
    QWord(Result) := QWord(1) or QWord(aTo) and SIGN_FLAG;
end;

class function TGDoubleHelper.Quantum(const aNum: Double): Double;
var
  nExp: Int32;
begin
  QWord(Result) := QWord(aNum) and not SIGN_FLAG;
  if QWord(Result) < EXP_MASK then
    begin
      nExp := Int32(QWord(Result) shr 52);
      case nExp of
        0, 1:  QWord(Result) := QWord(1);
        2..52: QWord(Result) := QWord(1) shl (nExp - 1);
      else
        QWord(Result) := QWord(nExp - 52) shl 52;
      end;
    end;
end;

class function TGDoubleHelper.Ulp(const aNum: Double): Double;
var
  next: Double;
begin
  QWord(Result) := QWord(aNum) and not SIGN_FLAG;
  if QWord(Result) < EXP_MASK then
    if QWord(Result) <> 0 then
      begin
        QWord(next) := QWord(Result) - 1;
        Result -= next;
      end
    else
      QWord(Result) := QWord(1);
end;

function TGDoubleHelper.IsZero: Boolean;
begin
  Result := IsZero(Self);
end;

function TGDoubleHelper.IsFinite: Boolean;
begin
  Result := IsFinite(Self);
end;

function TGDoubleHelper.IsExactInt: Boolean;
begin
  Result := IsExactInt(Self);
end;

function TGDoubleHelper.IsExactInt(out aValue: Int64): Boolean;
begin
  Result := IsExactInt(Self);
  if Result then aValue := System.Trunc(Self);
end;

procedure TGDoubleHelper.Negate;
begin
  Negate(Self);
end;

function TGDoubleHelper.CopySign(const aSgn: Double): Double;
begin
  Result := CopySign(Self, aSgn);
end;

function TGDoubleHelper.IsNan(out aQuit: Boolean): Boolean;
begin
  Result := IsNan(Self, aQuit);
end;

function TGDoubleHelper.SetPayload: Double;
begin
  Result := SetPayload(Self);
end;

function TGDoubleHelper.SetSignalPayload(out aSNaN: Double): Boolean;
begin
  Result := SetSignalPayload(Self, aSNaN);
end;

function TGDoubleHelper.GetPayload: Double;
begin
  Result := GetPayload(Self);
end;

function TGDoubleHelper.GetPayloadI: Int64;
begin
  Result := GetPayloadI(Self);
end;

function TGDoubleHelper.NextToward(const aValue: Double): Double;
begin
  Result := NextAfter(Self, aValue);
end;

function TGDoubleHelper.Quantum: Double;
begin
  Result := Quantum(Self);
end;

function TGDoubleHelper.Ulp: Double;
begin
  Result := Ulp(Self);
end;

{$ifdef FPC_HAS_TYPE_EXTENDED}
class function TGExtendedHelper.IsZero(const aValue: Extended): Boolean;
begin
  Result := (TPWord(aValue).PExp and not SIGN_FLAG = 0)and(TPWord(aValue).Mantis = 0);
end;

class function TGExtendedHelper.HashCode(const aValue: Extended): SizeInt;
const
  Zero: Extended = 0.0;
begin
  if IsZero(aValue) then
    Result := HashFunc.HashBuf(@Zero, SizeOf(Zero))
  else
    Result := HashFunc.HashBuf(@aValue, SizeOf(aValue));
end;

class function TGExtendedHelper.Equal(const L, R: Extended): Boolean;
begin
  Result := L = R;
end;

class function TGExtendedHelper.Less(const L, R: Extended): Boolean;
begin
  Result := L < R;
end;

class function TGExtendedHelper.IsFinite(const aValue: Extended): Boolean;
begin
  case TPWord(aValue).PExp and EXP_MASK of
    0:        Result := TPWord(aValue).Mantis and INT_FLAG = 0;
    EXP_MASK: Result := False;
  else
    Result := TPWord(aValue).Mantis and INT_FLAG <> 0;
  end;
end;

class function TGExtendedHelper.IsExactInt(const aValue: Extended): Boolean;
begin
  if not IsFinite(aValue) then exit(False);
  Result := (System.Frac(aValue) = 0) and (aValue >= Low(Int64)) and (aValue <= High(Int64));
end;

class function TGExtendedHelper.IsExactInt(const aValue: Extended; out aIntValue: Int64): Boolean;
begin
  Result := IsExactInt(aValue);
  if Result then aIntValue := System.Trunc(aValue);
end;

class procedure TGExtendedHelper.Negate(var aValue: Extended);
begin
  TPWord(aValue).PExp := TPWord(aValue).PExp xor SIGN_FLAG;
end;

class function TGExtendedHelper.CopySign(const aMagn, aSgn: Extended): Extended;
begin
  Result := aMagn;
  TPWord(Result).PExp := TPWord(Result).PExp and EXP_MASK or TPWord(aSgn).PExp and SIGN_FLAG;
end;

class function TGExtendedHelper.IsNan(const aValue: Extended; out aQuiet: Boolean): Boolean;
begin
  Result := False;
  if TPWord(aValue).PExp and EXP_MASK <> EXP_MASK then exit;
  case Byte(TPWord(aValue).Mantis shr 62) of
    2:  begin Result := True; aQuiet := False end;
    3:  begin Result := True; aQuiet := True end;
  end;
end;

{$PUSH}{$WARN 5036 OFF : Local variable "$1" does not seem to be initialized}
class function TGExtendedHelper.SetPayload(const aValue: Extended): Extended;
var
  I: Int64;
begin
  if IsExactInt(aValue, I) and (QWord(I) <= MAX_PAYLOAD) then
    with TPWord(Result) do
      begin
        Mantis := QWord(I) or QUIET_MASK;
        PExp := EXP_MASK;
      end
  else
    Result := 0;
end;

class function TGExtendedHelper.SetPayload(const aValue: Int64): Extended;
begin
  if QWord(aValue) <= MAX_PAYLOAD then
    with TPWord(Result) do
      begin
        Mantis := QWord(aValue) or QUIET_MASK;
        PExp := EXP_MASK;
      end
  else
    Result := 0;
end;

class function TGExtendedHelper.SetSignalPayload(const aPayload: Extended; out aSNaN: Extended): Boolean;
var
  I: Int64;
begin
  Result := IsExactInt(aPayload, I) and (QWord(I) <= MAX_PAYLOAD);
  if Result then
    with TPWord(aSNaN) do
      begin
        Mantis := QWord(I) or INT_FLAG;
        PExp := EXP_MASK;
      end
  else
    aSNaN := 0;
end;
{$POP}

class function TGExtendedHelper.SetSignalPayload(const aPayload: Int64; out aSNaN: Extended): Boolean;
begin
  Result := QWord(aPayload) <= MAX_PAYLOAD;
  if Result then
    with TPWord(aSNaN) do
      begin
        Mantis := QWord(aPayload) or INT_FLAG;
        PExp := EXP_MASK;
      end
  else
    aSNaN := 0;
end;

class function TGExtendedHelper.GetPayload(const aValue: Extended): Extended;
var
  Dummy: Boolean;
begin
  if IsNan(aValue, Dummy) then
    Result := Int64(TPWord(aValue).Mantis and MAX_PAYLOAD)
  else
    Result := -1;
end;

class function TGExtendedHelper.GetPayloadI(const aValue: Extended): Int64;
var
  Dummy: Boolean;
begin
  if IsNan(aValue, Dummy) then
    Result := Int64(TPWord(aValue).Mantis and MAX_PAYLOAD)
  else
    Result := -1;
end;

class function TGExtendedHelper.Unordered(const L, R: Extended): Boolean;
var
  Dummy: Boolean;
begin
  Result := IsNan(L, Dummy) or IsNan(R, Dummy);
end;

class function TGExtendedHelper.IsGreater(const L, R: Extended): Boolean;
begin
  if Unordered(L, R) then exit(False);
  Result := L > R;
end;

class function TGExtendedHelper.IsGreaterEqual(const L, R: Extended): Boolean;
begin
  if Unordered(L, R) then exit(False);
  Result := L >= R;
end;

class function TGExtendedHelper.IsLess(const L, R: Extended): Boolean;
begin
  if Unordered(L, R) then exit(False);
  Result := L < R;
end;

class function TGExtendedHelper.IsLessEqual(const L, R: Extended): Boolean;
begin
  if Unordered(L, R) then exit(False);
  Result := L <= R;
end;

class function TGExtendedHelper.IsEqual(const L, R: Extended): Boolean;
begin
  if Unordered(L, R) then exit(False);
  Result := L = R;
end;

class function TGExtendedHelper.IsNotEqual(const L, R: Extended): Boolean;
begin
  if Unordered(L, R) then exit(False);
  Result := L <> R;
end;

class function TGExtendedHelper.TotalOrder(const L, R: Extended): Boolean;
var
  LNeg, RNeg: Boolean;
begin
  LNeg := Boolean(TPWord(L).PExp shr 15);
  RNeg := Boolean(TPWord(R).PExp shr 15);
  if LNeg xor RNeg then exit(LNeg);
  if LNeg then
    if TPWord(L).PExp and EXP_MASK = TPWord(R).PExp and EXP_MASK then
      Result := TPWord(L).Mantis > TPWord(R).Mantis
    else
      Result := TPWord(L).PExp and EXP_MASK > TPWord(R).PExp and EXP_MASK
  else
    if TPWord(L).PExp and EXP_MASK = TPWord(R).PExp and EXP_MASK then
      Result := TPWord(L).Mantis < TPWord(R).Mantis
    else
      Result := TPWord(L).PExp and EXP_MASK < TPWord(R).PExp and EXP_MASK;
end;

class function TGExtendedHelper.Minimum(const L, R: Extended): Extended;
var
  Quiet: Boolean;
begin
  if IsNan(L, Quiet) then
    if Quiet then
      exit(L)
    else
      exit(NaN)
  else
    if IsNan(R, Quiet) then
      if Quiet then
        exit(R)
      else
        exit(NaN);
  if L < R then
    Result := L
  else
    if R < L then
      Result := R
    else
      if IsZero(L) and (L.Sign xor R.Sign) then
        if L.Sign then
          Result := L
        else
          Result := R
      else
        Result := L;
end;

class function TGExtendedHelper.Maximum(const L, R: Extended): Extended;
var
  Quiet: Boolean;
begin
  if IsNan(L, Quiet) then
    if Quiet then
      exit(L)
    else
      exit(NaN)
  else
    if IsNan(R, Quiet) then
      if Quiet then
        exit(R)
      else
        exit(NaN);
  if L > R then
    Result := L
  else
    if R > L then
      Result := R
    else
      if IsZero(L) and (L.Sign xor R.Sign) then
        if R.Sign then
          Result := L
        else
          Result := R
      else
        Result := L;
end;

class function TGExtendedHelper.NextAfter(const aNum, aTo: Extended): Extended;
var
  Dummy: Boolean;
begin
  if IsNan(aNum, Dummy) then
    exit(aNum)
  else
    if IsNan(aTo, Dummy) then
      exit(aTo);
  if aNum = aTo then exit(aTo);
  if aNum = 0 then begin
    TPWord(Result).Mantis := QWord(1);
    TPWord(Result).PExp :=  TPWord(aTo).PExp and SIGN_FLAG;
  end else
    if(aNum.Sign xor aTo.Sign)or(System.Abs(aNum) > System.Abs(aTo))then begin
      //to zero
      TPWord(Result).PExp := TPWord(aNum).PExp;
      TPWord(Result).Mantis := Pred(TPWord(aNum).Mantis);
      if TPWord(Result).Mantis and INT_FLAG = 0 then
        if TPWord(Result).PExp and EXP_MASK <> 0 then begin
          Dec(TPWord(Result).PExp);
          if TPWord(Result).PExp and EXP_MASK <> 0 then
            TPWord(Result).Mantis := TPWord(Result).Mantis or INT_FLAG;
        end;
    end else begin
      TPWord(Result).PExp := TPWord(aNum).PExp;
      TPWord(Result).Mantis := Succ(TPWord(aNum).Mantis);
      if TPWord(Result).Mantis = 0 then begin
        Inc(TPWord(Result).PExp);
        TPWord(Result).Mantis := INT_FLAG;
      end;
    end;
end;

class function TGExtendedHelper.Quantum(const aNum: Extended): Extended;
begin
  Result := System.Abs(aNum);
  if IsFinite(Result) then
    if Result < MaxValue then
      Result := NextAfter(Result, MaxValue) - Result
    else
      Result -= NextAfter(Result, 0);
end;

class function TGExtendedHelper.Ulp(const aNum: Extended): Extended;
begin
  Result := System.Abs(aNum);
  if IsFinite(Result) then
    Result -= NextAfter(Result, -1);
end;

function TGExtendedHelper.IsZero: Boolean;
begin
  Result := IsZero(Self);
end;

function TGExtendedHelper.IsFinite: Boolean;
begin
  Result := IsFinite(Self);
end;

function TGExtendedHelper.IsExactInt: Boolean;
begin
  Result := IsExactInt(Self);
end;

function TGExtendedHelper.IsExactInt(out aValue: Int64): Boolean;
begin
  Result := IsExactInt(Self, aValue);
end;

procedure TGExtendedHelper.Negate;
begin
  Negate(Self);
end;

function TGExtendedHelper.CopySign(const aSgn: Extended): Extended;
begin
  Result := CopySign(Self, aSgn);
end;

function TGExtendedHelper.IsNan(out aQuit: Boolean): Boolean;
begin
  Result := IsNan(Self, aQuit);
end;

function TGExtendedHelper.SetPayload: Extended;
begin
  Result := SetPayload(Self);
end;

function TGExtendedHelper.SetSignalPayload(out aSNaN: Extended): Boolean;
begin
  Result := SetSignalPayload(Self, aSNaN);
end;

function TGExtendedHelper.GetPayload: Extended;
begin
  Result := GetPayload(Self);
end;

function TGExtendedHelper.GetPayloadI: Int64;
begin
  Result := GetPayloadI(Self);
end;

function TGExtendedHelper.NextToward(const aValue: Extended): Extended;
begin
  Result := NextAfter(Self, aValue);
end;

function TGExtendedHelper.Quantum: Extended;
begin
  Result := Quantum(Self);
end;

function TGExtendedHelper.Ulp: Extended;
begin
  Result := Ulp(Self);
end;
{$ENDIF}

{$IF DECLARED(Comp)}
{ TCompHelper }

class function TCompHelper.HashCode(aValue: Comp): SizeInt;
begin
  Result := QWord.HashCode(QWord(aValue));
end;

class function TCompHelper.Equal(L, R: Comp): Boolean;
begin
  Result := L = R;
end;

class function TCompHelper.Less(L, R: Comp): Boolean;
begin
  Result := L < R;
end;
{$ENDIF}

class function TGDateTimeHelper.HashCode(aValue: TDateTime): SizeInt;
begin
  Result := HashFunc.HashQWord(QWord(aValue));
end;

class function TGDateTimeHelper.Equal(L, R: TDateTime): Boolean;
begin
  Result := L = R;
end;

class function TGDateTimeHelper.Less(L, R: TDateTime): Boolean;
begin
  Result := L < R;
end;

class function TGDateHelper.HashCode(aValue: TDate): SizeInt;
begin
  Result := HashFunc.HashQWord(QWord(aValue));
end;

class function TGDateHelper.Equal(L, R: TDate): Boolean;
begin
  Result := L = R;
end;

class function TGDateHelper.Less(L, R: TDate): Boolean;
begin
  Result := L < R;
end;

class function TGTimeHelper.HashCode(aValue: TTime): SizeInt;
begin
  Result := HashFunc.HashQWord(QWord(aValue));
end;

class function TGTimeHelper.Equal(L, R: TTime): Boolean;
begin
  Result := L = R;
end;

class function TGTimeHelper.Less(L, R: TTime): Boolean;
begin
  Result := L < R;
end;

class function TGCurrencyHelper.GetMaxValue: Currency;
begin
  Result := MaxCurrency;
end;

class function TGCurrencyHelper.GetMinValue: Currency;
begin
  Result := MinCurrency;
end;

class function TGCurrencyHelper.HashCode(const aValue: Currency): SizeInt;
begin
  Result := HashFunc.HashQWord(QWord(aValue));
end;

class function TGCurrencyHelper.Equal(const L, R: Currency): Boolean;
begin
  Result := L = R;
end;

class function TGCurrencyHelper.Less(const L, R: Currency): Boolean;
begin
  Result := L < R;
end;

{$IF FPC_FULLVERSION<=30300}
function TGCurrencyHelper.ToString: string;
begin
  Result := CurrToStr(Self);
end;
{$ENDIF}

class function TGObjectHelper.HashCode(aValue: TObject): SizeInt;
begin
{$IF DEFINED(CPU64)}
  if aValue <> nil then
    Result := HashFunc.HashQWord(aValue.GetHashCode)
  else
    Result := HashFunc.HashQWord(0);
{$ELSEIF DEFINED(CPU32)}
  if aValue <> nil then
    Result := HashFunc.HashDWord(aValue.GetHashCode)
  else
    Result := HashFunc.HashDWord(0);
{$ELSE}
  if aValue <> nil then
    Result := HashFunc.HashWord(aValue.GetHashCode)
  else
    Result := HashFunc.HashWord(0);
{$ENDIF}
end;

class function TGObjectHelper.Equal(L, R: TObject): Boolean;
begin
  Result := L.Equals(R);
end;

class function TGObjectHelper.Less(L, R: TObject): Boolean;
begin
  Result := Pointer(L) < Pointer(R);
end;

function TGObjectHelper.GetSelfRef: TObject;
begin
  Result := Self;
end;

class function TVariantHelper.HashCode(const aValue: Variant): SizeInt;
begin
  Result := HashFunc.HashBuf(@aValue, SizeOf(System.Variant), 0);
end;

{$PUSH}{$WARN 5026 OFF : Value parameter assignrd but never used }
class function TVariantHelper.Equal(const L, R: Variant): Boolean;
begin
  Result := CompareByte(L, R, SizeOf(Variant)) = 0;
end;
{$POP}

class function TVariantHelper.Less(const L, R: Variant): Boolean;
begin
  Result := VarCompareValue(L, R) = vrLessThan;
end;

function TStringArrayHelper.GetLength: SizeInt;
begin
  Result := System.Length(Self);
end;

procedure TStringArrayHelper.SetLen(aValue: SizeInt);
begin
  System.SetLength(Self, aValue);
end;

function TStringArrayHelper.IsEmpty: Boolean;
begin
  Result := Self = nil;
end;

function TStringArrayHelper.NonEmpty: Boolean;
begin
  Result := Self <> nil;
end;

procedure TStringArrayHelper.Add(const aValue: string);
var
  len: SizeInt;
begin
  len := System.Length(Self);
  System.SetLength(Self, len + 1);
  Self[len] := aValue;
end;

{ TPointHelper }

class function TPointHelper.HashCode(const aValue: TPoint): SizeInt;
begin
{$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := HashFunc.HashQWord(QWord(aValue));
{$ELSE }
  Result := HashFunc.HashBuf(@aValue, SizeOf(aValue));
{$ENDIF }
end;

class function TPointHelper.Equal(const L, R: TPoint): Boolean;
begin
  Result := L = R;
end;

{ TPrioTaskHelper }

class function TPrioTaskHelper.Less(L, R: IPriorityTask): Boolean;
begin
  Result := L.GetPriority < R.GetPriority;
end;

function CompareShortInt(const L, R: ShortInt): Boolean;
begin
  Result := ShortInt.Less(L, R);
end;

function CompareUByte(const L, R: Byte): Boolean;
begin
  Result := Byte.Less(L, R);
end;

function CompareSmallInt(const L, R: SmallInt): Boolean;
begin
  Result := SmallInt.Less(L, R);
end;

function CompareWord(const L, R: Word): Boolean;
begin
  Result := Word.Less(L, R);
end;

function CompareLongInt(const L, R: LongInt): Boolean;
begin
  Result := LongInt.Less(L, R);
end;

function CompareDWord(const L, R: DWord): Boolean;
begin
  Result := DWord.Less(L, R);
end;

function CompareInt64(const L, R: Int64): Boolean;
begin
  Result := Int64.Less(L, R);
end;

function CompareQWord(const L, R: QWord): Boolean;
begin
  Result := QWord.Less(L, R);
end;

function CompareChar(const L, R: AnsiChar): Boolean;
begin
  Result := AnsiChar.Less(L, R);
end;

function CompareSingle(const L, R: Single): Boolean;
begin
  Result := Single.Less(L, R);
end;

function CompareDouble(const L, R: Double): Boolean;
begin
  Result := Double.Less(L, R);
end;

function CompareExtended(const L, R: Extended): Boolean;
begin
  Result := Extended.Less(L, R);
end;

{$IF DECLARED(Comp)}
function CompareComp(const L, R: Comp): Boolean;
begin
  Result := Comp.Less(L, R);
end;
{$ENDIF}

function CompareCurrency(const L, R: Currency): Boolean;
begin
  Result := Currency.Less(L, R);
end;

function CompareShortStr(const L, R: shortstring): Boolean;
begin
  Result := shortstring.Less(L, R);
end;

function CompareLStr(const L, R: string): Boolean;
begin
  Result := ansistring.Less(L, R);
end;

function CompareAStr(const L, R: ansistring): Boolean;
begin
  Result := ansistring.Less(L, R);
end;

function CompareWStr(const L, R: widestring): Boolean;
begin
  Result := widestring.Less(L, R);
end;

function CompareVariant(const L, R: Variant): Boolean;
begin
  Result := Variant.Less(L, R);
end;

function CompareObj(const L, R: TObject): Boolean;
begin
  Result := TObject.Less(L, R);
end;

function CompareWChar(const L, R: WideChar): Boolean;
begin
  Result := WideChar.Less(L, R);
end;

function CompareUStr(const L, R: unicodestring): Boolean;
begin
  Result := unicodestring.Less(L, R);
end;

function ComparePointer(const L, R: Pointer): Boolean;
begin
  Result := Pointer.Less(L, R);
end;

function ShortIntEqual(const L, R: ShortInt): Boolean;
begin
  Result := ShortInt.Equal(L, R);
end;

function UByteEqual(const L, R: Byte): Boolean;
begin
  Result := Byte.Equal(L, R);
end;

function SmallIntEqual(const L, R: SmallInt): Boolean;
begin
  Result := SmallInt.Equal(L, R);
end;

function WordEqual(const L, R: Word): Boolean;
begin
  Result := Word.Equal(L, R);
end;

function LongIntEqual(const L, R: LongInt): Boolean;
begin
  Result := LongInt.Equal(L, R);
end;

function DWordEqual(const L, R: DWord): Boolean;
begin
  Result := DWord.Equal(L, R);
end;

function Int64Equal(const L, R: Int64): Boolean;
begin
  Result := Int64.Equal(L, R);
end;

function QWordEqual(const L, R: QWord): Boolean;
begin
  Result := QWord.Equal(L, R);
end;

function CharEqual(const L, R: AnsiChar): Boolean;
begin
  Result := AnsiChar.Equal(L, R);
end;

function SingleEqual(const L, R: Single): Boolean;
begin
  Result := Single.Equal(L, R);
end;

function DoubleEqual(const L, R: Double): Boolean;
begin
  Result := Double.Equal(L, R);
end;

function ExtendedEqual(const L, R: Extended): Boolean;
begin
  Result := Extended.Equal(L, R);
end;

function CompEqual(const L, R: Comp): Boolean;
begin
  Result := Comp.Equal(L, R);
end;

function CurrencyEqual(const L, R: Currency): Boolean;
begin
  Result := Currency.Equal(L, R);
end;

function ShortStrEqual(const L, R: shortstring): Boolean;
begin
  Result := shortstring.Equal(L, R);
end;

function LStrEqual(const L, R: string): Boolean;
begin
  Result := ansistring.Equal(L, R);
end;

function AStrEqual(const L, R: ansistring): Boolean;
begin
  Result := ansistring.Equal(L, R);
end;

function WStrEqual(const L, R: widestring): Boolean;
begin
  Result := widestring.Equal(L, R);
end;

function VariantEqual(const L, R: Variant): Boolean;
begin
  Result := Variant.Equal(L, R);
end;

function ObjEqual(const L, R: TObject): Boolean;
begin
  Result := TObject.Equal(L, R);
end;

function WCharEqual(const L, R: WideChar): Boolean;
begin
  Result := WideChar.Equal(L, R);
end;

function UStrEqual(const L, R: unicodestring): Boolean;
begin
  Result := unicodestring.Equal(L, R);
end;

function PointerEqual(const L, R: Pointer): Boolean;
begin
  Result := Pointer.Equal(L, R);
end;

function HashShortInt(const aValue: ShortInt): SizeInt;
begin
  Result := ShortInt.HashCode(aValue);
end;

function HashUByte(const aValue: Byte): SizeInt;
begin
  Result := Byte.HashCode(aValue);
end;

function HashSmallInt(const aValue: SmallInt): SizeInt;
begin
  Result := SmallInt.HashCode(aValue);
end;

function HashWord(const aValue: Word): SizeInt;
begin
  Result := Word.HashCode(aValue);
end;

function HashLongInt(const aValue: LongInt): SizeInt;
begin
  Result := LongInt.HashCode(aValue);
end;

function HashDWord(const aValue: DWord): SizeInt;
begin
  Result := DWord.HashCode(aValue);
end;

function HashInt64(const aValue: Int64): SizeInt;
begin
  Result := Int64.HashCode(aValue);
end;

function HashQWord(const aValue: QWord): SizeInt;
begin
  Result := QWord.HashCode(aValue);
end;

function HashChar(const aValue: AnsiChar): SizeInt;
begin
  Result := AnsiChar.HashCode(aValue);
end;

function HashSingle(const aValue: Single): SizeInt;
begin
  Result := Single.HashCode(aValue);
end;

function HashDouble(const aValue: Double): SizeInt;
begin
  Result := Double.HashCode(aValue);
end;

function HashExtended(const aValue: Extended): SizeInt;
begin
  Result := Extended.HashCode(aValue);
end;

function HashComp(const aValue: Comp): SizeInt;
begin
  Result := Comp.HashCode(aValue);
end;

function HashCurrency(const aValue: Currency): SizeInt;
begin
  Result := Currency.HashCode(aValue);
end;

function HashShortStr(const aValue: shortstring): SizeInt;
begin
  Result := shortstring.HashCode(aValue);
end;

function HashLStr(const aValue: string): SizeInt;
begin
  Result := ansistring.HashCode(aValue);
end;

function HashAStr(const aValue: ansistring): SizeInt;
begin
  Result := ansistring.HashCode(aValue);
end;

function HashWStr(const aValue: widestring): SizeInt;
begin
  Result := widestring.HashCode(aValue);
end;

function HashVariant(const aValue: Variant): SizeInt;
begin
  Result := Variant.HashCode(aValue);
end;

function HashObj(const aValue: TObject): SizeInt;
begin
  Result := TObject.HashCode(aValue);
end;

function HashWChar(const aValue: WideChar): SizeInt;
begin
  Result := WideChar.HashCode(aValue);
end;

function HashUStr(const aValue: unicodestring): SizeInt;
begin
  Result := unicodestring.HashCode(aValue);
end;

function HashPointer(const aValue: Pointer): SizeInt;
begin
  Result := Pointer.HashCode(aValue);
end;

{ TGDefaults.TComparer }

class function TGDefaults.TComparer.Less(const L, R: T): Boolean;
begin
  Result := CFLess(L, R);
end;

{ TGDefaults.TEqualityComparer }

class function TGDefaults.TEqualityComparer.Equal(const L, R: T): Boolean;
begin
  Result := CFEqualCompare(L, R);
end;

class function TGDefaults.TEqualityComparer.HashCode(const aValue: T): SizeInt;
begin
  Result := CFHashCode(aValue);
end;

{ TGDefaults.TComparator }

function TGDefaults.TComparator.LessCompare(const L, R: T): Boolean;
begin
  Result := CFLess(L, R);
end;

function TGDefaults.TComparator.EqualCompare(const L, R: T): Boolean;
begin
  Result := CFEqualCompare(L, R);
end;

{ TGDefaultComparer }

class constructor TGDefaults.Init;
var
  p: PTypeInfo;
begin
  p := System.TypeInfo(T);
  if p <> nil then
    case p^.Kind of
      tkInteger:
        InitInt(GetTypeData(p));
      tkChar:
        begin
          CFLess := TLess(@CompareChar);
          CFEqualCompare := TEqualCompare(@CharEqual);
          CFHashCode := THashCode(@HashChar);
        end;
      tkFloat:
        InitFloat(GetTypeData(p));
      tkSString:
        begin
          CFLess := TLess(@CompareShortStr);
          CFEqualCompare := TEqualCompare(@ShortStrEqual);
          CFHashCode := THashCode(@HashShortStr);
        end;
      tkLString:
        begin
          CFLess := TLess(@CompareLStr);
          CFEqualCompare := TEqualCompare(@LStrEqual);
          CFHashCode := THashCode(@HashLStr);
        end;
      tkAString:
        begin
          CFLess := TLess(@CompareAStr);
          CFEqualCompare := TEqualCompare(@AStrEqual);
          CFHashCode := THashCode(@HashAStr);
        end;
      tkWString:
        begin
          CFLess := TLess(@CompareWStr);
          CFEqualCompare := TEqualCompare(@WStrEqual);
          CFHashCode := THashCode(@HashWStr);
        end;
      tkVariant:
        begin
          CFLess := TLess(@CompareVariant);
          CFEqualCompare := TEqualCompare(@VariantEqual);
          CFHashCode := THashCode(@HashVariant);
        end;
      tkClass:
        begin
          CFLess := TLess(@CompareObj);
          CFEqualCompare := TEqualCompare(@ObjEqual);
          CFHashCode := THashCode(@HashObj);
        end;
      tkWChar:
        begin
          CFLess := TLess(@CompareWChar);
          CFEqualCompare := TEqualCompare(@WCharEqual);
          CFHashCode := THashCode(@HashWChar);
        end;
      tkInt64:
        begin
          CFLess := TLess(@CompareInt64);
          CFEqualCompare := TEqualCompare(@Int64Equal);
          CFHashCode := THashCode(@HashInt64);
        end;
      tkQWord:
        begin
          CFLess := TLess(@CompareQWord);
          CFEqualCompare := TEqualCompare(@QWordEqual);
          CFHashCode := THashCode(@HashQWord);
        end;
      tkUString:
        begin
          CFLess := TLess(@CompareUStr);
          CFEqualCompare := TEqualCompare(@UStrEqual);
          CFHashCode := THashCode(@HashUStr);
        end;
      tkUChar:
        begin
          CFLess := TLess(@CompareWChar);
          CFEqualCompare := TEqualCompare(@WCharEqual);
          CFHashCode := THashCode(@HashWChar);
        end;
      tkPointer:
        begin
          CFLess := TLess(@ComparePointer);
          CFEqualCompare := TEqualCompare(@PointerEqual);
          CFHashCode := THashCode(@HashPointer);
        end;
    else
      CFLess := TLess(@CompareBin);
      CFEqualCompare := TEqualCompare(@EqualBin);
      CFHashCode := THashCode(@HashBin);
    end
  else
    begin
      CFLess := TLess(@CompareBin);
      CFEqualCompare := TEqualCompare(@EqualBin);
      CFHashCode := THashCode(@HashBin);
    end;
end;

class procedure TGDefaults.InitInt(aData: PTypeData);
begin
  case aData^.OrdType of
    otSByte:
      begin
        CFLess := TLess(@CompareShortInt);
        CFEqualCompare := TEqualCompare(@ShortIntEqual);
        CFHashCode := THashCode(@HashShortInt);
      end;
    otUByte:
      begin
        CFLess := TLess(@CompareUByte);
        CFEqualCompare := TEqualCompare(@UByteEqual);
        CFHashCode := THashCode(@HashUByte);
      end;
    otSWord:
      begin
        CFLess := TLess(@CompareSmallInt);
        CFEqualCompare := TEqualCompare(@SmallIntEqual);
        CFHashCode := THashCode(@HashSmallInt);
      end;
    otUWord:
      begin
        CFLess := TLess(@CompareWord);
        CFEqualCompare := TEqualCompare(@WordEqual);
        CFHashCode := THashCode(@HashWord);
      end;
    otSLong:
      begin
        CFLess := TLess(@CompareLongInt);
        CFEqualCompare := TEqualCompare(@LongIntEqual);
        CFHashCode := THashCode(@HashLongInt);
      end;
    otULong:
      begin
        CFLess := TLess(@CompareDWord);
        CFEqualCompare := TEqualCompare(@DWordEqual);
        CFHashCode := THashCode(@HashDWord);
      end;
    otSQWord:
      begin
        CFLess := TLess(@CompareInt64);
        CFEqualCompare := TEqualCompare(@Int64Equal);
        CFHashCode := THashCode(@HashInt64);
      end;
    otUQWord:
      begin
        CFLess := TLess(@CompareQWord);
        CFEqualCompare := TEqualCompare(@QWordEqual);
        CFHashCode := THashCode(@HashQWord);
      end;
  end;
end;

class procedure TGDefaults.InitFloat(aData: PTypeData);
begin
  case aData^.FloatType of
    ftSingle:
      begin
        CFLess := TLess(@CompareSingle);
        CFEqualCompare := TEqualCompare(@SingleEqual);
        CFHashCode := THashCode(@HashSingle);
      end;
    ftDouble:
      begin
        CFLess := TLess(@CompareDouble);
        CFEqualCompare := TEqualCompare(@DoubleEqual);
        CFHashCode := THashCode(@HashDouble);
      end;
    ftExtended:
      begin
        CFLess := TLess(@CompareExtended);
        CFEqualCompare := TEqualCompare(@ExtendedEqual);
        CFHashCode := THashCode(@HashExtended);
      end;
    ftComp:
      begin
        CFLess := TLess(@CompareComp);
        CFEqualCompare := TEqualCompare(@CompEqual);
        CFHashCode := THashCode(@HashComp);
      end;
    ftCurr:
      begin
        CFLess := TLess(@CompareCurrency);
        CFEqualCompare := TEqualCompare(@CurrencyEqual);
        CFHashCode := THashCode(@HashCurrency);
      end;
  end;
end;

class function TGDefaults.CompareBin(const L, R: T): Boolean;
begin
  Result := CompareMemRange(@L, @R, SizeOf(T)) < 0;
end;

class function TGDefaults.EqualBin(const L, R: T): Boolean;
begin
  Result := CompareMemRange(@L, @R, SizeOf(T)) = 0;
end;

class function TGDefaults.HashBin(const aValue: T): SizeInt;
begin
  Result := HashFunc.HashBuf(@aValue, SizeOf(T));
end;

class function TGDefaults.GetOnLess: TOnLess;
begin
  Result := @CFComparator.LessCompare;
end;

class function TGDefaults.GetOnEqualCompare: TOnEqualCompare;
begin
  Result := @CFComparator.EqualCompare;
end;

end.

