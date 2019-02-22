{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Helpers for some basic types.                                           *
*                                                                           *
*   Copyright(c) 2018-2019 A.Koverdyaev(avk)                                *
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
unit LGHelpers;

{$MODE OBJFPC}{$H+}
{$MODESWITCH TYPEHELPERS}
{$MODESWITCH ADVANCEDRECORDS}
{$INLINE ON}

interface

uses
  Classes,
  SysUtils,
  typinfo,
  variants,
  LGUtils,
  LGHash;

type

  TGGuidHelper = record helper(TGuidHelper) for TGUID
  type
    class function HashCode(constref aValue: TGUID): SizeInt; static; inline;
    class function Equal(constref L, R: TGUID): Boolean; static; inline;
    class function Compare(constref L, R: TGUID): SizeInt; static; inline;
  end;

  TStrHelper = type helper for string
    class function HashCode(constref aValue: string): SizeInt; static; inline;
    class function Equal(constref L, R: string): Boolean; static; inline;
    class function Compare(constref L, R: string): SizeInt; static; inline;
  end;

  TAStrHelper = type helper(TStringHelper) for ansistring
    class function HashCode(constref aValue: ansistring): SizeInt; static; inline;
    class function Equal(constref L, R: ansistring): Boolean; static; inline;
    class function Compare(constref L, R: ansistring): SizeInt; static;
  end;

  TWStrHelper = type helper for widestring
    class function HashCode(constref aValue: widestring): SizeInt; static; inline;
    class function Equal(constref L, R: widestring): Boolean; static; inline;
    class function Compare(constref L, R: widestring): SizeInt; static;
  end;

  TUStrHelper = type helper for unicodestring
    class function HashCode(constref aValue: unicodestring): SizeInt; static; inline;
    class function Equal(constref L, R: unicodestring): Boolean; static; inline;
    class function Compare(constref L, R: unicodestring): SizeInt; static;
  end;

  TShortStrHelper = type helper for shortstring
    class function HashCode(constref aValue: shortstring): SizeInt; static; inline;
    class function Equal(constref L, R: shortstring): Boolean; static; inline;
    class function Compare(constref L, R: shortstring): SizeInt; static; inline;
  end;

  TGACharHelper = type helper for AnsiChar
    class function HashCode(aValue: AnsiChar): SizeInt; static; inline;
    class function Equal(L, R: AnsiChar): Boolean; static; inline;
    class function Compare(L, R: AnsiChar): SizeInt; static; inline;
  end;

  TGWCharHelper = type helper for WideChar
    class function HashCode(aValue: WideChar): SizeInt; static; inline;
    class function Equal(L, R: WideChar): Boolean; static; inline;
    class function Compare(L, R: WideChar): SizeInt; static; inline;
  end;

  TGByteHelper = type helper(TByteHelper) for Byte
    class function HashCode(aValue: Byte): SizeInt; static; inline;
    class function Equal(L, R: Byte): Boolean; static; inline;
    class function Compare(L, R: Byte): SizeInt; static; inline;
  end;

  TGShortIntHelper = type helper(TShortIntHelper) for ShortInt
    class function HashCode(aValue: ShortInt): SizeInt; static; inline;
    class function Equal(L, R: ShortInt): Boolean; static; inline;
    class function Compare(L, R: ShortInt): SizeInt; static; inline;
  end;

  TGWordHelper = type helper(TWordHelper) for Word
    class function HashCode(aValue: Word): SizeInt; static; inline;
    class function Equal(L, R: Word): Boolean; static; inline;
    class function Compare(L, R: Word): SizeInt; static; inline;
  end;

  TGSmallIntHelper = type helper(TSmallIntHelper) for SmallInt
    class function HashCode(aValue: SmallInt): SizeInt; static; inline;
    class function Equal(L, R: SmallInt): Boolean; static; inline;
    class function Compare(L, R: SmallInt): SizeInt; static; inline;
  end;

  TGDWordHelper = type helper(TCardinalHelper) for DWord
    class function HashCode(aValue: DWord): SizeInt; static; inline;
    class function Equal(L, R: DWord): Boolean; static; inline;
    class function Compare(L, R: DWord): SizeInt; static; inline;
  end;

  TGLongIntHelper = type helper(TIntegerHelper) for LongInt
    class function HashCode(aValue: LongInt): SizeInt; static; inline;
    class function Equal(L, R: LongInt): Boolean; static; inline;
    class function Compare(L, R: LongInt): SizeInt; static; inline;
  end;

  TGQWordHelper = type helper(TQWordHelper) for QWord
    class function HashCode(aValue: QWord): SizeInt; static; inline;
    class function Equal(L, R: QWord): Boolean; static; inline;
    class function Compare(L, R: QWord): SizeInt; static; inline;
  end;

  TGInt64Helper = type helper(TInt64Helper) for Int64
    class function HashCode(aValue: Int64): SizeInt; static; inline;
    class function Equal(L, R: Int64): Boolean; static; inline;
    class function Compare(L, R: Int64): SizeInt; static; inline;
  end;

  TGNativeUIntHelper = type helper(TNativeUIntHelper) for NativeUInt
    class function HashCode(aValue: NativeUInt): SizeInt; static; inline;
    class function Equal(L, R: NativeUInt): Boolean; static; inline;
    class function Compare(L, R: NativeUInt): SizeInt; static; inline;
  end;

  TGNativeIntHelper = type helper(TNativeIntHelper) for NativeInt
    class function HashCode(aValue: NativeInt): SizeInt; static; inline;
    class function Equal(L, R: NativeInt): Boolean; static; inline;
    class function Compare(L, R: NativeInt): SizeInt; static; inline;
  end;

  TGPointerHelper = type helper for Pointer
    class function HashCode(aValue: Pointer): SizeInt; static; inline;
    class function Equal(L, R: Pointer): Boolean; static; inline;
    class function Compare(L, R: Pointer): SizeInt; static; inline;
  end;

 // currently special float types (nan, infinity) are ignored

  TGSingleHelper = type helper(TSingleHelper) for Single
    function IsZero: Boolean; inline;
    class function HashCode(aValue: Single): SizeInt; static; inline;
    class function Equal(L, R: Single): Boolean; static; inline;
    class function Compare(L, R: Single): SizeInt; static; inline;
  end;

  TGDoubleHelper = type helper(TDoubleHelper) for Double
    function IsZero: Boolean; inline;
    class function HashCode(aValue: Double): SizeInt; static; inline;
    class function Equal(L, R: Double): Boolean; static; inline;
    class function Compare(L, R: Double): SizeInt; static; inline;
  end;

{$ifdef FPC_HAS_TYPE_EXTENDED}
  TGExtendedHelper = type helper(TExtendedHelper) for Extended
    function IsZero: Boolean; inline;
    class function HashCode(aValue: Extended): SizeInt; static; inline;
    class function Equal(L, R: Extended): Boolean; static; inline;
    class function Compare(L, R: Extended): SizeInt; static; inline;
  end;
{$ENDIF}

  TCompHelper = type helper for Comp
    class function HashCode(aValue: Comp): SizeInt; static; inline;
    class function Equal(L, R: Comp): Boolean; static; inline;
    class function Compare(L, R: Comp): SizeInt; static; inline;
  end;

  TGDateTimeHelper = type helper for TDateTime
    class function HashCode(aValue: TDateTime): SizeInt; static; inline;
    class function Equal(L, R: TDateTime): Boolean; static; inline;
    class function Compare(L, R: TDateTime): SizeInt; static; inline;
  end;

  TGDateHelper = type helper for TDate
    class function HashCode(aValue: TDate): SizeInt; static; inline;
    class function Equal(L, R: TDate): Boolean; static; inline;
    class function Compare(L, R: TDate): SizeInt; static; inline;
  end;

  TGTimeHelper = type helper for TTime
    class function HashCode(aValue: TTime): SizeInt; static; inline;
    class function Equal(L, R: TTime): Boolean; static; inline;
    class function Compare(L, R: TTime): SizeInt; static; inline;
  end;

  TGCurrencyHelper = type helper for Currency
  const
    MinValue: Currency = -922337203685477.5808;
    MaxValue: Currency = 922337203685477.5807;
    class function HashCode(const aValue: Currency): SizeInt; static; inline;
    class function Equal(const L, R: Currency): Boolean; static; inline;
    class function Compare(const L, R: Currency): SizeInt; static; inline;
    function ToString: string; inline;
  end;

  TGObjectHelper = class helper for TObject
    class function HashCode(aValue: TObject): SizeInt; static; inline;
    class function Equal(L, R: TObject): Boolean; static; inline;
    class function Compare(L, R: TObject): SizeInt; static; inline;
  end;

  TVariantHelper = type helper for Variant
    class function HashCode(constref aValue: Variant): SizeInt; static; inline;
    class function Equal(constref L, R: Variant): Boolean; static; inline;
    class function Compare(constref L, R: Variant): SizeInt; static; inline;
  end;

  TStringArrayHelper = type helper for TStringArray
  private
    function  GetLength: SizeInt;
    procedure SetLen(aValue: SizeInt);
  public
    function  IsEmpty: Boolean;
    function  NonEmpty: Boolean;
    procedure Add(constref aValue: string);
    property  Length: SizeInt read GetLength write SetLen;
  end;

  TPointHelper = type helper for TPoint
    class function HashCode(constref aValue: TPoint): SizeInt; static; inline;
    class function Equal(constref L, R: TPoint): Boolean; static; inline;
  end;

  TPrioTaskHelper = type helper for IPriorityTask
    class function Compare(L, R: IPriorityTask): SizeInt; static; inline;
  end;

  PTypeInfo = TypInfo.PTypeInfo;
  PTypeData = TypInfo.PTypeData;

  generic TGDefaults<T> = class
  public
  type
    TCompare          = specialize TGCompare<T>;
    TOnCompare        = specialize TGOnCompare<T>;
    TEqualCompare     = specialize TGEqualCompare<T>;
    TOnEqualCompare   = specialize TGOnEqualCompare<T>;

    TComparer = class
      class function Compare(constref L, R: T): SizeInt; static; inline;
    end;

    TEqualityComparer = class
      class function Equal(constref L, R: T): Boolean; static; inline;
      class function HashCode(constref aValue: T): SizeInt; static; inline;
    end;

  private
  type
    THashCode = function(constref aValue: T): SizeInt;

    TComparator = object
      function Compare(constref L, R: T): SizeInt;
      function EqualCompare(constref L, R: T): Boolean;
    end;

  class var
    CFCompare: TCompare;
    CFEqualCompare: TEqualCompare;
    CFHashCode: THashCode;
    CFComparator: TComparator;
    class constructor Init;
    class procedure InitInt(aData: PTypeData); static;
    class procedure InitFloat(aData: PTypeData); static;
    class function CompareBin(constref L, R: T): SizeInt; static;
    class function EqualBin(constref L, R: T): Boolean; static;
    class function HashBin(constref aValue: T): SizeInt; static;
    class function GetOnCompare: TOnCompare; static; inline;
    class function GetOnEqualCompare: TOnEqualCompare; static; inline;
  public
    class property Compare: TCompare read CFCompare;
    class property EqualCompare: TEqualCompare read CFEqualCompare;
    class property OnCompare: TOnCompare read GetOnCompare;
    class property OnEqualCompare: TOnEqualCompare read GetOnEqualCompare;
  end;

  function CompareShortInt(constref L, R: ShortInt): SizeInt;
  function CompareUByte(constref L, R: Byte): SizeInt;
  function CompareSmallInt(constref L, R: SmallInt): SizeInt;
  function CompareWord(constref L, R: Word): SizeInt;
  function CompareLongInt(constref L, R: LongInt): SizeInt;
  function CompareDWord(constref L, R: DWord): SizeInt;
  function CompareInt64(constref L, R: Int64): SizeInt;
  function CompareQWord(constref L, R: QWord): SizeInt;
  function CompareChar(constref L, R: AnsiChar): SizeInt;
  function CompareSingle(constref L, R: Single): SizeInt;
  function CompareDouble(constref L, R: Double): SizeInt;
  function CompareExtended(constref L, R: Extended): SizeInt;
  function CompareComp(constref L, R: Comp): SizeInt;
  function CompareCurrency(constref L, R: Currency): SizeInt;
  function CompareShortStr(constref L, R: shortstring): SizeInt;
  function CompareLStr(constref L, R: string): SizeInt;
  function CompareAStr(constref L, R: ansistring): SizeInt;
  function CompareWStr(constref L, R: widestring): SizeInt;
  function CompareVariant(constref L, R: Variant): SizeInt;
  function CompareObj(constref L, R: TObject): SizeInt;
  function CompareWChar(constref L, R: WideChar): SizeInt;
  function CompareUStr(constref L, R: unicodestring): SizeInt;
  function ComparePointer(constref L, R: Pointer): SizeInt;

  function ShortIntEqual(constref L, R: ShortInt): Boolean;
  function UByteEqual(constref L, R: Byte): Boolean;
  function SmallIntEqual(constref L, R: SmallInt): Boolean;
  function WordEqual(constref L, R: Word): Boolean;
  function LongIntEqual(constref L, R: LongInt): Boolean;
  function DWordEqual(constref L, R: DWord): Boolean;
  function Int64Equal(constref L, R: Int64): Boolean;
  function QWordEqual(constref L, R: QWord): Boolean;
  function CharEqual(constref L, R: AnsiChar): Boolean;
  function SingleEqual(constref L, R: Single): Boolean;
  function DoubleEqual(constref L, R: Double): Boolean;
  function ExtendedEqual(constref L, R: Extended): Boolean;
  function CompEqual(constref L, R: Comp): Boolean;
  function CurrencyEqual(constref L, R: Currency): Boolean;
  function ShortStrEqual(constref L, R: shortstring): Boolean;
  function LStrEqual(constref L, R: string): Boolean;
  function AStrEqual(constref L, R: ansistring): Boolean;
  function WStrEqual(constref L, R: widestring): Boolean;
  function VariantEqual(constref L, R: Variant): Boolean;
  function ObjEqual(constref L, R: TObject): Boolean;
  function WCharEqual(constref L, R: WideChar): Boolean;
  function UStrEqual(constref L, R: unicodestring): Boolean;
  function PointerEqual(constref L, R: Pointer): Boolean;

  function HashShortInt(constref aValue: ShortInt): SizeInt;
  function HashUByte(constref aValue: Byte): SizeInt;
  function HashSmallInt(constref aValue: SmallInt): SizeInt;
  function HashWord(constref aValue: Word): SizeInt;
  function HashLongInt(constref aValue: LongInt): SizeInt;
  function HashDWord(constref aValue: DWord): SizeInt;
  function HashInt64(constref aValue: Int64): SizeInt;
  function HashQWord(constref aValue: QWord): SizeInt;
  function HashChar(constref aValue: AnsiChar): SizeInt;
  function HashSingle(constref aValue: Single): SizeInt;
  function HashDouble(constref aValue: Double): SizeInt;
  function HashExtended(constref aValue: Extended): SizeInt;
  function HashComp(constref aValue: Comp): SizeInt;
  function HashCurrency(constref aValue: Currency): SizeInt;
  function HashShortStr(constref aValue: shortstring): SizeInt;
  function HashLStr(constref aValue: string): SizeInt;
  function HashAStr(constref aValue: ansistring): SizeInt;
  function HashWStr(constref aValue: widestring): SizeInt;
  function HashVariant(constref aValue: Variant): SizeInt;
  function HashObj(constref aValue: TObject): SizeInt;
  function HashWChar(constref aValue: WideChar): SizeInt;
  function HashUStr(constref aValue: unicodestring): SizeInt;
  function HashPointer(constref aValue: Pointer): SizeInt;

implementation
{$Q-}{$B-}{$COPERATORS ON}{$MACRO ON}
{$DEFINE HashFunc := TxxHash32LE}
{.$DEFINE HashFunc := TMurmur3LE}

class function TGGuidHelper.HashCode(constref aValue: TGUID): SizeInt;
begin
  Result := HashFunc.HashGuid(aValue);
end;

class function TGGuidHelper.Equal(constref L, R: TGUID): Boolean;
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

class function TGGuidHelper.Compare(constref L, R: TGUID): SizeInt;
begin
  // todo: compare as 128-bit numbers ???
  Result := CompareMemRange(@L, @R, SizeOf(TGUID));
end;

{ TStrHelper }

class function TStrHelper.HashCode(constref aValue: string): SizeInt;
begin
  Result := HashFunc.HashStr(aValue);
end;

class function TStrHelper.Equal(constref L, R: string): Boolean;
begin
  Result := L = R;
end;

class function TStrHelper.Compare(constref L, R: string): SizeInt;
begin
  Result := CompareStr(L, R);
end;

class function TAStrHelper.HashCode(constref aValue: ansistring): SizeInt;
begin
  Result := HashFunc.HashStr(aValue);
end;

class function TAStrHelper.Equal(constref L, R: ansistring): Boolean;
begin
  Result := L = R;
end;

class function TAStrHelper.Compare(constref L, R: ansistring): SizeInt;
begin
  //Result := StrComp(PAnsiChar(L), PAnsiChar(R));
  Result := AnsiCompareStr(L, R);
end;

class function TWStrHelper.HashCode(constref aValue: widestring): SizeInt;
begin
  Result := HashFunc.HashBuf(PWideChar(aValue), System.Length(aValue) * SizeOf(System.WideChar));
end;

class function TWStrHelper.Equal(constref L, R: widestring): Boolean;
begin
  Result := L = R;
end;

class function TWStrHelper.Compare(constref L, R: widestring): SizeInt;
begin
  //Result := StrComp(PWideChar(L), PWideChar(R));
  Result := WideCompareStr(L, R);
end;

class function TUStrHelper.HashCode(constref aValue: unicodestring): SizeInt;
begin
  Result := HashFunc.HashBuf(PUnicodeChar(aValue), System.Length(aValue) * SizeOf(System.UnicodeChar));
end;

class function TUStrHelper.Equal(constref L, R: unicodestring): Boolean;
begin
  Result := L = R;
end;

class function TUStrHelper.Compare(constref L, R: unicodestring): SizeInt;
begin
  Result := UnicodeCompareStr(L, R);
end;

class function TShortStrHelper.HashCode(constref aValue: shortstring): SizeInt;
begin
  Result := HashFunc.HashBuf(@aValue[1], System.Length(aValue));
end;

class function TShortStrHelper.Equal(constref L, R: shortstring): Boolean;
begin
  Result := L = R;
end;

class function TShortStrHelper.Compare(constref L, R: shortstring): SizeInt;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
end;

class function TGACharHelper.HashCode(aValue: AnsiChar): SizeInt;
begin
  //Result := Ord(aValue);
  Result := Ord(aValue) xor Ord(aValue) shr 5;
end;

class function TGACharHelper.Equal(L, R: AnsiChar): Boolean;
begin
  Result := L = R;
end;

class function TGACharHelper.Compare(L, R: AnsiChar): SizeInt;
begin
  Result := SizeInt(L) - SizeInt(R);
end;

class function TGWCharHelper.HashCode(aValue: WideChar): SizeInt;
begin
  Result := HashFunc.HashWord(Word(aValue));
end;

class function TGWCharHelper.Equal(L, R: WideChar): Boolean;
begin
  Result := L = R;
end;

class function TGWCharHelper.Compare(L, R: WideChar): SizeInt;
begin
{$IFNDEF CPU16}
  Result := SizeInt(L) - SizeInt(R);
{$ELSE !CPU16}
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
{$ENDIF !CPU16}
end;

class function TGByteHelper.HashCode(aValue: Byte): SizeInt;
begin
  Result := aValue xor aValue shr 5;
end;

class function TGByteHelper.Equal(L, R: Byte): Boolean;
begin
  Result := L = R;
end;

class function TGByteHelper.Compare(L, R: Byte): SizeInt;
begin
  Result := SizeInt(L) - SizeInt(R);
end;

class function TGShortIntHelper.HashCode(aValue: ShortInt): SizeInt;
begin
  Result := aValue xor aValue shr 5;
end;

class function TGShortIntHelper.Equal(L, R: ShortInt): Boolean;
begin
  Result := L = R;
end;

class function TGShortIntHelper.Compare(L, R: ShortInt): SizeInt;
begin
  Result := SizeInt(L) - SizeInt(R);
end;

class function TGWordHelper.HashCode(aValue: Word): SizeInt;
begin
  Result := HashFunc.HashWord(aValue);
end;

class function TGWordHelper.Equal(L, R: Word): Boolean;
begin
  Result := L = R;
end;

class function TGWordHelper.Compare(L, R: Word): SizeInt;
begin
{$IFNDEF CPU16}
  Result := SizeInt(L) - SizeInt(R);
{$ELSE !CPU16}
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
{$ENDIF !CPU16}
end;

class function TGSmallIntHelper.HashCode(aValue: SmallInt): SizeInt;
begin
  Result := HashFunc.HashWord(Word(aValue));
end;

class function TGSmallIntHelper.Equal(L, R: SmallInt): Boolean;
begin
  Result := L = R;
end;

class function TGSmallIntHelper.Compare(L, R: SmallInt): SizeInt;
begin
{$IFNDEF CPU16}
  Result := SizeInt(L) - SizeInt(R);
{$ELSE !CPU16}
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
{$ENDIF !CPU16}
end;

class function TGDWordHelper.HashCode(aValue: DWord): SizeInt;
begin
  Result := HashFunc.HashDWord(aValue);
end;

class function TGDWordHelper.Equal(L, R: DWord): Boolean;
begin
  Result := L = R;
end;

class function TGDWordHelper.Compare(L, R: DWord): SizeInt;
begin
{$IFDEF CPU64}
  Result := SizeInt(L) - SizeInt(R);
{$ELSE CPU64}
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
{$ENDIF CPU64}
end;

class function TGLongIntHelper.HashCode(aValue: LongInt): SizeInt;
begin
  Result := SizeInt(HashFunc.HashDWord(DWord(aValue)));
end;

class function TGLongIntHelper.Equal(L, R: LongInt): Boolean;
begin
  Result := L = R;
end;

class function TGLongIntHelper.Compare(L, R: LongInt): SizeInt;
begin
{$IFDEF CPU64}
  Result := SizeInt(L) - SizeInt(R);
{$ELSE CPU64}
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
{$ENDIF CPU64}
end;

class function TGQWordHelper.HashCode(aValue: QWord): SizeInt;
begin
  Result := HashFunc.HashQWord(aValue);
end;

class function TGQWordHelper.Equal(L, R: QWord): Boolean;
begin
  Result := L = R;
end;

class function TGQWordHelper.Compare(L, R: QWord): SizeInt;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
end;

class function TGInt64Helper.HashCode(aValue: Int64): SizeInt;
begin
  Result := HashFunc.HashQWord(QWord(aValue));
end;

class function TGInt64Helper.Equal(L, R: Int64): Boolean;
begin
  Result := L = R;
end;

class function TGInt64Helper.Compare(L, R: Int64): SizeInt;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
end;

class function TGNativeUIntHelper.HashCode(aValue: NativeUInt): SizeInt;
begin
{$IF DEFINED(CPU64)}
  Result := HashFunc.HashQWord(aValue);
{$ELSEIF DEFINED(CPU32)}
  Result := HashFunc.HashDWord(aValue);
{$ELSE}
  Result := HashFunc.HasWord(aValue);
{$ENDIF}
end;

class function TGNativeUIntHelper.Equal(L, R: NativeUInt): Boolean;
begin
  Result := L = R;
end;

class function TGNativeUIntHelper.Compare(L, R: NativeUInt): SizeInt;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
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

class function TGNativeIntHelper.Compare(L, R: NativeInt): SizeInt;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
end;

class function TGPointerHelper.HashCode(aValue: Pointer): SizeInt;
begin
  Result := SizeUInt.HashCode({%H-}SizeUInt(aValue));
end;

class function TGPointerHelper.Equal(L, R: Pointer): Boolean;
begin
  Result := L = R;
end;

class function TGPointerHelper.Compare(L, R: Pointer): SizeInt;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
end;

function TGSingleHelper.IsZero: Boolean;
begin
  Result:= (DWord(Self) and $7fffffff) = 0;
end;

class function TGSingleHelper.HashCode(aValue: Single): SizeInt;
begin
  if aValue.IsZero then
    Result := HashFunc.HashDWord(DWord(Single(0.0)))
  else
    Result := HashFunc.HashDWord(DWord(aValue));
end;

class function TGSingleHelper.Equal(L, R: Single): Boolean;
begin
  Result := L = R;
end;

class function TGSingleHelper.Compare(L, R: Single): SizeInt;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
end;

function TGDoubleHelper.IsZero: Boolean;
begin
  Result := (QWord(Self) and $7fffffffffffffff) = 0;
end;

class function TGDoubleHelper.HashCode(aValue: Double): SizeInt;
begin
  if aValue.IsZero then
    Result := HashFunc.HashQWord(QWord(Double(0.0)))
  else
    Result := HashFunc.HashQWord(QWord(aValue));
end;

class function TGDoubleHelper.Equal(L, R: Double): Boolean;
begin
  Result := L = R;
end;

class function TGDoubleHelper.Compare(L, R: Double): SizeInt;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
end;
{$ifdef FPC_HAS_TYPE_EXTENDED}
function TGExtendedHelper.IsZero: Boolean;
begin
  Result := SpecialType < fsDenormal;
end;

class function TGExtendedHelper.HashCode(aValue: Extended): SizeInt;
const
  Zero: Extended = 0.0;
begin
  if aValue.IsZero then
    Result := HashFunc.HashBuf(@Zero, SizeOf(Zero))
  else
    Result := HashFunc.HashBuf(@aValue, SizeOf(aValue));
end;

class function TGExtendedHelper.Equal(L, R: Extended): Boolean;
begin
  Result := L = R;
end;

class function TGExtendedHelper.Compare(L, R: Extended): SizeInt;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
end;
{$ENDIF}

{ TCompHelper }

class function TCompHelper.HashCode(aValue: Comp): SizeInt;
begin
  Result := QWord.HashCode(QWord(aValue));
end;

class function TCompHelper.Equal(L, R: Comp): Boolean;
begin
  Result := L = R;
end;

class function TCompHelper.Compare(L, R: Comp): SizeInt;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
end;

class function TGDateTimeHelper.HashCode(aValue: TDateTime): SizeInt;
begin
  Result := HashFunc.HashQWord(QWord(aValue));
end;

class function TGDateTimeHelper.Equal(L, R: TDateTime): Boolean;
begin
  Result := L = R;
end;

class function TGDateTimeHelper.Compare(L, R: TDateTime): SizeInt;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
end;

class function TGDateHelper.HashCode(aValue: TDate): SizeInt;
begin
  Result := HashFunc.HashQWord(QWord(aValue));
end;

class function TGDateHelper.Equal(L, R: TDate): Boolean;
begin
  Result := L = R;
end;

class function TGDateHelper.Compare(L, R: TDate): SizeInt;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
end;

class function TGTimeHelper.HashCode(aValue: TTime): SizeInt;
begin
  Result := HashFunc.HashQWord(QWord(aValue));
end;

class function TGTimeHelper.Equal(L, R: TTime): Boolean;
begin
  Result := L = R;
end;

class function TGTimeHelper.Compare(L, R: TTime): SizeInt;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
end;

class function TGCurrencyHelper.HashCode(const aValue: Currency): SizeInt;
begin
  Result := HashFunc.HashQWord(QWord(aValue));
end;

class function TGCurrencyHelper.Equal(const L, R: Currency): Boolean;
begin
  Result := L = R;
end;

class function TGCurrencyHelper.Compare(const L, R: Currency): SizeInt;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
end;

function TGCurrencyHelper.ToString: string;
begin
  Result := CurrToStr(Self);
end;

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

class function TGObjectHelper.Compare(L, R: TObject): SizeInt;
begin
  Result := Pointer.Compare(Pointer(L), Pointer(R));
end;

class function TVariantHelper.HashCode(constref aValue: Variant): SizeInt;
begin
  Result := HashFunc.HashBuf(@aValue, SizeOf(System.Variant), 0);
end;

class function TVariantHelper.Equal(constref L, R: Variant): Boolean;
begin
  Result := VarCompareValue(L, R) = vrEqual;
end;

class function TVariantHelper.Compare(constref L, R: Variant): SizeInt;
begin
  case VarCompareValue(L, R) of
    vrGreaterThan: Result := 1;
    vrLessThan:    Result := -1;
    vrEqual:       Result := 0;
  else //vrNotEqual
    if VarIsEmpty(L) or VarIsNull(L) then
      Result := 1
    else
      Result := -1;
  end;
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

procedure TStringArrayHelper.Add(constref aValue: string);
var
  len: SizeInt;
begin
  len := System.Length(Self);
  System.SetLength(Self, len + 1);
  Self[len] := aValue;
end;

{ TPointHelper }

class function TPointHelper.HashCode(constref aValue: TPoint): SizeInt;
begin
{$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := HashFunc.HashQWord(QWord(aValue));
{$ELSE }
  Result := HashFunc.HashBuf(@aValue, SizeOf(aValue));
{$ENDIF }
end;

class function TPointHelper.Equal(constref L, R: TPoint): Boolean;
begin
  Result := L = R;
end;

{ TPrioTaskHelper }

class function TPrioTaskHelper.Compare(L, R: IPriorityTask): SizeInt;
begin
  Result := SizeInt(L.GetPriority) - SizeInt(R.GetPriority);
end;

function CompareShortInt(constref L, R: ShortInt): SizeInt;
begin
  Result := ShortInt.Compare(L, R);
end;

function CompareUByte(constref L, R: Byte): SizeInt;
begin
  Result := Byte.Compare(L, R);
end;

function CompareSmallInt(constref L, R: SmallInt): SizeInt;
begin
  Result := SmallInt.Compare(L, R);
end;

function CompareWord(constref L, R: Word): SizeInt;
begin
  Result := Word.Compare(L, R);
end;

function CompareLongInt(constref L, R: LongInt): SizeInt;
begin
  Result := LongInt.Compare(L, R);
end;

function CompareDWord(constref L, R: DWord): SizeInt;
begin
  Result := DWord.Compare(L, R);
end;

function CompareInt64(constref L, R: Int64): SizeInt;
begin
  Result := Int64.Compare(L, R);
end;

function CompareQWord(constref L, R: QWord): SizeInt;
begin
  Result := QWord.Compare(L, R);
end;

function CompareChar(constref L, R: AnsiChar): SizeInt;
begin
  Result := AnsiChar.Compare(L, R);
end;

function CompareSingle(constref L, R: Single): SizeInt;
begin
  Result := Single.Compare(L, R);
end;

function CompareDouble(constref L, R: Double): SizeInt;
begin
  Result := Double.Compare(L, R);
end;

function CompareExtended(constref L, R: Extended): SizeInt;
begin
  Result := Extended.Compare(L, R);
end;

function CompareComp(constref L, R: Comp): SizeInt;
begin
  Result := Comp.Compare(L, R);
end;

function CompareCurrency(constref L, R: Currency): SizeInt;
begin
  Result := Currency.Compare(L, R);
end;

function CompareShortStr(constref L, R: shortstring): SizeInt;
begin
  Result := shortstring.Compare(L, R);
end;

function CompareLStr(constref L, R: string): SizeInt;
begin
  Result := ansistring.Compare(L, R);
end;

function CompareAStr(constref L, R: ansistring): SizeInt;
begin
  Result := ansistring.Compare(L, R);
end;

function CompareWStr(constref L, R: widestring): SizeInt;
begin
  Result := widestring.Compare(L, R);
end;

function CompareVariant(constref L, R: Variant): SizeInt;
begin
  Result := Variant.Compare(L, R);
end;

function CompareObj(constref L, R: TObject): SizeInt;
begin
  Result := TObject.Compare(L, R);
end;

function CompareWChar(constref L, R: WideChar): SizeInt;
begin
  Result := WideChar.Compare(L, R);
end;

function CompareUStr(constref L, R: unicodestring): SizeInt;
begin
  Result := unicodestring.Compare(L, R);
end;

function ComparePointer(constref L, R: Pointer): SizeInt;
begin
  Result := Pointer.Compare(L, R);
end;

function ShortIntEqual(constref L, R: ShortInt): Boolean;
begin
  Result := ShortInt.Equal(L, R);
end;

function UByteEqual(constref L, R: Byte): Boolean;
begin
  Result := Byte.Equal(L, R);
end;

function SmallIntEqual(constref L, R: SmallInt): Boolean;
begin
  Result := SmallInt.Equal(L, R);
end;

function WordEqual(constref L, R: Word): Boolean;
begin
  Result := Word.Equal(L, R);
end;

function LongIntEqual(constref L, R: LongInt): Boolean;
begin
  Result := LongInt.Equal(L, R);
end;

function DWordEqual(constref L, R: DWord): Boolean;
begin
  Result := DWord.Equal(L, R);
end;

function Int64Equal(constref L, R: Int64): Boolean;
begin
  Result := Int64.Equal(L, R);
end;

function QWordEqual(constref L, R: QWord): Boolean;
begin
  Result := QWord.Equal(L, R);
end;

function CharEqual(constref L, R: AnsiChar): Boolean;
begin
  Result := AnsiChar.Equal(L, R);
end;

function SingleEqual(constref L, R: Single): Boolean;
begin
  Result := Single.Equal(L, R);
end;

function DoubleEqual(constref L, R: Double): Boolean;
begin
  Result := Double.Equal(L, R);
end;

function ExtendedEqual(constref L, R: Extended): Boolean;
begin
  Result := Extended.Equal(L, R);
end;

function CompEqual(constref L, R: Comp): Boolean;
begin
  Result := Comp.Equal(L, R);
end;

function CurrencyEqual(constref L, R: Currency): Boolean;
begin
  Result := Currency.Equal(L, R);
end;

function ShortStrEqual(constref L, R: shortstring): Boolean;
begin
  Result := shortstring.Equal(L, R);
end;

function LStrEqual(constref L, R: string): Boolean;
begin
  Result := ansistring.Equal(L, R);
end;

function AStrEqual(constref L, R: ansistring): Boolean;
begin
  Result := ansistring.Equal(L, R);
end;

function WStrEqual(constref L, R: widestring): Boolean;
begin
  Result := widestring.Equal(L, R);
end;

function VariantEqual(constref L, R: Variant): Boolean;
begin
  Result := Variant.Equal(L, R);
end;

function ObjEqual(constref L, R: TObject): Boolean;
begin
  Result := TObject.Equal(L, R);
end;

function WCharEqual(constref L, R: WideChar): Boolean;
begin
  Result := WideChar.Equal(L, R);
end;

function UStrEqual(constref L, R: unicodestring): Boolean;
begin
  Result := unicodestring.Equal(L, R);
end;

function PointerEqual(constref L, R: Pointer): Boolean;
begin
  Result := Pointer.Equal(L, R);
end;

function HashShortInt(constref aValue: ShortInt): SizeInt;
begin
  Result := ShortInt.HashCode(aValue);
end;

function HashUByte(constref aValue: Byte): SizeInt;
begin
  Result := Byte.HashCode(aValue);
end;

function HashSmallInt(constref aValue: SmallInt): SizeInt;
begin
  Result := SmallInt.HashCode(aValue);
end;

function HashWord(constref aValue: Word): SizeInt;
begin
  Result := Word.HashCode(aValue);
end;

function HashLongInt(constref aValue: LongInt): SizeInt;
begin
  Result := LongInt.HashCode(aValue);
end;

function HashDWord(constref aValue: DWord): SizeInt;
begin
  Result := DWord.HashCode(aValue);
end;

function HashInt64(constref aValue: Int64): SizeInt;
begin
  Result := Int64.HashCode(aValue);
end;

function HashQWord(constref aValue: QWord): SizeInt;
begin
  Result := QWord.HashCode(aValue);
end;

function HashChar(constref aValue: AnsiChar): SizeInt;
begin
  Result := AnsiChar.HashCode(aValue);
end;

function HashSingle(constref aValue: Single): SizeInt;
begin
  Result := Single.HashCode(aValue);
end;

function HashDouble(constref aValue: Double): SizeInt;
begin
  Result := Double.HashCode(aValue);
end;

function HashExtended(constref aValue: Extended): SizeInt;
begin
  Result := Extended.HashCode(aValue);
end;

function HashComp(constref aValue: Comp): SizeInt;
begin
  Result := Comp.HashCode(aValue);
end;

function HashCurrency(constref aValue: Currency): SizeInt;
begin
  Result := Currency.HashCode(aValue);
end;

function HashShortStr(constref aValue: shortstring): SizeInt;
begin
  Result := shortstring.HashCode(aValue);
end;

function HashLStr(constref aValue: string): SizeInt;
begin
  Result := ansistring.HashCode(aValue);
end;

function HashAStr(constref aValue: ansistring): SizeInt;
begin
  Result := ansistring.HashCode(aValue);
end;

function HashWStr(constref aValue: widestring): SizeInt;
begin
  Result := widestring.HashCode(aValue);
end;

function HashVariant(constref aValue: Variant): SizeInt;
begin
  Result := Variant.HashCode(aValue);
end;

function HashObj(constref aValue: TObject): SizeInt;
begin
  Result := TObject.HashCode(aValue);
end;

function HashWChar(constref aValue: WideChar): SizeInt;
begin
  Result := WideChar.HashCode(aValue);
end;

function HashUStr(constref aValue: unicodestring): SizeInt;
begin
  Result := unicodestring.HashCode(aValue);
end;

function HashPointer(constref aValue: Pointer): SizeInt;
begin
  Result := Pointer.HashCode(aValue);
end;

{ TGDefaults.TComparer }

class function TGDefaults.TComparer.Compare(constref L, R: T): SizeInt;
begin
  Result := CFCompare(L, R);
end;

{ TGDefaults.TEqualityComparer }

class function TGDefaults.TEqualityComparer.Equal(constref L, R: T): Boolean;
begin
  Result := CFEqualCompare(L, R);
end;

class function TGDefaults.TEqualityComparer.HashCode(constref aValue: T): SizeInt;
begin
  Result := CFHashCode(aValue);
end;

{ TGDefaults.TComparator }

function TGDefaults.TComparator.Compare(constref L, R: T): SizeInt;
begin
  Result := CFCompare(L, R);
end;

function TGDefaults.TComparator.EqualCompare(constref L, R: T): Boolean;
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
    begin
      case p^.Kind of
        tkInteger:
          InitInt(GetTypeData(p));
        tkChar:
          begin
            CFCompare := TCompare(@CompareChar);
            CFEqualCompare := TEqualCompare(@CharEqual);
            CFHashCode := THashCode(@HashChar);
          end;
        tkFloat:
          InitFloat(GetTypeData(p));
        tkSString:
          begin
            CFCompare := TCompare(@CompareShortStr);
            CFEqualCompare := TEqualCompare(@ShortStrEqual);
            CFHashCode := THashCode(@HashShortStr);
          end;
        tkLString:
          begin
            CFCompare := TCompare(@CompareLStr);
            CFEqualCompare := TEqualCompare(@LStrEqual);
            CFHashCode := THashCode(@HashLStr);
          end;
        tkAString:
          begin
            CFCompare := TCompare(@CompareAStr);
            CFEqualCompare := TEqualCompare(@AStrEqual);
            CFHashCode := THashCode(@HashAStr);
          end;
        tkWString:
          begin
            CFCompare := TCompare(@CompareWStr);
            CFEqualCompare := TEqualCompare(@WStrEqual);
            CFHashCode := THashCode(@HashWStr);
          end;
        tkVariant:
          begin
            CFCompare := TCompare(@CompareVariant);
            CFEqualCompare := TEqualCompare(@VariantEqual);
            CFHashCode := THashCode(@HashVariant);
          end;
        tkClass:
          begin
            CFCompare := TCompare(@CompareObj);
            CFEqualCompare := TEqualCompare(@ObjEqual);
            CFHashCode := THashCode(@HashObj);
          end;
        tkWChar:
          begin
            CFCompare := TCompare(@CompareWChar);
            CFEqualCompare := TEqualCompare(@WCharEqual);
            CFHashCode := THashCode(@HashWChar);
          end;
        tkInt64:
          begin
            CFCompare := TCompare(@CompareInt64);
            CFEqualCompare := TEqualCompare(@Int64Equal);
            CFHashCode := THashCode(@HashInt64);
          end;
        tkQWord:
          begin
            CFCompare := TCompare(@CompareQWord);
            CFEqualCompare := TEqualCompare(@QWordEqual);
            CFHashCode := THashCode(@HashQWord);
          end;
        tkUString:
          begin
            CFCompare := TCompare(@CompareUStr);
            CFEqualCompare := TEqualCompare(@UStrEqual);
            CFHashCode := THashCode(@HashUStr);
          end;
        tkUChar:
          begin
            CFCompare := TCompare(@CompareWChar);
            CFEqualCompare := TEqualCompare(@WCharEqual);
            CFHashCode := THashCode(@HashWChar);
          end;
        tkPointer:
          begin
            CFCompare := TCompare(@ComparePointer);
            CFEqualCompare := TEqualCompare(@PointerEqual);
            CFHashCode := THashCode(@HashPointer);
          end;
      else
        CFCompare := TCompare(@CompareBin);
        CFEqualCompare := TEqualCompare(@EqualBin);
        CFHashCode := THashCode(@HashBin);
      end;
    end
  else
    begin
      CFCompare := TCompare(@CompareBin);
      CFEqualCompare := TEqualCompare(@EqualBin);
      CFHashCode := THashCode(@HashBin);
    end;
end;

class procedure TGDefaults.InitInt(aData: PTypeData);
begin
  case aData^.OrdType of
    otSByte:
      begin
        CFCompare := TCompare(@CompareShortInt);
        CFEqualCompare := TEqualCompare(@ShortIntEqual);
        CFHashCode := THashCode(@HashShortInt);
      end;
    otUByte:
      begin
        CFCompare := TCompare(@CompareUByte);
        CFEqualCompare := TEqualCompare(@UByteEqual);
        CFHashCode := THashCode(@HashUByte);
      end;
    otSWord:
      begin
        CFCompare := TCompare(@CompareSmallInt);
        CFEqualCompare := TEqualCompare(@SmallIntEqual);
        CFHashCode := THashCode(@HashSmallInt);
      end;
    otUWord:
      begin
        CFCompare := TCompare(@CompareWord);
        CFEqualCompare := TEqualCompare(@WordEqual);
        CFHashCode := THashCode(@HashWord);
      end;
    otSLong:
      begin
        CFCompare := TCompare(@CompareLongInt);
        CFEqualCompare := TEqualCompare(@LongIntEqual);
        CFHashCode := THashCode(@HashLongInt);
      end;
    otULong:
      begin
        CFCompare := TCompare(@CompareDWord);
        CFEqualCompare := TEqualCompare(@DWordEqual);
        CFHashCode := THashCode(@HashDWord);
      end;
    otSQWord:
      begin
        CFCompare := TCompare(@CompareInt64);
        CFEqualCompare := TEqualCompare(@Int64Equal);
        CFHashCode := THashCode(@HashInt64);
      end;
    otUQWord:
      begin
        CFCompare := TCompare(@CompareQWord);
        CFEqualCompare := TEqualCompare(@QWordEqual);
        CFHashCode := THashCode(@HashQWord);
      end;
  else
    CFCompare := TCompare(@CompareBin);
    CFEqualCompare := TEqualCompare(@EqualBin);
    CFHashCode := THashCode(@HashBin);
  end;
end;

class procedure TGDefaults.InitFloat(aData: PTypeData);
begin
  case aData^.FloatType of
    ftSingle:
      begin
        CFCompare := TCompare(@CompareSingle);
        CFEqualCompare := TEqualCompare(@SingleEqual);
        CFHashCode := THashCode(@HashSingle);
      end;
    ftDouble:
      begin
        CFCompare := TCompare(@CompareDouble);
        CFEqualCompare := TEqualCompare(@DoubleEqual);
        CFHashCode := THashCode(@HashDouble);
      end;
    ftExtended:
      begin
        CFCompare := TCompare(@CompareExtended);
        CFEqualCompare := TEqualCompare(@ExtendedEqual);
        CFHashCode := THashCode(@HashExtended);
      end;
    ftComp:
      begin
        CFCompare := TCompare(@CompareComp);
        CFEqualCompare := TEqualCompare(@CompEqual);
        CFHashCode := THashCode(@HashComp);
      end;
    ftCurr:
      begin
        CFCompare := TCompare(@CompareCurrency);
        CFEqualCompare := TEqualCompare(@CurrencyEqual);
        CFHashCode := THashCode(@HashCurrency);
      end;
  else
    CFCompare := TCompare(@CompareBin);
    CFEqualCompare := TEqualCompare(@EqualBin);
    CFHashCode := THashCode(@HashBin);
  end;
end;

class function TGDefaults.CompareBin(constref L, R: T): SizeInt;
begin
  Result := CompareMemRange(@L, @R, SizeOf(T));
end;

class function TGDefaults.EqualBin(constref L, R: T): Boolean;
begin
  Result := CompareMemRange(@L, @R, SizeOf(T)) = 0;
end;

class function TGDefaults.HashBin(constref aValue: T): SizeInt;
begin
  Result := HashFunc.HashBuf(@aValue, SizeOf(T));
end;

class function TGDefaults.GetOnCompare: TOnCompare;
begin
  Result := @CFComparator.Compare;
end;

class function TGDefaults.GetOnEqualCompare: TOnEqualCompare;
begin
  Result := @CFComparator.EqualCompare;
end;

end.

