{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Some useful string helpers.                                             *
*                                                                           *
*   Copyright(c) 2018-2021 A.Koverdyaev(avk)                                *
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
unit lgStrHelpers;

{$mode objfpc}{$H+}
{$MODESWITCH TYPEHELPERS}
{$MODESWITCH ADVANCEDRECORDS}
{$INLINE ON}

interface

uses

  Classes, SysUtils, Math, RegExpr,
  lgUtils,
  lgHelpers,
  lgArrayHelpers,
  lgAbstractContainer,
  lgMiscUtils;

type

  TAnsiStrHelper = type helper(TAStrHelper) for ansistring
  private
  type
    TStrEnumerable = class(specialize TGAutoEnumerable<string>)
    private
      FString: string;
      FStartIndex,
      FCurrLen,
      FCurrIndex: SizeInt;
      FDelimiters: TSysCharSet;
    protected
      function  GetCurrent: string; override;
    public
      constructor Create(const aValue: string; const aDelimiters: TSysCharSet);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

  public
  type
    IStrEnumerable = specialize IGEnumerable<string>;

  const
    WhiteSpaces = [#0..' '];
    AsciiDelimiters = [#0..#255] - ['a'..'z', 'A'..'Z', '0'..'9', '_'];
    function StripWhiteSpaces: ansistring; inline;
    function StripChar(aChar: AnsiChar): ansistring;
    function StripChars(constref aChars: TSysCharSet): ansistring;
    // only single byte delimiters allowed
    function Words(constref aDelimiters: TSysCharSet = AsciiDelimiters): IStrEnumerable; inline;
  end;

  TRegexMatch = class
  protected
  type
    TStrEnumerable = class(specialize TGAutoEnumerable<string>)
    private
      FRegex: TRegExpr;
      FInputString: string;
      FInCycle: Boolean;
    protected
      function  GetCurrent: string; override;
    public
      constructor Create(aRegex: TRegExpr; const s: string);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

  var
    FRegex: TRegExpr;
    function  GetExpression: string;
    function  GetModifierStr: string;
    procedure SetExpression(aValue: string);
    procedure SetModifierStr(aValue: string);
  public
  type
    IStrEnumerable = specialize IGEnumerable<string>;

    constructor Create;
    constructor Create(const aRegExpression: string);
    constructor Create(const aRegExpression, aModifierStr: string);
    destructor Destroy; override;
    function Matches(const aValue: string): IStrEnumerable; inline;
    property Expression: string read GetExpression write SetExpression;
    property ModifierStr: string read GetModifierStr write SetModifierStr;
  end;

  TStringListHelper = class helper for TStringList
  public
  type
    IStrEnumerable = specialize IGEnumerable<string>;

    function AsEnumerable: IStrEnumerable; inline;
  end;

  { TBmSearch implements Boyer-Moore exact string matching algorithm  in a variant somewhat
    similar to Fast-Search from D.Cantone, S.Faro: "Fast-Search: A New Efﬁcient Variant of
    the Boyer-Moore String Matching Algorithm" 2003 }
  TBmSearch = record
  private
  type
    PMatcher = ^TBmSearch;

    TStrEnumerator = record
    private
      FCurrIndex: SizeInt;
      FHeap: ansistring;
      FMatcher: PMatcher;
      function GetCurrent: SizeInt; inline;
    public
      function MoveNext: Boolean;
      property Current: SizeInt read GetCurrent;
    end;

    TByteEnumerator = record
      FCurrIndex,
      FHeapLen: SizeInt;
      FHeap: PByte;
      FMatcher: PMatcher;
      function GetCurrent: SizeInt; inline;
    public
      function MoveNext: Boolean;
      property Current: SizeInt read GetCurrent;
    end;

  var
    FBcShift: array[Byte] of Integer; //bad character shifts
    FGsShift: array of Integer;       //good suffix shifts
    FNeedle: ansistring;
    procedure FillBc;
    procedure FillGs;
    function  DoFind(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
    function  FindNext(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
    function  Find(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
  public
  type
    TIntArray = array of SizeInt;

    TStrMatches = record
    private
      FHeap: string;
      FMatcher: PMatcher;
    public
      function GetEnumerator: TStrEnumerator; inline;
    end;

    TByteMatches = record
    private
      FHeapLen: SizeInt;
      FHeap: PByte;
      FMatcher: PMatcher;
    public
      function GetEnumerator: TByteEnumerator; inline;
    end;
  { initializes the algorithm with a search pattern }
    constructor Create(const aPattern: ansistring);
    constructor Create(const aPattern: array of Byte);
  { returns an enumerator of indices(1-based) of all occurrences of pattern in s }
    function Matches(const s: ansistring): TStrMatches; inline;
  { returns an enumerator of indices(0-based) of all occurrences of pattern in a }
    function Matches(const a: array of Byte): TByteMatches;
  { returns the index of the next occurrence of the pattern in s,
    starting at index aOffset(1-based) or 0 if there is no occurrence;
    to get the index of the next occurrence, you need to pass in aOffset
    the index of the previous occurrence, increased by one }
    function NextMatch(const s: ansistring; aOffset: SizeInt = 1): SizeInt;
  { returns the index of the next occurrence of the pattern in a,
    starting at index aOffset(0-based) or -1 if there is no occurrence;
    to get the index of the next occurrence, you need to pass in aOffset
    the index of the previous occurrence, increased by one }
    function NextMatch(const a: array of Byte; aOffset: SizeInt = 0): SizeInt;
  { returns in an array the indices(1-based) of all occurrences of the pattern in s }
    function FindMatches(const s: ansistring): TIntArray;
  { returns in an array the indices(0-based) of all occurrences of the pattern in a }
    function FindMatches(const a: array of Byte): TIntArray;
  end;

  { TBmhrSearch implements a variant of the Boyer-Moore-Horspool-Raita algorithm;
    degrades noticeably on short alphabets }
  TBmhrSearch = record
  private
  type
    PMatcher = ^TBmhrSearch;

    TStrEnumerator = record
    private
      FCurrIndex: SizeInt;
      FHeap: ansistring;
      FMatcher: PMatcher;
      function GetCurrent: SizeInt; inline;
    public
      function MoveNext: Boolean;
      property Current: SizeInt read GetCurrent;
    end;

    TByteEnumerator = record
      FCurrIndex,
      FHeapLen: SizeInt;
      FHeap: PByte;
      FMatcher: PMatcher;
      function GetCurrent: SizeInt; inline;
    public
      function MoveNext: Boolean;
      property Current: SizeInt read GetCurrent;
    end;
  var
    FBcShift: array[Byte] of Integer; //bad character shifts
    FNeedle: ansistring;
    procedure FillBc;
    function  Find(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
    function  FindNext(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
  public
  type
    TIntArray = array of SizeInt;

    TStrMatches = record
    private
      FHeap: string;
      FMatcher: PMatcher;
    public
      function GetEnumerator: TStrEnumerator; inline;
    end;

    TByteMatches = record
    private
      FHeapLen: SizeInt;
      FHeap: PByte;
      FMatcher: PMatcher;
    public
      function GetEnumerator: TByteEnumerator; inline;
    end;
  { initializes the algorithm with a search pattern }
    constructor Create(const aPattern: ansistring);
    constructor Create(const aPattern: array of Byte);
  { returns an enumerator of indices(1-based) of all occurrences of pattern in s }
    function Matches(const s: ansistring): TStrMatches; inline;
  { returns an enumerator of indices(0-based) of all occurrences of pattern in a }
    function Matches(const a: array of Byte): TByteMatches;
  { returns the index of the next occurrence of the pattern in s,
    starting at index aOffset(1-based) or 0 if there is no occurrence;
    to get the index of the next occurrence, you need to pass in aOffset
    the index of the previous occurrence, increased by one }
    function NextMatch(const s: ansistring; aOffset: SizeInt = 1): SizeInt;
  { returns the index of the next occurrence of the pattern in a,
    starting at index aOffset(0-based) or -1 if there is no occurrence;
    to get the index of the next occurrence, you need to pass in aOffset
    the index of the previous occurrence, increased by one }
    function NextMatch(const a: array of Byte; aOffset: SizeInt = 0): SizeInt;
  { returns in an array the indices(1-based) of all occurrences of the pattern in s }
    function FindMatches(const s: ansistring): TIntArray;
  { returns in an array the indices(0-based) of all occurrences of the pattern in a }
    function FindMatches(const a: array of Byte): TIntArray;
  end;

  { TBmSearchCI implements case insensitive variant of TBmSearch }
  TBmSearchCI = record
  private
  type
    PMatcher = ^TBmSearchCI;

    TEnumerator = record
    private
      FCurrIndex: SizeInt;
      FHeap: ansistring;
      FMatcher: PMatcher;
      function GetCurrent: SizeInt; inline;
    public
      function MoveNext: Boolean;
      property Current: SizeInt read GetCurrent;
    end;

  var
    FLoCaseMap: array[Byte] of Byte;
    FBcShift: array[Byte] of Integer; //bad character shifts
    FGsShift: array of Integer;       //good suffix shifts
    FNeedle: ansistring;
    procedure FillMap;
    procedure FillBc;
    procedure FillGs;
    function  DoFind(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
    function  FindNext(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
    function  Find(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
  public
  type
    TIntArray = array of SizeInt;

    TMatches = record
    private
      FHeap: string;
      FMatcher: PMatcher;
    public
      function GetEnumerator: TEnumerator; inline;
    end;
  { initializes the algorithm with a search pattern }
    constructor Create(const aPattern: ansistring);
  { returns an enumerator of indices(1-based) of all occurrences of pattern in s }
    function Matches(const s: ansistring): TMatches; inline;
  { returns the index of the next occurrence of the pattern in s,
    starting at index aOffset(1-based) or 0 if there is no occurrence;
    to get the index of the next occurrence, you need to pass in aOffset
    the index of the previous occurrence, increased by one }
    function NextMatch(const s: ansistring; aOffset: SizeInt = 1): SizeInt;
  { returns in an array the indices(1-based) of all occurrences of the pattern in s }
    function FindMatches(const s: ansistring): TIntArray;
  end;

{the following functions with string parameters are only suitable for single-byte encodings }

{ returns True if aSub is a subsequence of aStr, False otherwise }
  function IsSubSequence(const aStr, aSub: ansistring): Boolean; inline;
{ returns the longest common subsequence(LCS) of sequences L and R, reducing the task to LIS,
  with O(RLogN) time complexity, where R is the number of the matching pairs in L and R;
  inspired by Dan Gusfield "Algorithms on Strings, Trees and Sequences", section 12.5 }
  function LcsGus(const L, R: ansistring): ansistring;
  function LcsGus(const L, R: array of Byte): TBytes;
{ returns the Levenshtein distance between L and R; used a simple dynamic programming
  algorithm with O(mn) time complexity, where n and m are the lengths of L and R respectively,
  and O(Max(m, n)) space complexity }
  function LevDistance(const L, R: ansistring): SizeInt;
  function LevDistance(const L, R: array of Byte): SizeInt;
{ returns the Levenshtein distance between L and R; a Pascal translation(well, almost :))
  of github.com/vaadin/gwt/dev/util/editdistance/ModifiedBerghelRoachEditDistance.java -
  a modified version of algorithm described by Berghel and Roach with O(min(n, m)*d)
  worst-case time complexity, where n and m are the lengths of L and R respectively
  and d is the edit distance computed }
  function LevDistanceMBR(const L, R: ansistring): SizeInt;
  function LevDistanceMBR(const L, R: array of Byte): SizeInt;
{ the same as above; the aLimit parameter indicates the maximum expected distance,
  if this value is exceeded when calculating the distance, then the function exits
  immediately and returns -1 }
  function LevDistanceMBR(const L, R: ansistring; aLimit: SizeInt): SizeInt;
  function LevDistanceMBR(const L, R: array of Byte; aLimit: SizeInt): SizeInt;
{ returns the Levenshtein distance between L and R; uses Myers's bit-vector algorithm }
  function LevDistanceMyers(const L, R: ansistring): SizeInt;
  function LevDistanceMyers(const L, R: array of Byte): SizeInt;
{ the same as above; the aLimit parameter indicates the maximum expected distance,
  if this value is exceeded when calculating the distance, then the function exits
  immediately and returns -1 }
  function LevDistanceMyers(const L, R: ansistring; aLimit: SizeInt): SizeInt;
  function LevDistanceMyers(const L, R: array of Byte; aLimit: SizeInt): SizeInt;

  function IsValidDotQuadIPv4(const s: ansistring): Boolean;
  function IsValidDotDecIPv4(const s: ansistring): Boolean;

implementation
{$B-}{$COPERATORS ON}

function IsSubSequence(const aStr, aSub: ansistring): Boolean;
begin
  Result := specialize TGSimpleArrayHelper<Byte>.IsSubSequence(
    PByte(aStr)[0..Pred(System.Length(aStr))], PByte(aSub)[0..Pred(System.Length(aSub))]);
end;

{$PUSH}{$WARN 5057 OFF}
function LcsGusImpl(pL, PR: PByte; aLenL, aLenR: SizeInt): TBytes;
type
  TNode = record
    Index,
    Next: SizeInt;
  end;
  TNodeList = array of TNode;
var
  MatchList: array[Byte] of SizeInt;
  NodeList: TNodeList;
  Tmp: TSizeIntArray;
  LocLis: TSizeIntArray;
  Tail: TBytes = nil;
  I, J, NodeIdx: SizeInt;
const
  INIT_SIZE = 256;
begin
  Result := nil;

  if pL = pR then
    exit(specialize TGArrayHelpUtil<Byte>.CreateCopy(pL[0..Pred(aLenL)]));

  I := 0;
  while (aLenL >= 0) and (pL[Pred(aLenL)] = pR[Pred(aLenR)]) do
    begin
      Dec(aLenL);
      Dec(aLenR);
      Inc(I);
    end;

  if I > 0 then
    Tail := specialize TGSimpleArrayHelper<Byte>.CreateCopy(pL[aLenL..Pred(aLenL + I)]);

  I := 0;
  while (I < aLenL) and (pL[I] = pR[I]) do
     Inc(I);
  if I > 0 then
    begin
      Result := specialize TGSimpleArrayHelper<Byte>.CreateCopy(pL[0..Pred(I)]);
      pL += I;
      pR += I;
      aLenL -= I;
      aLenR -= I;
    end;

  TSizeIntHelper.Fill(MatchList, NULL_INDEX);

  System.SetLength(NodeList, INIT_SIZE);
  J := 0;
  for I := 0 to Pred(aLenR) do
    begin
      if System.Length(NodeList) = J then
        System.SetLength(NodeList, J * 2);
      NodeList[J].Index := I;
      NodeList[J].Next := MatchList[pR[I]];
      MatchList[pR[I]] := J;
      Inc(J);
    end;

  System.SetLength(Tmp, INIT_SIZE);
  J := 0;
  for I := 0 to Pred(aLenL) do
    begin
      NodeIdx := MatchList[pL[I]];
      while NodeIdx <> NULL_INDEX do
        begin
          if System.Length(Tmp) = J then
            System.SetLength(Tmp, J * 2);
          Tmp[J] := NodeList[NodeIdx].Index;
          NodeIdx := NodeList[NodeIdx].Next;
          Inc(J);
        end;
    end;
  System.SetLength(Tmp, J);

  if Tmp <> nil then
    begin
      NodeList := nil;

      LocLis := TSizeIntHelper.Lis(Tmp);
      if LocLis = nil then
        begin
          System.SetLength(Result, Succ(System.Length(Result)));
          Result[System.High(Result)] := pR[Tmp[0]];
        end
      else
        begin
          Tmp := nil;
          J := System.Length(Result);
          System.SetLength(Result, J+System.Length(LocLis));
          for I := 0 to System.High(LocLis) do
            Result[I+J] := pR[LocLis[I]];
        end;
    end;
  if Tail <> nil then
    System.Insert(Tail, Result, System.Length(Result));
end;
{$POP}

function LcsGus(const L, R: ansistring): ansistring;
var
  b: TBytes;
begin
  Result := '';
  if (L = '') or (R = '') then
    exit;
  if System.Length(L) <= System.Length(R) then
    b := LcsGusImpl(Pointer(L), Pointer(R), System.Length(L), System.Length(R))
  else
    b := LcsGusImpl(Pointer(R), Pointer(L), System.Length(R), System.Length(L));
  System.SetLength(Result, System.Length(b));
  System.Move(Pointer(b)^, Pointer(Result)^, System.Length(b));
end;

function LcsGus(const L, R: array of Byte): TBytes;
begin
  if (System.Length(L) = 0) or (System.Length(R) = 0) then
    exit(nil);
  if System.Length(L) <= System.Length(R) then
    Result := LcsGusImpl(@L[0], @R[0], System.Length(L), System.Length(R))
  else
    Result := LcsGusImpl(@R[0], @L[0], System.Length(R), System.Length(L));
end;

const
  MAX_STATIC = 1024;

function LevDistanceDpImpl(pL, pR: PByte; aLenL, aLenR: SizeInt): SizeInt;
var
  StBuf: array[0..Pred(MAX_STATIC)] of SizeInt;
  Buf: array of SizeInt = nil;
  I, J, Prev, Next: SizeInt;
  Dist: PSizeInt;
  b: Byte;
begin
  //here aLenL <= aLenR
  if pL = pR then
    exit(aLenR - aLenL);

  while (aLenL > 0) and (pL[Pred(aLenL)] = pR[Pred(aLenR)]) do
    begin
      Dec(aLenL);
      Dec(aLenR);
    end;

  I := 0;
  while (I < aLenL) and (pL^ = pR^) do
    begin
      Inc(pL);
      Inc(pR);
      Inc(I);
    end;

  if I = aLenL then
    exit(aLenR - I);

  aLenL -= I;
  aLenR -= I;

  if aLenR < MAX_STATIC then
    Dist := @StBuf[0]
  else
    begin
      System.SetLength(Buf, Succ(aLenR));
      Dist := Pointer(Buf);
    end;
  for I := 0 to aLenR do
    Dist[I] := I;

  for I := 1 to aLenL do
    begin
      Prev := I;
      b := pL[I-1];
{$IFDEF CPU64}
      J := 1;
      while J < aLenL - 3 do
        begin
          Next := MinOf3(Dist[J-1]+Ord(pR[J-1]<>b), Prev+1, Dist[J]+1);
          Dist[J-1] := Prev;

          Prev := MinOf3(Dist[J]+Ord(pR[J]<>b), Next+1, Dist[J+1]+1);
          Dist[J] := Next;

          Next := MinOf3(Dist[J+1]+Ord(pR[J+1]<>b), Prev+1, Dist[J+2]+1);
          Dist[J+1] := Prev;

          Prev := MinOf3(Dist[J+2]+Ord(pR[J+2]<>b), Next+1, Dist[J+3]+1);
          Dist[J+2] := Next;

          J += 4;
        end;
      for J := J to aLenR do
{$ELSE CPU64}
      for J := 1 to aLenR do
{$ENDIF}
        begin
          if pR[J-1] = b then
            Next := Dist[J-1]
          else
            Next := Succ(MinOf3(Dist[J-1], Prev, Dist[J]));
          Dist[J-1] := Prev;
          Prev := Next;
        end;
      Dist[aLenR] := Prev;
    end;
  Result := Dist[aLenR];
end;

function LevDistance(const L, R: ansistring): SizeInt;
begin
  if System.Length(L) = 0 then
    exit(System.Length(R))
  else
    if System.Length(R) = 0 then
      exit(System.Length(L));
  if System.Length(L) <= System.Length(R) then
    Result := LevDistanceDpImpl(Pointer(L), Pointer(R), System.Length(L), System.Length(R))
  else
    Result := LevDistanceDpImpl(Pointer(R), Pointer(L), System.Length(R), System.Length(L));
end;

function LevDistance(const L, R: array of Byte): SizeInt;
begin
  if System.Length(L) = 0 then
    exit(System.Length(R))
  else
    if System.Length(R) = 0 then
      exit(System.Length(L));
  if System.Length(L) <= System.Length(R) then
    Result := LevDistanceDpImpl(@L[0], @R[0], System.Length(L), System.Length(R))
  else
    Result := LevDistanceDpImpl(@R[0], @L[0], System.Length(R), System.Length(L));
end;

function LevDistanceMbrImpl(pL, pR: PByte; aLenL, aLenR, aLimit: SizeInt): SizeInt;

  function FindRow(k, aDist, aLeft, aAbove, aRight: SizeInt): SizeInt;
  var
    I, MaxRow: SizeInt;
  begin
    if aDist = 0 then
      I := 0
    else
      I := MaxOf3(aLeft, aAbove + 1, aRight + 1);
    MaxRow := Min(aLenL - k, aLenR);
    while (I < MaxRow) and (pR[I] = pL[I + k]) do
      Inc(I);
    FindRow := I;
  end;

var
  StBuf: array[0..Pred(MAX_STATIC)] of SizeInt;
  Buf: array of SizeInt = nil;

  CurrL, CurrR, LastL, LastR, PrevL, PrevR: PSizeInt;
  I, DMain, Dist, Diagonal, CurrRight, CurrLeft, Row: SizeInt;
  tmp: Pointer;
  Even: Boolean = True;
begin
  //here aLenL <= aLenR
  if aLenR - aLenL > aLimit then
    exit(NULL_INDEX);

  if pL = pR then
    exit(aLenR - aLenL);

  while (aLenL > 0) and (pL[Pred(aLenL)] = pR[Pred(aLenR)]) do
    begin
      Dec(aLenL);
      Dec(aLenR);
    end;

  I := 0;
  while (I < aLenL) and (pL^ = pR^) do
    begin
      Inc(pL);
      Inc(pR);
      Inc(I);
    end;

  if I = aLenL then
    exit(aLenR - I);

  aLenL -= I;
  aLenR -= I;

  if aLimit > aLenR then
    aLimit := aLenR;

  DMain := aLenL - aLenR;
  Dist := -DMain;

  if aLimit < MAX_STATIC div 6 then
    begin
      CurrL := @StBuf[0];
      LastL := @StBuf[Succ(aLimit)];
      PrevL := @StBuf[Succ(aLimit)*2];
      CurrR := @StBuf[Succ(aLimit)*3];
      LastR := @StBuf[Succ(aLimit)*4];
      PrevR := @StBuf[Succ(aLimit)*5];
    end
  else
    begin
      System.SetLength(Buf, Succ(aLimit)*6);
      CurrL := Pointer(Buf);
      LastL := @Buf[Succ(aLimit)];
      PrevL := @Buf[Succ(aLimit)*2];
      CurrR := @Buf[Succ(aLimit)*3];
      LastR := @Buf[Succ(aLimit)*4];
      PrevR := @Buf[Succ(aLimit)*5];
    end;

  for I := 0 to Dist do
    begin
      LastR[I] := Dist - I - 1;
      PrevR[I] := NULL_INDEX;
    end;

  repeat

    Diagonal := (Dist - DMain) div 2;
    if Even then
      LastR[Diagonal] := NULL_INDEX;

    CurrRight := NULL_INDEX;

    while Diagonal > 0 do
      begin
        CurrRight :=
          FindRow( DMain + Diagonal, Dist - Diagonal, PrevR[Diagonal - 1], LastR[Diagonal], CurrRight);
        CurrR[Diagonal] := CurrRight;
        Dec(Diagonal);
      end;

    Diagonal := (Dist + DMain) div 2;

    if Even then
      begin
        LastL[Diagonal] := Pred((Dist - DMain) div 2);
        CurrLeft := NULL_INDEX;
      end
    else
      CurrLeft := (Dist - DMain) div 2;

    while Diagonal > 0 do
      begin
        CurrLeft :=
          FindRow(DMain - Diagonal, Dist - Diagonal, CurrLeft, LastL[Diagonal], PrevL[Diagonal - 1]);
        CurrL[Diagonal] := CurrLeft;
        Dec(Diagonal);
      end;

    Row := FindRow(DMain, Dist, CurrLeft, LastL[0], CurrRight);

    if Row = aLenR then
      break;

    Inc(Dist);
    if Dist > aLimit then
      exit(NULL_INDEX);

    CurrR[0] := Row;
    CurrL[0] := Row;

    tmp := PrevL;
    PrevL := LastL;
    LastL := CurrL;
    CurrL := tmp;

    tmp := PrevR;
    PrevR := LastR;
    LastR := CurrR;
    CurrR := tmp;

    Even := not Even;

  until False;

  Result := Dist;
end;

function LevDistanceMBR(const L, R: ansistring): SizeInt;
begin
  if L = '' then
    exit(System.Length(R))
  else
    if R = '' then
      exit(System.Length(L));
  if System.Length(L) <= System.Length(R) then
    Result := LevDistanceMbrImpl(Pointer(L), Pointer(R), System.Length(L), System.Length(R), System.Length(R))
  else
    Result := LevDistanceMbrImpl(Pointer(R), Pointer(L), System.Length(R), System.Length(L), System.Length(L));
end;

function LevDistanceMBR(const L, R: array of Byte): SizeInt;
begin
  if System.Length(L) = 0 then
    exit(System.Length(R))
  else
    if System.Length(R) = 0 then
      exit(System.Length(L));
  if System.Length(L) <= System.Length(R) then
    Result := LevDistanceMbrImpl(@L[0], @R[0], System.Length(L), System.Length(R), System.Length(R))
  else
    Result := LevDistanceMbrImpl(@R[0], @L[0], System.Length(R), System.Length(L), System.Length(L));
end;

function LevDistanceMBR(const L, R: ansistring; aLimit: SizeInt): SizeInt;
begin
  if aLimit < 0 then
    aLimit := 0;
  if L = '' then
    if System.Length(R) <= aLimit then
      exit(System.Length(R))
    else
      exit(NULL_INDEX)
  else
    if R = '' then
      if System.Length(L) <= aLimit then
        exit(System.Length(L))
      else
        exit(NULL_INDEX);
  if System.Length(L) <= System.Length(R) then
    Result := LevDistanceMbrImpl(Pointer(L), Pointer(R), System.Length(L), System.Length(R), aLimit)
  else
    Result := LevDistanceMbrImpl(Pointer(R), Pointer(L), System.Length(R), System.Length(L), aLimit);
end;

function LevDistanceMBR(const L, R: array of Byte; aLimit: SizeInt): SizeInt;
begin
  if aLimit < 0 then
    aLimit := 0;
  if System.Length(L) = 0 then
    if System.Length(R) <= aLimit then
      exit(System.Length(R))
    else
      exit(NULL_INDEX)
  else
    if System.Length(R) = 0 then
      if System.Length(L) <= aLimit then
        exit(System.Length(L))
      else
        exit(NULL_INDEX);
  if System.Length(L) <= System.Length(R) then
    Result := LevDistanceMbrImpl(@L[0], @R[0], System.Length(L), System.Length(R), aLimit)
  else
    Result := LevDistanceMbrImpl(@R[0], @L[0], System.Length(R), System.Length(L), aLimit);
end;

{$PUSH}{$WARN 5057 OFF}{$WARN 5036 OFF}{$Q-}{$R-}
{ in terms of the Hyyrö's
  "Explaining and Extending the Bit-parallel Approximate String Matching Algorithm of Myers" }
function LevDistMyersD(pL, pR: PByte; aLenL, aLenR: SizeInt): SizeInt;
var
  Pm: array[Byte] of DWord;
  PmI, Hp, Hn, Vp, Vn, D0: DWord;
  I: SizeInt;
begin
  System.FillChar(Pm, SizeOf(Pm), 0);
  for I := 0 to Pred(aLenL) do
    Pm[pL[I]] := Pm[pL[I]] or QWord(1) shl I;

  Result := aLenL;
  Vn := 0;
  Vp := High(DWord);

  for I := 0 to Pred(aLenR) do
    begin
      PmI := Pm[pR[I]];
      D0 := (((PmI and Vp) + Vp) xor Vp) or PmI or Vn;
      Hp := Vn or not(D0 or Vp);
      Hn := D0 and Vp;
      Vp := Hn shl 1 or not(D0 or Hp shl 1 or 1);
      Vn := D0 and (Hp shl 1 or 1);
      if Hn and (DWord(1) shl Pred(aLenL)) <> 0 then
        Dec(Result)
      else
        if Hp and (DWord(1) shl Pred(aLenL)) <> 0 then
          Inc(Result);
      //Result -= SizeInt((Hn and (DWord(1) shl Pred(aLenL))) shr Pred(aLenL));
      //Result += SizeInt((Hp and (DWord(1) shl Pred(aLenL))) shr Pred(aLenL));
    end;
end;

function LevDistMyersD(pL, pR: PByte; aLenL, aLenR, aLimit: SizeInt): SizeInt;
var
  Pm: array[Byte] of DWord;
  PmI, HPos, HNeg, VPos, VNeg, D0: DWord;
  I: SizeInt;
begin
  System.FillChar(Pm, SizeOf(Pm), 0);
  for I := 0 to Pred(aLenL) do
    Pm[pL[I]] := Pm[pL[I]] or QWord(1) shl I;

  Result := aLenL;
  aLimit += aLenR - aLenL;
  VNeg := 0;
  VPos := High(DWord);

  for I := 0 to Pred(aLenR) do
    begin
      PmI := Pm[pR[I]];
      D0 := (((PmI and VPos) + VPos) xor VPos) or PmI or VNeg;
      HPos := VNeg or not(D0 or VPos);
      HNeg := D0 and VPos;
      VPos := HNeg shl 1 or not(D0 or HPos shl 1 or 1);
      VNeg := D0 and (HPos shl 1 or 1);
      if HNeg and (QWord(1) shl Pred(aLenL)) <> 0 then
        Dec(Result)
      else
        begin
          if HPos and (QWord(1) shl Pred(aLenL)) <> 0 then
            begin
              Inc(Result);
              aLimit -= 2;
            end
          else
            Dec(aLimit);
          if aLimit < 0 then
            exit(NULL_INDEX);
        end;
    end;
end;

function LevDistMyersQ(pL, pR: PByte; aLenL, aLenR: SizeInt): SizeInt;
var
  Pm: array[Byte] of QWord;
  PmI, Hp, Hv, Vp, Vn, D0: QWord;
  I: SizeInt;
begin
  System.FillChar(Pm, SizeOf(Pm), 0);
  for I := 0 to Pred(aLenL) do
    Pm[pL[I]] := Pm[pL[I]] or QWord(1) shl I;

  Result := aLenL;
  Vn := 0;
  Vp := High(QWord);

  for I := 0 to Pred(aLenR) do
    begin
      PmI := Pm[pR[I]];
      D0 := (((PmI and Vp) + Vp) xor Vp) or PmI or Vn;
      Hp := Vn or not(D0 or Vp);
      Hv := D0 and Vp;
      Vp := Hv shl 1 or not(D0 or Hp shl 1 or 1);
      Vn := D0 and (Hp shl 1 or 1);
      if Hv and (QWord(1) shl Pred(aLenL)) <> 0 then
        Dec(Result)
      else
        if Hp and (QWord(1) shl Pred(aLenL)) <> 0 then
          Inc(Result);
    end;
end;

function LevDistMyersQ(pL, pR: PByte; aLenL, aLenR, aLimit: SizeInt): SizeInt;
var
  Pm: array[Byte] of QWord;
  PmI, HPos, HNeg, VPos, VNeg, D0: QWord;
  I: SizeInt;
begin
  System.FillChar(Pm, SizeOf(Pm), 0);
  for I := 0 to Pred(aLenL) do
    Pm[pL[I]] := Pm[pL[I]] or QWord(1) shl I;

  Result := aLenL;
  aLimit += aLenR - aLenL;
  VNeg := 0;
  VPos := High(QWord);

  for I := 0 to Pred(aLenR) do
    begin
      PmI := Pm[pR[I]];
      D0 := (((PmI and VPos) + VPos) xor VPos) or PmI or VNeg;
      HPos := VNeg or not(D0 or VPos);
      HNeg := D0 and VPos;
      VPos := HNeg shl 1 or not(D0 or HPos shl 1 or 1);
      VNeg := D0 and (HPos shl 1 or 1);
      if HNeg and (QWord(1) shl Pred(aLenL)) <> 0 then
        Dec(Result)
      else
        begin
          if HPos and (QWord(1) shl Pred(aLenL)) <> 0 then
            begin
              Inc(Result);
              aLimit -= 2;
            end
          else
            Dec(aLimit);
          if aLimit < 0 then
            exit(NULL_INDEX);
        end;
    end;
end;

const
  BLOCK_SIZE = BitSizeOf(QWord);
  BSIZE_MASK = Pred(BLOCK_SIZE);
  BSIZE_LOG  = 6;

{ in terms of Myers }
function LevDistMyersDQ(pL, pR: PByte; aLenL, aLenR: SizeInt): SizeInt;
var
  Peq: array[Byte] of array[0..1] of QWord;
  Eq0, Eq1, Ph, Mh, Pv0, Mv0, Pv1, Mv1, Xv, Xh, Hin: QWord;
  I: SizeInt;
begin
  System.FillChar(Peq, SizeOf(Peq), 0);
  for I := 0 to Pred(BLOCK_SIZE) do
    Peq[pL[I]][0] := Peq[pL[I]][0] or QWord(1) shl I;
  for I := BLOCK_SIZE to Pred(aLenL) do
    Peq[pL[I]][1] := Peq[pL[I]][1] or QWord(1) shl (I - BLOCK_SIZE);

  Result := aLenL;
  Pv0 := High(QWord);
  Pv1 := High(QWord);
  Mv0 := 0;
  Mv1 := 0;

  for I := 0 to Pred(aLenR) do
    begin
      Eq0 := Peq[pR[I]][0];
      Eq1 := Peq[pR[I]][1];
      ///////////////////////
      Xv := Mv0 or Eq0;
      Xh := ((Pv0 and Eq0 + Pv0) xor Pv0) or Eq0;
      Ph := Mv0 or not(Xh or Pv0);
      Mh := Pv0 and Xh;
      Hin := Ph shr BSIZE_MASK - Mh shr BSIZE_MASK;
      Ph := Ph shl 1 or 1;
      Pv0 := Mh shl 1 or not(Xv or Ph);
      Mv0 := Xv and Ph;
      ///////////////////////
      Xv := Mv1 or Eq1;
      Eq1 := Eq1 or Hin shr BSIZE_MASK;
      Xh := ((Pv1 and Eq1 + Pv1) xor Pv1) or Eq1;
      Ph := Mv1 or not(Xh or Pv1);
      Mh := Pv1 and Xh;
      ///////////////////////
      if Mh and (QWord(1) shl Pred(aLenL - BLOCK_SIZE)) <> 0 then
        Dec(Result)
      else
        if Ph and (QWord(1) shl Pred(aLenL - BLOCK_SIZE)) <> 0 then
          Inc(Result);
      ///////////////////////
      Ph := Ph shl 1 or (Hin + 1) shr 1;
      Pv1 := (Mh shl 1 or Hin shr BSIZE_MASK) or not(Xv or Ph);
      Mv1 := Xv and Ph;
    end;
end;

function LevDistMyersDQ(pL, pR: PByte; aLenL, aLenR, aLimit: SizeInt): SizeInt;
var
  Peq: array[Byte] of array[0..1] of QWord;
  Eq0, Eq1, Ph, Mh, Pv0, Mv0, Pv1, Mv1, Xv, Xh, Hin: QWord;
  I: SizeInt;
begin
  System.FillChar(Peq, SizeOf(Peq), 0);
  for I := 0 to Pred(BLOCK_SIZE) do
    Peq[pL[I]][0] := Peq[pL[I]][0] or QWord(1) shl I;
  for I := BLOCK_SIZE to Pred(aLenL) do
    Peq[pL[I]][1] := Peq[pL[I]][1] or QWord(1) shl (I - BLOCK_SIZE);

  Result := aLenL;
  aLimit += aLenR - aLenL;
  Pv0 := High(QWord);
  Pv1 := High(QWord);
  Mv0 := 0;
  Mv1 := 0;

  for I := 0 to Pred(aLenR) do
    begin
      Eq0 := Peq[pR[I]][0];
      Eq1 := Peq[pR[I]][1];
      ///////////////////////
      Xv := Mv0 or Eq0;
      Xh := ((Pv0 and Eq0 + Pv0) xor Pv0) or Eq0;
      Ph := Mv0 or not(Xh or Pv0);
      Mh := Pv0 and Xh;
      Hin := Ph shr BSIZE_MASK - Mh shr BSIZE_MASK;
      Ph := Ph shl 1 or 1;
      Pv0 := Mh shl 1 or not(Xv or Ph);
      Mv0 := Xv and Ph;
      ///////////////////////
      Xv := Mv1 or Eq1;
      Eq1 := Eq1 or Hin shr BSIZE_MASK;
      Xh := ((Pv1 and Eq1 + Pv1) xor Pv1) or Eq1;
      Ph := Mv1 or not(Xh or Pv1);
      Mh := Pv1 and Xh;
      ///////////////////////
      if Mh and (QWord(1) shl Pred(aLenL - BLOCK_SIZE)) <> 0 then
        Dec(Result)
      else
        begin
          if Ph and (QWord(1) shl Pred(aLenL - BLOCK_SIZE)) <> 0 then
            begin
              Inc(Result);
              aLimit -= 2;
            end
          else
            Dec(aLimit);
          if aLimit < 0 then
            exit(NULL_INDEX);
        end;
      ///////////////////////
      Ph := Ph shl 1 or (Hin + 1) shr 1;
      Pv1 := (Mh shl 1 or Hin shr BSIZE_MASK) or not(Xv or Ph);
      Mv1 := Xv and Ph;
    end;
end;

{ recodes sequences to determine alphabet size and minimize memory usage;
  returns the size of the alphabet }
function RecodeSeq(pL, pR: PByte; aLenL, aLenR: SizeInt; out aBuffer: TBytes): SizeInt;
var
  InTable: array[Byte] of Boolean;
  CodeTable: array[Byte] of Byte;
  I: SizeInt;
  b: Byte;
begin
  System.FillChar(InTable, SizeOf(InTable), 0);
  System.SetLength(aBuffer, aLenL + aLenR);
  Result := 0;
  for I := 0 to Pred(aLenL) do
    begin
      b := pL[I];
      if not InTable[b] then
        begin
          CodeTable[b] := Result;
          Inc(Result);
          InTable[b] := True;
        end;
      aBuffer[I] := CodeTable[b];
    end;
  for I := aLenL to Pred(aLenL + aLenR) do
    begin
      b := pR[I-aLenL];
      if not InTable[b] then
        begin
          CodeTable[b] := Result;
          Inc(Result);
          InTable[b] := True;
        end;
      aBuffer[I] := CodeTable[b];
    end;
end;

type
  TPeq = record
    Peq: array of PQWord;
    Buffer: array of QWord;
    BlockCount: SizeInt;
  end;

procedure CreatePeq(aSeq: PByte; aSeqLen, AlphabetSize: SizeInt; out aPeq: TPeq);
var
  I, J, BCount, LastRow, Pad: SizeInt;
begin
  LastRow := aSeqLen and BSIZE_MASK;
  BCount := aSeqLen shr BSIZE_LOG + Ord(LastRow <> 0);
  aPeq.BlockCount := BCount;
  System.SetLength(aPeq.Peq, AlphabetSize);

  System.SetLength(aPeq.Buffer, BCount * AlphabetSize);
  if LastRow <> 0 then
    Pad := System.High(QWord) shl LastRow
  else
    Pad := 0;
  J := 0;
  with aPeq do
    for I := 0 to Pred(AlphabetSize) do
      begin
        Peq[I] := @Buffer[J];
        Peq[I][Pred(BCount)] := Pad; ////////////???
        J += BCount;
      end;
  with aPeq do
    for I := 0 to Pred(aSeqLen) do
      Peq[aSeq[I]][I shr BSIZE_LOG] := Peq[aSeq[I]][I shr BSIZE_LOG] or QWord(1) shl (I and BSIZE_MASK);
end;

type
  TBlock = record
    P,
    M: QWord;
    Score: SizeInt;
  end;

{ with some imrovements from:
  Martin Sosic and Mile Sikic, "Edlib: a C/C 11 library for fast, exact sequence
  alignment using edit distance" }
function LevDistMyersCutoff(const aPeq: TPeq; pR: PByte; aLenL, aLenR, K: SizeInt): SizeInt;
  function ReadBlockCell(const aBlock: TBlock; aIndex: SizeInt): SizeInt;
  var
    I: SizeInt;
  begin
    Result := aBlock.Score;
    for I := BSIZE_MASK downto Succ(aIndex) do
      if aBlock.P and (QWord(1) shl I) <> 0 then
        Dec(Result)
      else
        if aBlock.M and (QWord(1) shl I) <> 0 then
          Inc(Result);
  end;
var
  Blocks: array of TBlock;
  Eq, Xv, Xh, Pv, Mv, Ph, Mh, HIn, HOut: QWord;
  I, J, First, Last: SizeInt;
begin
  //here aLenL <= aLenR and K >= aLenR - aLenL
  K := Math.Min(k, aLenR);
  First := 0;
  I := Succ(Math.Min(K, (K - aLenR + aLenL) div 2));
  Last := Pred(Math.Min(aPeq.BlockCount, I shr BSIZE_LOG + Ord(I and BSIZE_MASK <> 0)));
  System.SetLength(Blocks, aPeq.BlockCount);
  Result := NULL_INDEX;

  for I := First to Last do
    with Blocks[I] do
      begin
        P := System.High(QWord);
        Score := BLOCK_SIZE * Succ(I);
      end;

  for I := 0 to Pred(aLenR) do
    begin
      HOut := 1;
      for J := First to Last do
        begin
          HIn := HOut;
          Eq := aPeq.Peq[pR[I]][J];
          Pv := Blocks[J].P;
          Mv := Blocks[J].M;
          Xv := Mv or Eq;
          Eq := Eq or HIn shr BSIZE_MASK;
          Xh := ((Pv and Eq + Pv) xor Pv) or Eq;
          Ph := Mv or not(Xh or Pv);
          Mh := Pv and Xh;

          HOut := Ph shr BSIZE_MASK - Mh shr BSIZE_MASK;

          Ph := Ph shl 1 or (HIn + 1) shr 1;

          Blocks[J].P := (Mh shl 1 or HIn shr BSIZE_MASK) or not(Xv or Ph);
          Blocks[J].M := Xv and Ph;
          Blocks[J].Score += SizeInt(HOut);
        end;
      // adjust last block
      if (Last < Pred(aPeq.BlockCount)) and
         (K-Blocks[Last].Score+BSIZE_MASK-aLenR+aLenL+I >= Last*BLOCK_SIZE) then
        begin
          Inc(Last);
          HIn := HOut;
          Eq := aPeq.Peq[pR[I]][Last];
          Pv := System.High(QWord);
          Mv := 0;
          Xv := Mv or Eq;
          Eq := Eq or HIn shr BSIZE_MASK;
          Xh := ((Pv and Eq + Pv) xor Pv) or Eq;
          Ph := Mv or not(Xh or Pv);
          Mh := Pv and Xh;

          HOut := Ph shr BSIZE_MASK - Mh shr BSIZE_MASK;

          Ph := Ph shl 1 or (HIn + 1) shr 1;

          Blocks[Last].P := (Mh shl 1 or HIn shr BSIZE_MASK) or not(Xv or Ph);
          Blocks[Last].M := Xv and Ph;
          Blocks[Last].Score := Blocks[Last-1].Score - SizeInt(HIn) + BLOCK_SIZE + SizeInt(HOut);
        end
      else
        while (Last >= First) and ((Blocks[Last].Score >= K + BLOCK_SIZE) or
              (K-Blocks[Last].Score+BSIZE_MASK-aLenR+aLenL+I+1 < Last*BLOCK_SIZE)) do
          Dec(Last);
      // adjust first block
      while (First <= Last) and ((Blocks[First].Score >= K + BLOCK_SIZE) or
            (Blocks[First].Score-K-aLenR+aLenL+I > (First+1)*BLOCK_SIZE-1)) do
        Inc(First);

      if Last < First then exit;
    end;

  if Last = Pred(aPeq.BlockCount) then
    begin
      I := Pred(aLenL and BSIZE_MASK);
      if I < 0 then I += BLOCK_SIZE;
      J := ReadBlockCell(Blocks[Last], I);
      if J <= K then
        Result := J;
    end;
end;
{$POP}

function LevDistMyers(pL, pR: PByte; aLenL, aLenR: SizeInt): SizeInt;
var
  Peq: TPeq;
  Buffer: TBytes;
  AlphabetSize, Limit: SizeInt;
begin
  //here aLenL <= aLenR
  AlphabetSize := RecodeSeq(pL, pR, aLenL, aLenR, Buffer);
  CreatePeq(Pointer(Buffer), aLenL, AlphabetSize, Peq);
  Limit := Math.Max(BLOCK_SIZE, aLenR - aLenL);
  repeat
    Result := LevDistMyersCutoff(Peq, @Buffer[aLenL], aLenL, aLenR, Limit);
    Limit += Limit;
  until Result <> NULL_INDEX;
end;

function LevDistMyers(pL, pR: PByte; aLenL, aLenR, aLimit: SizeInt): SizeInt;
var
  Peq: TPeq;
  Buffer: TBytes;
  AlphabetSize: SizeInt;
begin
  AlphabetSize := RecodeSeq(pL, pR, aLenL, aLenR, Buffer);
  CreatePeq(Pointer(Buffer), aLenL, AlphabetSize, Peq);
  Result := LevDistMyersCutoff(Peq, @Buffer[aLenL], aLenL, aLenR, aLimit);
end;

function GetLevDistMyers(pL, pR: PByte; aLenL, aLenR: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  //here aLenL <= aLenR
  if pL = pR then
    exit(aLenR - aLenL);

  while (aLenL > 0) and (pL[Pred(aLenL)] = pR[Pred(aLenR)]) do
    begin
      Dec(aLenL);
      Dec(aLenR);
    end;

  I := 0;
  while (I < aLenL) and (pL^ = pR^) do
    begin
      Inc(pL);
      Inc(pR);
      Inc(I);
    end;

  if I = aLenL then
    exit(aLenR - I);

  aLenL -= I;
  aLenR -= I;

  case aLenL of
    1..BitSizeOf(DWord):
      Result := LevDistMyersD(pL, pR, aLenL, aLenR);
    BitSizeOf(DWord)+1..BitSizeOf(QWord):
      Result := LevDistMyersQ(pL, pR, aLenL, aLenR);
    BitSizeOf(QWord)+1..BitSizeOf(QWord)*2:
      Result := LevDistMyersDQ(pL, pR, aLenL, aLenR);
  else
    Result := LevDistMyers(pL, pR, aLenL, aLenR);
  end;
end;

function LevDistanceMyers(const L, R: ansistring): SizeInt;
begin
  if L = '' then
    exit(System.Length(R))
  else
    if R = '' then
      exit(System.Length(L));
  if System.Length(L) <= System.Length(R) then
    Result := GetLevDistMyers(Pointer(L), Pointer(R), System.Length(L), System.Length(R))
  else
    Result := GetLevDistMyers(Pointer(R), Pointer(L), System.Length(R), System.Length(L));
end;

function LevDistanceMyers(const L, R: array of Byte): SizeInt;
begin
  if System.Length(L) = 0 then
    exit(System.Length(R))
  else
    if System.Length(R) = 0 then
      exit(System.Length(L));
  if System.Length(L) <= System.Length(R) then
    Result := GetLevDistMyers(@L[0], @R[0], System.Length(L), System.Length(R))
  else
    Result := GetLevDistMyers(@R[0], @L[0], System.Length(R), System.Length(L));
end;

function GetLevDistMyers(pL, pR: PByte; aLenL, aLenR, aLimit: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  //here aLenL <= aLenR
  if aLenR - aLenL > aLimit then
    exit(NULL_INDEX);

  if pL = pR then
    exit(aLenR - aLenL);

  while (aLenL > 0) and (pL[Pred(aLenL)] = pR[Pred(aLenR)]) do
    begin
      Dec(aLenL);
      Dec(aLenR);
    end;

  I := 0;
  while (I < aLenL) and (pL^ = pR^) do
    begin
      Inc(pL);
      Inc(pR);
      Inc(I);
    end;

  if I = aLenL then
    exit(aLenR - I);

  if aLimit = 0 then  //////////
    exit(NULL_INDEX); //////////

  aLenL -= I;
  aLenR -= I;

  if aLimit > aLenR then
    aLimit := aLenR;

  case aLenL of
    1..BitSizeOf(DWord):
      Result := LevDistMyersD(pL, pR, aLenL, aLenR, aLimit);
    BitSizeOf(DWord)+1..BitSizeOf(QWord):
      Result := LevDistMyersQ(pL, pR, aLenL, aLenR, aLimit);
    BitSizeOf(QWord)+1..BitSizeOf(QWord)*2:
      Result := LevDistMyersDQ(pL, pR, aLenL, aLenR, aLimit);
  else
    Result := LevDistMyers(pL, pR, aLenL, aLenR, aLimit);
  end;
end;

function LevDistanceMyers(const L, R: ansistring; aLimit: SizeInt): SizeInt;
begin
  if aLimit < 0 then
    aLimit := 0;
  if L = '' then
    if System.Length(R) <= aLimit then
      exit(System.Length(R))
    else
      exit(NULL_INDEX)
  else
    if R = '' then
      if System.Length(L) <= aLimit then
        exit(System.Length(L))
      else
        exit(NULL_INDEX);
  if System.Length(L) <= System.Length(R) then
    Result := GetLevDistMyers(Pointer(L), Pointer(R), System.Length(L), System.Length(R), aLimit)
  else
    Result := GetLevDistMyers(Pointer(R), Pointer(L), System.Length(R), System.Length(L), aLimit);
end;

function LevDistanceMyers(const L, R: array of Byte; aLimit: SizeInt): SizeInt;
begin
  if aLimit < 0 then
    aLimit := 0;
  if System.Length(L) = 0 then
    if System.Length(R) <= aLimit then
      exit(System.Length(R))
    else
      exit(NULL_INDEX)
  else
    if System.Length(R) = 0 then
      if System.Length(L) <= aLimit then
        exit(System.Length(L))
      else
        exit(NULL_INDEX);
  if System.Length(L) <= System.Length(R) then
    Result := GetLevDistMyers(@L[0], @R[0], System.Length(L), System.Length(R), aLimit)
  else
    Result := GetLevDistMyers(@R[0], @L[0], System.Length(R), System.Length(L), aLimit);
end;

{$PUSH}{$WARN 5036 OFF}
function IsValidDotQuadIPv4(const s: ansistring): Boolean;
type
  TRadix = (raDec, raOct, raHex);
var
  I, OctetIdx, CharIdx: Integer;
  Buf: array[0..3] of AnsiChar;
  Radix: TRadix;
  function OctetInRange: Boolean; inline;
  begin
    if CharIdx = 0 then exit(False);
    case Radix of
      raDec:
        begin
          if CharIdx = 4 then exit(False);
          if CharIdx = 3 then
            begin
              if Buf[0] > '2' then exit(False);
              if Buf[0] = '2' then
                begin
                  if Buf[1] > '5' then exit(False);
                  if (Buf[1] = '5') and (Buf[2] > '5') then exit(False);
                end;
            end;
        end;
      raOct:
        if (CharIdx = 4) and (Buf[1] > '3') then exit(False); //377
      raHex:
        if CharIdx < 3 then exit(False);
    end;
    CharIdx := 0;
    OctetInRange := True;
  end;
var
  p: PAnsiChar absolute s;
begin
  if DWord(System.Length(s) - 7) > DWord(12) then exit(False);
  OctetIdx := 0;
  CharIdx := 0;
  for I := 0 to Pred(System.Length(s)) do
    if p[I] <> '.' then
      begin
        if CharIdx = 4 then exit(False);
        case p[I] of
          '0'..'9':
            begin
              case CharIdx of
                0: Radix := raDec;
                1:
                  if Buf[0] = '0' then
                    begin
                      if p[I] > '7' then exit(False);
                      Radix := raOct;
                    end;
              else
                if (Radix = raOct) and (p[I] > '7') then exit(False);
              end;
            end;
          'X', 'x':
            begin
              if (CharIdx <> 1) or (Buf[0] <> '0') then exit(False);
              Radix := raHex;
            end;
          'A'..'F', 'a'..'f':
            if Radix <> raHex then exit(False);
        else
          exit(False);
        end;
        Buf[CharIdx] := p[I];
        Inc(CharIdx);
      end
    else
      begin
        if (OctetIdx = 3) or not OctetInRange then exit(False);
        Inc(OctetIdx);
      end;
  Result := (OctetIdx = 3) and OctetInRange;
end;

function IsValidDotDecIPv4(const s: ansistring): Boolean;
var
  I, OctetIdx, CharIdx: Integer;
  Buf: array[0..3] of AnsiChar;
  function OctetInRange: Boolean; inline;
  begin
    if CharIdx = 0 then exit(False);
    if CharIdx = 3 then
      begin
        if Buf[0] > '2' then exit(False);
        if Buf[0] = '2' then
          begin
            if Buf[1] > '5' then exit(False);
            if (Buf[1] = '5') and (Buf[2] > '5') then exit(False);
          end;
      end;
    CharIdx := 0;
    OctetInRange := True;
  end;
var
  p: PAnsiChar absolute s;
begin
  if DWord(System.Length(s) - 7) > DWord(8) then exit(False);
  OctetIdx := 0;
  CharIdx := 0;
  for I := 0 to Pred(System.Length(s)) do
    case p[I] of
      '0'..'9':
        begin
          if CharIdx = 3 then exit(False);
          if (CharIdx = 1) and (Buf[0] = '0') then exit(False);
          Buf[CharIdx] := p[I];
          Inc(CharIdx);
        end;
      '.':
        begin
          if (OctetIdx = 3) or not OctetInRange then exit(False);
          Inc(OctetIdx);
        end
    else
      exit(False);
    end;
  Result := (OctetIdx = 3) and OctetInRange;
end;
{$POP}

{ TAnsiStrHelper.TStrEnumerable }

function TAnsiStrHelper.TStrEnumerable.GetCurrent: string;
begin
  Result := System.Copy(FString, FStartIndex, FCurrLen);
end;

constructor TAnsiStrHelper.TStrEnumerable.Create(const aValue: string; const aDelimiters: TSysCharSet);
begin
  inherited Create;
  FString := aValue;
  FDelimiters := aDelimiters;
  FStartIndex := 1;
  FCurrLen := 0;
  FCurrIndex := 0;
end;

function TAnsiStrHelper.TStrEnumerable.MoveNext: Boolean;
var
  I, Len: SizeInt;
begin
  Len := 0;
  for I := Succ(FCurrIndex) to System.Length(FString) do
    if not (FString[I] in FDelimiters) then
      begin
        if Len = 0 then
          FStartIndex := I;
        Inc(Len);
      end
    else
      if Len <> 0 then
        begin
          FCurrIndex := I;
          FCurrLen := Len;
          exit(True);
        end;
  FCurrIndex := System.Length(FString);
  if Len <> 0 then
    begin
      FCurrLen := Len;
      exit(True);
    end;
  Result := False;
end;

procedure TAnsiStrHelper.TStrEnumerable.Reset;
begin
  FStartIndex := 1;
  FCurrLen := 0;
  FCurrIndex := 0;
end;

{ TAnsiStrHelper }

function TAnsiStrHelper.StripWhiteSpaces: ansistring;
begin
  Result := StripChars(WhiteSpaces);
end;

function TAnsiStrHelper.StripChar(aChar: AnsiChar): ansistring;
var
  I, J: SizeInt;
  pRes, pSelf: PAnsiChar;
  c: AnsiChar;
begin
  if Self = '' then
    exit('');
  SetLength(Result, System.Length(Self));
  pSelf := PAnsiChar(Self);
  pRes := PAnsiChar(Result);
  J := 0;
  for I := 0 to Pred(System.Length(Self)) do
    begin
      c := pSelf[I];
      if c <> aChar then
        begin
          pRes[J] := c;
          Inc(J);
        end;
    end;
  SetLength(Result, J);
end;

function TAnsiStrHelper.StripChars(constref aChars: TSysCharSet): ansistring;
var
  I, J: SizeInt;
  pRes, pSelf: PAnsiChar;
  c: AnsiChar;
begin
  if Self = '' then
    exit('');
  SetLength(Result, System.Length(Self));
  pSelf := PAnsiChar(Self);
  pRes := PAnsiChar(Result);
  J := 0;
  for I := 0 to Pred(System.Length(Self)) do
    begin
      c := pSelf[I];
      if not (c in aChars) then
        begin
          pRes[J] := c;
          Inc(J);
        end;
    end;
  SetLength(Result, J);
end;

function TAnsiStrHelper.Words(constref aDelimiters: TSysCharSet): IStrEnumerable;
begin
  Result := TStrEnumerable.Create(Self, aDelimiters);
end;

{ TRegexMatch.TStrEnumerable }

function TRegexMatch.TStrEnumerable.GetCurrent: string;
begin
  Result := FRegex.Match[0];
end;

constructor TRegexMatch.TStrEnumerable.Create(aRegex: TRegExpr; const s: string);
begin
  inherited Create;
  FRegex := aRegex;
  FInputString := s;
end;

function TRegexMatch.TStrEnumerable.MoveNext: Boolean;
begin
  if FInCycle then
    Result := FRegex.ExecNext
  else
    begin
      FInCycle := True;
      Result := FRegex.Exec(FInputString);
    end;
end;

procedure TRegexMatch.TStrEnumerable.Reset;
begin
  FInCycle := False;
end;

{ TRegexMatch }

function TRegexMatch.GetExpression: string;
begin
  Result := FRegex.Expression;
end;

function TRegexMatch.GetModifierStr: string;
begin
  Result := FRegex.ModifierStr;
end;

procedure TRegexMatch.SetExpression(aValue: string);
begin
  FRegex.Expression := aValue;
end;

procedure TRegexMatch.SetModifierStr(aValue: string);
begin
  FRegex.ModifierStr := aValue;
end;

constructor TRegexMatch.Create;
begin
  FRegex := TRegExpr.Create;
end;

constructor TRegexMatch.Create(const aRegExpression: string);
begin
  FRegex := TRegExpr.Create(aRegExpression);
end;

constructor TRegexMatch.Create(const aRegExpression, aModifierStr: string);
begin
  FRegex := TRegExpr.Create(aRegExpression);
  FRegex.ModifierStr := aModifierStr;
end;

destructor TRegexMatch.Destroy;
begin
  FRegex.Free;
  inherited;
end;

function TRegexMatch.Matches(const aValue: string): IStrEnumerable;
begin
  Result := TStrEnumerable.Create(FRegex, aValue);
end;

{ TStringListHelper }

function TStringListHelper.AsEnumerable: IStrEnumerable;
begin
  Result := specialize TGClassEnumerable<string, TStringList, TStringsEnumerator>.Create(Self);
end;

{ TBmSearch.TStrEnumerator }

function TBmSearch.TStrEnumerator.GetCurrent: SizeInt;
begin
  Result := Succ(FCurrIndex);
end;

function TBmSearch.TStrEnumerator.MoveNext: Boolean;
var
  I: SizeInt;
begin
  if FCurrIndex < Pred(System.Length(FHeap)) then
    begin
      I := FMatcher^.FindNext(PByte(FHeap), System.Length(FHeap), FCurrIndex);
      if I <> NULL_INDEX then
        begin
          FCurrIndex := I;
          exit(True);
        end;
    end;
  Result := False;
end;

{ TBmSearch.TByteEnumerator }

function TBmSearch.TByteEnumerator.GetCurrent: SizeInt;
begin
  Result := FCurrIndex;
end;

function TBmSearch.TByteEnumerator.MoveNext: Boolean;
var
  I: SizeInt;
begin
  if FCurrIndex < Pred(FHeapLen) then
    begin
      I := FMatcher^.FindNext(FHeap, FHeapLen, FCurrIndex);
      if I <> NULL_INDEX then
        begin
          FCurrIndex := I;
          exit(True);
        end;
    end;
  Result := False;
end;

{ TBmSearch.TStrMatches }

function TBmSearch.TStrMatches.GetEnumerator: TStrEnumerator;
begin
  Result.FCurrIndex := NULL_INDEX;
  Result.FHeap := FHeap;
  Result.FMatcher := FMatcher;
end;

{ TBmSearch.TByteMatches }

function TBmSearch.TByteMatches.GetEnumerator: TByteEnumerator;
begin
  Result.FCurrIndex := NULL_INDEX;
  Result.FHeapLen := FHeapLen;
  Result.FHeap := FHeap;
  Result.FMatcher := FMatcher;
end;

{ TBmSearch }

procedure TBmSearch.FillBc;
var
  I, Len: Integer;
  p: PByte absolute FNeedle;
begin
  Len := System.Length(FNeedle);
  specialize TGArrayHelpUtil<Integer>.Fill(FBcShift, Len);
  for I := 0 to Len - 2 do
    FBcShift[p[I]] := Pred(Len - I);
end;

procedure TBmSearch.FillGs;
var
  I, J, LastPrefix, Len: Integer;
  IsPrefix: Boolean;
  p: PByte absolute FNeedle;
begin
  Len := System.Length(FNeedle);
  SetLength(FGsShift, Len);
  LastPrefix := Pred(Len);
  for I := Pred(Len) downto 0 do
    begin
      IsPrefix := True;
      for J := 0 to Len - I - 2 do
        if (p[J] <> p[J + Succ(I)]) then
          begin
            IsPrefix := False;
            break;
          end;
      if IsPrefix then
        LastPrefix := Succ(I);
      FGsShift[I] := LastPrefix + Len - Succ(I);
    end;
  for I := 0 to Len - 2 do
    begin
      J := 0;
      while (p[I - J] = p[Pred(Len - J)]) and (J < I) do
        Inc(J);
      if p[I - J] <> p[Pred(Len - J)] then
        FGsShift[Pred(Len - J)] := Pred(Len + J - I);
    end;
end;

function TBmSearch.DoFind(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
var
  J, NeedLast: SizeInt;
  p: PByte absolute FNeedle;
begin
  NeedLast := Pred(System.Length(FNeedle));
  while I < aHeapLen do
    begin
      while (I < aHeapLen) and (aHeap[I] <> p[NeedLast]) do
        I += FBcShift[aHeap[I]];
      if I >= aHeapLen then break;
      J := Pred(NeedLast);
      Dec(I);
      while (J <> NULL_INDEX) and (aHeap[I] = p[J]) do
        begin
          Dec(I);
          Dec(J);
        end;
      if J = NULL_INDEX then
        exit(Succ(I))
      else
        I += FGsShift[J];
    end;
  Result := NULL_INDEX;
end;

function TBmSearch.FindNext(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
begin
  if I = NULL_INDEX then
    Result := DoFind(aHeap, aHeapLen, I + System.Length(FNeedle))
  else
    Result := DoFind(aHeap, aHeapLen, I + FGsShift[0]);
end;

function TBmSearch.Find(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
begin
  Result := DoFind(aHeap, aHeapLen, I + Pred(System.Length(FNeedle)));
end;

constructor TBmSearch.Create(const aPattern: string);
begin
  FGsShift := nil;
  if aPattern <> '' then
    begin
      FNeedle := System.Copy(aPattern, 1, System.Length(aPattern));
      FillBc;
      FillGs;
    end
  else
    FNeedle := '';
end;

constructor TBmSearch.Create(const aPattern: array of Byte);
begin
  FGsShift := nil;
  System.SetLength(FNeedle, System.Length(aPattern));
  if System.Length(aPattern) <> 0 then
    begin
      System.Move(aPattern[0], Pointer(FNeedle)^, System.Length(aPattern));
      FillBc;
      FillGs;
    end;
end;

function TBmSearch.Matches(const s: string): TStrMatches;
begin
  if FNeedle <> '' then
    Result.FHeap := s
  else
   Result.FHeap := '';
  Result.FMatcher := @Self;
end;

function TBmSearch.Matches(const a: array of Byte): TByteMatches;
begin
  if FNeedle <> '' then
    Result.FHeapLen := System.Length(a)
  else
    Result.FHeapLen := 0;
  if System.Length(a) <> 0 then
    Result.FHeap := @a[0]
  else
    Result.FHeap := nil;
  Result.FMatcher := @Self;
end;

function TBmSearch.NextMatch(const s: string; aOffset: SizeInt): SizeInt;
begin
  if (FNeedle = '') or (s = '') then exit(0);
  if aOffset < 1 then
    aOffset := 1;
  Result := Succ(Find(PByte(s), System.Length(s), Pred(aOffset)));
end;

function TBmSearch.NextMatch(const a: array of Byte; aOffset: SizeInt): SizeInt;
begin
  if (FNeedle = '') or (System.Length(a) = 0) then exit(NULL_INDEX);
  if aOffset < 0 then
    aOffset := 0;
  Result := Find(@a[0], System.Length(a), aOffset);
end;

function TBmSearch.FindMatches(const s: string): TIntArray;
var
  I, J: SizeInt;
begin
  Result := nil;
  if (FNeedle = '') or (s = '') then exit;
  I := NULL_INDEX;
  J := 0;
  System.SetLength(Result, ARRAY_INITIAL_SIZE);
  repeat
    I := FindNext(PByte(s), System.Length(s), I);
    if I <> NULL_INDEX then
      begin
        if System.Length(Result) = J then
          System.SetLength(Result, J * 2);
        Result[J] := Succ(I);
        Inc(J);
      end;
  until I = NULL_INDEX;
  System.SetLength(Result, J);
end;

function TBmSearch.FindMatches(const a: array of Byte): TIntArray;
var
  I, J: SizeInt;
begin
  Result := nil;
  if (FNeedle = '') or (System.Length(a) = 0) then exit;
  I := NULL_INDEX;
  J := 0;
  System.SetLength(Result, ARRAY_INITIAL_SIZE);
  repeat
    I := FindNext(@a[0], System.Length(a), I);
    if I <> NULL_INDEX then
      begin
        if System.Length(Result) = J then
          System.SetLength(Result, J * 2);
        Result[J] := I;
        Inc(J);
      end;
  until I = NULL_INDEX;
  System.SetLength(Result, J);
end;

{ TBmhrSearch.TStrEnumerator }

function TBmhrSearch.TStrEnumerator.GetCurrent: SizeInt;
begin
   Result := Succ(FCurrIndex);
end;

function TBmhrSearch.TStrEnumerator.MoveNext: Boolean;
var
  I: SizeInt;
begin
  if FCurrIndex < Pred(System.Length(FHeap)) then
    begin
      I := FMatcher^.FindNext(PByte(FHeap), System.Length(FHeap), FCurrIndex);
      if I <> NULL_INDEX then
        begin
          FCurrIndex := I;
          exit(True);
        end;
    end;
  Result := False;
end;

{ TBmhrSearch.TByteEnumerator }

function TBmhrSearch.TByteEnumerator.GetCurrent: SizeInt;
begin
  Result := FCurrIndex;
end;

function TBmhrSearch.TByteEnumerator.MoveNext: Boolean;
var
  I: SizeInt;
begin
  if FCurrIndex < Pred(FHeapLen) then
    begin
      I := FMatcher^.FindNext(FHeap, FHeapLen, FCurrIndex);
      if I <> NULL_INDEX then
        begin
          FCurrIndex := I;
          exit(True);
        end;
    end;
  Result := False;
end;

{ TBmhrSearch.TStrMatches }

function TBmhrSearch.TStrMatches.GetEnumerator: TStrEnumerator;
begin
  Result.FCurrIndex := NULL_INDEX;
  Result.FHeap := FHeap;
  Result.FMatcher := FMatcher;
end;

{ TBmhrSearch.TByteMatches }

function TBmhrSearch.TByteMatches.GetEnumerator: TByteEnumerator;
begin
  Result.FCurrIndex := NULL_INDEX;
  Result.FHeapLen := FHeapLen;
  Result.FHeap := FHeap;
  Result.FMatcher := FMatcher;
end;

{ TBmhrSearch }

procedure TBmhrSearch.FillBc;
var
  I, Len: Integer;
  p: PByte absolute FNeedle;
begin
  Len := System.Length(FNeedle);
  specialize TGArrayHelpUtil<Integer>.Fill(FBcShift, Len);
  for I := 0 to Len - 2 do
    FBcShift[p[I]] := Pred(Len - I);
end;

function TBmhrSearch.Find(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
var
  NeedLast, J: Integer;
  p: PByte absolute FNeedle;
begin
  case System.Length(FNeedle) of
    1:
      begin
        J := IndexByte(aHeap[I], aHeapLen - I, p^);
        if J < 0 then exit(J);
        exit(I + J);
      end;
    2:
      while I <= aHeapLen - 2 do
        begin
          if(aHeap[I + 1] = p[1]) and (aHeap[I] = p^) then
            exit(I);
          I += FBcShift[aHeap[I + 1]];
        end;
    3:
      while I <= aHeapLen - 3 do
        begin
          if(aHeap[I + 2] = p[2]) and (aHeap[I] = p^) and
            (aHeap[I + 1] = p[1]) then
            exit(I);
          I += FBcShift[aHeap[I + 2]];
        end;
  else
    begin
      NeedLast := Pred(System.Length(FNeedle));
      while I <= aHeapLen - Succ(NeedLast) do
        begin
          if(aHeap[I + NeedLast] = p[NeedLast]) and (aHeap[I] = p^) and
            (aHeap[I + NeedLast shr 1] = p[NeedLast shr 1]) and
            (CompareByte(aHeap[Succ(I)], p[1], Pred(NeedLast)) = 0) then
            exit(I);
          I += FBcShift[aHeap[I + NeedLast]];
        end;
    end;
  end;
  Result := NULL_INDEX;
end;

function TBmhrSearch.FindNext(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
begin
  if I > aHeapLen - System.Length(FNeedle) then exit(NULL_INDEX);
  if I = NULL_INDEX then
    I := 0
  else
    I += FBcShift[aHeap[I + Pred(System.Length(FNeedle))]];
  Result := Find(aHeap, aHeapLen, I);
end;

constructor TBmhrSearch.Create(const aPattern: string);
begin
  if aPattern <> '' then
    begin
      FNeedle := System.Copy(aPattern, 1, System.Length(aPattern));
      FillBc;
    end
  else
    FNeedle := '';
end;

constructor TBmhrSearch.Create(const aPattern: array of Byte);
begin
  System.SetLength(FNeedle, System.Length(aPattern));
  if System.Length(aPattern) <> 0 then
    begin
      System.Move(aPattern[0], Pointer(FNeedle)^, System.Length(aPattern));
      FillBc;
    end;
end;

function TBmhrSearch.Matches(const s: string): TStrMatches;
begin
  if FNeedle <> '' then
    Result.FHeap := s
  else
   Result.FHeap := '';
  Result.FMatcher := @Self;
end;

function TBmhrSearch.Matches(const a: array of Byte): TByteMatches;
begin
  if FNeedle <> '' then
    Result.FHeapLen := System.Length(a)
  else
    Result.FHeapLen := 0;
  if System.Length(a) <> 0 then
    Result.FHeap := @a[0]
  else
    Result.FHeap := nil;
  Result.FMatcher := @Self;
end;

function TBmhrSearch.NextMatch(const s: string; aOffset: SizeInt): SizeInt;
begin
  if (FNeedle = '') or (s = '') then exit(0);
  if aOffset < 1 then
    aOffset := 1;
  Result := Succ(Find(PByte(s), System.Length(s), Pred(aOffset)));
end;

function TBmhrSearch.NextMatch(const a: array of Byte; aOffset: SizeInt): SizeInt;
begin
  if (FNeedle = '') or (System.Length(a) = 0) then exit(NULL_INDEX);
  if aOffset < 0 then
    aOffset := 0;
  Result := Find(@a[0], System.Length(a), aOffset);
end;

function TBmhrSearch.FindMatches(const s: string): TIntArray;
var
  I, J: SizeInt;
begin
  Result := nil;
  if (FNeedle = '') or (s = '') then exit;
  I := NULL_INDEX;
  J := 0;
  System.SetLength(Result, ARRAY_INITIAL_SIZE);
  repeat
    I := FindNext(PByte(s), System.Length(s), I);
    if I <> NULL_INDEX then
      begin
        if System.Length(Result) = J then
          System.SetLength(Result, J * 2);
        Result[J] := Succ(I);
        Inc(J);
      end;
  until I = NULL_INDEX;
  System.SetLength(Result, J);
end;

function TBmhrSearch.FindMatches(const a: array of Byte): TIntArray;
var
  I, J: SizeInt;
begin
  Result := nil;
  if (FNeedle = '') or (System.Length(a) = 0) then exit;
  I := NULL_INDEX;
  J := 0;
  System.SetLength(Result, ARRAY_INITIAL_SIZE);
  repeat
    I := FindNext(@a[0], System.Length(a), I);
    if I <> NULL_INDEX then
      begin
        if System.Length(Result) = J then
          System.SetLength(Result, J * 2);
        Result[J] := I;
        Inc(J);
      end;
  until I = NULL_INDEX;
  System.SetLength(Result, J);
end;

{ TBmSearchCI.TEnumerator }

function TBmSearchCI.TEnumerator.GetCurrent: SizeInt;
begin
  Result := Succ(FCurrIndex);
end;

function TBmSearchCI.TEnumerator.MoveNext: Boolean;
var
  I: SizeInt;
begin
  if FCurrIndex < Pred(System.Length(FHeap)) then
    begin
      I := FMatcher^.FindNext(PByte(FHeap), System.Length(FHeap), FCurrIndex);
      if I <> NULL_INDEX then
        begin
          FCurrIndex := I;
          exit(True);
        end;
    end;
  Result := False;
end;

{ TBmSearchCI.TMatches }

function TBmSearchCI.TMatches.GetEnumerator: TEnumerator;
begin
  Result.FCurrIndex := NULL_INDEX;
  Result.FHeap := FHeap;
  Result.FMatcher := FMatcher;
end;

{ TBmSearchCI }

procedure TBmSearchCI.FillMap;
var
  I: Integer;
begin
  for I := 0 to 255 do
    FLoCaseMap[I] := Ord(AnsiLowerCase(Char(I))[1]);
end;

procedure TBmSearchCI.FillBc;
var
  I, Len: Integer;
  p: PByte absolute FNeedle;
begin
  Len := System.Length(FNeedle);
  specialize TGArrayHelpUtil<Integer>.Fill(FBcShift, Len);
  for I := 0 to Len - 2 do
    FBcShift[p[I]] := Pred(Len - I);
end;

procedure TBmSearchCI.FillGs;
var
  I, J, LastPrefix, Len: Integer;
  IsPrefix: Boolean;
  p: PByte absolute FNeedle;
begin
  Len := System.Length(FNeedle);
  SetLength(FGsShift, Len);
  LastPrefix := Pred(Len);
  for I := Pred(Len) downto 0 do
    begin
      IsPrefix := True;
      for J := 0 to Len - I - 2 do
        if (p[J] <> p[J + Succ(I)]) then
          begin
            IsPrefix := False;
            break;
          end;
      if IsPrefix then
        LastPrefix := Succ(I);
      FGsShift[I] := LastPrefix + Len - Succ(I);
    end;
  for I := 0 to Len - 2 do
    begin
      J := 0;
      while (p[I - J] = p[Pred(Len - J)]) and (J < I) do
        Inc(J);
      if p[I - J] <> p[Pred(Len - J)] then
        FGsShift[Pred(Len - J)] := Pred(Len + J - I);
    end;
end;

function TBmSearchCI.DoFind(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
var
  J, NeedLast: SizeInt;
  p: PByte absolute FNeedle;
begin
  NeedLast := Pred(System.Length(FNeedle));
  while I < aHeapLen do
    begin
      while (I < aHeapLen) and (FLoCaseMap[aHeap[I]] <> p[NeedLast]) do
        I += FBcShift[FLoCaseMap[aHeap[I]]];
      if I >= aHeapLen then break;
      J := Pred(NeedLast);
      Dec(I);
      while (J <> NULL_INDEX) and (FLoCaseMap[aHeap[I]] = p[J]) do
        begin
          Dec(I);
          Dec(J);
        end;
      if J = NULL_INDEX then
        exit(Succ(I))
      else
        I += FGsShift[J];
    end;
  Result := NULL_INDEX;
end;

function TBmSearchCI.FindNext(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
begin
  if I = NULL_INDEX then
    Result := DoFind(aHeap, aHeapLen, I + System.Length(FNeedle))
  else
    Result := DoFind(aHeap, aHeapLen, I + FGsShift[0]);
end;

function TBmSearchCI.Find(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
begin
  Result := DoFind(aHeap, aHeapLen, I + Pred(System.Length(FNeedle)));
end;

constructor TBmSearchCI.Create(const aPattern: string);
var
  I: Integer;
  p: PByte;
begin
  FGsShift := nil;
  FNeedle := '';
  FillMap;
  if aPattern <> '' then
    begin
      System.SetLength(FNeedle, System.Length(aPattern));
      p := PByte(FNeedle);
      for I := 1 to System.Length(aPattern) do
        p[Pred(I)] := FLoCaseMap[Ord(aPattern[I])];
      FillBc;
      FillGs;
    end;
end;

function TBmSearchCI.Matches(const s: string): TMatches;
begin
  if FNeedle <> '' then
    Result.FHeap := s
  else
   Result.FHeap := '';
  Result.FMatcher := @Self;
end;

function TBmSearchCI.NextMatch(const s: string; aOffset: SizeInt): SizeInt;
begin
  if (FNeedle = '') or (s = '') then exit(0);
  if aOffset < 1 then
    aOffset := 1;
  Result := Succ(Find(PByte(s), System.Length(s), Pred(aOffset)));
end;

function TBmSearchCI.FindMatches(const s: string): TIntArray;
var
  I, J: SizeInt;
begin
  Result := nil;
  if (FNeedle = '') or (s = '') then exit;
  I := NULL_INDEX;
  J := 0;
  System.SetLength(Result, ARRAY_INITIAL_SIZE);
  repeat
    I := FindNext(PByte(s), System.Length(s), I);
    if I <> NULL_INDEX then
      begin
        if System.Length(Result) = J then
          System.SetLength(Result, J * 2);
        Result[J] := Succ(I);
        Inc(J);
      end;
  until I = NULL_INDEX;
  System.SetLength(Result, J);
end;

end.

