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
      FHeap: string;
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
    FNeedle: string;
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
    constructor Create(const aPattern: string);
    constructor Create(const aPattern: array of Byte);
  { returns an enumerator of indices(1-based) of all occurrences of pattern in s }
    function Matches(const s: string): TStrMatches; inline;
  { returns an enumerator of indices(0-based) of all occurrences of pattern in a }
    function Matches(const a: array of Byte): TByteMatches;
  { returns the index of the next occurrence of the pattern in s,
    starting at index aOffset(1-based) or 0 if there is no occurrence;
    to get the index of the next occurrence, you need to pass in aOffset
    the index of the previous occurrence, increased by one }
    function NextMatch(const s: string; aOffset: SizeInt = 1): SizeInt;
  { returns the index of the next occurrence of the pattern in a,
    starting at index aOffset(0-based) or -1 if there is no occurrence;
    to get the index of the next occurrence, you need to pass in aOffset
    the index of the previous occurrence, increased by one }
    function NextMatch(const a: array of Byte; aOffset: SizeInt = 0): SizeInt;
  { returns in an array the indices(1-based) of all occurrences of the pattern in s }
    function FindMatches(const s: string): TIntArray;
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
      FHeap: string;
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
    FNeedle: string;
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
    constructor Create(const aPattern: string);
    constructor Create(const aPattern: array of Byte);
  { returns an enumerator of indices(1-based) of all occurrences of pattern in s }
    function Matches(const s: string): TStrMatches; inline;
  { returns an enumerator of indices(0-based) of all occurrences of pattern in a }
    function Matches(const a: array of Byte): TByteMatches;
  { returns the index of the next occurrence of the pattern in s,
    starting at index aOffset(1-based) or 0 if there is no occurrence;
    to get the index of the next occurrence, you need to pass in aOffset
    the index of the previous occurrence, increased by one }
    function NextMatch(const s: string; aOffset: SizeInt = 1): SizeInt;
  { returns the index of the next occurrence of the pattern in a,
    starting at index aOffset(0-based) or -1 if there is no occurrence;
    to get the index of the next occurrence, you need to pass in aOffset
    the index of the previous occurrence, increased by one }
    function NextMatch(const a: array of Byte; aOffset: SizeInt = 0): SizeInt;
  { returns in an array the indices(1-based) of all occurrences of the pattern in s }
    function FindMatches(const s: string): TIntArray;
  { returns in an array the indices(0-based) of all occurrences of the pattern in a }
    function FindMatches(const a: array of Byte): TIntArray;
  end;

  { TBmSearchCI implements case insensitive variant of TBmSearch }
  TBmSearchCI = record
  private
  type
    PMatcher = ^TBmSearchCI;

    TStrEnumerator = record
    private
      FCurrIndex: SizeInt;
      FHeap: string;
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
    FNeedle: string;
    procedure FillMap;
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
  { initializes the algorithm with a search pattern }
    constructor Create(const aPattern: string);
  { returns an enumerator of indices(1-based) of all occurrences of pattern in s }
    function Matches(const s: string): TStrMatches; inline;
  { returns the index of the next occurrence of the pattern in s,
    starting at index aOffset(1-based) or 0 if there is no occurrence;
    to get the index of the next occurrence, you need to pass in aOffset
    the index of the previous occurrence, increased by one }
    function NextMatch(const s: string; aOffset: SizeInt = 1): SizeInt;
  { returns in an array the indices(1-based) of all occurrences of the pattern in s }
    function FindMatches(const s: string): TIntArray;
  end;


{ returns True if aSub is a subsequence of aStr, False otherwise }
  function IsSubSequence(const aStr, aSub: string): Boolean;
{ returns longest common subsequence(LCS); from Dan Gusfield
  "Algorithms on Strings, Trees and Sequences", section 12.5 }
  function LcsGus(const L, R: string): string;
  function LcsGus(const L, R: array of Byte): TBytes;
{ returns Levenshtein distance between L and R }
  function LevenshteinDistance(const L, R: string): SizeInt;
  function LevenshteinDistance(const L, R: array of Byte): SizeInt;
  function IsValidDotQuadIPv4(const s: string): Boolean;
  function IsValidDotDecIPv4(const s: string): Boolean;

implementation
{$B-}{$COPERATORS ON}

{$PUSH}{$WARN 5036 off}
function IsSubSequence(const aStr, aSub: string): Boolean;
var
  I, J: SizeInt;
  pStr: PAnsiChar absolute aStr;
  pSub: PAnsiChar absolute aSub;
begin
  I := 0;
  J := 0;
  while (I < System.Length(aStr)) and (J < System.Length(aSub)) do
    begin
      if pStr[I] = pSub[J] then
        Inc(J);
      Inc(I);
    end;
  Result := J = System.Length(aSub);
end;

function GetLcsG(const pL, PR: PByte; const aLenL, aLenR: SizeInt): TBytes;
type
  TNode = record
    Index,
    Next: SizeInt;
  end;
  TNodeList = array of TNode;
var
  SeqTbl: array[Byte] of SizeInt;
  NodeList: TNodeList = nil;
  Tmp: TSizeIntArray = nil;
  LocLis: TSizeIntArray;
  I, J, From, NodeIdx: SizeInt;
begin
  From := 0;
  while (From < aLenL) and (pL[From] = pR[From]) do
    Inc(From);
  Result := specialize TGArrayHelpUtil<Byte>.CreateCopy(pL[0..Pred(From)]);
  if From = aLenL then exit;

  for I := 0 to 255 do
    SeqTbl[I] := NULL_INDEX;

  System.SetLength(NodeList, ARRAY_INITIAL_SIZE);
  J := 0;
  for I := From to Pred(aLenR) do
    begin
      if System.Length(NodeList) = J then
        System.SetLength(NodeList, J * 2);
      NodeList[J].Index := I;
      NodeList[J].Next := SeqTbl[pR[I]];
      SeqTbl[pR[I]] := J;
      Inc(J);
    end;

  System.SetLength(Tmp, ARRAY_INITIAL_SIZE);
  J := 0;
  for I := From to Pred(aLenL) do
    begin
      NodeIdx := SeqTbl[pL[I]];
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
  if Tmp = nil then exit;
  NodeList := nil;

  LocLis := TSizeIntHelper.Lis(Tmp);
  if LocLis = nil then
    begin
      System.SetLength(Result, Succ(System.Length(Result)));
      Result[From] := pR[Tmp[0]];
      exit;
    end;
  Tmp := nil;

  System.SetLength(Result, System.Length(Result) + System.Length(LocLis));
  for I := 0 to System.High(LocLis) do
    Result[I+From] := pR[LocLis[I]];
end;

function LcsGus(const L, R: string): string;
var
  I: SizeInt;
  b: TBytes = nil;
begin
  Result := '';
  if (L = '') or (R = '') then exit;
  if Pointer(L) = Pointer(R) then
    exit(System.Copy(L, 1, System.Length(L)));
  if (System.Length(L) = 1) then
    begin
      for I := 1 to System.Length(R) do
        if L[1] = R[I] then
          exit(L[1]);
      exit;
    end
  else
    if (System.Length(R) = 1) then
      begin
        for I := 1 to System.Length(L) do
          if R[1] = L[I] then
            exit(R[1]);
        exit;
      end;

  if System.Length(L) <= System.Length(R) then
    b := GetLcsG(Pointer(L), Pointer(R), System.Length(L), System.Length(R))
  else
    b := GetLcsG(Pointer(R), Pointer(L), System.Length(R), System.Length(L));

  System.SetLength(Result, System.Length(b));
  System.Move(Pointer(b)^, Pointer(Result)^, System.Length(b));
end;

function LcsGus(const L, R: array of Byte): TBytes;
var
  I: SizeInt;
begin
  Result := nil;
  if (System.Length(L) = 0) or (System.Length(R) = 0) then
    exit(nil);
  if @L[0] = @R[0] then
    exit(specialize TGArrayHelpUtil<Byte>.CreateCopy(L));
  if System.Length(L) = 1 then
    begin
      for I := 0 to Pred(System.Length(R)) do
        if L[0] = R[I] then
          exit([L[0]]);
      exit(nil);
    end
  else
    if System.Length(R) = 1 then
      begin
        for I := 0 to Pred(System.Length(L)) do
          if R[0] = L[I] then
            exit([R[0]]);
        exit(nil);
      end;

  if System.Length(L) <= System.Length(R) then
    Result := GetLcsG(@L[0], @R[0], System.Length(L), System.Length(R))
  else
    Result := GetLcsG(@R[0], @L[0], System.Length(R), System.Length(L));
end;

function MinOf3(const a, b, c: SizeInt): SizeInt; inline;
begin
  Result := a;
  if b < Result then
    Result := b;
  if c < Result then
    Result := c;
end;

const
  MAX_STATIC = 1024;

function LevDist(pL, pR: PByte; aLenL, aLenR: SizeInt): SizeInt;
var
  StBuf: array[0..Pred(MAX_STATIC)] of SizeInt;
  Buf: array of SizeInt = nil;
  I, J, Prev, Next: SizeInt;
  Dist: PSizeInt;
  b: Byte;
begin
  //here aLenL <= aLenR and L <> R
  while pL[Pred(aLenL)] = pR[Pred(aLenR)] do
    begin
      Dec(aLenL);
      Dec(aLenR);
    end;

  I := 0;
  while pL^ = pR^ do
    begin
      Inc(pL);
      Inc(pR);
      Inc(I);
    end;
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
          if pR[J-1] = b then Next := Dist[J-1]
          else Next := MinOf3(Dist[J-1]+1, Prev+1, Dist[J]+1);
          Dist[J-1] := Prev; Prev := Next;

          if pR[J] = b then Next := Dist[J]
          else Next := MinOf3(Dist[J]+1, Prev+1, Dist[J+1]+1);
          Dist[J] := Prev; Prev := Next;

          if pR[J+1] = b then Next := Dist[J+1]
          else Next := MinOf3(Dist[J+1]+1, Prev+1, Dist[J+2]+1);
          Dist[J+1] := Prev; Prev := Next;

          if pR[J+2] = b then Next := Dist[J+2]
          else Next := MinOf3(Dist[J+2]+1, Prev+1, Dist[J+3]+1);
          Dist[J+2] := Prev; Prev := Next;

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
            Next := MinOf3(Dist[J-1]+1, Prev+1, Dist[J]+1);
          Dist[J-1] := Prev;
          Prev := Next;
        end;
      Dist[aLenR] := Prev;
    end;
  Result := Dist[aLenR];
end;

function LevenshteinDistance(const L, R: string): SizeInt;
begin
  if L = '' then
    exit(System.Length(R))
  else
    if R = '' then
      exit(System.Length(L));
  if L = R then
    exit(0);
  if System.Length(L) <= System.Length(R) then
    Result := LevDist(Pointer(L), Pointer(R), System.Length(L), System.Length(R))
  else
    Result := LevDist(Pointer(R), Pointer(L), System.Length(R), System.Length(L));
end;

function LevenshteinDistance(const L, R: array of Byte): SizeInt;
begin
  if System.Length(L) = 0 then
    exit(System.Length(R))
  else
    if System.Length(R) = 0 then
      exit(System.Length(L));
  if specialize TGOrdinalArrayHelper<Byte>.Same(L, R) then
    exit(0);
  if System.Length(L) <= System.Length(R) then
    Result := LevDist(@L[0], @R[0], System.Length(L), System.Length(R))
  else
    Result := LevDist(@R[0], @L[0], System.Length(R), System.Length(L));
end;

function IsValidDotQuadIPv4(const s: string): Boolean;
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

function IsValidDotDecIPv4(const s: string): Boolean;
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

{ TBmSearchCI.TStrEnumerator }

function TBmSearchCI.TStrEnumerator.GetCurrent: SizeInt;
begin
  Result := Succ(FCurrIndex);
end;

function TBmSearchCI.TStrEnumerator.MoveNext: Boolean;
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

{ TBmSearchCI.TStrMatches }

function TBmSearchCI.TStrMatches.GetEnumerator: TStrEnumerator;
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

function TBmSearchCI.Matches(const s: string): TStrMatches;
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

