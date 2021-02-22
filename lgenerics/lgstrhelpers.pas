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

  Classes, SysUtils, RegExpr,
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

    function GetEnumerable: IStrEnumerable; inline;
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

implementation
{$Q-}{$B-}{$COPERATORS ON}

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

function TStringListHelper.GetEnumerable: IStrEnumerable;
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

function Max(L, R: Integer): Integer; inline;
begin
  if L < R then
    Result := R
  else
    Result := L;
end;

function TBmSearch.FindNext(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
var
  J, NeedLast: SizeInt;
  p: PByte absolute FNeedle;
begin
  NeedLast := Pred(System.Length(FNeedle));
  if I = NULL_INDEX then
    I += System.Length(FNeedle)
  else
    I += FGsShift[0];
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

function TBmSearch.Find(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
var
  J, NeedLast: SizeInt;
  p: PByte absolute FNeedle;
begin
  NeedLast := Pred(System.Length(FNeedle));
  I += NeedLast;
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

constructor TBmSearch.Create(const aPattern: string);
begin
  FGsShift := nil;
  if aPattern <> '' then
    FNeedle := System.Copy(aPattern, 1, System.Length(aPattern))
  else
    FNeedle := '';
  if FNeedle <> '' then
    begin
      FillBc;
      FillGs;
    end;
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
  NeedLast := Pred(System.Length(FNeedle));
  case System.Length(FNeedle) of
    1:
      begin
        J := IndexByte(aHeap[I], aHeapLen - I, p^);
        if J < 0 then exit(J);
        exit(I + J);
      end;
    2:
      while I <= aHeapLen - Succ(NeedLast) do
        begin
          if(aHeap[I + 1] = p[1]) and (aHeap[I] = p^) then
            exit(I);
          I += FBcShift[aHeap[I + NeedLast]];
        end;
    3:
      while I <= aHeapLen - Succ(NeedLast) do
        begin
          if(aHeap[I + NeedLast] = p[NeedLast]) and (aHeap[I] = p^) and
            (aHeap[I + NeedLast shr 1] = p[NeedLast shr 1]) then
            exit(I);
          I += FBcShift[aHeap[I + NeedLast]];
        end;
  else
    while I <= aHeapLen - Succ(NeedLast) do
      begin
        if(aHeap[I + NeedLast] = p[NeedLast]) and (aHeap[I] = p^) and
          (aHeap[I + NeedLast shr 1] = p[NeedLast shr 1]) and
          (CompareByte(aHeap[Succ(I)], p[1], Pred(NeedLast)) = 0) then
          exit(I);
        I += FBcShift[aHeap[I + NeedLast]];
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
    FNeedle := System.Copy(aPattern, 1, System.Length(aPattern))
  else
    FNeedle := '';
  if FNeedle <> '' then
    FillBc;
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

function TBmSearchCI.FindNext(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
var
  J, NeedLast: SizeInt;
  p: PByte absolute FNeedle;
begin
  NeedLast := Pred(System.Length(FNeedle));
  if I = NULL_INDEX then
    I += System.Length(FNeedle)
  else
    I += FGsShift[0];
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

function TBmSearchCI.Find(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
var
  J, NeedLast: SizeInt;
  p: PByte absolute FNeedle;
begin
  NeedLast := Pred(System.Length(FNeedle));
  I += NeedLast;
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

