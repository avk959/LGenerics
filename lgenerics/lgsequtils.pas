{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Some algorithms on generic sequences.                                   *
*                                                                           *
*   Copyright(c) 2021 A.Koverdyaev(avk)                                     *
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
unit lgSeqUtils;

{$MODE OBJFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$INLINE ON}

interface

uses

  Classes, SysUtils, Math, UnicodeData,
  lgUtils,
  {%H-}lgHelpers,
  lgArrayHelpers,
  lgHashTable,
  lgHash;

type
  { TGBmSearch implements the Boyer-Moore exact pattern matching algorithm for
    arbitrary sequences in a variant called Fast-Search }
  generic TGBmSearch<T, TEqRel> = record
  public
  type
    TArray   = array of T;
    PItem    = ^T;
  private
  type
    THelper  = specialize TGArrayHelpUtil<T>;
    TEntry   = specialize TGMapEntry<T, Integer>;
    TMap     = specialize TGLiteChainHashTable<T, TEntry, TEqRel>;
    PMatcher = ^TGBmSearch;

    TEnumerator = record
      FCurrIndex,
      FHeapLen: SizeInt;
      FHeap: PItem;
      FMatcher: PMatcher;
      function GetCurrent: SizeInt; inline;
    public
      function MoveNext: Boolean;
      property Current: SizeInt read GetCurrent;
    end;

  var
    FBcShift: TMap;
    FGsShift: array of Integer;
    FNeedle: TArray;
    function  BcShift(const aValue: T): Integer; inline;
    procedure FillBc;
    procedure FillGs;
    function  DoFind(aHeap: PItem; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
    function  FindNext(aHeap: PItem; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
    function  Find(aHeap: PItem; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
  public
  type
    TIntArray = array of SizeInt;

    TMatches = record
    private
      FHeapLen: SizeInt;
      FHeap: PItem;
      FMatcher: PMatcher;
    public
      function GetEnumerator: TEnumerator; inline;
    end;

  { initializes the algorithm with a search pattern }
    constructor Create(const aPattern: array of T);
  { returns an enumerator of indices(0-based) of all occurrences of pattern in a }
    function Matches(const a: array of T): TMatches;
  { returns the index of the next occurrence of the pattern in a,
    starting at index aOffset(0-based) or -1 if there is no occurrence;
    to get the index of the next occurrence, you need to pass in aOffset
    the index of the previous occurrence, increased by one }
    function NextMatch(const a: array of T; aOffset: SizeInt): SizeInt;
  { returns in an array the indices(0-based) of all occurrences of the pattern in a }
    function FindMatches(const a: array of T): TIntArray;
  end;

  { TUniHasher }
  TUniHasher = record
    class function HashCode(aValue: UnicodeChar): SizeInt; static; inline;
    class function Equal(L, R: UnicodeChar): Boolean; static; inline;
  end;

  { TDwHasher }
  TDwHasher = record
    class function HashCode(aValue: DWord): SizeInt; static; inline;
    class function Equal(L, R: DWord): Boolean; static; inline;
  end;

  { TGSeqUtil provides several algorithms for arbitrary sequences
      TEqRel must provide:
      class function HashCode([const[ref]] aValue: T): SizeInt;
      class function Equal([const[ref]] L, R: T): Boolean; }
  generic TGSeqUtil<T, TEqRel> = record
  public
  type
    TArray = array of T;
    PItem = ^T;

  private
  type
    TNode = record
      Index,
      Next: SizeInt;
      constructor Create(aIndex, aNext: SizeInt);
    end;

    TNodeList = array of TNode;
    TEntry    = specialize TGMapEntry<T, SizeInt>;
    TMap      = specialize TGLiteChainHashTable<T, TEntry, TEqRel>;
    THelper   = specialize TGArrayHelpUtil<T>;

  const
    MAX_STATIC = 1024;

    class function Eq(const L, R: T): Boolean; static; inline;
    class function GetLis(const a: array of SizeInt; aMaxLen: SizeInt): TSizeIntArray; static;
    class function LcsGusImpl(L, R: PItem; aLenL, aLenR: SizeInt): TArray; static;
    class function LevDistImpl(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt; static;
    class function LevDistMbrImpl(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt; static;
  public
  { returns True if aSub is a subsequence of aSeq, False otherwise }
    class function IsSubSequence(const aSeq, aSub: array of T): Boolean; static;
    class function IsSubSequence(pSeq, pSub: PItem; aSecLen, aSubLen: SizeInt): Boolean; static;
  { returns Levenshtein distance between L and R; used a simple dynamic programming algorithm
    with O(mn) time complexity, where n and m are the lengths of L and R respectively,
    and O(Max(m, n)) space complexity }
    class function LevDistance(L, R: PItem; aLenL, aLenR: SizeInt): SizeInt; static;
    class function LevDistance(const L, R: array of T): SizeInt; static;
  { returns the Levenshtein distance between L and R; a Pascal translation of
    github.com/vaadin/gwt/dev/util/editdistance/ModifiedBerghelRoachEditDistance.java -
    a modified version of algorithm described by Berghel and Roach with O(min(aLenL, aLenR)*d)
    worst-case time complexity, where d is the edit distance computed  }
    class function LevDistanceMBR(L, R: PItem; aLenL, aLenR: SizeInt): SizeInt; static;
    class function LevDistanceMBR(const L, R: array of T): SizeInt; static;
  { the same as above; the aLimit parameter indicates the maximum expected distance,
    if this value is exceeded when calculating the distance, then the function exits
    immediately and returns -1 }
    class function LevDistanceMBR(L, R: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt; static;
    class function LevDistanceMBR(const L, R: array of T; aLimit: SizeInt): SizeInt; static;
  { returns the longest common subsequence(LCS) of sequences L and R, reducing the task to LIS,
    with O(RLogN) time complexity, where R is the number of the matching pairs in L and R;
    inspired by Dan Gusfield "Algorithms on Strings, Trees and Sequences", section 12.5; }
    class function LcsGus(L, R: PItem; aLenL, aLenR: SizeInt): TArray; static;
    class function LcsGus(const L, R: array of T): TArray; static;
  end;

{ the responsibility for the correctness and normalization of the strings lies with the user }
  function IsSubSequence(const aStr, aSub: unicodestring): Boolean;
{ these functions expect UTF-8 encoded strings as parameters; the responsibility
  for the correctness and normalization of the strings lies with the user }
  function IsSubSequenceUtf8(const aStr, aSub: ansistring): Boolean;
  function LevDistanceUtf8(const L, R: ansistring): SizeInt;
  function LevDistanceMBRUtf8(const L, R: ansistring): SizeInt;
  function LevDistanceMBRUtf8(const L, R: ansistring; aLimit: SizeInt): SizeInt;
  function LcsGusUtf8(const L, R: ansistring): ansistring;

implementation
{$B-}{$COPERATORS ON}

{ TGBmSearch.TEnumerator }

function TGBmSearch.TEnumerator.GetCurrent: SizeInt;
begin
  Result := FCurrIndex;
end;

function TGBmSearch.TEnumerator.MoveNext: Boolean;
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

{ TGBmSearch.TMatches }

function TGBmSearch.TMatches.GetEnumerator: TEnumerator;
begin
  Result.FCurrIndex := NULL_INDEX;
  Result.FHeapLen := FHeapLen;
  Result.FHeap := FHeap;
  Result.FMatcher := FMatcher;
end;

{ TGBmSearch }

function TGBmSearch.BcShift(const aValue: T): Integer;
var
  p: ^TEntry;
begin
  p := FBcShift.Find(aValue);
  if p <> nil then
    exit(p^.Value);
  Result := System.Length(FNeedle);
end;

procedure TGBmSearch.FillBc;
var
  I, Len: Integer;
  p: PItem absolute FNeedle;
  pe: ^TEntry;
begin
  Len := System.Length(FNeedle);
  for I := 0 to Len - 2 do
    if FBcShift.FindOrAdd(p[I], pe) then
      pe^.Value := Pred(Len - I)
    else
      pe^ := TEntry.Create(p[I], Pred(Len - I));
end;

procedure TGBmSearch.FillGs;
var
  I, J, LastPrefix, Len: Integer;
  IsPrefix: Boolean;
  p: PItem absolute FNeedle;
begin
  Len := System.Length(FNeedle);
  SetLength(FGsShift, Len);
  LastPrefix := Pred(Len);
  for I := Pred(Len) downto 0 do
    begin
      IsPrefix := True;
      for J := 0 to Len - I - 2 do
        if not TEqRel.Equal(p[J], p[J + Succ(I)]) then
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
      while TEqRel.Equal(p[I - J], p[Pred(Len - J)]) and (J < I) do
        Inc(J);
      if not TEqRel.Equal(p[I - J], p[Pred(Len - J)]) then
        FGsShift[Pred(Len - J)] := Pred(Len + J - I);
    end;
end;

function TGBmSearch.DoFind(aHeap: PItem; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
var
  J, NeedLast: SizeInt;
  p: PItem absolute FNeedle;
begin
  NeedLast := Pred(System.Length(FNeedle));
  while I < aHeapLen do
    begin
      while (I < aHeapLen) and not TEqRel.Equal(aHeap[I], p[NeedLast]) do
        I += BcShift(aHeap[I]);
      if I >= aHeapLen then
        break;
      J := Pred(NeedLast);
      Dec(I);
      while (J <> NULL_INDEX) and TEqRel.Equal(aHeap[I], p[J]) do
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

function TGBmSearch.FindNext(aHeap: PItem; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
begin
  if I = NULL_INDEX then
    Result := DoFind(aHeap, aHeapLen, I + System.Length(FNeedle))
  else
    Result := DoFind(aHeap, aHeapLen, I + FGsShift[0]);
end;

function TGBmSearch.Find(aHeap: PItem; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
begin
  Result := DoFind(aHeap, aHeapLen, I + Pred(System.Length(FNeedle)));
end;

constructor TGBmSearch.Create(const aPattern: array of T);
begin
  FBcShift := Default(TMap);
  FGsShift := nil;
  FNeedle := THelper.CreateCopy(aPattern);
  if FNeedle <> nil then
    begin
      FillBc;
      FillGs;
    end;
end;

function TGBmSearch.Matches(const a: array of T): TMatches;
begin
  if FNeedle <> nil then
    Result.FHeapLen := System.Length(a)
  else
    Result.FHeapLen := 0;
  if System.Length(a) <> 0 then
    Result.FHeap := @a[0]
  else
    Result.FHeap := nil;
  Result.FMatcher := @Self;
end;

function TGBmSearch.NextMatch(const a: array of T; aOffset: SizeInt): SizeInt;
begin
  if (FNeedle = nil) or (System.Length(a) = 0) then exit(NULL_INDEX);
  if aOffset < 0 then
    aOffset := 0;
  Result := Find(@a[0], System.Length(a), aOffset);
end;

function TGBmSearch.FindMatches(const a: array of T): TIntArray;
var
  I, J: SizeInt;
begin
  Result := nil;
  if (FNeedle = nil) or (System.Length(a) = 0) then exit;
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

{ TUniHasher }

class function TUniHasher.HashCode(aValue: UnicodeChar): SizeInt;
begin
  Result := JdkHashW(Ord(aValue));
end;

class function TUniHasher.Equal(L, R: UnicodeChar): Boolean;
begin
  Result := L = R;
end;

{ TDwHasher }

class function TDwHasher.HashCode(aValue: DWord): SizeInt;
begin
  Result := JdkHash(aValue);
end;

class function TDwHasher.Equal(L, R: DWord): Boolean;
begin
  Result := L = R;
end;

{ TGSeqUtil.TNode }

constructor TGSeqUtil.TNode.Create(aIndex, aNext: SizeInt);
begin
  Index := aIndex;
  Next := aNext;
end;

{ TGSeqUtil }

class function TGSeqUtil.Eq(const L, R: T): Boolean;
begin
  Result := TEqRel.Equal(L, R);
end;

class function TGSeqUtil.GetLis(const a: array of SizeInt; aMaxLen: SizeInt): TSizeIntArray;
var
  TailIdx: array of SizeInt = nil;
  function CeilIdx(aValue, R: SizeInt): SizeInt;
  var
    L, M: SizeInt;
  begin
    L := 0;
    while L < R do
      begin
        {$PUSH}{$Q-}{$R-}M := (L + R) shr 1;{$POP}
        if aValue <= a[TailIdx[M]] then
          R := M
        else
          L := Succ(M);
      end;
    CeilIdx := R;
  end;
var
  Parents: array of SizeInt;
  I, Idx, Len: SizeInt;
begin
  System.SetLength(TailIdx, aMaxLen);
  Parents := TSizeIntHelper.CreateAndFill(NULL_INDEX, System.Length(a));
  Result := nil;
  Len := 1;
  for I := 1 to System.High(a) do
    if a[I] < a[TailIdx[0]] then
      TailIdx[0] := I
    else
      if a[TailIdx[Pred(Len)]] < a[I] then
        begin
          Parents[I] := TailIdx[Pred(Len)];
          TailIdx[Len] := I;
          Inc(Len);
        end
      else
        begin
          Idx := CeilIdx(a[I], Pred(Len));
          Parents[I] := TailIdx[Pred(Idx)];
          TailIdx[Idx] := I;
        end;
  System.SetLength(Result, Len);
  Idx := TailIdx[Pred(Len)];
  for I := Pred(Len) downto 0 do
    begin
      Result[I] := a[Idx];
      Idx := Parents[Idx];
    end;
end;

class function TGSeqUtil.LcsGusImpl(L, R: PItem; aLenL, aLenR: SizeInt): TArray;
var
  MatchList: TMap;
  NodeList: TNodeList;
  Tmp: TSizeIntArray;
  Lis: TSizeIntArray;
  Tail: TArray = nil;
  I, J, NodeIdx: SizeInt;
  p: ^TEntry;
const
  INIT_SIZE = 256;
begin
  //here aLenL <= aLenR
  Result := nil;

  if L = R then
    exit(THelper.CreateCopy(L[0..Pred(aLenL)]));

  I := 0;
  while (aLenL >= 0) and TEqRel.Equal(L[Pred(aLenL)], R[Pred(aLenR)]) do
    begin
      Dec(aLenL);
      Dec(aLenR);
      Inc(I);
    end;

  if I > 0 then
    Tail := THelper.CreateCopy(L[aLenL..Pred(aLenL + I)]);

  I := 0;
  while (I < aLenL) and TEqRel.Equal(L[I], R[I]) do
     Inc(I);
  if I > 0 then
    begin
      Result := THelper.CreateCopy(L[0..Pred(I)]);
      L += I;
      R += I;
      aLenL -= I;
      aLenR -= I;
    end;

  for I := 0 to Pred(aLenL) do
    if not MatchList.FindOrAdd(L[I], p) then
      p^ := TEntry.Create(L[I], NULL_INDEX);

  System.SetLength(NodeList, INIT_SIZE);
  J := 0;
  for I := 0 to Pred(aLenR) do
    begin
      p := MatchList.Find(R[I]);
      if p <> nil then
        begin
          if System.Length(NodeList) = J then
            System.SetLength(NodeList, J * 2);
          NodeList[J] := TNode.Create(I, p^.Value);
          p^.Value := J;
          Inc(J);
        end;
    end;
  System.SetLength(NodeList, J);

  System.SetLength(Tmp, lgUtils.RoundUpTwoPower(J));
  J := 0;
  for I := 0 to Pred(aLenL) do
    begin
      NodeIdx := MatchList.Find(L[I])^.Value;
      while NodeIdx <> NULL_INDEX do
        with NodeList[NodeIdx] do
          begin
            if System.Length(Tmp) = J then
              System.SetLength(Tmp, J * 2);
            Tmp[J] := Index;
            NodeIdx := Next;
            Inc(J);
          end;
    end;
  System.SetLength(Tmp, J);

  if Tmp <> nil then
    begin
      NodeList := nil;
      Lis := GetLis(Tmp, aLenL);
      Tmp := nil;
      J := System.Length(Result);
      System.SetLength(Result, J + System.Length(Lis));
      for I := 0 to System.High(Lis) do
        Result[I+J] := R[Lis[I]];
    end;

  if Tail <> nil then
    System.Insert(Tail, Result, System.Length(Result));
end;

class function TGSeqUtil.LevDistImpl(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt;
var
  StBuf: array[0..Pred(MAX_STATIC)] of SizeInt;
  Buf: array of SizeInt = nil;
  I, J, Prev, Next: SizeInt;
  Dist: PSizeInt;
  v: T;
begin
  //here aLenL <= aLenR
  if pL = pR then
    exit(aLenR - aLenL);

  while (aLenL > 0) and Eq(pL[Pred(aLenL)], pR[Pred(aLenR)]) do
    begin
      Dec(aLenL);
      Dec(aLenR);
    end;

  I := 0;
  while (I < aLenL) and Eq(pL^, pR^) do
    begin
      Inc(pL);
      Inc(pR);
      Inc(I);
    end;

  if I = aLenL then
    exit(aLenR);

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
      v := pL[I-1];
{$IFDEF CPU64}
      J := 1;
      while J < aLenL - 3 do
        begin
          Next := MinOf3(Dist[J-1]+SizeInt(Eq(pR[J-1], v)), Prev+1, Dist[J]+1);
          Dist[J-1] := Prev; Prev := Next;

          Next := MinOf3(Dist[J]+SizeInt(Eq(pR[J], v)), Prev+1, Dist[J+1]+1);
          Dist[J] := Prev; Prev := Next;

          Next := MinOf3(Dist[J+1]+SizeInt(Eq(pR[J+1], v)), Prev+1, Dist[J+2]+1);
          Dist[J+1] := Prev; Prev := Next;

          Next := MinOf3(Dist[J+2]+SizeInt(Eq(pR[J+2], v)), Prev+1, Dist[J+3]+1);
          Dist[J+2] := Prev; Prev := Next;

          J += 4;
        end;
      for J := J to aLenR do
{$ELSE CPU64}
      for J := 1 to aLenR do
{$ENDIF}
        begin
          if Eq(pR[J-1], v) then
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

class function TGSeqUtil.LevDistMbrImpl(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt;

  function FindRow(k, aDist, aLeft, aAbove, aRight: SizeInt): SizeInt;
  var
    I, MaxRow: SizeInt;
  begin
    if aDist = 0 then I := 0
    else I := MaxOf3(aLeft, aAbove + 1, aRight + 1);
    MaxRow := Min(aLenL - k, aLenR);
    while (I < MaxRow) and Eq(pR[I], pL[I + k]) do
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

  while (aLenL > 0) and Eq(pL[Pred(aLenL)], pR[Pred(aLenR)]) do
    begin
      Dec(aLenL);
      Dec(aLenR);
    end;

  I := 0;
  while (I < aLenL) and Eq(pL^, pR^) do
    begin
      Inc(pL);
      Inc(pR);
      Inc(I);
    end;

  if I = aLenL then
    exit(aLenR);

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

class function TGSeqUtil.IsSubSequence(const aSeq, aSub: array of T): Boolean;
var
  I, J: SizeInt;
begin
  I := 0;
  J := 0;
  while (I < System.Length(aSeq)) and (J < System.Length(aSub)) do
    begin
      if TEqRel.Equal(aSeq[I], aSub[J]) then
        Inc(J);
      Inc(I);
    end;
  Result := J = System.Length(aSub);
end;

class function TGSeqUtil.IsSubSequence(pSeq, pSub: PItem; aSecLen, aSubLen: SizeInt): Boolean;
var
  I, J: SizeInt;
begin
  I := 0;
  J := 0;
  while (I < aSecLen) and (J < aSubLen) do
    begin
      if TEqRel.Equal(pSeq[I], pSub[J]) then
        Inc(J);
      Inc(I);
    end;
  Result := J = aSubLen;
end;

class function TGSeqUtil.LevDistance(L, R: PItem; aLenL, aLenR: SizeInt): SizeInt;
begin
  if aLenL = 0 then
    exit(aLenR)
  else
    if aLenR = 0 then
      exit(aLenL);
  if aLenL <= aLenR then
    Result := LevDistImpl(L, R, aLenL, aLenR)
  else
    Result := LevDistImpl(R, L, aLenR, aLenL);
end;

class function TGSeqUtil.LevDistance(const L, R: array of T): SizeInt;
begin
  if System.Length(L) = 0 then
    exit(System.Length(R))
  else
    if System.Length(R) = 0 then
      exit(System.Length(L));
  if System.Length(L) <= System.Length(R) then
    Result := LevDistImpl(@L[0], @R[0], System.Length(L), System.Length(R))
  else
    Result := LevDistImpl(@R[0], @L[0], System.Length(R), System.Length(L));
end;

class function TGSeqUtil.LevDistanceMBR(L, R: PItem; aLenL, aLenR: SizeInt): SizeInt;
begin
  if aLenL = 0 then
    exit(aLenR)
  else
    if aLenR = 0 then
      exit(aLenL);
  if aLenL <= aLenR then
    Result := LevDistMbrImpl(L, R, aLenL, aLenR, aLenR)
  else
    Result := LevDistMbrImpl(R, L, aLenR, aLenL, aLenL);
end;

class function TGSeqUtil.LevDistanceMBR(const L, R: array of T): SizeInt;
begin
  if System.Length(L) = 0 then
    exit(System.Length(R))
  else
    if System.Length(R) = 0 then
      exit(System.Length(L));
  if System.Length(L) <= System.Length(R) then
    Result := LevDistMbrImpl(@L[0], @R[0], System.Length(L), System.Length(R), System.Length(R))
  else
    Result := LevDistMbrImpl(@R[0], @L[0], System.Length(R), System.Length(L), System.Length(L));
end;

class function TGSeqUtil.LevDistanceMBR(L, R: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt;
begin
  if aLimit < 0 then
    aLimit := 0;
  if aLenL = 0 then
    if aLenR <= aLimit then
      exit(aLenR)
    else
      exit(NULL_INDEX)
  else
    if aLenR = 0 then
      if aLenL <= aLimit then
        exit(aLenL)
      else
        exit(NULL_INDEX);
  if aLenL <= aLenR then
    Result := LevDistMbrImpl(L, R, aLenL, aLenR, aLimit)
  else
    Result := LevDistMbrImpl(R, L, aLenR, aLenL, aLimit);
end;

class function TGSeqUtil.LevDistanceMBR(const L, R: array of T; aLimit: SizeInt): SizeInt;
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
    Result := LevDistMbrImpl(@L[0], @R[0], System.Length(L), System.Length(R), aLimit)
  else
    Result := LevDistMbrImpl(@R[0], @L[0], System.Length(R), System.Length(L), aLimit);
end;

class function TGSeqUtil.LcsGus(L, R: PItem; aLenL, aLenR: SizeInt): TArray;
begin
  if (aLenL = 0) or (aLenR = 0) then
    exit(nil);
  if aLenL <= aLenR then
    Result := LcsGusImpl(L, R, aLenL, aLenR)
  else
    Result := LcsGusImpl(R, L, aLenR, aLenL);
end;

class function TGSeqUtil.LcsGus(const L, R: array of T): TArray;
begin
  if (System.Length(L) = 0) or (System.Length(R) = 0) then
    exit(nil);
  if System.Length(L) <= System.Length(R) then
    Result := LcsGusImpl(@L[0], @R[0], System.Length(L), System.Length(R))
  else
    Result := LcsGusImpl(@R[0], @L[0], System.Length(R), System.Length(L));
end;

type
  TChar32     = DWord;
  PChar32     = ^TChar32;
  TChar32Seq  = array of TChar32;
  TChar32Util = specialize TGSeqUtil<TChar32, TDwHasher>;
  TByte4      = array[0..3] of Byte;

const
  MAX_STATIC = TChar32Util.MAX_STATIC;

function CodePointLen(b: Byte): SizeInt; inline;
begin
  case b of
    0..127   : Result := 1;
    128..223 : Result := 2;
    224..239 : Result := 3;
    240..247 : Result := 4;
  else
    Result := 0;
  end;
end;

function CodePointToChar32(p: PByte; out aSize: SizeInt): TChar32; inline;
begin
  case p^ of
    0..127:
      begin
        aSize := 1;
        Result := p^;
      end;
    128..223:
      begin
        aSize := 2;
        Result := TChar32(TChar32(p[0] and $1f) shl 6 or (p[1] and $3f));
      end;
    224..239:
      begin
        aSize := 3;
        Result := TChar32(TChar32(p[0] and $f) shl 12 or TChar32(p[1] and $3f) shl 6 or (p[2] and $3f));
      end;
    240..247:
      begin
        aSize := 4;
        Result := TChar32(TChar32(p[0] and $7) shl 18 or TChar32(p[1] and $3f) shl 12 or
                          TChar32(p[2] and $3f) shl 6 or (p[3] and $3f));
      end;
  else
    aSize := 0;
    Result := 0;
  end;
end;

function Utf8Len(const s: ansistring): SizeInt;
var
  I: SizeInt;
  p: PByte absolute s;
begin
  Result := 0;
  I := 0;
  while I < System.Length(s) do
    begin
      I += CodePointLen(p[I]);
      Inc(Result);
    end;
end;

function ToChar32Seq(const s: ansistring): TChar32Seq;
var
  r: TChar32Seq = nil;
  I, J, Len: SizeInt;
  p: PByte absolute s;
begin
  System.SetLength(r, System.Length(s));
  I := 0;
  J := 0;
  while I < System.Length(s) do
    begin
      r[J] := CodePointToChar32(@p[I], Len);
      Inc(J);
      I += Len;
    end;
  System.SetLength(r, J);
  Result := r;
end;

procedure ToChar32Seq(const s: ansistring; out a: array of TChar32; out aLen: SizeInt);
var
  I, Size: SizeInt;
  p: PByte absolute s;
begin
  I := 0;
  aLen := 0;
  while I < System.Length(s) do
    begin
      a[aLen] := CodePointToChar32(@p[I], Size);
      Inc(aLen);
      I += Size;
    end;
end;

function IsSubSequence(const aStr, aSub: unicodestring): Boolean;
var
  I, J: SizeInt;
  pStr: PUnicodeChar absolute aStr;
  pSub: PUnicodeChar absolute aSub;
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

function IsSubSequenceUtf8(const aStr, aSub: ansistring): Boolean;
var
  I, J, LenStr, LenSub: SizeInt;
  vStr, vSub: DWord;
  pStr: PByte absolute aStr;
  pSub: PByte absolute aSub;
begin
  I := 0;
  J := 0;
  vSub := CodePointToChar32(pSub, LenSub);
  while (I < System.Length(aStr)) and (J < System.Length(aSub)) do
    begin
      vStr := CodePointToChar32(@pStr[I], LenStr);
      if vStr = vSub then
        begin
          Inc(J, LenSub);
          vSub := CodePointToChar32(@pSub[J], LenSub);
        end;
      Inc(I, LenStr);
    end;
  Result := J = System.Length(aSub);
end;

function LevDistanceUtf8(const L, R: ansistring): SizeInt;
var
  LBufSt, RBufSt: array[0..Pred(MAX_STATIC)] of TChar32;
  LBuf: TChar32Seq = nil;
  RBuf: TChar32Seq = nil;
  LenL, LenR: SizeInt;
  pL, pR: PChar32;
begin
  if System.Length(L) <= MAX_STATIC then
    begin
      ToChar32Seq(L, LBufSt, LenL);
      pL := @LBufSt[0];
    end
  else
    begin
      LBuf := ToChar32Seq(L);
      LenL := System.Length(LBuf);
      pL := Pointer(LBuf);
    end;
  if System.Length(R) <= MAX_STATIC then
    begin
      ToChar32Seq(R, RBufSt, LenR);
      pR := @RBufSt[0];
    end
  else
    begin
      RBuf := ToChar32Seq(R);
      LenR := System.Length(RBuf);
      pR := Pointer(RBuf);
    end;
  Result := TChar32Util.LevDistance(pL, pR, LenL, LenR);
end;

function LevDistanceMBRUtf8(const L, R: ansistring): SizeInt;
var
  LBufSt, RBufSt: array[0..Pred(MAX_STATIC)] of TChar32;
  LBuf: TChar32Seq = nil;
  RBuf: TChar32Seq = nil;
  LenL, LenR: SizeInt;
  pL, pR: PChar32;
begin
  if System.Length(L) <= MAX_STATIC then
    begin
      ToChar32Seq(L, LBufSt, LenL);
      pL := @LBufSt[0];
    end
  else
    begin
      LBuf := ToChar32Seq(L);
      LenL := System.Length(LBuf);
      pL := Pointer(LBuf);
    end;
  if System.Length(R) <= MAX_STATIC then
    begin
      ToChar32Seq(R, RBufSt, LenR);
      pR := @RBufSt[0];
    end
  else
    begin
      RBuf := ToChar32Seq(R);
      LenR := System.Length(RBuf);
      pR := Pointer(RBuf);
    end;
  Result := TChar32Util.LevDistanceMBR(pL, pR, LenL, LenR);
end;

function LevDistanceMBRUtf8(const L, R: ansistring; aLimit: SizeInt): SizeInt;
var
  LBufSt, RBufSt: array[0..Pred(MAX_STATIC)] of TChar32;
  LBuf: TChar32Seq = nil;
  RBuf: TChar32Seq = nil;
  LenL, LenR: SizeInt;
  pL, pR: PChar32;
begin
  if System.Length(L) <= MAX_STATIC then
    begin
      ToChar32Seq(L, LBufSt, LenL);
      pL := @LBufSt[0];
    end
  else
    begin
      LBuf := ToChar32Seq(L);
      LenL := System.Length(LBuf);
      pL := Pointer(LBuf);
    end;
  if System.Length(R) <= MAX_STATIC then
    begin
      ToChar32Seq(R, RBufSt, LenR);
      pR := @RBufSt[0];
    end
  else
    begin
      RBuf := ToChar32Seq(R);
      LenR := System.Length(RBuf);
      pR := Pointer(RBuf);
    end;
  Result := TChar32Util.LevDistanceMBR(pL, pR, LenL, LenR, aLimit);
end;

function Char32ToUtf8Char(c32: TChar32; out aBytes: TByte4): Integer;
begin
  case c32 of
    0..127:
      begin
        aBytes[0] := Byte(c32);
        Result := 1;
      end;
    128..$7ff:
      begin
        aBytes[0] := Byte(c32 shr 6 or $c0);
        aBytes[1] := Byte(c32 and $3f or $80);
        Result := 2;
      end;
    $800..$ffff:
      begin
        aBytes[0] := Byte(c32 shr 12 or $e0);
        aBytes[1] := Byte(c32 shr 6) and $3f or $80;
        aBytes[2] := Byte(c32 and $3f) or $80;
        Result := 3;
      end;
    $10000..$10ffff:
      begin
        aBytes[0] := Byte(c32 shr 18) or $f0;
        aBytes[1] := Byte(c32 shr 12) and $3f or $80;
        aBytes[2] := Byte(c32 shr  6) and $3f or $80;
        aBytes[3] := Byte(c32 and $3f) or $80;
        Result := 4;
      end
  else
    Result := 0;
  end;
end;

function Char32Utf8Len(c32: TChar32): Integer; inline;
begin
  case c32 of
    0..127:          Result := 1;
    128..$7ff:       Result := 2;
    $800..$ffff:     Result := 3;
    $10000..$10ffff: Result := 4;
  else
    Result := 0;
  end;
end;

function Char32SeqUtf8Len(const r: TChar32Seq): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I := 0 to System.High(r) do
    Result += Char32Utf8Len(r[I]);
end;

function Char32SeqToUtf8(const aSeq: TChar32Seq): ansistring;
var
  s: string = '';
  I, J: SizeInt;
  Curr: TChar32;
  Len: Integer;
  p: PByte;
begin
  System.SetLength(s, System.Length(aSeq));
  p := Pointer(s);
  I := 0;
  for J := 0 to System.High(aSeq) do
    begin
      Curr := aSeq[J];
      Len := Char32Utf8Len(Curr);
      if System.Length(s) < I + Len then
        begin
          System.SetLength(s, (I + Len)*2);
          p := Pointer(s);
        end;
      case Len of
        1: p[I] := Byte(Curr);
        2:
          begin
            p[I  ] := Byte(Curr shr 6 or $c0);
            p[I+1] := Byte(Curr and $3f or $80);
          end;
        3:
          begin
            p[I  ] := Byte(Curr shr 12 or $e0);
            p[I+1] := Byte(Curr shr 6) and $3f or $80;
            p[I+2] := Byte(Curr and $3f) or $80;
          end;
        4:
          begin
            p[I  ] := Byte(Curr shr 18) or $f0;
            p[I+1] := Byte(Curr shr 12) and $3f or $80;
            p[I+2] := Byte(Curr shr  6) and $3f or $80;
            p[I+3] := Byte(Curr and $3f) or $80;
          end;
      else
      end;
      I += Len;
    end;
  System.SetLength(s, I);
  Result := s;
end;

function LcsGusUtf8(const L, R: ansistring): ansistring;
var
  LBufSt, RBufSt: array[0..Pred(MAX_STATIC)] of TChar32;
  LBuf: TChar32Seq = nil;
  RBuf: TChar32Seq = nil;
  LenL, LenR: SizeInt;
  pL, pR: PChar32;
begin
  if System.Length(L) <= MAX_STATIC then
    begin
      ToChar32Seq(L, LBufSt, LenL);
      pL := @LBufSt[0];
    end
  else
    begin
      LBuf := ToChar32Seq(L);
      LenL := System.Length(LBuf);
      pL := Pointer(LBuf);
    end;
  if System.Length(R) <= MAX_STATIC then
    begin
      ToChar32Seq(R, RBufSt, LenR);
      pR := @RBufSt[0];
    end
  else
    begin
      RBuf := ToChar32Seq(R);
      LenR := System.Length(RBuf);
      pR := Pointer(RBuf);
    end;
  Result := Char32SeqToUtf8(TChar32Util.LcsGus(pL, pR, LenL, LenR));
end;

end.
