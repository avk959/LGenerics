{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Some algorithms on generic sequences.                                   *
*                                                                           *
*   Copyright(c) 2022-2024 A.Koverdyaev(avk)                                *
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
{$MODESWITCH NESTEDPROCVARS}
{$MODESWITCH ARRAYOPERATORS}
{$INLINE ON}

interface

uses

  Classes, SysUtils, Math,
  lgUtils,
  {%H-}lgHelpers,
  lgArrayHelpers,
  lgVector,
  lgQueue,
  lgHashTable,
  lgHashMap,
  lgHash,
  lgHashMultiSet;

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
    private
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

  { TUcs4Hasher }
  TUcs4Hasher = record
    class function HashCode(aValue: Ucs4Char): SizeInt; static; inline;
    class function Equal(L, R: Ucs4Char): Boolean; static; inline;
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
    TDiff = record
      SourceChanges,                   //here True indicates deletion
      TargetChanges: array of Boolean; //here True indicates insertion
    end;

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
    TVector   = specialize TGLiteVector<T>;
    THelper   = class(specialize TGArrayHelpUtil<T>);
    TSnake    = record
      StartRow, StartCol,
      EndRow, EndCol: SizeInt;
      procedure SetStartCell(aRow, aCol: SizeInt); inline;
      procedure SetEndCell(aRow, aCol: SizeInt); inline;
    end;

    TQEntry  = specialize TGMapEntry<T, QWord>;
    TQMap    = specialize TGLiteChainHashTable<T, TQEntry, TEqRel>;
    TDQValue = array[0..1] of QWord;
    TDQEntry = specialize TGMapEntry<T, TDQValue>;
    TDQMap   = specialize TGLiteChainHashTable<T, TDQEntry, TEqRel>;
    TPQEntry = specialize TGMapEntry<T, PQWord>;
    TPQMap   = specialize TGLiteChainHashTable<T, TPQEntry, TEqRel>;
    TPeq     = record
      Map: TPQMap;
      Buffer: array of QWord;
      BlockCount: SizeInt;
    end;
    TBlock   = record
      P,
      M: QWord;
      Score: SizeInt;
    end;
    TMultiSetType = specialize TGLiteChainHashMultiSet<T, TEqRel>;
    TMultiSet     = TMultiSetType.TMultiSet;

  const
    MAX_STATIC = 512;
    BLOCK_SIZE = BitSizeOf(QWord);
    BSIZE_MASK = Pred(BLOCK_SIZE);
    BSIZE_LOG  = 6;

    class function  SkipPrefix(var pL, pR: PItem; var aLenL, aLenR: SizeInt): SizeInt; static; inline;
    class function  SkipSuffix(pL, pR: PItem; var aLenL, aLenR: SizeInt): SizeInt; static; inline;
    class function  GetLis(const a: array of SizeInt; aMaxLen: SizeInt): TSizeIntArray; static;
    class function  IsTrivialDist(const L, R: array of T; aLimit: SizeInt; out aDist: SizeInt): Boolean; static; //inline;
    class function  LcsGusImpl(L, R: PItem; aLenL, aLenR: SizeInt): TArray; static;
    class function  LcsKRImpl(pL, pR: PItem; aLenL, aLenR: SizeInt): TArray; static;
    class function  LcsMyersImpl(pL, pR: PItem; aLenL, aLenR: SizeInt): TArray; static;
    class function  LevDistImpl(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt; static;
    class function  LevDistMbrImpl(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt; static;
    class function  LevDistMbrDyn(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt; static;
    class function  GetLevDistMbr(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt; static;
    class function  LevDistMyersQ(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt; static;
    class function  LevDistMyersQ(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt; static;
    class function  LevDistMyersDQ(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt; static;
    class function  LevDistMyersDQ(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt; static;
    class procedure CreatePeq(aSeq: PItem; aSeqLen: SizeInt; out aPeq: TPeq); static;
    class function  LevDistMyersCutoff(const aPeq: TPeq; pR: PItem; aLenL, aLenR, K: SizeInt): SizeInt; static;
    class function  LevDistMyersDyn(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt; static;
    class function  LevDistMyers(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt; static;
    class function  GetLevDistMyers(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt; static;
    class function  GetLevDistMyers(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt; static;
    class function  LcsDistMyersImpl(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt; static;
    class function  GetLcsDistMyers(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt; static;
    class function  DumDistMbrImpl(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt; static;
    class function  DumDistMbrDyn(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt; static;
    class function  GetDumDistMbr(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt; static;
  public
  { returns True if L and R are identical sequence of elements }
    class function Same(const L, R: array of T): Boolean; static;
  { returns True if L is a combinatorial permutation of the elements of R(or vise versa) }
    class function IsPermutation(const L, R: array of T): Boolean; static;
  { returns True if aSub is a subsequence of aSeq, False otherwise }
    class function IsSubSequence(const aSeq, aSub: array of T): Boolean; static;
  { returns True if L is a prefix of R; an empty array cannot be a prefix of any other array }
    class function IsPrefix(const L, R: array of T): Boolean; static;
  { returns True if L is a suffix of R; an empty array cannot be a suffix of any other array }
    class function IsSuffix(const L, R: array of T): Boolean; static;
  { returns the length of the common prefix L and R }
    class function CommonPrefixLen(const L, R: array of T): SizeInt; static;
  { returns the length of the common suffix L and R }
    class function CommonSuffixLen(const L, R: array of T): SizeInt; static;
  { returns Levenshtein distance between L and R; used a simple dynamic programming algorithm
    with O(mn) time complexity, where n and m are the lengths of L and R respectively,
    and O(Max(m, n)) space complexity }
    class function LevDistance(const L, R: array of T): SizeInt; static;
  { returns the Levenshtein distance between L and R; a Pascal translation of
    github.com/vaadin/gwt/dev/util/editdistance/ModifiedBerghelRoachEditDistance.java -
    a modified version of algorithm described by Berghel and Roach with O(min(|L|,|R|))*d)
    worst-case time complexity, where d is the edit distance computed  }
    class function LevDistanceMBR(const L, R: array of T): SizeInt; static;
  { the same as above; the aLimit parameter indicates the maximum expected distance,
    if this value is exceeded when calculating the distance, then the function exits
    immediately and returns -1; if aLimit < 0 it will be computed dynamically }
    class function LevDistanceMBR(const L, R: array of T; aLimit: SizeInt): SizeInt; static;
  { returns the Levenshtein distance between L and R; uses the Myers bit-vector algorithm
    with O(dn/w) time complexity, where n is Max(Length(L), Length(R)),
    d is edit distance computed, and w is the size of a computer word }
    class function LevDistanceMyers(const L, R: array of T): SizeInt; static;
  { the same as above; the aLimit parameter indicates the maximum expected distance,
    if this value is exceeded when calculating the distance, then the function exits
    immediately and returns -1; if aLimit < 0 it will be computed dynamically }
    class function LevDistanceMyers(const L, R: array of T; aLimit: SizeInt): SizeInt; static;
  { the LCS edit distance allows only two operations: insertion and deletion; uses slightly
    modified Myers algorithm with O((|L|+|R|)D) time complexity and linear space complexity
    from Eugene W. Myers(1986), "An O(ND) Difference Algorithm and Its Variations" }
    class function LcsDistanceMyers(const L, R: array of T): SizeInt; static;
  { the same as above; the aLimit parameter indicates the maximum expected distance,
    if this value is exceeded when calculating the distance, then the function exits
    immediately and returns -1; aLimit < 0 means it is unknown }
    class function LcsDistanceMyers(const L, R: array of T; aLimit: SizeInt): SizeInt; static;
  { returns the Damerau-Levenshtein distance(restricted) between L and R using
    modified Berghel-Roach algorithm }
    class function DumDistanceMBR(const L, R: array of T): SizeInt; static;
  { the same as above; the aLimit parameter indicates the maximum expected distance,
    if this value is exceeded when calculating the distance, then the function exits
    immediately and returns -1; if aLimit < 0 it will be computed dynamically }
    class function DumDistanceMBR(const L, R: array of T; aLimit: SizeInt): SizeInt; static;
  { returns the longest common subsequence(LCS) of sequences L and R, reducing the task to LIS,
    with O(SLogN) time complexity, where S is the number of the matching pairs in L and R;
    inspired by Dan Gusfield "Algorithms on Strings, Trees and Sequences", section 12.5; }
    class function LcsGus(const L, R: array of T): TArray; static;
  { recursive, returns the longest common subsequence(LCS) of sequences L and R;
    uses Kumar-Rangan algorithm for LCS with space complexity O(n) and time complexity O(n(m-p)), where
    m = Min(length(L), length(R)), n = Max(length(L), length(R)), and p is the length of the LCS computed:
    S. Kiran Kumar and C. Pandu Rangan(1987) "A Linear Space Algorithm for the LCS Problem" }
    class function LcsKR(const L, R: array of T): TArray; static;
  { recursive, returns the longest common subsequence(LCS) of sequences L and R;
    uses Myers algorithm for LCS with space complexity O(m+n) and time complexity O((m+n)*d), where
    n and m are the lengths of L and R respectively, and d is the size of the minimum edit script
    for L and R (d = m + n - 2*p, where p is the lenght of the LCS) }
    class function LcsMyers(const L, R: array of T): TArray; static;
  { returns similarity ratio using specified distance algorithm;
    aLimit specifies the lower bound of the required similarity(0<aLimit<=1.0),
    if the obtained value is less than the specified value, zero will be returned;
    aLimit <= 0 does not impose any restrictions on the obtained values }
    class function SimRatio(const L, R: array of T; aLimit: Double = Double(0);
                            Algo: TSeqDistanceAlgo = sdaDefault): Double; static;
  type
    TLcsAlgo = (laGus, laKr, laMyers);

    class function Diff(const aSource, aTarget: array of T; aLcsAlgo: TLcsAlgo = laMyers): TDiff; static;
  end;

  TUcs4Seq  = array of Ucs4Char;
  TUcs4Less = function(const L, R: array of Ucs4Char): Boolean;

{ the responsibility for the correctness and normalization of the strings lies with the user }
  function IsSubSequenceUtf16(const aStr, aSub: unicodestring): Boolean;
  function Utf16ToUcs4Seq(const s: unicodestring): TUcs4Seq;
  function Ucs4SeqToUtf16(const s: TUcs4Seq): unicodestring;
{ returns the Levenshtein distance using simple DP algorithm }
  function LevDistanceUtf16(const L, R: unicodestring): SizeInt; inline;
{ returns the Levenshtein distance using Berghel-Roach algorithm }
  function LevDistanceMbrUtf16(const L, R: unicodestring): SizeInt; inline;
  function LevDistanceMbrUtf16(const L, R: unicodestring; aLimit: SizeInt): SizeInt; inline;
{ returns the Levenshtein distance using Myers bit-vector algorithm }
  function LevDistanceMyersUtf16(const L, R: unicodestring): SizeInt; inline;
  function LevDistanceMyersUtf16(const L, R: unicodestring; aLimit: SizeInt): SizeInt; inline;
{ returns the LCS distance using Myers O(ND) algorithm }
  function LcsDistanceMyersUtf16(const L, R: unicodestring): SizeInt; inline;
  function LcsDistanceMyersUtf16(const L, R: unicodestring; aLimit: SizeInt): SizeInt; inline;
{ returns the restricted Damerau-Levenshtein distance using Berghel-Roach algorithm }
  function DumDistanceMbrUtf16(const L, R: unicodestring): SizeInt; inline;
  function DumDistanceMbrUtf16(const L, R: unicodestring; aLimit: SizeInt): SizeInt; inline;
  function LcsGusUtf16(const L, R: unicodestring): unicodestring; inline;
  function LcsKRUtf16(const L, R: unicodestring): unicodestring; inline;
  function LcsMyersUtf16(const L, R: unicodestring): unicodestring; inline;
{ returns similarity ratio using specified distance algorithm; aLimit specifies
  the lower bound of the required similarity(0<=aLimit<=1.0), if the obtained value
  is less than the specified value, zero will be returned }
  function SimRatioUtf16(const L, R: unicodestring; aLimit: Double = Double(0); Algo: TSeqDistanceAlgo = sdaDefault): Double;
{ similarity ratio using the Levenshtein distance with some preprocessing of the input text;
  elements from aStopChars must be code points otherwise they will be ignored  }
  function SimRatioExUtf16(
    const L, R: unicodestring;
    const aStopChars: array of unicodestring;
    aMode: TSimMode = smSimple;
    const aOptions: TSimOptions = [];
    aLimit: Double = Double(0);
    Algo: TSeqDistanceAlgo = sdaDefault;
    aLess: TUcs4Less = nil
  ): Double;
{ Pascal translation of https://github.com/cyb70289/utf8/blob/master/lookup.c }
  function Utf8ValidateDfa(const s: rawbytestring): Boolean;
{ branchy range validator based on Table 3-7 of Unicode Standard }
  function Utf8Validate(const s: rawbytestring): Boolean;
{ these functions expect UTF-8 encoded strings as parameters;
  the responsibility for the correctness and normalization of the strings lies with the user }
  function Utf8StrLen(const s: string): SizeInt; inline;
  function Utf8CodePointLength(p: PAnsiChar; aByteCount: SizeInt): SizeInt;
  function Utf8ToLower(const s: string): string;
  function Utf8ToUpper(const s: string): string;
  function IsSubSequenceUtf8(const aStr, aSub: string): Boolean;
  function Utf8ToUcs4Seq(const s: string): TUcs4Seq; inline;
  function Ucs4SeqToUtf8(const s: TUcs4Seq): string;
{ returns the Levenshtein distance using simple DP algorithm }
  function LevDistanceUtf8(const L, R: string): SizeInt; inline;
{ returns the Levenshtein distance using Berghel-Roach algorithm }
  function LevDistanceMbrUtf8(const L, R: string): SizeInt; inline;
  function LevDistanceMbrUtf8(const L, R: string; aLimit: SizeInt): SizeInt; inline;
{ returns the Levenshtein distance using Myers bit-vector algorithm }
  function LevDistanceMyersUtf8(const L, R: string): SizeInt; inline;
  function LevDistanceMyersUtf8(const L, R: string; aLimit: SizeInt): SizeInt; inline;
{ returns the LCS distance using Myers O(ND) algorithm }
  function LcsDistanceMyersUtf8(const L, R: string): SizeInt; inline;
  function LcsDistanceMyersUtf8(const L, R: string; aLimit: SizeInt): SizeInt; inline;
{ returns the restricted Damerau-Levenshtein distance using Berghel-Roach algorithm }
  function DumDistanceMbrUtf8(const L, R: string): SizeInt; inline;
  function DumDistanceMbrUtf8(const L, R: string; aLimit: SizeInt): SizeInt; inline;
  function LcsGusUtf8(const L, R: string): string; inline;
  function LcsKrUtf8(const L, R: string): string; inline;
  function LcsMyersUtf8(const L, R: string): string; inline;
{ returns similarity ratio using specified distance algorithm; aLimit specifies
  the lower bound of the required similarity(0<=aLimit<=1.0), if the obtained value
  is less than the specified value, zero will be returned }
  function SimRatioUtf8(const L, R: string; aLimit: Double = Double(0); Algo: TSeqDistanceAlgo = sdaDefault): Double;
  function SimRatioExUtf8(
    const L, R: string;
    const aStopChars: array of string;
    aMode: TSimMode = smSimple;
    const aOptions: TSimOptions = [];
    aLimit: Double = Double(0);
    Algo: TSeqDistanceAlgo = sdaDefault;
    aLess: TUcs4Less = nil
  ): Double;
{ returns an array, each element of which contains the similarity ratio between
  aPattern and the corresponding element in the aValues array }
  function SimRatioListUtf8(
    const aPattern: string;
    const aValues: array of string;
    const aStopChars: array of string;
    aMode: TSimMode = smSimple;
    const aOptions: TSimOptions = [];
    aLimit: Double = Double(0);
    Algo: TSeqDistanceAlgo = sdaDefault;
    aLess: TUcs4Less = nil
  ): specialize TGArray<Double>;
type
  TStringRatio = record
    Value: string;
    Ratio: Double;
  end;
{ returns an array of pairs sorted by descending similarity ratio and containing only those
  strings whose similarity ratio is not less than the specified boundary aLimit }
  function SelectSimilarUtf8(
    const aPattern: string;
    const aValues: array of string;
    const aStopChars: array of string;
    aLimit: Double;
    aMode: TSimMode = smSimple;
    const aOptions: TSimOptions = [];
    Algo: TSeqDistanceAlgo = sdaDefault;
    aLess: TUcs4Less = nil
  ): specialize TGArray<TStringRatio>;

type
  { TFuzzySearchEdp: approximate string matching with K differences;
    expects UTF-8 encoded strings as parameters;
    uses old and simple Ukkonen EDP algorithm with linear space complexity and O(KN)
    expected time complexity }
  TFuzzySearchEdp = record
  private
  type
    TEnumerator = record
    private
      FPattern: TUcs4Seq;
      FD: array of SizeInt;
      FText: string;
      FK,
      FTop,
      FPointIndex,
      FTextIndex: SizeInt;
      function GetCurrent: SizeInt; inline;
    public
      function MoveNext: Boolean;
      property Current: SizeInt read GetCurrent;
    end;

    TMatches = record
    private
      FSearch: ^TFuzzySearchEdp;
      FK: SizeInt;
      FText: string;
    public
      function GetEnumerator: TEnumerator;
    end;
  var
    FPattern: TUcs4Seq;
    FD: array of SizeInt;
    function GetInitialized: Boolean; inline;
    function GetLength: SizeInt;
  public
    constructor Create(const aPattern: string);
    procedure Init(const aPattern: string);
  { returns an enumerator of indexes of code points(1-based) in aText such that
    there is an index I such that LevenshteinDistance(aPattern, aText[I..Current]) <= K;
    K MUST be less then |aPattern| }
    function Matches(const aText: string; K: SizeInt): TMatches;
    property Initialized: Boolean read GetInitialized;
    property Length: SizeInt read GetLength;
  end;

  TMatch     = LgUtils.TSeqMatch;
  TOnMatch   = specialize TGOnTest<TMatch>;
  TNestMatch = specialize TGNestTest<TMatch>;

  { TFuzzySearchBitap: approximate string matching with K mismatches;
    expects UTF-8 encoded strings as parameters;
    uses Bitap algorithm with O(KN) time complexity }
  TFuzzySearchBitap = record
  public
  const
    MAX_PATTERN_CP = Pred(BitSizeOf(QWord)); // maximum number of code points in the pattern

  private
  type
    TMapType     = specialize TGLiteChainHashMap<Ucs4Char, QWord, TUcs4Hasher>;
    TCharMap     = TMapType.TMap;
    THelper      = specialize TGArrayHelpUtil<QWord>;
    PSearchBitap = ^TFuzzySearchBitap;

    TEnumerator = record
    private
      FText: string;
      FTable: array of QWord;
      FQueue: array of SizeInt;
      FSearch: PSearchBitap;
      FTextIndex: SizeInt;
      FqHead: Integer;
      FMatch: TMatch;
      function  GetCurrent: TMatch; inline;
      procedure Init(pSearch: PSearchBitap; const aText: string; aOfs: SizeInt; aK: Integer);
    public
      function  MoveNext: Boolean;
      property  Current: TMatch read GetCurrent;
    end;

  public
  type
    TMatches = record
    private
      FSearch: PSearchBitap;
      FText: string;
      FOfs: SizeInt;
      FK: Integer;
    public
      function GetEnumerator: TEnumerator;
    end;

  private
  type
    TNestFound = function(const m: TMatch): Boolean is nested;
  var
    FCharMap: TCharMap;
    FLength: Integer;
    FIgnoreCase: Boolean;
    function  GetInitialized: Boolean; inline;
    function  GetLength: Integer; inline;
    function  GetCaseInsensitive: Boolean; inline;
    procedure DoSearch(const aText: string; K: Integer; aOffset: SizeInt; aFound: TNestFound);
  public
  { aPattern must be a valid non-empty UTF-8 string containing no more than MAX_PATTERN_CP code points }
    constructor Create(const aPattern: string; aIgnoreCase: Boolean = False);
    procedure Init(const aPattern: string; aIgnoreCase: Boolean = False);
  { returns the start position(1-based) and length(in bytes) of the next approximate
    occurrence of the pattern in the string aText that has a Hamming distance of at most K,
    starting from the index aOffset; returns TMatch(0,0) if instance is not initialized or
    no occurrence is found or 0 > K >= MAX_PATTERN_CP }
    function NextMatch(const aText: string; K: Integer; aOffset: SizeInt = 1): TMatch;
  { returns an array of all approximate occurrences of the pattern in the string aText
    that have a Hamming distance of at most K, starting from the index aOffset;
    if aLimit is greater than zero, it returns no more than aLimit occurrences,
    otherwise it returns all found occurrences; returns an empty array if no occurrence
    is found or the instance is not initialized or 0 > K >= MAX_PATTERN_CP }
    function FindMatches(const aText: string; K: Integer; aOffset: SizeInt = 1; aLimit: SizeInt = 0): specialize TGArray<TMatch>;
  { enumerates the approximate occurrences of the pattern in the string aText that have
    a Hamming distance of at most K, starting from the index aOffset }
    function Matches(const aText: string; K: Integer; aOffset: SizeInt = 1): TMatches;
    property Initialized: Boolean read GetInitialized;
  { pattern length (in code points) }
    property Length: Integer read GetLength;
    property CaseInsensitive: Boolean read GetCaseInsensitive;
  end;

{$PUSH}{$INTERFACES COM}
  { IACSearchUtf8: Aho-Corasick automation for the exact set matching problem;
    expects UTF-8 encoded strings as parameters }
  IACSearchUtf8 = interface
  ['{AA85E5D4-5FBA-4D3C-BD16-858FC26B619C}']
    function  GetNodeCount: SizeInt;
    function  GetPatternCount: SizeInt;
    function  GetCaseInsensitive: Boolean;
    function  ContainsPattern(const s: string): Boolean;
  { returns an array of all matches(1-based, in bytes) found in the string s,
    starting at position aOffset within aCount bytes;
    any value of aCount < 1 implies a search to the end of the text }
    function  FindMatches(const s: string; aOffset: SizeInt = 1; aCount: SizeInt = 0): specialize TGArray<TMatch>;
  { searches in the string s starting at position aOffset within aCount bytes, passing
    the found matches(1-based, in bytes) to the callback aOnMatch();
    immediately exits the procedure if aOnMatch() returns False;
    any value of aCount < 1 implies a search to the end of the text }
    procedure Search(const s: string; aOnMatch: TOnMatch; aOffset: SizeInt = 1; aCount: SizeInt = 0);
    procedure Search(const s: string; aOnMatch: TNestMatch; aOffset: SizeInt = 1; aCount: SizeInt = 0);
  { returns True if at least one match is found in the string s, starting at position aOffset
    within aCount bytes; any value of aCount < 1 implies a search to the end of the text }
    function  ContainsMatch(const s: string; aOffset: SizeInt = 1; aCount: SizeInt = 0): Boolean;
    property  NodeCount: SizeInt read GetNodeCount;
    property  PatternCount: SizeInt read GetPatternCount;
    property  CaseInsensitive: Boolean read GetCaseInsensitive;
  end;
{$POP}
{ aPatterns specifies a set of patterns;
  aIgnoreCase set to True specifies a case-insensitive search;
  by default the function tries to build a DFA first(DFA has noticeably better performance),
  which may not be possible if the patterns contain characters outside the BMP or the patterns
  alphabet is too large, if aForceNFA is set to True the function will immediately build NFA }
  function CreateACSearchFsm(const aPatterns: array of string; aIgnoreCase: Boolean = False;
                             aForceNFA: Boolean = False): IACSearchUtf8;

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}
uses
  Character, UnicodeData;

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

{ TUcs4Hasher }
class function TUcs4Hasher.HashCode(aValue: Ucs4Char): SizeInt;
begin
  Result := JdkHash(DWord(aValue));
end;

class function TUcs4Hasher.Equal(L, R: Ucs4Char): Boolean;
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

class function TGSeqUtil.SkipPrefix(var pL, pR: PItem; var aLenL, aLenR: SizeInt): SizeInt;
begin
  //implied aLenL <= aLenR
  Result := 0;

  while (Result < aLenL) and TEqRel.Equal(pL[Result], pR[Result]) do
    Inc(Result);

  pL += Result;
  pR += Result;
  aLenL -= Result;
  aLenR -= Result;
end;

class function TGSeqUtil.SkipSuffix(pL, pR: PItem; var aLenL, aLenR: SizeInt): SizeInt;
begin
  //implied aLenL <= aLenR
  Result := 0;
  while (aLenL > 0) and TEqRel.Equal(pL[Pred(aLenL)], pR[Pred(aLenR)]) do
    begin
      Dec(aLenL);
      Dec(aLenR);
      Inc(Result);
    end;
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

class function TGSeqUtil.IsTrivialDist(const L, R: array of T; aLimit: SizeInt; out aDist: SizeInt): Boolean;
begin
  if aLimit = 0 then
    if Same(L, R) then
      begin
        aDist := 0;
        exit(True);
      end
    else
      exit(False);
  if System.Length(L) = 0 then
    begin
      if SizeUInt(System.Length(R)) <= SizeUInt(aLimit) then
        aDist := System.Length(R)
      else
        aDist := NULL_INDEX;
      exit(True);
    end
  else
    if System.Length(R) = 0 then
      begin
        if SizeUInt(System.Length(L)) <= SizeUInt(aLimit) then
          aDist := System.Length(L)
        else
          aDist := NULL_INDEX;
        exit(True);
      end;
  Result := False;
end;

class function TGSeqUtil.LcsGusImpl(L, R: PItem; aLenL, aLenR: SizeInt): TArray;
var
  MatchList: TMap;
  NodeList: TNodeList;
  Tmp: TSizeIntArray;
  LocLis: TSizeIntArray;
  I, J, PrefixLen, SuffixLen, NodeIdx: SizeInt;
  p: ^TEntry;
const
  INIT_SIZE = 256;
begin
  //here aLenL <= aLenR
  Result := nil;

  if L = R then
    exit(THelper.CreateCopy(L[0..Pred(aLenL)]));

  SuffixLen := SkipSuffix(L, R, aLenL, aLenR);
  PrefixLen := SkipPrefix(L, R, aLenL, aLenR);

  if aLenL = 0 then
    begin
      System.SetLength(Result, PrefixLen + SuffixLen);
      THelper.CopyItems(L - PrefixLen, Pointer(Result), PrefixLen);
      THelper.CopyItems(L + aLenL, @Result[PrefixLen], SuffixLen);
      exit;
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
      LocLis := GetLis(Tmp, aLenL);
      Tmp := nil;
      J := System.Length(Result);
      System.SetLength(Result, J + System.Length(LocLis));
      for I := 0 to System.High(LocLis) do
        Result[I+J] := R[LocLis[I]];

      System.SetLength(Result, PrefixLen + System.Length(LocLis) + SuffixLen);
      for I := 0 to System.High(LocLis) do
        Result[I+PrefixLen] := R[LocLis[I]];
      THelper.CopyItems(L - PrefixLen, Pointer(Result), PrefixLen);
      THelper.CopyItems(L + aLenL, @Result[PrefixLen+System.Length(LocLis)], SuffixLen);
    end
  else
    begin
      System.SetLength(Result, PrefixLen + SuffixLen);
      if Result = nil then exit;
      THelper.CopyItems(L - PrefixLen, Pointer(Result), PrefixLen);
      THelper.CopyItems(L + aLenL, @Result[PrefixLen], SuffixLen);
    end;
end;

{$PUSH}{$WARN 5089 OFF}
class function TGSeqUtil.LcsKRImpl(pL, pR: PItem; aLenL, aLenR: SizeInt): TArray;
var
  LocLcs: TVector;
  R1, R2, LL, LL1, LL2: PSizeInt;
  R, S: SizeInt;
  procedure FillOne(LFirst, RFirst, RLast: SizeInt; DirectOrd: Boolean);
  var
    I, J, LoR, PosR, Tmp: SizeInt;
  begin
    J := 1;
    I := S;
    if DirectOrd then begin
      R2[0] := RLast - RFirst + 2;
      while I > 0 do begin
        if J > R then
          LoR := 0
        else
          LoR := R1[J];
        PosR := R2[J - 1] - 1;
        while (PosR > LoR) and not TEqRel.Equal(pL[LFirst+(I-1)], pR[RFirst+(PosR-1)]) do
          Dec(PosR);
        Tmp := Math.Max(LoR, PosR);
        if Tmp = 0 then break;
        R2[J] := Tmp;
        Dec(I);
        Inc(J);
      end;
    end else begin
      R2[0] := RFirst - RLast + 2;
      while I > 0 do begin
        if J > R then
          LoR := 0
        else
          LoR := R1[J];
        PosR := R2[J - 1] - 1;
        while (PosR > LoR) and not TEqRel.Equal(pL[LFirst-(I-1)], pR[RFirst-(PosR-1)]) do
          Dec(PosR);
        Tmp := Math.Max(LoR, PosR);
        if Tmp = 0 then break;
        R2[J] := Tmp;
        Dec(I);
        Inc(J);
      end;
    end;
    R := Pred(J);
  end;
  procedure Swap(var L, R: Pointer); inline;
  var
    Tmp: Pointer;
  begin
    Tmp := L;
    L := R;
    R := Tmp;
  end;
  procedure CalMid(LFirst, LLast, RFirst, RLast, Waste: SizeInt; L: PSizeInt; DirectOrd: Boolean);
  var
    P: SizeInt;
  begin
    if DirectOrd then
      S := Succ(LLast - LFirst)
    else
      S := Succ(LFirst - LLast);
    P := S - Waste;
    R := 0;
    while S >= P do begin
      FillOne(LFirst, RFirst, RLast, DirectOrd);
      Swap(R2, R1);
      Dec(S);
    end;
    System.Move(R1^, L^, Succ(R) * SizeOf(SizeInt));
  end;
  procedure SolveBaseCase(LFirst, LLast, RFirst, RLast, LcsLen: SizeInt);
  var
    I: SizeInt;
  begin
    CalMid(LFirst, LLast, RFirst, RLast, Succ(LLast - LFirst - LcsLen), LL, True);
    I := 0;
    while (I < LcsLen) and TEqRel.Equal(pL[LFirst+I], pR[RFirst+LL[LcsLen-I]-1]) do begin
      LocLcs.Add(pL[LFirst+I]);
      Inc(I);
    end;
    Inc(I);
    while I <= LLast - LFirst do begin
      LocLcs.Add(pL[LFirst+I]);
      Inc(I);
    end;
  end;
  procedure FindPerfectCut(LFirst, LLast, RFirst, RLast, LcsLen: SizeInt; out U, V: SizeInt);
  var
    I, LocR1, LocR2, K, W: SizeInt;
  begin
    W := Succ(LLast - LFirst - LcsLen) div 2;
    CalMid(LLast, LFirst, RLast, RFirst, W, LL1, False);
    LocR1 := R;
    for I := 0 to LocR1 do
      LL1[I] := RLast - RFirst - LL1[I] + 2;
    CalMid(LFirst, LLast, RFirst, RLast, W, LL2, True);
    LocR2 := R;
    K := Math.Max(LocR1, LocR2);
    while K > 0 do begin
      if (K <= LocR1) and (LcsLen - K <= LocR2) and (LL1[K] < LL2[LcsLen - K]) then break;
      Dec(K);
    end;
    U := K + W;
    V := LL1[K];
  end;
  procedure Lcs(LFirst, LLast, RFirst, RLast, LcsLen: SizeInt);
  var
    U, V, W: SizeInt;
  begin
    if (LLast < LFirst) or (RLast < RFirst) or (LcsLen < 1) then exit;
    if Succ(LLast - LFirst - LcsLen) < 2 then
      SolveBaseCase(LFirst, LLast, RFirst, RLast, LcsLen)
    else begin
      FindPerfectCut(LFirst, LLast, RFirst, RLast, LcsLen, U, V);
      W := Succ(LLast - LFirst - LcsLen) div 2;
      Lcs(LFirst, Pred(LFirst + U), RFirst, Pred(RFirst + V), U - W);
      Lcs(LFirst + U, LLast, RFirst + V, RLast, LcsLen + W - U);
    end;
  end;
  function GetLcsLen: SizeInt;
  begin
    R := 0;
    S := Succ(aLenL);
    while S > R do begin
      Dec(S);
      FillOne(0, 0, Pred(aLenR), True);
      Swap(R2, R1);
    end;
    Result := S;
  end;
var
  StBuf: array[0..Pred(MAX_STATIC)] of SizeInt;
  Buf: array of SizeInt;
  PrefixLen, SuffixLen: SizeInt;
begin
  //here aLenL <= aLenR
  Result := nil;

  if pL = pR then
    exit(THelper.CreateCopy(pL[0..Pred(aLenL)]));

  SuffixLen := SkipSuffix(pL, pR, aLenL, aLenR);
  PrefixLen := SkipPrefix(pL, pR, aLenL, aLenR);

  if aLenL = 0 then
    begin
      System.SetLength(Result, PrefixLen + SuffixLen);
      THelper.CopyItems(pL - PrefixLen, Pointer(Result), PrefixLen);
      THelper.CopyItems(pL + aLenL, @Result[PrefixLen], SuffixLen);
      exit;
    end;

  if MAX_STATIC >= Succ(aLenR)*5 then
    begin
      R1 := @StBuf[0];
      R2 := @StBuf[Succ(aLenR)];
      LL := @StBuf[Succ(aLenR)*2];
      LL1 := @StBuf[Succ(aLenR)*3];
      LL2 := @StBuf[Succ(aLenR)*4];
    end
  else
    begin
      System.SetLength(Buf, Succ(aLenR)*5);
      R1 := @Buf[0];
      R2 := @Buf[Succ(aLenR)];
      LL := @Buf[Succ(aLenR)*2];
      LL1 := @Buf[Succ(aLenR)*3];
      LL2 := @Buf[Succ(aLenR)*4];
    end;

  LocLcs.EnsureCapacity(aLenL);

  Lcs(0, Pred(aLenL), 0, Pred(aLenR), GetLcsLen());
  Buf := nil;

  System.SetLength(Result, PrefixLen + LocLcs.Count + SuffixLen);
  if Result = nil then exit;

  if LocLcs.NonEmpty then
    THelper.CopyItems(LocLcs.UncMutable[0], @Result[PrefixLen], LocLcs.Count);
  THelper.CopyItems(pL - PrefixLen, Pointer(Result), PrefixLen);
  THelper.CopyItems(pL + aLenL, @Result[PrefixLen + LocLcs.Count], SuffixLen);
end;
{$POP}

{ TGSeqUtil.TSnake }

procedure TGSeqUtil.TSnake.SetStartCell(aRow, aCol: SizeInt);
begin
  StartRow := aRow;
  StartCol := aCol;
end;

procedure TGSeqUtil.TSnake.SetEndCell(aRow, aCol: SizeInt);
begin
  EndRow := aRow;
  EndCol := aCol;
end;

{$PUSH}{$WARN 5089 OFF}{$WARN 5037 OFF}
class function TGSeqUtil.LcsMyersImpl(pL, pR: PItem; aLenL, aLenR: SizeInt): TArray;
var
  LocLcs: TVector;
  V0, V1: PSizeInt;
  function FindMiddleShake(LFirst, LLast, RFirst, RLast: SizeInt; out aSnake: TSnake): SizeInt;
  var
    LenL, LenR, Delta, Mid, D, K, Row, Col: SizeInt;
    ForV, RevV: PSizeInt;
    OddDelta: Boolean;
  begin
    LenL := Succ(LLast - LFirst);
    LenR := Succ(RLast - RFirst);
    Delta := LenL - LenR;
    OddDelta := Odd(Delta);
    Mid := (LenL + LenR) div 2 + Ord(OddDelta);
    ForV := @V0[Succ(Mid)];
    RevV := @V1[Succ(Mid)];
    ForV[1] := 0;
    RevV[1] := 0;
    for D := 0 to Mid do
      begin
        K := -D;
        while K <= D do
          begin
            if (K = -D) or ((K <> D) and (ForV[K - 1] < ForV[K + 1])) then
              Row := ForV[K + 1]
            else
              Row := ForV[K - 1] + 1;
            Col := Row - K;
            aSnake.SetStartCell(LFirst + Row, RFirst + Col);
            while (Row < LenL) and (Col < LenR) and TEqRel.Equal(pL[LFirst + Row], pR[RFirst + Col]) do
              begin
                Inc(Row);
                Inc(Col);
              end;
            ForV[K] := Row;
            if OddDelta and (K >= Delta - D + 1) and (K <= Delta + D - 1) and
               (Row + RevV[Delta - K] >= LenL) then
              begin
                aSnake.SetEndCell(LFirst + Row, RFirst + Col);
                exit(Pred(D * 2));
              end;
            K += 2;
          end;

        K := -D;
        while K <= D do
          begin
            if (K = -D) or ((K <> D) and (RevV[K - 1] < RevV[K + 1])) then
              Row := RevV[K + 1]
            else
              Row := RevV[K - 1] + 1;
            Col := Row - K;
            aSnake.SetEndCell(Succ(LLast - Row), Succ(RLast - Col));
            while (Row < LenL) and (Col < LenR) and TEqRel.Equal(pL[LLast-Row], pR[RLast-Col]) do
              begin
                Inc(Row);
                Inc(Col);
              end;
            RevV[K] := Row;
            if not OddDelta and (K <= D + Delta) and (K >= Delta - D) and
              (Row + ForV[Delta - K] >= LenL) then
              begin
                aSnake.SetStartCell(Succ(LLast - Row), Succ(RLast - Col));
                exit(D * 2);
              end;
            K += 2;
          end;
      end;
    Result := NULL_INDEX;
    raise Exception.Create('Internal error in ' + {$I %CURRENTROUTINE%});
  end;
  procedure Lcs(LFirst, LLast, RFirst, RLast: SizeInt);
  var
    Snake: TSnake;
    I: SizeInt;
  begin
    if (LLast < LFirst) or (RLast < RFirst) then exit;
    if FindMiddleShake(LFirst, LLast, RFirst, RLast, Snake) > 1 then
      begin
        Lcs(LFirst, Pred(Snake.StartRow), RFirst, Pred(Snake.StartCol));
        for I := Snake.StartRow to Pred(Snake.EndRow) do
          LocLcs.Add(pL[I]);
        Lcs(Snake.EndRow, LLast, Snake.EndCol, RLast);
      end
    else
      if LLast - LFirst < RLast - RFirst then
        for I := LFirst to LLast do
          LocLcs.Add(pL[I])
      else
        for I := RFirst to RLast do
          LocLcs.Add(pR[I]);
  end;
var
  StBuf: array[0..Pred(MAX_STATIC)] of SizeInt;
  Buf: array of SizeInt;
  PrefixLen, SuffixLen: SizeInt;
begin
  //here aLenL <= aLenR
  Result := nil;

  if pL = pR then
    exit(THelper.CreateCopy(pL[0..Pred(aLenL)]));

  SuffixLen := SkipSuffix(pL, pR, aLenL, aLenR);
  PrefixLen := SkipPrefix(pL, pR, aLenL, aLenR);

  if aLenL = 0 then
    begin
      System.SetLength(Result, PrefixLen + SuffixLen);
      THelper.CopyItems(pL - PrefixLen, Pointer(Result), PrefixLen);
      THelper.CopyItems(pL + aLenL, @Result[PrefixLen], SuffixLen);
      exit;
    end;

  if MAX_STATIC >= (aLenL+aLenR+2)*2 then
    begin
      V0 := @StBuf[0];
      V1 := @StBuf[(aLenL+aLenR+2)];
    end
  else
    begin
      System.SetLength(Buf, (aLenL+aLenR+2)*2);
      V0 := @Buf[0];
      V1 := @Buf[(aLenL+aLenR+2)];
    end;

  LocLcs.EnsureCapacity(aLenL);

  Lcs(0, Pred(aLenL), 0, Pred(aLenR));
  Buf := nil;

  System.SetLength(Result, PrefixLen + LocLcs.Count + SuffixLen);
  if Result = nil then exit;

  if LocLcs.NonEmpty then
    THelper.CopyItems(LocLcs.UncMutable[0], @Result[PrefixLen], LocLcs.Count);
  THelper.CopyItems(pL - PrefixLen, Pointer(Result), PrefixLen);
  THelper.CopyItems(pL + aLenL, @Result[PrefixLen + LocLcs.Count], SuffixLen);
end;
{$POP}

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

  SkipSuffix(pL, pR, aLenL, aLenR);
  SkipPrefix(pL, pR, aLenL, aLenR);

  if aLenL = 0 then
    exit(aLenR);

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
      for J := 1 to aLenR do
        begin
          if TEqRel.Equal(pR[J-1], v) then
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
  function FindRow(k, aDist, aLeft, aAbove, aRight: SizeInt): SizeInt; inline;
  var
    I, MaxRow: SizeInt;
  begin
    if aDist = 0 then I := 0
    else I := MaxOf3(aLeft, aAbove + 1, aRight + 1);
    MaxRow := Min(aLenL - k, aLenR);
    while (I < MaxRow) and TEqRel.Equal(pR[I], pL[I + k]) do
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
          FindRow(DMain + Diagonal, Dist - Diagonal, PrevR[Diagonal - 1], LastR[Diagonal], CurrRight);
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

class function TGSeqUtil.LevDistMbrDyn(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt;
var
  K: SizeInt;
begin
  //here aLenL <= aLenR
  if pL = pR then
    exit(aLenR - aLenL);

  SkipSuffix(pL, pR, aLenL, aLenR);
  SkipPrefix(pL, pR, aLenL, aLenR);

  if aLenL = 0 then
    exit(aLenR);

  K := 0;
  repeat
    if K <> 0 then
      K := Math.Min(K * 2, aLenR)
    else
      K := Math.Max(aLenR - aLenL, 2); // 2 ???
    Result := LevDistMbrImpl(pL, pR, aLenL, aLenR, K);
  until Result <> NULL_INDEX;
end;

class function TGSeqUtil.GetLevDistMbr(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt;
begin
  //here aLenL <= aLenR
  if aLimit < 0 then
    exit(LevDistMbrDyn(pL, pR, aLenL, aLenR));

  if aLenR - aLenL > aLimit then
    exit(NULL_INDEX);

  if pL = pR then
    exit(aLenR - aLenL);

  SkipSuffix(pL, pR, aLenL, aLenR);
  SkipPrefix(pL, pR, aLenL, aLenR);

  if aLenL = 0 then
    exit(aLenR);

  if aLimit = 0 then  //////////
    exit(NULL_INDEX); //////////

  Result := LevDistMbrImpl(pL, pR, aLenL, aLenR, aLimit);
end;

class function TGSeqUtil.LevDistMyersQ(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt;
var
  Map: TQMap;
  PmI, Hp, Hv, Vp, Vn, D0: QWord;
  I: SizeInt;
  p: ^TQEntry;
begin
  //here aLenL <= aLenR
  Map.EnsureCapacity(aLenL);
  for I := 0 to Pred(aLenL) do
    begin
      if not Map.FindOrAdd(pL[I], p) then
        p^.Key := pL[I];
      p^.Value := p^.Value or (QWord(1) shl I);
    end;

  Result := aLenL;
  Vn := 0;
  Vp := High(QWord);

  for I := 0 to Pred(aLenR) do
    begin
      p := Map.Find(pR[I]);
      if p <> nil then
        PmI := p^.Value
      else
        PmI := 0;
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

class function TGSeqUtil.LevDistMyersQ(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt;
var
  Map: TQMap;
  PmI, Hp, Hn, Vp, Vn, D0: QWord;
  I: SizeInt;
  p: ^TQEntry;
begin
  //here aLenL <= aLenR
  Map.EnsureCapacity(aLenL);
  for I := 0 to Pred(aLenL) do
    begin
      if not Map.FindOrAdd(pL[I], p) then
        p^.Key := pL[I];
      p^.Value := p^.Value or (QWord(1) shl I);
    end;

  Result := aLenL;
  aLimit += aLenR - aLenL;
  Vn := 0;
  Vp := High(QWord);

  for I := 0 to Pred(aLenR) do
    begin
      p := Map.Find(pR[I]);
      if p <> nil then
        PmI := p^.Value
      else
        PmI := 0;
      D0 := (((PmI and Vp) + Vp) xor Vp) or PmI or Vn;
      Hp := Vn or not(D0 or Vp);
      Hn := D0 and Vp;
      Vp := Hn shl 1 or not(D0 or Hp shl 1 or 1);
      Vn := D0 and (Hp shl 1 or 1);
      if Hn and (QWord(1) shl Pred(aLenL)) <> 0 then
        Dec(Result)
      else
        begin
          if Hp and (QWord(1) shl Pred(aLenL)) <> 0 then
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

class function TGSeqUtil.LevDistMyersDQ(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt;
var
  Map: TDqMap;
  Eq0, Eq1, Ph, Mh, Pv0, Mv0, Pv1, Mv1, Xv, Xh, Hin: QWord;
  I: SizeInt;
  p: ^TDqEntry;
begin
  //here aLenL <= aLenR
  Map.EnsureCapacity(aLenL);
  for I := 0 to Pred(BLOCK_SIZE) do
    begin
      if not Map.FindOrAdd(pL[I], p) then
        p^.Key := pL[I];
      p^.Value[0] := p^.Value[0] or (QWord(1) shl I);
    end;
  for I := BLOCK_SIZE to Pred(aLenL) do
    begin
      if not Map.FindOrAdd(pL[I], p) then
        p^.Key := pL[I];
      p^.Value[1] := p^.Value[1] or (QWord(1) shl (I - BLOCK_SIZE));
    end;

  Result := aLenL;
  Pv0 := High(QWord);
  Pv1 := High(QWord);
  Mv0 := 0;
  Mv1 := 0;

  for I := 0 to Pred(aLenR) do
    begin
      p := Map.Find(pR[I]);
      if p <> nil then
        begin
          Eq0 := p^.Value[0];
          Eq1 := p^.Value[1];
        end
      else
        begin
          Eq0 := 0;
          Eq1 := 0;
        end;
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

class function TGSeqUtil.LevDistMyersDQ(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt;
var
  Map: TDqMap;
  Eq0, Eq1, Ph, Mh, Pv0, Mv0, Pv1, Mv1, Xv, Xh, Hin: QWord;
  I: SizeInt;
  p: ^TDqEntry;
begin
  //here aLenL <= aLenR
  Map.EnsureCapacity(aLenL);
  for I := 0 to Pred(BLOCK_SIZE) do
    begin
      if not Map.FindOrAdd(pL[I], p) then
        p^.Key := pL[I];
      p^.Value[0] := p^.Value[0] or (QWord(1) shl I);
    end;
  for I := BLOCK_SIZE to Pred(aLenL) do
    begin
      if not Map.FindOrAdd(pL[I], p) then
        p^.Key := pL[I];
      p^.Value[1] := p^.Value[1] or (QWord(1) shl (I - BLOCK_SIZE));
    end;

  Result := aLenL;
  aLimit += aLenR - aLenL;
  Pv0 := High(QWord);
  Pv1 := High(QWord);
  Mv0 := 0;
  Mv1 := 0;

  for I := 0 to Pred(aLenR) do
    begin
      p := Map.Find(pR[I]);
      if p <> nil then
        begin
          Eq0 := p^.Value[0];
          Eq1 := p^.Value[1];
        end
      else
        begin
          Eq0 := 0;
          Eq1 := 0;
        end;
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

class procedure TGSeqUtil.CreatePeq(aSeq: PItem; aSeqLen: SizeInt; out aPeq: TPeq);
var
  I, J, BCount, LastRow: SizeInt;
  Pad: QWord;
  p: ^TPQEntry;
begin
  with aPeq.Map do
    begin
      EnsureCapacity(Math.Min(256, aSeqLen));
      for I := 0 to Pred(aSeqLen) do
        if not FindOrAdd(aSeq[I], p) then
          p^.Key := aSeq[I];
    end;

  LastRow := aSeqLen and BSIZE_MASK;
  BCount := aSeqLen shr BSIZE_LOG + Ord(LastRow <> 0);
  aPeq.BlockCount := BCount;

  System.SetLength(aPeq.Buffer, BCount * aPeq.Map.Count);
  if LastRow <> 0 then
    Pad := System.High(QWord) shl LastRow
  else
    Pad := 0;

  J := 0;
  with aPeq do
    begin
      for p in Map do
        begin
          p^.Value := @Buffer[J];
          p^.Value[Pred(BCount)] := Pad;
          J += BCount;
        end;

      for I := 0 to Pred(aSeqLen) do
        begin
          p := Map.Find(aSeq[I]);
          p^.Value[I shr BSIZE_LOG] := p^.Value[I shr BSIZE_LOG] or QWord(1) shl (I and BSIZE_MASK);
        end;
    end;
end;

class function TGSeqUtil.LevDistMyersCutoff(const aPeq: TPeq; pR: PItem; aLenL, aLenR, K: SizeInt): SizeInt;
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
  Blocks: array of TBlock = nil;
  Equ, Xv, Xh, Pv, Mv, Ph, Mh, HIn, HOut: QWord;
  I, J, First, Last: SizeInt;
  p: ^TPQEntry;
begin
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
          p := aPeq.Map.Find(pR[I]);
          if p <> nil then
            Equ := p^.Value[J]
          else
            Equ := 0;
          Pv := Blocks[J].P;
          Mv := Blocks[J].M;
          Xv := Mv or Equ;
          Equ := Equ or HIn shr BSIZE_MASK;
          Xh := ((Pv and Equ + Pv) xor Pv) or Equ;
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
          p := aPeq.Map.Find(pR[I]);
          if p <> nil then
            Equ := p^.Value[Last]
          else
            Equ := 0;
          Pv := System.High(QWord);
          Mv := 0;
          Xv := Mv or Equ;
          Equ := Equ or HIn shr BSIZE_MASK;
          Xh := ((Pv and Equ + Pv) xor Pv) or Equ;
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

class function TGSeqUtil.LevDistMyersDyn(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt;
var
  Peq: TPeq;
  Limit: SizeInt;
begin
  //here aLenL <= aLenR
  CreatePeq(pL, aLenL, Peq);
  Limit := Math.Max(BLOCK_SIZE, aLenR - aLenL);
  repeat
    Result := LevDistMyersCutoff(Peq, pR, aLenL, aLenR, Limit);
    Limit += Limit;
  until Result <> NULL_INDEX;
end;

class function TGSeqUtil.LevDistMyers(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt;
var
  Peq: TPeq;
begin
  //here aLenL <= aLenR
  CreatePeq(pL, aLenL, Peq);
  Result := LevDistMyersCutoff(Peq, pR, aLenL, aLenR, aLimit);
end;

class function TGSeqUtil.GetLevDistMyers(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt;
begin
  //here aLenL <= aLenR
  if pL = pR then
    exit(aLenR - aLenL);

  SkipSuffix(pL, pR, aLenL, aLenR);
  SkipPrefix(pL, pR, aLenL, aLenR);

  if aLenL = 0 then
    exit(aLenR);

  case aLenL of
    1..BitSizeOf(QWord):
      Result := LevDistMyersQ(pL, pR, aLenL, aLenR);
    BitSizeOf(QWord)+1..BitSizeOf(QWord)*2:
      Result := LevDistMyersDQ(pL, pR, aLenL, aLenR);
  else
    Result := LevDistMyersDyn(pL, pR, aLenL, aLenR);
  end;
end;

class function TGSeqUtil.GetLevDistMyers(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt;
begin
  //here aLenL <= aLenR
  if aLimit < 0 then
    exit(GetLevDistMyers(pL, pR, aLenL, aLenR));

  if aLenR - aLenL > aLimit then
    exit(NULL_INDEX);

  if pL = pR then
    exit(aLenR - aLenL);

  SkipSuffix(pL, pR, aLenL, aLenR);
  SkipPrefix(pL, pR, aLenL, aLenR);

  if aLenL = 0 then
    exit(aLenR);

  if aLimit = 0 then
    exit(NULL_INDEX);

  if aLimit > aLenR then
    aLimit := aLenR;

  case aLenL of
    1..BitSizeOf(QWord):
      Result := LevDistMyersQ(pL, pR, aLenL, aLenR, aLimit);
    BitSizeOf(QWord)+1..BitSizeOf(QWord)*2:
      Result := LevDistMyersDQ(pL, pR, aLenL, aLenR, aLimit);
  else
    Result := LevDistMyers(pL, pR, aLenL, aLenR, aLimit);
  end;
end;

class function TGSeqUtil.LcsDistMyersImpl(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt;
var
  I, J, D, K, HiK: SizeInt;
  V: PSizeInt;
  StBuf: array[0..Pred(MAX_STATIC)] of SizeInt;
  Buf: array of SizeInt = nil;
begin
  if aLenL + aLenR < Pred(MAX_STATIC) then
    begin
      System.FillChar(StBuf, (aLenL + aLenR + 2) * SizeOf(SizeInt), 0);
      V := @StBuf[Succ(aLenL)];
    end
  else
    begin
      System.SetLength(Buf, aLenL + aLenR + 2);
      V := @Buf[Succ(aLenL)];
    end;

  for D := 0 to aLenL + aLenR do
    begin
      K := -(D - 2 * Math.Max(0, D - aLenL));
      HiK := D - 2 * Math.Max(0, D - aLenR);
      while K <= HiK do
        begin
          if (K = -D) or ((K <> D) and (V[K - 1] < V[K + 1])) then
            J := V[K + 1]
          else
            J := V[K - 1] + 1;
          I := J - K;
          while (J < aLenR) and (I < aLenL) and TEqRel.Equal(pL[I], pR[J]) do
            begin
              Inc(J);
              Inc(I);
            end;
          if (I = aLenL) and (J = aLenR) then exit(D);
          V[K] := J;
          K += 2;
        end;
      if D = aLimit then break;
    end;

  Result := NULL_INDEX;
end;

class function TGSeqUtil.GetLcsDistMyers(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt;
begin
  //here aLenL <= aLenR
  if aLimit < 0 then
    aLimit := aLenL + aLenR;

  if aLenR - aLenL > aLimit then
    exit(NULL_INDEX);

  if pL = pR then
    exit(aLenR - aLenL);

  SkipSuffix(pL, pR, aLenL, aLenR);
  SkipPrefix(pL, pR, aLenL, aLenR);

  if aLenL = 0 then
    exit(aLenR);

  if aLimit = 0 then
    exit(NULL_INDEX);

  if aLimit > aLenL + aLenR then
    aLimit := aLenL + aLenR;

  Result := LcsDistMyersImpl(pL, pR, aLenL, aLenR, aLimit);
end;

class function TGSeqUtil.DumDistMbrImpl(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt;
  function FindRow(k, aDist, aLeft, aAbove, aRight: SizeInt): SizeInt; inline;
  var
    I, MaxRow: SizeInt;
  begin
    if aDist = 0 then I := 0
    else
      I := MaxOf3(
        aLeft,
        aAbove+Ord(TEqRel.Equal(pR[aAbove+1],pL[aAbove+k])and TEqRel.Equal(pR[aAbove],pL[aAbove+k+1]))+1,
        aRight+1);
    MaxRow := Min(aLenL - k, aLenR);
    while (I < MaxRow) and TEqRel.Equal(pR[I], pL[I + k]) do
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
          FindRow(DMain + Diagonal, Dist - Diagonal, PrevR[Diagonal - 1], LastR[Diagonal], CurrRight);
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

class function TGSeqUtil.DumDistMbrDyn(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt;
var
  K: SizeInt;
begin
  //here aLenL <= aLenR
  if pL = pR then
    exit(aLenR - aLenL);

  SkipSuffix(pL, pR, aLenL, aLenR);
  SkipPrefix(pL, pR, aLenL, aLenR);

  if aLenL = 0 then
    exit(aLenR);

  K := 0;
  repeat
    if K <> 0 then
      K := Math.Min(K * 2, aLenR)
    else
      K := Math.Max(aLenR - aLenL, 2); // 2 ???
    Result := DumDistMbrImpl(pL, pR, aLenL, aLenR, K);
  until Result <> NULL_INDEX;
end;

class function TGSeqUtil.GetDumDistMbr(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt;
begin
  //here aLenL <= aLenR
  if aLimit < 0 then
    exit(DumDistMbrDyn(pL, pR, aLenL, aLenR));

  if aLenR - aLenL > aLimit then
    exit(NULL_INDEX);

  if pL = pR then
    exit(aLenR - aLenL);

  SkipSuffix(pL, pR, aLenL, aLenR);
  SkipPrefix(pL, pR, aLenL, aLenR);

  if aLenL = 0 then
    exit(aLenR);

  if aLimit = 0 then  //////////
    exit(NULL_INDEX); //////////

  Result := DumDistMbrImpl(pL, pR, aLenL, aLenR, aLimit);
end;

class function TGSeqUtil.Same(const L, R: array of T): Boolean;
var
  I: SizeInt;
begin
  if System.Length(L) <> System.Length(R) then
    exit(False);
  if (System.Length(L) > 0) and (@L[0] <> @R[0]) then
    for I := 0 to System.High(L) do
      if not TEqRel.Equal(L[I], R[I]) then
        exit(False);
  Result := True;
end;

class function TGSeqUtil.IsPermutation(const L, R: array of T): Boolean;
var
  LCounter, RCounter: TMultiSet;
begin
  if System.Length(L) = 0 then
    exit(System.Length(R) = 0);
  if System.Length(L) <> System.Length(R) then
    exit(False);
  if Same(L, R) then
    exit(True);
  LCounter.AddAll(L);
  RCounter.AddAll(R);
  Result := LCounter.IsEqual(RCounter);
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

class function TGSeqUtil.IsPrefix(const L, R: array of T): Boolean;
var
  I: SizeInt;
begin
  if (System.Length(L) = 0) or (System.Length(L) > System.Length(R)) then
    exit(False);
  for I := 0 to System.High(L) do
    if not TEqRel.Equal(L[I], R[I]) then
      exit(False);
  Result := True;
end;

class function TGSeqUtil.IsSuffix(const L, R: array of T): Boolean;
var
  I, J: SizeInt;
begin
  if (System.Length(L) = 0) or (System.Length(L) > System.Length(R)) then
    exit(False);
  J := System.High(R);
  for I := System.High(L) downto 0 do
    begin
      if not TEqRel.Equal(L[I], R[J]) then
        exit(False);
      Dec(J);
    end;
  Result := True;
end;

class function TGSeqUtil.CommonPrefixLen(const L, R: array of T): SizeInt;
var
  I: SizeInt;
begin
  Result := Math.Min(System.Length(L), System.Length(R));
  for I := 0 to Pred(Result) do
    if not TEqRel.Equal(L[I], R[I]) then
      exit(I);
end;

class function TGSeqUtil.CommonSuffixLen(const L, R: array of T): SizeInt;
var
  I, LIdx, RIdx: SizeInt;
begin
  LIdx := System.High(L);
  RIdx := System.High(R);
  for I := 0 to Math.Min(LIdx, RIdx) do
    begin
      if not TEqRel.Equal(L[LIdx], R[RIdx]) then
        exit(I);
      Dec(LIdx);
      Dec(RIdx);
    end;
  Result := Math.Min(System.Length(L), System.Length(R));
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

class function TGSeqUtil.LevDistanceMBR(const L, R: array of T): SizeInt;
begin
  if System.Length(L) = 0 then
    exit(System.Length(R))
  else
    if System.Length(R) = 0 then
      exit(System.Length(L));
  if System.Length(L) <= System.Length(R) then
    Result := GetLevDistMbr(@L[0], @R[0], System.Length(L), System.Length(R), System.Length(R))
  else
    Result := GetLevDistMbr(@R[0], @L[0], System.Length(R), System.Length(L), System.Length(L));
end;

class function TGSeqUtil.LevDistanceMBR(const L, R: array of T; aLimit: SizeInt): SizeInt;
begin
  if IsTrivialDist(L, R, aLimit, Result) then exit;
  if System.Length(L) <= System.Length(R) then
    Result := GetLevDistMbr(@L[0], @R[0], System.Length(L), System.Length(R), aLimit)
  else
    Result := GetLevDistMbr(@R[0], @L[0], System.Length(R), System.Length(L), aLimit);
end;

class function TGSeqUtil.LevDistanceMyers(const L, R: array of T): SizeInt;
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

class function TGSeqUtil.LevDistanceMyers(const L, R: array of T; aLimit: SizeInt): SizeInt;
begin
  if IsTrivialDist(L, R, aLimit, Result) then exit;
  if System.Length(L) <= System.Length(R) then
    Result := GetLevDistMyers(@L[0], @R[0], System.Length(L), System.Length(R), aLimit)
  else
    Result := GetLevDistMyers(@R[0], @L[0], System.Length(R), System.Length(L), aLimit);
end;

class function TGSeqUtil.LcsDistanceMyers(const L, R: array of T): SizeInt;
begin
  if System.Length(L) = 0 then
    exit(System.Length(R))
  else
    if System.Length(R) = 0 then
      exit(System.Length(L));
  if System.Length(L) <= System.Length(R) then
    Result := GetLcsDistMyers(@L[0], @R[0], System.Length(L), System.Length(R), System.Length(L) + System.Length(R))
  else
    Result := GetLcsDistMyers(@R[0], @L[0], System.Length(R), System.Length(L), System.Length(L) + System.Length(R));
end;

class function TGSeqUtil.LcsDistanceMyers(const L, R: array of T; aLimit: SizeInt): SizeInt;
begin
  if IsTrivialDist(L, R, aLimit, Result) then exit;
  if System.Length(L) <= System.Length(R) then
    Result := GetLcsDistMyers(@L[0], @R[0], System.Length(L), System.Length(R), aLimit)
  else
    Result := GetLcsDistMyers(@R[0], @L[0], System.Length(R), System.Length(L), aLimit);
end;

class function TGSeqUtil.DumDistanceMBR(const L, R: array of T): SizeInt;
begin
  if System.Length(L) = 0 then
    exit(System.Length(R))
  else
    if System.Length(R) = 0 then
      exit(System.Length(L));
  if System.Length(L) <= System.Length(R) then
    Result := GetDumDistMbr(@L[0], @R[0], System.Length(L), System.Length(R), System.Length(R))
  else
    Result := GetDumDistMbr(@R[0], @L[0], System.Length(R), System.Length(L), System.Length(L));
end;

class function TGSeqUtil.DumDistanceMBR(const L, R: array of T; aLimit: SizeInt): SizeInt;
begin
  if IsTrivialDist(L, R, aLimit, Result) then exit;
  if System.Length(L) <= System.Length(R) then
    Result := GetDumDistMbr(@L[0], @R[0], System.Length(L), System.Length(R), aLimit)
  else
    Result := GetDumDistMbr(@R[0], @L[0], System.Length(R), System.Length(L), aLimit);
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

class function TGSeqUtil.LcsKR(const L, R: array of T): TArray;
begin
  if (System.Length(L) = 0) or (System.Length(R) = 0) then
    exit(nil);
  if System.Length(L) <= System.Length(R) then
    Result := LcsKRImpl(@L[0], @R[0], System.Length(L), System.Length(R))
  else
    Result := LcsKRImpl(@R[0], @L[0], System.Length(R), System.Length(L));
end;

class function TGSeqUtil.LcsMyers(const L, R: array of T): TArray;
begin
  if (System.Length(L) = 0) or (System.Length(R) = 0) then
    exit(nil);
  if System.Length(L) <= System.Length(R) then
    Result := LcsMyersImpl(@L[0], @R[0], System.Length(L), System.Length(R))
  else
    Result := LcsMyersImpl(@R[0], @L[0], System.Length(R), System.Length(L));
end;

class function TGSeqUtil.SimRatio(const L, R: array of T; aLimit: Double; Algo: TSeqDistanceAlgo): Double;
var
  Len, Limit, Dist: SizeInt;
begin
  if (System.Length(L) = 0) and (System.Length(R) = 0) then exit(Double(1.0));
  if aLimit > 1 then aLimit := Double(1);
  if Algo = sdaLcsMyers then
    Len := System.Length(L) + System.Length(R)
  else
    Len := Math.Max(System.Length(L), System.Length(R));
  if aLimit = Double(0) then begin
    case Algo of
      sdaDefault,
      sdaLevMyers: Dist := LevDistanceMyers(L, R);
      sdaLevMBR:   Dist := LevDistanceMbr(L, R);
      sdaLcsMyers: Dist := LcsDistanceMyers(L, R);
      sdaDumMBR:   Dist := DumDistanceMbr(L, R);
    end;
    exit(Double(Len - Dist)/Double(Len));
  end;
  if Limit > 0 then
    Limit := Len - {$IFDEF CPU64}Ceil64{$ELSE}Ceil{$ENDIF}(aLimit*Len)
  else
    Limit := -1;
  case Algo of
    sdaDefault:
      if aLimit > Double(0.90) then  // ???
        Dist := LevDistanceMbr(L, R, Limit)
      else
        Dist := LevDistanceMyers(L, R, Limit);
    sdaLevMBR:   Dist := LevDistanceMbr(L, R, Limit);
    sdaLevMyers: Dist := LevDistanceMyers(L, R, Limit);
    sdaLcsMyers: Dist := LcsDistanceMyers(L, R, Limit);
    sdaDumMBR:   Dist := DumDistanceMbr(L, R, Limit);
  end;
  if Dist <> NULL_INDEX then
    Result := Double(Len - Dist)/Double(Len)
  else
    Result := Double(0);
end;

class function TGSeqUtil.Diff(const aSource, aTarget: array of T; aLcsAlgo: TLcsAlgo): TDiff;
var
  Lcs: TArray;
  I, SrcIdx, TrgIdx: SizeInt;
  v: T;
  Del: array of Boolean = nil;
  Ins: array of Boolean = nil;
begin
  case aLcsAlgo of
    laGus: Lcs := LcsGus(aSource, aTarget);
    laKr:  Lcs := LcsKr(aSource, aTarget);
  else// laMyers
    Lcs := LcsMyers(aSource, aTarget);
  end;
  System.SetLength(Del, System.Length(aSource));
  System.SetLength(Ins, System.Length(aTarget));
  SrcIdx := 0;
  TrgIdx := 0;
  for I := 0 to System.High(Lcs) do
    begin
      v := Lcs[I];
      while not TEqRel.Equal(v, aSource[SrcIdx]) do
        begin
          Del[SrcIdx] := True;
          Inc(SrcIdx)
        end;
      while not TEqRel.Equal(v, aTarget[TrgIdx]) do
        begin
          Ins[TrgIdx] := True;
          Inc(TrgIdx)
        end;
      Inc(SrcIdx);
      Inc(TrgIdx);
    end;
  for I := SrcIdx to System.High(Del) do
    Del[I] := True;
  for I := TrgIdx to System.High(Ins) do
    Ins[I] := True;
  Result.SourceChanges := Del;
  Result.TargetChanges := Ins;
end;

type
  TUcs4Util    = specialize TGSeqUtil<Ucs4Char, TUcs4Hasher>;
  TByte4       = array[0..3] of Byte;
  TByte3       = array[0..2] of Byte;
  TByte2       = array[0..1] of Byte;
  PByte4       = ^TByte4;
  PByte3       = ^TByte3;
  PByte2       = ^TByte2;
  TUcs4Rec     = record Key: Ucs4Char end;
  TUcs4CharSet = specialize TGLiteChainHashTable<Ucs4Char, TUcs4Rec, TUcs4Hasher>;

const
  MAX_STATIC       = TUcs4Util.MAX_STATIC;
  UNICODE_BAD_CHAR = $fffd;

procedure Utf16ToUcs4SeqImpl(const s: unicodestring; pSeq: PUcs4Char; out aSeqLen: SizeInt);
var
  I, Len: SizeInt;
  p: PWideChar;
  c: Ucs4Char;
begin
  Len := System.Length(s);
  p := Pointer(s);
  aSeqLen := 0;
  I := 0;
  while I < Len do
    begin
      c := Ucs4Char(p[I]);
      if (c <= $d7ff) or (c >= $e000) then
        pSeq[aSeqLen] := c
      else
        if (c <= $dbff) and (I < Len-1) and (p[I+1] >= #$dc00)and(p[I+1] <= #$dfff) then
          begin
            pSeq[aSeqLen] := (c - $d7c0) shl 10 + (Ucs4Char(p[I+1]) xor $dc00);
            Inc(I);
          end
        else { invalid surrogate pair }
          pSeq[aSeqLen] := UNICODE_BAD_CHAR;
      Inc(I);
      Inc(aSeqLen);
    end;
end;

procedure Utf16ToUcs4SeqImpl(const s: unicodestring; out aSeq: TUcs4Seq);
var
  I, Len, Count: SizeInt;
  p: PWideChar;
  c: Ucs4Char;
begin
  Len := System.Length(s);
  p := Pointer(s);
  System.SetLength(aSeq, Len);
  Count := 0;
  I := 0;
  while I < Len do
    begin
      c := Ucs4Char(p[I]);
      if (c <= $d7ff) or (c >= $e000) then
        aSeq[Count] := c
      else
        if (c <= $dbff) and (I < Len-1) and (p[I+1] >= #$dc00)and(p[I+1] <= #$dfff) then
          begin
            aSeq[Count] := (c - $d7c0) shl 10 + (Ucs4Char(p[I+1]) xor $dc00);
            Inc(I);
          end
        else { invalid surrogate pair }
          aSeq[Count] := UNICODE_BAD_CHAR;
      Inc(I);
      Inc(Count);
    end;
  System.SetLength(aSeq, Count);
end;

function Utf16ToUcs4Char(p: PWideChar; aStrLen: SizeInt; out aLen: SizeInt): Ucs4Char;
var
  c: Ucs4Char;
begin
  c := Ucs4Char(p^);
  aLen := 1;
  if (c <= $d7ff) or (c >= $e000) then
    Result := c
  else
    if (c <= $dbff) and (aStrLen > 1) and (p[1] >= #$dc00)and(p[1] <= #$dfff) then
      begin
        Result := (c - $d7c0) shl 10 + (Ucs4Char(p[1]) xor $dc00);
        Inc(aLen);
      end
    else
      Result := UNICODE_BAD_CHAR;
end;

function IsSubSequenceUtf16(const aStr, aSub: unicodestring): Boolean;
var
  I, J, PtStrSize, PtSubSize, LenStr, LenSub: SizeInt;
  cStr, cSub: Ucs4Char;
  pStr: PWideChar absolute aStr;
  pSub: PWideChar absolute aSub;
begin
  if aSub = '' then exit(True);
  LenStr := System.Length(aStr);
  LenSub := System.Length(aSub);
  I := 0;
  J := 0;
  cSub := Utf16ToUcs4Char(pSub, LenSub, PtSubSize);
  while (I < LenStr) and (J < LenSub) do
    begin
      cStr := Utf16ToUcs4Char(@pStr[I], LenStr - I, PtStrSize);
      if cStr = cSub then
        begin
          Inc(J, PtSubSize);
          cSub := Utf16ToUcs4Char(@pSub[J], LenSub - J, PtSubSize);
        end;
      Inc(I, PtStrSize);
    end;
  Result := J = LenSub;
end;

function Utf16ToUcs4Seq(const s: unicodestring): TUcs4Seq;
begin
  Utf16ToUcs4SeqImpl(s, Result);
end;

function Ucs4SeqToUtf16(const s: TUcs4Seq): unicodestring;
var
  I, Len: SizeInt;
  c: Ucs4Char;
  p: PWideChar;
begin
  Len := 0;
  for I := 0 to System.High(s) do
    Len += Succ(Ord((s[I] > $ffff) and (DWord(s[I]) <= $10ffff)));
  System.SetLength(Result, Len);
  p := Pointer(Result);
  for I := 0 to System.High(s) do
    begin
      c := s[I];
      if c <= $ffff then
        p^ := WideChar(c)
      else
        if (DWord(c) <= $10ffff) then
          begin
            p^ := WideChar(c shr 10 + $d7c0);
            p[1] := WideChar(c and $3ff + $dc00);
            Inc(p);
          end
        else  { invalid code point }
          p^ := WideChar(UNICODE_BAD_CHAR);
      Inc(p);
    end;
end;

type
  TDistFunInternSpec = (
    dfisDyn, dfisMbr, dfisMyers, dfisMyersLcs, dfisMbrDum, dfisMbrBound, dfisMyersBound,
    dfisMyersLcsBound, dfisMbrDumBound);

function GenericDistanceUtf16(const L, R: unicodestring; aLimit: SizeInt; aSpec: TDistFunInternSpec): SizeInt;
var
  LBufSt, RBufSt: array[0..Pred(MAX_STATIC)] of Ucs4Char;
  LBuf: TUcs4Seq = nil;
  RBuf: TUcs4Seq = nil;
  LenL, LenR: SizeInt;
  pL, pR: PUcs4Char;
begin
  if System.Length(L) <= MAX_STATIC then
    begin
      pL := @LBufSt[0];
      Utf16ToUcs4SeqImpl(L, pL, LenL);
    end
  else
    begin
      Utf16ToUcs4SeqImpl(L, LBuf);
      LenL := System.Length(LBuf);
      pL := Pointer(LBuf);
    end;
  if System.Length(R) <= MAX_STATIC then
    begin
      pR := @RBufSt[0];
      Utf16ToUcs4SeqImpl(R, pR, LenR);
    end
  else
    begin
      Utf16ToUcs4SeqImpl(R, RBuf);
      LenR := System.Length(RBuf);
      pR := Pointer(RBuf);
    end;
  case aSpec of
    dfisDyn:           Result := TUcs4Util.LevDistance(pL[0..Pred(LenL)], pR[0..Pred(LenR)]);
    dfisMbr:           Result := TUcs4Util.LevDistanceMBR(pL[0..Pred(LenL)], pR[0..Pred(LenR)]);
    dfisMyers:         Result := TUcs4Util.LevDistanceMyers(pL[0..Pred(LenL)], pR[0..Pred(LenR)]);
    dfisMyersLcs:      Result := TUcs4Util.LcsDistanceMyers(pL[0..Pred(LenL)], pR[0..Pred(LenR)]);
    dfisMbrDum:        Result := TUcs4Util.DumDistanceMBR(pL[0..Pred(LenL)], pR[0..Pred(LenR)]);
    dfisMbrBound:      Result := TUcs4Util.LevDistanceMBR(pL[0..Pred(LenL)], pR[0..Pred(LenR)], aLimit);
    dfisMyersBound:    Result := TUcs4Util.LevDistanceMyers(pL[0..Pred(LenL)], pR[0..Pred(LenR)], aLimit);
    dfisMyersLcsBound: Result := TUcs4Util.LcsDistanceMyers(pL[0..Pred(LenL)], pR[0..Pred(LenR)], aLimit);
  else
    //dfisMbrDumBound
    Result := TUcs4Util.DumDistanceMBR(pL[0..Pred(LenL)], pR[0..Pred(LenR)], aLimit);
  end;
end;

function LevDistanceUtf16(const L, R: unicodestring): SizeInt;
begin
  Result := GenericDistanceUtf16(L, R, -1, dfisDyn);
end;

function LevDistanceMbrUtf16(const L, R: unicodestring): SizeInt;
begin
  Result := GenericDistanceUtf16(L, R, -1, dfisMbr);
end;

function LevDistanceMbrUtf16(const L, R: unicodestring; aLimit: SizeInt): SizeInt;
begin
  Result := GenericDistanceUtf16(L, R, aLimit, dfisMbrBound);
end;

function LevDistanceMyersUtf16(const L, R: unicodestring): SizeInt;
begin
  Result := GenericDistanceUtf16(L, R, -1, dfisMyers);
end;

function LevDistanceMyersUtf16(const L, R: unicodestring; aLimit: SizeInt): SizeInt;
begin
  Result := GenericDistanceUtf16(L, R, aLimit, dfisMyersBound);
end;

function LcsDistanceMyersUtf16(const L, R: unicodestring): SizeInt;
begin
  Result := GenericDistanceUtf16(L, R, -1, dfisMyersLcs);
end;

function LcsDistanceMyersUtf16(const L, R: unicodestring; aLimit: SizeInt): SizeInt;
begin
  Result := GenericDistanceUtf16(L, R, aLimit, dfisMyersLcsBound);
end;

function DumDistanceMbrUtf16(const L, R: unicodestring): SizeInt;
begin
  Result := GenericDistanceUtf16(L, R, -1, dfisMbrDum);
end;

function DumDistanceMbrUtf16(const L, R: unicodestring; aLimit: SizeInt): SizeInt;
begin
  Result := GenericDistanceUtf16(L, R, aLimit, dfisMbrDumBound);
end;

function LcsGenegicUtf16(const L, R: unicodestring; aSpec: TUcs4Util.TLcsAlgo): unicodestring;
var
  LBufSt, RBufSt: array[0..Pred(MAX_STATIC)] of Ucs4Char;
  LBuf: TUcs4Seq = nil;
  RBuf: TUcs4Seq = nil;
  LenL, LenR: SizeInt;
  pL, pR: PUcs4Char;
begin
  if System.Length(L) <= MAX_STATIC then
    begin
      pL := @LBufSt[0];
      Utf16ToUcs4SeqImpl(L, pL, LenL);
    end
  else
    begin
      Utf16ToUcs4SeqImpl(L, LBuf);
      LenL := System.Length(LBuf);
      pL := Pointer(LBuf);
    end;
  if System.Length(R) <= MAX_STATIC then
    begin
      pR := @RBufSt[0];
      Utf16ToUcs4SeqImpl(R, pR, LenR);
    end
  else
    begin
      Utf16ToUcs4SeqImpl(R, RBuf);
      LenR := System.Length(RBuf);
      pR := Pointer(RBuf);
    end;
  case aSpec of
    laGus: Result := Ucs4SeqToUtf16(TUcs4Util.LcsGus(pL[0..Pred(LenL)], pR[0..Pred(LenR)]));
    laKR:  Result := Ucs4SeqToUtf16(TUcs4Util.LcsKR(pL[0..Pred(LenL)], pR[0..Pred(LenR)]));
  else
    Result := Ucs4SeqToUtf16(TUcs4Util.LcsMyers(pL[0..Pred(LenL)], pR[0..Pred(LenR)]));
  end;
end;

function LcsGusUtf16(const L, R: unicodestring): unicodestring;
begin
  Result := LcsGenegicUtf16(L, R, laGus);
end;

function LcsKRUtf16(const L, R: unicodestring): unicodestring;
begin
  Result := LcsGenegicUtf16(L, R, laKR);
end;

function LcsMyersUtf16(const L, R: unicodestring): unicodestring;
begin
  Result := LcsGenegicUtf16(L, R, laMyers);
end;

{$PUSH}{$WARN 5089 OFF}
function SimRatioGeneric(const L, R: array of Ucs4Char; constref aStopChars: TUcs4CharSet;
  aMode: TSimMode; aPartial: Boolean; aLimit: Double; Algo: TSeqDistanceAlgo; aLess: TUcs4Less): Double;
type
  TWord       = record Start: PUcs4Char; Len: SizeInt end;
  PWord       = ^TWord;
  TWordArray  = array of TWord;
  TSplitFun   = function(const s: array of Ucs4Char; out aCount: SizeInt; out aBuf: TWordArray;
                         aForceDyn: Boolean): PWord is nested;
  THelper     = specialize TGNestedArrayHelper<TWord>;
  TUcs4Helper = specialize TGArrayHelpUtil<Ucs4Char>;
const
  UCS4_SPACE: Ucs4Char = 32;
var
  StBuf: array[0..Pred(MAX_STATIC)] of TWord;

  function SplitMerge(const s: array of Ucs4Char): TUcs4Seq;
  var
    I, J: SizeInt;
    pS, pR: PUcs4Char;
    NewWord: Boolean;
  begin
    if aStopChars.Count = 0 then exit(TUcs4Helper.CreateCopy(s));
    if System.Length(s) = 0 then exit(nil);
    System.SetLength(Result, System.Length(s));
    pS := @s[0];
    pR := Pointer(Result);
    I := 0;
    while (I < System.Length(s)) and (aStopChars.Find(pS[I]) <> nil) do Inc(I);
    J := 0;
    NewWord := False;
    for I := I to System.High(s) do
      if aStopChars.Find(pS[I]) <> nil then
        NewWord := True
      else begin
        if NewWord then begin
          pR[J] := UCS4_SPACE;
          Inc(J);
          NewWord := False;
        end;
        pR[J] := pS[I];
        Inc(J);
      end;
    System.SetLength(Result, J);
  end;

  function Less(const L, R: TWord): Boolean; inline;
  begin
    Result := aLess(L.Start[0..Pred(L.Len)], R.Start[0..Pred(R.Len)]);
  end;

  function LessDef(const L, R: TWord): Boolean;
  var
    I: SizeInt;
  begin
    for I := 0 to Pred(Math.Min(L.Len, R.Len)) do
      if L.Start[I] <> R.Start[I] then exit(L.Start[I] < R.Start[I]);
    LessDef := L.Len < R.Len;
  end;

  function Equal(const L, R: TWord): Boolean;
  var
    I: SizeInt;
  begin
    if L.Len <> R.Len then exit(False);
    for I := 0 to Pred(L.Len) do
      if L.Start[I] <> R.Start[I] then exit(False);
    Equal := True;
  end;

  function SplitAndSort(const s: array of Ucs4Char; out aCount: SizeInt; out aBuf: TWordArray; aForceDyn: Boolean): PWord;
  var
    p: PUcs4Char;
    Words: PWord;
    I, Count, CurrLen: SizeInt;
    CurrStart: PUcs4Char;
  begin
    if aForceDyn or (System.Length(s) div 2 + System.Length(s) and 1 > MAX_STATIC) then begin
      System.SetLength(aBuf, System.Length(s) div 2 + System.Length(s) and 1);
      Words := Pointer(aBuf);
    end else
      Words := @StBuf[0];

    if System.Length(s) <> 0 then
      p := @s[0]
    else
      p := nil;
    CurrStart := p;
    CurrLen := 0;
    Count := 0;
    for I := 0 to System.High(s) do
      if aStopChars.Find(p[I]) <> nil then begin
        if CurrLen = 0 then continue;
        Words[Count].Start := CurrStart;
        Words[Count].Len := CurrLen;
        CurrLen := 0;
        Inc(Count);
      end else begin
        if CurrLen = 0 then
          CurrStart := @p[I];
        Inc(CurrLen);
      end;
    if CurrLen <> 0 then begin
      Words[Count].Start := CurrStart;
      Words[Count].Len := CurrLen;
      Inc(Count);
    end;
    if aLess <> nil then
      THelper.Sort(Words[0..Pred(Count)], @Less)
    else
      THelper.Sort(Words[0..Pred(Count)], @LessDef);
    aCount := Count;
    Result := Words;
  end;

  function SplitMerge(const s: array of Ucs4Char; aSplit: TSplitFun): TUcs4Seq;
  var
    Words: PWord;
    Buf: TWordArray = nil;
    I, J, Count, Len: SizeInt;
    pR: PUcs4Char;
  begin
    Words := aSplit(s, Count, Buf, False);
    System.SetLength(Result, System.Length(s));
    pR := Pointer(Result);
    Len := 0;
    for I := 0 to Pred(Count) do begin
      if I > 0 then begin
        Len += Words[I].Len + 1;
        pR^ := UCS4_SPACE;
        Inc(pR);
      end else
        Len += Words[I].Len;
      for J := 0 to Pred(Words[I].Len) do
        with Words[I] do
          pR[J] := Start[J];
      pR += Words[I].Len;
    end;
    System.SetLength(Result, Len);
  end;

  function SplitMergeSorted(const s: array of Ucs4Char): TUcs4Seq;
  begin
    Result := SplitMerge(s, @SplitAndSort);
  end;

  function SplitSortedSet(const s: array of Ucs4Char; out aCount: SizeInt; out aBuf: TWordArray; aForceDyn: Boolean): PWord;
  var
    I, J, Count: SizeInt;
  begin
    Result := SplitAndSort(s, Count, aBuf, aForceDyn);
    I := 0;
    J := 0;
    while I < Count do begin
      if I <> J then
        Result[J] := Result[I];
      Inc(I);
      while (I < Count) and Equal(Result[I], Result[J]) do Inc(I);
      Inc(J);
    end;
    aCount := J;
  end;

  function SplitMergeSortedSet(const s: array of Ucs4Char): TUcs4Seq;
  begin
    Result := SplitMerge(s, @SplitSortedSet);
  end;

  function SimPartial(const L, R: TUcs4Seq): Double;
  var
    I: SizeInt;
  begin
    if L = nil then
      if R = nil then exit(Double(1))
      else exit(Double(0))
    else
      if R = nil then exit(Double(0));
    Result := Double(0);
    if System.Length(L) <= System.Length(R) then
      for I := 0 to System.Length(R) - System.Length(L) do begin
        Result := Math.Max(
          Result,
          TUcs4Util.SimRatio(
            PUcs4Char(L)[0..System.High(L)], PUcs4Char(R)[I..I+System.High(L)], aLimit, Algo));
        if Result = Double(1) then break;
      end
    else
      for I := 0 to System.Length(L) - System.Length(R) do begin
        Result := Math.Max(
          Result,
          TUcs4Util.SimRatio(
            PUcs4Char(R)[0..System.High(R)], PUcs4Char(L)[I..I+System.High(R)], aLimit, Algo));
        if Result = Double(1) then break;
      end;
  end;

  function Merge(aSrcLen: SizeInt; aWords: PWord; const aIndices: TBoolVector): TUcs4Seq;
  var
    I, J, Len: SizeInt;
    pR: PUcs4Char;
    NotFirst: Boolean;
  begin
    System.SetLength(Result, aSrcLen);
    pR := Pointer(Result);
    NotFirst := False;
    Len := 0;
    for I in aIndices do begin
      if NotFirst then begin
        Len += aWords[I].Len + 1;
        pR^ := UCS4_SPACE;
        Inc(pR);
      end else begin
        Len += aWords[I].Len;
        NotFirst := True;
      end;
      for J := 0 to Pred(aWords[I].Len) do
        with aWords[I] do
          pR[J] := Start[J];
      pR += aWords[I].Len;
    end;
    System.SetLength(Result, Len);
  end;

  function WordSetPairwise(const L, R: array of Ucs4Char): Double;
  var
    WordsL, WordsR: PWord;
    BufL, BufR: TWordArray;
    IntersectIdx, DiffIdxL, DiffIdxR: TBoolVector;
    I, J, CountL, CountR: SizeInt;
    Intersection, SetL, SetR: TUcs4Seq;
  begin
    WordsL := SplitSortedSet(L, CountL, BufL, False);
    WordsR := SplitSortedSet(R, CountR, BufR, True);
    IntersectIdx.EnsureCapacity(CountL);
    DiffIdxL.InitRange(CountL);
    DiffIdxR.InitRange(CountR);

    if aLess <> nil then
      for I := 0 to Pred(CountL) do begin
        J := THelper.BinarySearch(WordsR[0..Pred(CountR)], WordsL[I], @Less);
        if J <> NULL_INDEX then begin
          IntersectIdx[I] := True;
          DiffIdxL[I] := False;
          DiffIdxR[J] := False;
        end;
      end
    else
      for I := 0 to Pred(CountL) do begin
        J := THelper.BinarySearch(WordsR[0..Pred(CountR)], WordsL[I], @LessDef);
        if J <> NULL_INDEX then begin
          IntersectIdx[I] := True;
          DiffIdxL[I] := False;
          DiffIdxR[J] := False;
        end;
      end;

    Intersection := Merge(System.Length(L), WordsL, IntersectIdx);
    if (Intersection <> nil) and aPartial then exit(Double(1));
    SetL := Merge(System.Length(L), WordsL, DiffIdxL);
    SetR := Merge(System.Length(R), WordsR, DiffIdxR);

    if Intersection <> nil then begin
      if SetL <> nil then
        SetL := Intersection + [UCS4_SPACE] + SetL
      else
        SetL := Intersection;
      if SetR <> nil then
        SetR := Intersection + [UCS4_SPACE] + SetR
      else
        SetR := Intersection;
    end;

    if aPartial then
      Result := SimPartial(SetL, SetR)
    else begin
      Result := TUcs4Util.SimRatio(Intersection, SetL, aLimit, Algo);
      if Result = Double(1) then exit;
      Result := Math.Max(Result, TUcs4Util.SimRatio(Intersection, SetR, aLimit, Algo));
      if Result = Double(1) then exit;
      Result := Math.Max(Result, TUcs4Util.SimRatio(SetL, SetR, aLimit, Algo));
    end;
  end;

begin
  case aMode of
    smSimple:
      if aPartial then
        Result := SimPartial(SplitMerge(L), SplitMerge(R))
      else
        Result := TUcs4Util.SimRatio(SplitMerge(L), SplitMerge(R), aLimit, Algo);
    smTokenSort:
    if aPartial then
      Result := SimPartial(SplitMergeSorted(L), SplitMergeSorted(R))
    else
      Result := TUcs4Util.SimRatio(SplitMergeSorted(L), SplitMergeSorted(R), aLimit, Algo);
    smTokenSet:
    if aPartial then
      Result := SimPartial(SplitMergeSortedSet(L), SplitMergeSortedSet(R))
    else
      Result := TUcs4Util.SimRatio(SplitMergeSortedSet(L), SplitMergeSortedSet(R), aLimit, Algo);
  else
    exit(WordSetPairwise(L, R));
  end;
end;
{$POP}

function IsSingleCodePointUtf16(const s: unicodestring; out aPt: Ucs4Char): Boolean;
var
  Len: SizeInt;
  p: PWideChar;
  c: Ucs4Char;
begin
  if (s = '') or (System.Length(s) > 2) then exit(False);
  Len := System.Length(s);
  p := PWideChar(s);
  c := Ucs4Char(p^);
  if (c <= $d7ff) or (c >= $e000) then
    aPt := c
  else
    if (c <= $dbff) and (Len > 1) and (p[1] >= #$dc00)and(p[1] <= #$dfff) then
      begin
        aPt := (c - $d7c0) shl 10 + (Ucs4Char(p[1]) xor $dc00);
        Dec(Len);
      end
    else
      aPt := UNICODE_BAD_CHAR;
  Dec(Len);
  Result := Len = 0;
end;

{$PUSH}{$WARN 5036 OFF}
function SimRatioExUtf16(const L, R: unicodestring; const aStopChars: array of unicodestring; aMode: TSimMode;
  const aOptions: TSimOptions; aLimit: Double; Algo: TSeqDistanceAlgo; aLess: TUcs4Less): Double;
var
  LBufSt, RBufSt: array[0..Pred(MAX_STATIC)] of Ucs4Char;
  LBuf: TUcs4Seq = nil;
  RBuf: TUcs4Seq = nil;
  LenL, LenR: SizeInt;
  pL, pR: PUcs4Char;

  StopChars: TUcs4CharSet;
  LocL, LocR: unicodestring;
  I: SizeInt;
  c: Ucs4Char;
  p: TUcs4CharSet.PEntry;
begin

  if soIgnoreCase in aOptions then
    begin
      LocL := TCharacter.ToLower(L, [TCharacterOption.coIgnoreInvalidSequence]);
      LocR := TCharacter.ToLower(R, [TCharacterOption.coIgnoreInvalidSequence]);
    end
  else
    begin
      LocL := L;
      LocR := R;
    end;

  for I := 0 to System.High(aStopChars) do
    if IsSingleCodePointUtf16(aStopChars[I], c) and not StopChars.FindOrAdd(c, p) then
      p^.Key := c;

  if System.Length(LocL) <= MAX_STATIC then
    begin
      pL := @LBufSt[0];
      Utf16ToUcs4SeqImpl(LocL, pL, LenL);
    end
  else
    begin
      Utf16ToUcs4SeqImpl(LocL, LBuf);
      LenL := System.Length(LBuf);
      pL := Pointer(LBuf);
    end;
  if System.Length(LocR) <= MAX_STATIC then
    begin
      pR := @RBufSt[0];
      Utf16ToUcs4SeqImpl(LocR, pR, LenR);
    end
  else
    begin
      Utf16ToUcs4SeqImpl(LocR, RBuf);
      LenR := System.Length(RBuf);
      pR := Pointer(RBuf);
    end;

  Result :=
    SimRatioGeneric(
      pL[0..Pred(LenL)], pR[0..Pred(LenR)], StopChars, aMode, soPartial in aOptions, aLimit, Algo, aLess);
end;
{$POP}

function SimRatioUtf16(const L, R: unicodestring; aLimit: Double; Algo: TSeqDistanceAlgo): Double;
var
  LBufSt, RBufSt: array[0..Pred(MAX_STATIC)] of Ucs4Char;
  LBuf: TUcs4Seq = nil;
  RBuf: TUcs4Seq = nil;
  LenL, LenR: SizeInt;
  pL, pR: PUcs4Char;
begin
  if System.Length(L) <= MAX_STATIC then
    begin
      pL := @LBufSt[0];
      Utf16ToUcs4SeqImpl(L, pL, LenL);
    end
  else
    begin
      Utf16ToUcs4SeqImpl(L, LBuf);
      LenL := System.Length(LBuf);
      pL := Pointer(LBuf);
    end;
  if System.Length(R) <= MAX_STATIC then
    begin
      pR := @RBufSt[0];
      Utf16ToUcs4SeqImpl(R, pR, LenR);
    end
  else
    begin
      Utf16ToUcs4SeqImpl(R, RBuf);
      LenR := System.Length(RBuf);
      pR := Pointer(RBuf);
    end;
  Result := TUcs4Util.SimRatio(pL[0..Pred(LenL)], pR[0..Pred(LenR)], aLimit, Algo);
end;

function Utf8CodePointLen(p: PByte; aStrLen: SizeInt): SizeInt; inline;
begin
  case p^ of
    0..$7f: ;
    $c2..$df:
      if (aStrLen > 1) and (p[1] in [$80..$bf]) then exit(2);
    $e0:
      if (aStrLen > 2) and (p[1] in [$a0..$bf]) and (p[2] in [$80..$bf]) then exit(3);
    $e1..$ec, $ee..$ef:
      if (aStrLen > 2) and (p[1] in [$80..$bf]) and (p[2] in [$80..$bf]) then exit(3);
    $ed:
      if (aStrLen > 2) and (p[1] in [$80..$9f]) and (p[2] in [$80..$bf]) then exit(3);
    $f0:
      if(aStrLen > 3)and(p[1]in[$90..$bf])and(p[2]in[$80..$bf])and(p[3]in[$80..$bf])then exit(4);
    $f1..$f3:
      if(aStrLen > 3)and(p[1]in[$80..$bf])and(p[2]in[$80..$bf])and(p[3]in[$80..$bf])then exit(4);
    $f4:
      if(aStrLen > 3)and(p[1]in[$80..$8f])and(p[2]in[$80..$bf])and(p[3]in[$80..$bf])then exit(4);
  else
  end;
  Result := 1;
end;

function Utf8CodePointLength(p: PAnsiChar; aByteCount: SizeInt): SizeInt;
begin
  Result := Utf8CodePointLen(PByte(p), aByteCount);
end;

function CodePointToUcs4Char(p: PByte; out aPtSize: SizeInt): Ucs4Char; //inline;
begin
  case p^ of
    0..$7f:
      begin
        aPtSize := 1;
        exit(p^);
      end;
    $c2..$df:
      if p[1] in [$80..$bf] then begin
        aPtSize := 2;
        exit(Ucs4Char(Ucs4Char(p[0] and $1f) shl 6 or Ucs4Char(p[1] and $3f)));
      end;
    $e0:
      if (p[1] in [$a0..$bf]) and (p[2] in [$80..$bf]) then begin
        aPtSize := 3;
        exit(Ucs4Char(Ucs4Char(p[0]and $f)shl 12 or Ucs4Char(p[1]and $3f)shl 6 or Ucs4Char(p[2] and $3f)));
      end;
    $e1..$ec, $ee..$ef:
      if (p[1] in [$80..$bf]) and (p[2] in [$80..$bf]) then begin
        aPtSize := 3;
        exit(Ucs4Char(Ucs4Char(p[0]and $f)shl 12 or Ucs4Char(p[1]and $3f)shl 6 or Ucs4Char(p[2]and $3f)));
      end;
    $ed:
      if (p[1] in [$80..$9f]) and (p[2] in [$80..$bf]) then begin
        aPtSize := 3;
        exit(Ucs4Char(Ucs4Char(p[0]and $f)shl 12 or Ucs4Char(p[1]and $3f)shl 6 or Ucs4Char(p[2]and $3f)));
      end;
    $f0:
      if (p[1] in [$90..$bf]) and (p[2] in [$80..$bf]) and (p[3] in [$80..$bf]) then begin
        aPtSize := 4;
        exit(Ucs4Char(Ucs4Char(p[0] and $7) shl 18 or Ucs4Char(p[1] and $3f) shl 12 or
             Ucs4Char(p[2] and $3f) shl 6 or Ucs4Char(p[3] and $3f)));
      end;
    $f1..$f3:
      if (p[1] in [$80..$bf]) and (p[2] in [$80..$bf]) and (p[3] in [$80..$bf]) then begin
        aPtSize := 4;
        exit(Ucs4Char(Ucs4Char(p[0] and $7) shl 18 or Ucs4Char(p[1] and $3f) shl 12 or
             Ucs4Char(p[2] and $3f) shl 6 or Ucs4Char(p[3] and $3f)));
      end;
    $f4:
      if (p[1] in [$80..$8f]) and (p[2] in [$80..$bf]) and (p[3] in [$80..$bf]) then begin
        aPtSize := 4;
        exit(Ucs4Char(Ucs4Char(p[0] and $7) shl 18 or Ucs4Char(p[1] and $3f) shl 12 or
             Ucs4Char(p[2] and $3f) shl 6 or Ucs4Char(p[3] and $3f)));
      end;
  else
  end;
  aPtSize := 1;
  Result := UNICODE_BAD_CHAR;
end;

function Utf8Len(const s: rawbytestring): SizeInt;
var
  I, StrLen: SizeInt;
  p: PByte absolute s;
begin
  StrLen := System.Length(s);
  Result := 0;
  I := 0;
  while I < StrLen do
    begin
      I += Utf8CodePointLen(@p[I], StrLen - I);
      Inc(Result);
    end;
end;

function Utf8ToUcs4SeqImpl(const s: rawbytestring): TUcs4Seq;
var
  r: TUcs4Seq = nil;
  I, J, PtSize, StrLen: SizeInt;
  p: PByte absolute s;
begin
  System.SetLength(r, System.Length(s));
  StrLen := System.Length(s);
  I := 0;
  J := 0;
  while I < StrLen do
    begin
      r[J] := CodePointToUcs4Char(@p[I], PtSize);
      Inc(J);
      I += PtSize;
    end;
  System.SetLength(r, J);
  Result := r;
end;

procedure Utf8ToUcs4SeqImpl(const s: rawbytestring; aPtr: PUcs4Char; out aLen: SizeInt);
var
  I, PtSize, StrLen: SizeInt;
  p: PByte absolute s;
begin
  StrLen := System.Length(s);
  I := 0;
  aLen := 0;
  while I < StrLen do
    begin
      aPtr[aLen] := CodePointToUcs4Char(@p[I], PtSize);
      Inc(aLen);
      I += PtSize;
    end;
end;

{ see http://bjoern.hoehrmann.de/utf-8/decoder/dfa; optimized version based on Rich Felker's variant }
{$PUSH}{$J-}
const
  UTF8_D: array[Byte] of Byte = (
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
      7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
      8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
     10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8);

  UTF8_S: array[0..107] of Byte = (
      0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
     12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
     12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
     12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
     12,36,12,12,12,12,12,12,12,12,12,12);
{$POP}

function Utf8ValidateDfa(const s: rawbytestring): Boolean;
var
  I: SizeInt;
  State: Byte;
const
  UTF8_REJECT = 12;
begin
  State := 0;
  for I := 1 to System.Length(s) do
    begin
      State := UTF8_S[State + UTF8_D[Byte(s[I])]];
      if State = UTF8_REJECT then exit(False);
    end;
  Result := State = 0;
end;

{
  The Unicode Standard, Version 14.0, Table 3-7. Well-Formed UTF-8 Byte Sequences

    +--------------------+------------+-------------+------------+-------------+
    | Code Points        | First Byte | Second Byte | Third Byte | Fourth Byte |
    +--------------------+------------+-------------+------------+-------------+
    | U+0000..U+007F     | 00..7F     |             |            |             |
    +--------------------+------------+-------------+------------+-------------+
    | U+0080..U+07FF     | C2..DF     | 80..BF      |            |             |
    +--------------------+------------+-------------+------------+-------------+
    | U+0800..U+0FFF     | E0         | A0..BF      | 80..BF     |             |
    +--------------------+------------+-------------+------------+-------------+
    | U+1000..U+CFFF     | E1..EC     | 80..BF      | 80..BF     |             |
    +--------------------+------------+-------------+------------+-------------+
    | U+D000..U+D7FF     | ED         | 80..9F      | 80..BF     |             |
    +--------------------+------------+-------------+------------+-------------+
    | U+E000..U+FFFF     | EE..EF     | 80..BF      | 80..BF     |             |
    +--------------------+------------+-------------+------------+-------------+
    | U+10000..U+3FFFF   | F0         | 90..BF      | 80..BF     | 80..BF      |
    +--------------------+------------+-------------+------------+-------------+
    | U+40000..U+FFFFF   | F1..F3     | 80..BF      | 80..BF     | 80..BF      |
    +--------------------+------------+-------------+------------+-------------+
    | U+100000..U+10FFFF | F4         | 80..8F      | 80..BF     | 80..BF      |
    +--------------------+------------+-------------+------------+-------------+
}
function Utf8Validate(const s: rawbytestring): Boolean;
var
  p, pEnd: PByte;
begin
  if s = '' then exit(True);
  p := Pointer(s);
  pEnd := p + System.Length(s);
  while p < pEnd do
    case p^ of
      0..$7f: Inc(p);
      $c2..$df:
        begin
          if not(p[1] in [$80..$bf]) then exit(False);
          p += 2;
        end;
      $e0:
        begin
          if not((p[1] in [$a0..$bf])and(p[2] in [$80..$bf])) then exit(False);
          p += 3;
        end;
      $e1..$ec, $ee..$ef:
        begin
          if not((p[1] in [$80..$bf])and(p[2] in [$80..$bf])) then exit(False);
          p += 3;
        end;
      $ed:
        begin
          if not((p[1] in [$80..$9f])and(p[2] in [$80..$bf])) then exit(False);
          p += 3;
        end;
      $f0:
        begin
          if not((p[1]in[$90..$bf])and(p[2]in[$80..$bf])and(p[3]in[$80..$bf]))then exit(False);
          p += 4;
        end;
      $f1..$f3:
        begin
          if not((p[1]in[$80..$bf])and(p[2]in[$80..$bf])and(p[3]in[$80..$bf]))then exit(False);
          p += 4;
        end;
      $f4:
        begin
          if not((p[1]in[$80..$8f])and(p[2]in[$80..$bf])and(p[3]in[$80..$bf]))then exit(False);
          p += 4;
        end;
    else
      exit(False);
    end;
  Result := True;
end;

function Utf8StrLen(const s: string): SizeInt;
begin
  Result := Utf8Len(s);
end;

procedure Ucs4Char2Utf8Buffer(var aBuf: PByte; c: DWord);
begin
  case c of
    0..127:
      begin
        aBuf[0] := Byte(c);
        Inc(aBuf);
      end;
    128..$7ff:
      begin
        aBuf[0] := Byte(c shr 6 or $c0);
        aBuf[1] := Byte(c and $3f or $80);
        aBuf += 2;
      end;
    $800..$d7ff, $e000..$ffff:
      begin
        aBuf[0] := Byte(c shr 12 or $e0);
        aBuf[1] := Byte(c shr 6) and $3f or $80;
        aBuf[2] := Byte(c and $3f) or $80;
        aBuf += 3;
      end;
    $10000..$10ffff:
      begin
        aBuf[0] := Byte(c shr 18) or $f0;
        aBuf[1] := Byte(c shr 12) and $3f or $80;
        aBuf[2] := Byte(c shr  6) and $3f or $80;
        aBuf[3] := Byte(c and $3f) or $80;
        aBuf += 4;
      end;
  else
    aBuf[0] := Ord('?');
    Inc(aBuf);
  end;
end;

function Utf8ToLower(const s: string): string;
var
  pv, pr, pEnd: PByte;
  LoC: DWord;
  PtSize: SizeInt;
  r : string;
begin
  if s = '' then exit('');
  System.SetLength(r, System.Length(s) * 2);// * 2 ???
  pv := PByte(s);
  pEnd := pv + System.Length(s);
  pr := PByte(r);
  PtSize := 0;
  while pv < pEnd do
    begin
      LoC := UnicodeData.GetProps(CodePointToUcs4Char(pv, PtSize))^.SimpleLowerCase;
      if LoC = 0 then
        begin
          case PtSize of
            1: pr^ := pv^;
            2: PByte2(pr)^ := PByte2(pv)^;
            3: PByte3(pr)^ := PByte3(pv)^;
            4: PByte4(pr)^ := PByte4(pv)^;
          else
          end;
          pr += PtSize;
        end
      else
        Ucs4Char2Utf8Buffer(pr, LoC);
      pv += PtSize;
    end;
  System.SetLength(r, pr - PByte(r));
  Result := r;
end;

function Utf8ToUpper(const s: string): string;
var
  pv, pr, pEnd: PByte;
  LoC: DWord;
  PtSize: SizeInt;
  r : string;
begin
  if s = '' then exit('');
  System.SetLength(r, System.Length(s) * 2);
  pv := PByte(s);
  pEnd := pv + System.Length(s);
  pr := PByte(r);
  PtSize := 0;
  while pv < pEnd do
    begin
      LoC := UnicodeData.GetProps(CodePointToUcs4Char(pv, PtSize))^.SimpleUpperCase;
      if LoC = 0 then
        begin
          case PtSize of
            1: pr^ := pv^;
            2: PByte2(pr)^ := PByte2(pv)^;
            3: PByte3(pr)^ := PByte3(pv)^;
            4: PByte4(pr)^ := PByte4(pv)^;
          else
          end;
          pr += PtSize;
        end
      else
        Ucs4Char2Utf8Buffer(pr, LoC);
      pv += PtSize;
    end;
  System.SetLength(r, pr - PByte(r));
  Result := r;
end;

function IsSubSequenceUtf8(const aStr, aSub: string): Boolean;
var
  I, J, PtSizeStr, PtSizeSub, LenStr, LenSub: SizeInt;
  cStr, cSub: Ucs4Char;
  pStr: PByte absolute aStr;
  pSub: PByte absolute aSub;
begin
  if aSub = '' then exit(True);
  LenStr := System.Length(aStr);
  LenSub := System.Length(aSub);
  I := 0;
  J := 0;
  cSub := CodePointToUcs4Char(pSub, PtSizeSub);
  while (I < LenStr) and (J < LenSub) do
    begin
      cStr := CodePointToUcs4Char(@pStr[I], PtSizeStr);
      if cStr = cSub then
        begin
          Inc(J, PtSizeSub);
          cSub := CodePointToUcs4Char(@pSub[J], PtSizeSub);
        end;
      Inc(I, PtSizeStr);
    end;
  Result := J = LenSub;
end;

function Utf8ToUcs4Seq(const s: string): TUcs4Seq;
begin
  Result := Utf8ToUcs4SeqImpl(s);
end;

function GenericDistanceUtf8(const L, R: string; aLimit: SizeInt; aSpec: TDistFunInternSpec): SizeInt;
var
  LBufSt, RBufSt: array[0..Pred(MAX_STATIC)] of Ucs4Char;
  LBuf: TUcs4Seq = nil;
  RBuf: TUcs4Seq = nil;
  LenL, LenR: SizeInt;
  pL, pR: PUcs4Char;
begin
  if System.Length(L) <= MAX_STATIC then
    begin
      pL := @LBufSt[0];
      Utf8ToUcs4SeqImpl(L, pL, LenL);
    end
  else
    begin
      LBuf := Utf8ToUcs4SeqImpl(L);
      LenL := System.Length(LBuf);
      pL := Pointer(LBuf);
    end;
  if System.Length(R) <= MAX_STATIC then
    begin
      pR := @RBufSt[0];
      Utf8ToUcs4SeqImpl(R, pR, LenR);
    end
  else
    begin
      RBuf := Utf8ToUcs4SeqImpl(R);
      LenR := System.Length(RBuf);
      pR := Pointer(RBuf);
    end;
  case aSpec of
    dfisDyn:           Result := TUcs4Util.LevDistance(pL[0..Pred(LenL)], pR[0..Pred(LenR)]);
    dfisMbr:           Result := TUcs4Util.LevDistanceMBR(pL[0..Pred(LenL)], pR[0..Pred(LenR)]);
    dfisMyers:         Result := TUcs4Util.LevDistanceMyers(pL[0..Pred(LenL)], pR[0..Pred(LenR)]);
    dfisMyersLcs:      Result := TUcs4Util.LcsDistanceMyers(pL[0..Pred(LenL)], pR[0..Pred(LenR)]);
    dfisMbrDum:        Result := TUcs4Util.DumDistanceMBR(pL[0..Pred(LenL)], pR[0..Pred(LenR)]);
    dfisMbrBound:      Result := TUcs4Util.LevDistanceMBR(pL[0..Pred(LenL)], pR[0..Pred(LenR)], aLimit);
    dfisMyersBound:    Result := TUcs4Util.LevDistanceMyers(pL[0..Pred(LenL)], pR[0..Pred(LenR)], aLimit);
    dfisMyersLcsBound: Result := TUcs4Util.LcsDistanceMyers(pL[0..Pred(LenL)], pR[0..Pred(LenR)], aLimit);
  else
    //dfisMbrDumBound
    Result := TUcs4Util.DumDistanceMBR(pL[0..Pred(LenL)], pR[0..Pred(LenR)], aLimit);
  end;
end;

function LevDistanceUtf8(const L, R: string): SizeInt;
begin
  Result := GenericDistanceUtf8(L, R, -1, dfisDyn);
end;

function LevDistanceMbrUtf8(const L, R: string): SizeInt;
begin
  Result := GenericDistanceUtf8(L, R, -1, dfisMbr);
end;

function LevDistanceMbrUtf8(const L, R: string; aLimit: SizeInt): SizeInt;
begin
  Result := GenericDistanceUtf8(L, R, aLimit, dfisMbrBound);
end;

function LevDistanceMyersUtf8(const L, R: string): SizeInt;
begin
  Result := GenericDistanceUtf8(L, R, -1, dfisMyers);
end;

function LevDistanceMyersUtf8(const L, R: string; aLimit: SizeInt): SizeInt;
begin
  Result := GenericDistanceUtf8(L, R, aLimit, dfisMyersBound);
end;

function LcsDistanceMyersUtf8(const L, R: string): SizeInt;
begin
  Result := GenericDistanceUtf8(L, R, -1, dfisMyersLcs);
end;

function LcsDistanceMyersUtf8(const L, R: string; aLimit: SizeInt): SizeInt;
begin
  Result := GenericDistanceUtf8(L, R, aLimit, dfisMyersLcsBound);
end;

function DumDistanceMbrUtf8(const L, R: string): SizeInt;
begin
  Result := GenericDistanceUtf8(L, R, -1, dfisMbrDum);
end;

function DumDistanceMbrUtf8(const L, R: string; aLimit: SizeInt): SizeInt;
begin
  Result := GenericDistanceUtf8(L, R, aLimit, dfisMbrDumBound);
end;

function Ucs4CharToUtf8Char(c: Ucs4Char; out aBytes: TByte4): Integer;
begin
  case c of
    0..127:
      begin
        aBytes[0] := Byte(c);
        Result := 1;
      end;
    128..$7ff:
      begin
        aBytes[0] := Byte(c shr 6 or $c0);
        aBytes[1] := Byte(c and $3f or $80);
        Result := 2;
      end;
    $800..$d7ff, $e000..$ffff:
      begin
        aBytes[0] := Byte(c shr 12 or $e0);
        aBytes[1] := Byte(c shr 6) and $3f or $80;
        aBytes[2] := Byte(c and $3f) or $80;
        Result := 3;
      end;
    $10000..$10ffff:
      begin
        aBytes[0] := Byte(c shr 18) or $f0;
        aBytes[1] := Byte(c shr 12) and $3f or $80;
        aBytes[2] := Byte(c shr  6) and $3f or $80;
        aBytes[3] := Byte(c and $3f) or $80;
        Result := 4;
      end;
  else
    aBytes[0] := Ord('?');
    Result := 1;
  end;
end;

function Ucs4CharUtf8Len(c: Ucs4Char): Integer; inline;
begin
  case c of
    0..127:          Result := 1;
    128..$7ff:       Result := 2;
    $800..$d7ff,
    $e000..$ffff:    Result := 3;
    $10000..$10ffff: Result := 4;
  else
    Result := 1;
  end;
end;

function Char32SeqUtf8Len(const r: TUcs4Seq): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I := 0 to System.High(r) do
    Result += Ucs4CharUtf8Len(r[I]);
end;

function Ucs4SeqToUtf8(const s: TUcs4Seq): string;
var
  r: string = '';
  I, J: SizeInt;
  Curr: Ucs4Char;
  Len: Integer;
  p: PByte;
  Bytes: TByte4;
begin
  System.SetLength(r, System.Length(s));
  p := Pointer(r);
  I := 0;
  for J := 0 to System.High(s) do
    begin
      Curr := s[J];
      Len := Ucs4CharToUtf8Char(Curr, Bytes);
      if System.Length(r) < I + Len then
        begin
          System.SetLength(r, (I + Len)*2);
          p := Pointer(r);
        end;
      case Len of
        1: p[I] := Byte(Curr);
        2: PByte2(@p[I])^ := PByte2(@Bytes)^;
        3: PByte3(@p[I])^ := PByte3(@Bytes)^;
      else
        // 4
        PByte4(@p[I])^ := Bytes;
      end;
      I += Len;
    end;
  System.SetLength(r, I);
  Result := r;
end;

function LcsGenegicUtf8(const L, R: string; aSpec: TUcs4Util.TLcsAlgo): string;
var
  LBufSt, RBufSt: array[0..Pred(MAX_STATIC)] of Ucs4Char;
  LBuf: TUcs4Seq = nil;
  RBuf: TUcs4Seq = nil;
  LenL, LenR: SizeInt;
  pL, pR: PUcs4Char;
begin
  if System.Length(L) <= MAX_STATIC then
    begin
      pL := @LBufSt[0];
      Utf8ToUcs4SeqImpl(L, pL, LenL);
    end
  else
    begin
      LBuf := Utf8ToUcs4SeqImpl(L);
      LenL := System.Length(LBuf);
      pL := Pointer(LBuf);
    end;
  if System.Length(R) <= MAX_STATIC then
    begin
      pR := @RBufSt[0];
      Utf8ToUcs4SeqImpl(R, pR, LenR);
    end
  else
    begin
      RBuf := Utf8ToUcs4SeqImpl(R);
      LenR := System.Length(RBuf);
      pR := Pointer(RBuf);
    end;
  case aSpec of
    laGus: Result := Ucs4SeqToUtf8(TUcs4Util.LcsGus(pL[0..Pred(LenL)], pR[0..Pred(LenR)]));
    laKR:  Result := Ucs4SeqToUtf8(TUcs4Util.LcsKR(pL[0..Pred(LenL)], pR[0..Pred(LenR)]));
  else
    Result := Ucs4SeqToUtf8(TUcs4Util.LcsMyers(pL[0..Pred(LenL)], pR[0..Pred(LenR)]));
  end;
end;

function LcsGusUtf8(const L, R: string): string;
begin
  Result := LcsGenegicUtf8(L, R, laGus);
end;

function LcsKrUtf8(const L, R: string): string;
begin
  Result := LcsGenegicUtf8(L, R, laKR);
end;

function LcsMyersUtf8(const L, R: string): string;
begin
  Result := LcsGenegicUtf8(L, R, laMyers);
end;

function SimRatioUtf8(const L, R: string; aLimit: Double; Algo: TSeqDistanceAlgo): Double;
var
  LBufSt, RBufSt: array[0..Pred(MAX_STATIC)] of Ucs4Char;
  LBuf: TUcs4Seq = nil;
  RBuf: TUcs4Seq = nil;
  LenL, LenR: SizeInt;
  pL, pR: PUcs4Char;
begin
  if System.Length(L) <= MAX_STATIC then
    begin
      pL := @LBufSt[0];
      Utf8ToUcs4SeqImpl(L, pL, LenL);
    end
  else
    begin
      LBuf := Utf8ToUcs4SeqImpl(L);
      LenL := System.Length(LBuf);
      pL := Pointer(LBuf);
    end;
  if System.Length(R) <= MAX_STATIC then
    begin
      pR := @RBufSt[0];
      Utf8ToUcs4SeqImpl(R, pR, LenR);
    end
  else
    begin
      RBuf := Utf8ToUcs4SeqImpl(R);
      LenR := System.Length(RBuf);
      pR := Pointer(RBuf);
    end;
  Result := TUcs4Util.SimRatio(pL[0..Pred(LenL)], pR[0..Pred(LenR)], aLimit, Algo);
end;

function IsSingleCodePointUtf8(const s: rawbytestring; out aPt: Ucs4Char): Boolean;
var
  PtLen: SizeInt;
begin
  if s = '' then exit(False);
  aPt := CodePointToUcs4Char(Pointer(s), PtLen);
  Result := PtLen = System.Length(s);
end;

function SimRatioExUtf8(const L, R: string; const aStopChars: array of string; aMode: TSimMode;
  const aOptions: TSimOptions; aLimit: Double; Algo: TSeqDistanceAlgo; aLess: TUcs4Less): Double;
var
  LBufSt, RBufSt: array[0..Pred(MAX_STATIC)] of Ucs4Char;
  LBuf: TUcs4Seq = nil;
  RBuf: TUcs4Seq = nil;
  LenL, LenR, I: SizeInt;
  pL, pR: PUcs4Char;

  StopChars: TUcs4CharSet;
  LocL, LocR: string;
  c: Ucs4Char;
  p: TUcs4CharSet.PEntry;
begin

  if soIgnoreCase in aOptions then
    begin
      LocL := Utf8ToLower(L);
      LocR := Utf8ToLower(R);
    end
  else
    begin
      LocL := L;
      LocR := R;
    end;

  for I := 0 to System.High(aStopChars) do
    if IsSingleCodePointUtf8(aStopChars[I], c) and not StopChars.FindOrAdd(c, p) then
      p^.Key := c;

  if System.Length(LocL) <= MAX_STATIC then
    begin
      pL := @LBufSt[0];
      Utf8ToUcs4SeqImpl(LocL, pL, LenL);
    end
  else
    begin
      LBuf := Utf8ToUcs4SeqImpl(LocL);
      LenL := System.Length(LBuf);
      pL := Pointer(LBuf);
    end;
  if System.Length(LocR) <= MAX_STATIC then
    begin
      pR := @RBufSt[0];
      Utf8ToUcs4SeqImpl(LocR, pR, LenR);
    end
  else
    begin
      RBuf := Utf8ToUcs4SeqImpl(LocR);
      LenR := System.Length(RBuf);
      pR := Pointer(RBuf);
    end;

  Result :=
    SimRatioGeneric(
      pL[0..Pred(LenL)], pR[0..Pred(LenR)], StopChars, aMode, soPartial in aOptions, aLimit, Algo, aLess);
end;

function SimRatioListUtf8(const aPattern: string; const aValues: array of string;
  const aStopChars: array of string; aMode: TSimMode; const aOptions: TSimOptions; aLimit: Double;
  Algo: TSeqDistanceAlgo; aLess: TUcs4Less): specialize TGArray<Double>;
var
  LBufSt, RBufSt: array[0..Pred(MAX_STATIC)] of Ucs4Char;
  LBuf: TUcs4Seq = nil;
  RBuf: TUcs4Seq = nil;
  LenL, LenR, I: SizeInt;
  pL, pR: PUcs4Char;

  StopChars: TUcs4CharSet;
  Pattern, Value: string;
  c: Ucs4Char;
  p: TUcs4CharSet.PEntry;
  r: array of Double;
begin
  if System.Length(aValues) < 1 then exit(nil);

  if soIgnoreCase in aOptions then
    Pattern := Utf8ToLower(aPattern)
  else
    Pattern := aPattern;

  for I := 0 to System.High(aStopChars) do
    if IsSingleCodePointUtf8(aStopChars[I], c) and not StopChars.FindOrAdd(c, p) then
      p^.Key := c;

  if System.Length(Pattern) <= MAX_STATIC then
    begin
      pL := @LBufSt[0];
      Utf8ToUcs4SeqImpl(Pattern, pL, LenL);
    end
  else
    begin
      LBuf := Utf8ToUcs4SeqImpl(Pattern);
      LenL := System.Length(LBuf);
      pL := Pointer(LBuf);
    end;

  System.SetLength(r, System.Length(aValues));

  for I := 0 to System.High(aValues) do begin
    if soIgnoreCase in aOptions then
      Value := Utf8ToLower(aValues[I])
    else
      Value := aValues[I];

    if System.Length(Value) <= MAX_STATIC then
      begin
        pR := @RBufSt[0];
        Utf8ToUcs4SeqImpl(Value, pR, LenR);
      end
    else
      begin
        RBuf := Utf8ToUcs4SeqImpl(Value);
        LenR := System.Length(RBuf);
        pR := Pointer(RBuf);
      end;
    r[I] := SimRatioGeneric(
      pL[0..Pred(LenL)], pR[0..Pred(LenR)], StopChars, aMode, soPartial in aOptions, aLimit, Algo, aLess);
  end;

  Result := r;
end;

function SelectSimilarUtf8(const aPattern: string; const aValues: array of string;
  const aStopChars: array of string; aLimit: Double; aMode: TSimMode; const aOptions: TSimOptions;
  Algo: TSeqDistanceAlgo; aLess: TUcs4Less): specialize TGArray<TStringRatio>;
  function Less(const L, R: TStringRatio): Boolean;
  begin
    Result := R.Ratio < L.Ratio;
  end;
var
  ratios: array of Double;
  r: array of TStringRatio;
  I, J: SizeInt;
begin
  ratios := SimRatioListUtf8(aPattern, aValues, aStopChars, aMode, aOptions, aLimit, Algo, aLess);
  System.SetLength(r, System.Length(ratios));
  J := 0;
  for I := 0 to System.High(ratios) do
    if ratios[I] > Double(0.0) then begin
      with r[J] do begin
        Value := aValues[I];
        Ratio := ratios[I];
      end;
      Inc(J);
    end;
  System.SetLength(r, J);
  specialize TGNestedArrayHelper<TStringRatio>.Sort(r, @Less);
  Result := r;
end;

{ TFuzzySearchEdp.TEnumerator }

function TFuzzySearchEdp.TEnumerator.GetCurrent: SizeInt;
begin
  Result := FPointIndex;
end;

function TFuzzySearchEdp.TEnumerator.MoveNext: Boolean;
var
  TextLen, I, Cost, Err: SizeInt;
  c: Ucs4Char;
begin
  TextLen := System.Length(FText);
  while FTextIndex <= TextLen do
    begin
      c := CodePointToUcs4Char(@FText[FTextIndex], I);
      FTextIndex += I;
      Inc(FPointIndex);
      Cost := 0;
      for I := 1 to FTop do
        begin
          if FPattern[I-1] = c then
            Err := Cost
          else
            Err := Succ(lgUtils.MinOf3(FD[I-1], FD[I], Cost));
          Cost := FD[I];
          FD[I] := Err;
        end;
      while FD[FTop] > FK do Dec(FTop);
      if FTop = System.Length(FPattern) then
        exit(True)
      else
        Inc(FTop);
    end;
  Result := False;
end;

{ TFuzzySearchEdp.TMatches }

function TFuzzySearchEdp.TMatches.GetEnumerator: TEnumerator;
var
  I: Integer;
begin
  Result.FPattern := nil;
  Result.FD := nil;
  Result.FText := '';
  Result.FTextIndex := 1;
  Result.FPointIndex := 0;
  if not FSearch^.Initialized or (SizeUInt(FK) >= SizeUInt(System.Length(FSearch^.FPattern))) then exit;
  Result.FPattern := FSearch^.FPattern;
  Result.FK := FK;
  Result.FTop := Succ(FK);
  Result.FText := FText;
  with FSearch^ do
    for I := 1 to System.High(FD) do
      FD[I] := I;
  Result.FD := FSearch^.FD;
end;

{ TFuzzySearchEdp }

function TFuzzySearchEdp.GetInitialized: Boolean;
begin
  Result := FPattern <> nil;
end;

function TFuzzySearchEdp.GetLength: SizeInt;
begin
  Result := System.Length(FPattern);
end;

constructor TFuzzySearchEdp.Create(const aPattern: string);
begin
  Init(aPattern);
end;

procedure TFuzzySearchEdp.Init(const aPattern: string);
begin
  FPattern := nil;
  FD := nil;
  if (aPattern = '') or not Utf8Validate(aPattern) then exit;
  FPattern := Utf8ToUcs4Seq(aPattern);
  System.SetLength(FD, Succ(System.Length(FPattern)));
end;

function TFuzzySearchEdp.Matches(const aText: string; K: SizeInt): TMatches;
begin
  Result.FText := aText;
  Result.FK := K;
  Result.FSearch := @Self;
end;

{ TFuzzySearchBitap.TMatches }

function TFuzzySearchBitap.TMatches.GetEnumerator: TEnumerator;
begin
  Result.Init(FSearch, FText, FOfs, FK);
end;

{ TFuzzySearchBitap }

function TFuzzySearchBitap.GetInitialized: Boolean;
begin
  Result := FCharMap.NonEmpty;
end;

function TFuzzySearchBitap.GetLength: Integer;
begin
  if FCharMap.NonEmpty then
    Result := FLength
  else
    Result := 0;
end;

function TFuzzySearchBitap.GetCaseInsensitive: Boolean;
begin
  if FCharMap.NonEmpty then
    Result := FIgnoreCase
  else
    Result := False;
end;

{$PUSH}{$WARN 5057 OFF}
procedure TFuzzySearchBitap.DoSearch(const aText: string; K: Integer; aOffset: SizeInt; aFound: TNestFound);
var
  Table: array[0..MAX_PATTERN_CP] of QWord;
  Queue: array[0..Pred(MAX_PATTERN_CP)] of SizeInt;
  TextPos, TextLen, cLen: SizeInt;
  vOld, vTemp, cMask, TestBit: QWord;
  I, qHead, PatLen: Integer;
  c: Ucs4Char;
begin
  if not Initialized or (DWord(K) >= DWord(MAX_PATTERN_CP)) or (aText = '') then
    exit;
  if aOffset < 1 then aOffset := 1;
  TextLen := System.Length(aText);
  if aOffset > TextLen then
    exit;
  PatLen := Length;
  K := Math.Min(K, PatLen);
  System.FillQWord(Table[0], K + 1, not QWord(1));
  qHead := 0;
  TextPos := aOffset;
  TestBit := QWord(1) shl PatLen;
  while TextPos <= TextLen do begin
    Queue[qHead] := TextPos;
    Inc(qHead);
    if qHead = PatLen then
      qHead := 0;
    c := CodePointToUcs4Char(@aText[TextPos], cLen);
    TextPos += cLen;
    cMask := FCharMap.GetValueDef(c, System.High(QWord));
    vOld := Table[0];
    Table[0] := (Table[0] or cMask) shl 1;
    for I := 1 to K do begin
      vTemp := Table[I];
      Table[I] := (vOld and (Table[I] or cMask)) shl 1;
      vOld := vTemp;
    end;
    if (Table[K] and TestBit = 0) and not
      aFound(TMatch.Make(Queue[qHead], TextPos - Queue[qHead])) then exit;
  end;
end;
{$POP}

{ TFuzzySearchBitap.TEnumerator }

function TFuzzySearchBitap.TEnumerator.GetCurrent: TMatch;
begin
  Result := FMatch;
end;

procedure TFuzzySearchBitap.TEnumerator.Init(pSearch: PSearchBitap; const aText: string; aOfs: SizeInt;
  aK: Integer);
begin
  FText := '';
  FTable := nil;
  FQueue := nil;
  FSearch := nil;
  FqHead := 0;
  FMatch := TMatch.Make(0, 0);
  if (pSearch = nil ) or not pSearch^.Initialized or (aText = '') then exit;
  if aOfs < 1 then aOfs := 1;
  if aOfs > System.Length(aText) then exit;
  if DWord(aK) >= DWord(MAX_PATTERN_CP) then exit;
  aK := Math.Min(aK, pSearch^.Length);
  FTextIndex := aOfs;
  FText := aText;
  FSearch := pSearch;
  FTable := THelper.CreateAndFill(not QWord(1), aK + 1);
  System.SetLength(FQueue, pSearch^.Length);
end;

function TFuzzySearchBitap.TEnumerator.MoveNext: Boolean;
var
  TextLen, cLen: SizeInt;
  vOld, vTemp, cMask, TestBit: QWord;
  PatLen, I, K: Integer;
  c: Ucs4Char;
begin
  if FSearch = nil then exit(False);
  TextLen := System.Length(FText);
  PatLen := System.Length(FQueue);
  K := System.High(FTable);
  TestBit := QWord(1) shl PatLen;
  while FTextIndex <= TextLen do begin
    FQueue[FqHead] := FTextIndex;
    Inc(FqHead);
    if FqHead = PatLen then
      FqHead := 0;
    c := CodePointToUcs4Char(@FText[FTextIndex], cLen);
    FTextIndex += cLen;
    cMask := FSearch^.FCharMap.GetValueDef(c, System.High(QWord));
    vOld := FTable[0];
    FTable[0] := (FTable[0] or cMask) shl 1;
    for I := 1 to K do begin
      vTemp := FTable[I];
      FTable[I] := (vOld and (FTable[I] or cMask)) shl 1;
      vOld := vTemp;
    end;
    if FTable[K] and TestBit = 0 then begin
      FMatch := TMatch.Make(FQueue[FqHead], FTextIndex - FQueue[FqHead]);
      exit(True);
    end;
  end;
  Result := False;
end;

constructor TFuzzySearchBitap.Create(const aPattern: string; aIgnoreCase: Boolean);
begin
  Init(aPattern, aIgnoreCase);
end;

procedure TFuzzySearchBitap.Init(const aPattern: string; aIgnoreCase: Boolean);
var
  I, CharIdx, PatLen, CharLen: SizeInt;
  p: PQWord;
  pProp: PUC_Prop;
  c, c2: Ucs4Char;
begin
  FCharMap := Default(TCharMap);
  FLength := 0;
  if (aPattern = '') or not Utf8Validate(aPattern) then exit;
  I := Utf8Len(aPattern);
  if I > MAX_PATTERN_CP then exit;
  FLength := I;
  FIgnoreCase := aIgnoreCase;
  I := 1;
  CharIdx := 0;
  PatLen := System.Length(aPattern);
  while I <= PatLen do begin
    c := CodePointToUcs4Char(@aPattern[I], CharLen);
    I += CharLen;
    p := FCharMap.GetMutValueDef(c, System.High(QWord));
    p^ := p^ and not(QWord(1) shl CharIdx);
    if aIgnoreCase then begin
      pProp := UnicodeData.GetProps(c);
      c2 := DWord(pProp^.SimpleLowerCase);
      if c2 <> 0 then begin
        p := FCharMap.GetMutValueDef(c2, System.High(QWord));
        p^ := p^ and not(QWord(1) shl CharIdx);
      end else begin
        c2 := DWord(pProp^.SimpleUpperCase);
        if c2 <> 0 then begin
          p := FCharMap.GetMutValueDef(c2, System.High(QWord));
          p^ := p^ and not(QWord(1) shl CharIdx);
        end;
      end;
    end;
    Inc(CharIdx);
  end;
end;

function TFuzzySearchBitap.NextMatch(const aText: string; K: Integer; aOffset: SizeInt): TMatch;
var
  m: TMatch;
  function Found(const aMatch: TMatch): Boolean;
  begin
    m := aMatch;
    Found := False;
  end;
begin
  m := TMatch.Make(0, 0);
  DoSearch(aText, K, aOffset, @Found);
  Result := m;
end;

function TFuzzySearchBitap.FindMatches(const aText: string; K: Integer; aOffset: SizeInt;
  aLimit: SizeInt): specialize TGArray<TMatch>;
var
  r: array of TMatch = nil;
  rLen: SizeInt = 0;
  function AddMatch(const aMatch: TMatch): Boolean;
  begin
    if System.Length(r) = rLen then
      System.SetLength(r, rLen * 2);
    r[rLen] := aMatch;
    Inc(rLen);
    AddMatch := rLen < aLimit;
  end;
begin
  System.SetLength(r, ARRAY_INITIAL_SIZE);
  if aLimit < 1 then
    aLimit := System.High(SizeInt);
  DoSearch(aText, K, aOffset, @AddMatch);
  System.SetLength(r, rLen);
  Result := r;
end;

function TFuzzySearchBitap.Matches(const aText: string; K: Integer; aOffset: SizeInt): TMatches;
begin
  if Initialized then begin
    Result.FSearch := @Self;
    Result.FText := aText;
    Result.FOfs := aOffset;
    Result.FK := K;
  end else begin
    Result.FSearch := nil;
    Result.FText := '';
    Result.FOfs := 0;
    Result.FK := 0;
  end;
end;

type
  { TACSearchUtf8Dfa }
  TACSearchUtf8Dfa = class(TInterfacedObject, IACSearchUtf8)
  private
  type
    TNode = record
      NextMove: array of SizeInt;// transition table
      Failure,                   // failure link
      Output,                    // output link
      Length: SizeInt;           // length(in code point) > 0 indicates a terminal node
    end;
  const
    BMP_MAX  = $ffff;
    MAX_CODE = High(ShortInt);
  private
    FTrie: array of TNode;
    FCodeTable: array[0..BMP_MAX] of ShortInt;
    FQueue: array of SizeInt;
    FMaxPatternLen,
    FNodeCount,
    FWordCount: SizeInt;
    FAlphabetSize: Integer;
    FIgnoreCase: Boolean;
    function  NewNode: SizeInt;
    procedure AddPattern(const aValue: string);
    function  BuildCodeTable(const aList: array of string): Boolean;
    procedure BuildLinks;
    procedure DoSearch(const s: string; aOnMatch: TOnMatch; aOffset, aCount: SizeInt);
    procedure DoSearch(const s: string; aOnMatch: TNestMatch; aOffset, aCount: SizeInt);
    procedure DoSearchCI(const s: string; aOnMatch: TOnMatch; aOffset, aCount: SizeInt);
    procedure DoSearchCI(const s: string; aOnMatch: TNestMatch; aOffset, aCount: SizeInt);
  public
    constructor Create(aIgnoreCase: Boolean = False);
    function  TryBuildFsm(const aList: array of string): Boolean;
    function  GetNodeCount: SizeInt;
    function  GetPatternCount: SizeInt;
    function  GetCaseInsensitive: Boolean;
    function  ContainsPattern(const aValue: string): Boolean;
    function  FindMatches(const aText: string; aOffset: SizeInt = 1; aCount: SizeInt = 0): specialize TGArray<TSeqMatch>;
    procedure Search(const aText: string; aOnMatch: TOnMatch; aOffset: SizeInt = 1; aCount: SizeInt = 0);
    procedure Search(const aText: string; aOnMatch: TNestMatch; aOffset: SizeInt = 1; aCount: SizeInt = 0);
    function  ContainsMatch(const aText: string; aOffset: SizeInt = 1; aCount: SizeInt = 0): Boolean;
  end;

  { TACSearchUtf8Nfa }
  TACSearchUtf8Nfa = class(TInterfacedObject, IACSearchUtf8)
  private
  type
    TNode = record
    type
      TCharMapType = specialize TGLiteChainHashMap<Ucs4Char, SizeInt, TUcs4Hasher>;
      TNextMove    = TCharMapType.TMap;
    public
      NextMove: TNextMove;// transition table
      Failure,            // failure link
      Output,             // output link
      Length: SizeInt;    // length(in code point) > 0 indicates a terminal node
    end;
  private
    FTrie: array of TNode;
    FQueue: array of SizeInt;
    FMaxPatternLen,
    FNodeCount,
    FWordCount: SizeInt;
    FIgnoreCase: Boolean;
    function  NewNode: SizeInt;
    procedure AddPattern(const aValue: string);
    procedure BuildLinks;
    procedure DoSearch(const s: string; aOnMatch: TOnMatch; aOffset, aCount: SizeInt);
    procedure DoSearch(const s: string; aOnMatch: TNestMatch; aOffset, aCount: SizeInt);
    procedure DoSearchCI(const s: string; aOnMatch: TOnMatch; aOffset, aCount: SizeInt);
    procedure DoSearchCI(const s: string; aOnMatch: TNestMatch; aOffset, aCount: SizeInt);
  public
    constructor Create(const aPatternList: array of string; aIgnoreCase: Boolean = False);
    function  GetNodeCount: SizeInt;
    function  GetPatternCount: SizeInt;
    function  GetCaseInsensitive: Boolean;
    function  ContainsPattern(const aValue: string): Boolean;
    function  FindMatches(const aText: string; aOffset: SizeInt = 1; aCount: SizeInt = 0): specialize TGArray<TSeqMatch>;
    procedure Search(const aText: string; aOnMatch: TOnMatch; aOffset: SizeInt = 1; aCount: SizeInt = 0);
    procedure Search(const aText: string; aOnMatch: TNestMatch; aOffset: SizeInt = 1; aCount: SizeInt = 0);
    function  ContainsMatch(const aText: string; aOffset: SizeInt = 1; aCount: SizeInt = 0): Boolean;
  end;

{ TACSearchUtf8Dfa }

function TACSearchUtf8Dfa.NewNode: SizeInt;
begin
  if FNodeCount = System.Length(FTrie) then
    System.SetLength(FTrie, FNodeCount * 2);
  System.SetLength(FTrie[FNodeCount].NextMove, FAlphabetSize);
  Result := FNodeCount;
  Inc(FNodeCount);
end;

procedure TACSearchUtf8Dfa.AddPattern(const aValue: string);
var
  Curr, Next, Len, LenUtf8: SizeInt;
  p, pEnd: PByte;
  c, Loc: Ucs4Char;
  Code: Integer;
begin
  if aValue = '' then exit;
  Curr := 0;
  p := Pointer(aValue);
  pEnd := p + System.Length(aValue);
  Len := 0;
  LenUtf8 := 0;
  while p < pEnd do
    begin
      c := CodePointToUcs4Char(p, Len);
      p += Len;
      Inc(LenUtf8);
      if FIgnoreCase then
        begin
          Loc := DWord(UnicodeData.GetProps(c)^.SimpleLowerCase);
          if Loc <> 0 then
            c := Loc;
        end;
      Code := FCodeTable[c];
      Next := FTrie[Curr].NextMove[Code];
      if Next = 0 then
        begin
          Next := NewNode;
          FTrie[Curr].NextMove[Code] := Next;
        end;
      Curr := Next;
    end;
  FWordCount += Ord(FTrie[Curr].Length = 0);
  if LenUtf8 > FMaxPatternLen then
    FMaxPatternLen := LenUtf8;
  if FIgnoreCase then
    FTrie[Curr].Length := LenUtf8
  else
    FTrie[Curr].Length := System.Length(aValue);
end;

function TACSearchUtf8Dfa.BuildCodeTable(const aList: array of string): Boolean;
var
  I, Len: SizeInt;
  p, pEnd: PByte;
  c, Loc: Ucs4Char;
begin
  System.FillChar(FCodeTable, SizeOf(FCodeTable), $ff);
  for I := 0 to System.High(aList) do
    begin
      p := Pointer(aList[I]);
      pEnd := p + System.Length(aList[I]);
      while p < pEnd do
        begin
          if FAlphabetSize = MAX_CODE then exit(False);
          c := CodePointToUcs4Char(p, Len);
          if c > BMP_MAX then exit(False);
          p += Len;
          if FIgnoreCase then
            begin
              Loc := DWord(UnicodeData.GetProps(c)^.SimpleLowerCase);
              if Loc <> 0 then
                c := Loc;
            end;
          if FCodeTable[c] = -1 then
            begin
              FCodeTable[c] := FAlphabetSize;
              Inc(FAlphabetSize);
            end;
        end;
    end;
  Result := True;
end;

procedure TACSearchUtf8Dfa.BuildLinks;
var
  Queue: specialize TGLiteQueue<SizeInt>;
  Curr, Next, Fail, Link: SizeInt;
  c: Integer;
begin
  for Curr in FTrie[0].NextMove do
    if Curr <> 0 then
      Queue.Enqueue(Curr);
  while Queue.TryDequeue(Curr) do
    for c := 0 to Pred(FAlphabetSize) do
      if FTrie[Curr].NextMove[c] <> 0 then
        begin
          Next := FTrie[Curr].NextMove[c];
          Queue.Enqueue(Next);
          Fail := FTrie[Curr].Failure;
          repeat
            Link := FTrie[Fail].NextMove[c];
            if Link <> 0 then
              begin
                FTrie[Next].Failure := Link;
                if FTrie[Link].Length <> 0 then
                  FTrie[Next].Output := Link
                else
                  FTrie[Next].Output := FTrie[Link].Output;
                break;
              end;
            if Fail = 0 then break;
            Fail := FTrie[Fail].Failure;
          until False;
        end
      else
        FTrie[Curr].NextMove[c] := FTrie[FTrie[Curr].Failure].NextMove[c];
end;

{$PUSH}{$MACRO ON}
procedure TACSearchUtf8Dfa.DoSearch(const s: string; aOnMatch: TOnMatch; aOffset, aCount: SizeInt);
var
  State, Tmp, cLen: SizeInt;
  p, pEnd, pText: PByte;
  c: Ucs4Char;
  Code: Integer;
begin
{$DEFINE DoSearchMacro :=
  if aOffset < 1 then aOffset := 1;
  if aCount < 1 then
    aCount := System.Length(s)
  else
    aCount := Math.Min(Pred(aOffset + aCount), System.Length(s));
  pText := Pointer(s);
  p := pText + Pred(aOffset);
  pEnd := pText + aCount;
  State := 0;
  while p < pEnd do begin
    c := CodePointToUcs4Char(p, cLen);
    p += cLen;
    if c > BMP_MAX then begin
      State := 0; continue;
    end;
    Code := FCodeTable[c];
    if Code = -1 then begin
      State := 0; continue;
    end;
    State := FTrie[State].NextMove[Code];
    if State = 0 then continue;
    if FTrie[State].Length <> 0 then
      if not aOnMatch(TMatch.Make(Succ(p - pText - FTrie[State].Length), FTrie[State].Length)) then
        exit;
    Tmp := State;
    while FTrie[Tmp].Output <> 0 do begin
      Tmp := FTrie[Tmp].Output;
      if not aOnMatch(TMatch.Make(Succ(p - pText - FTrie[Tmp].Length), FTrie[Tmp].Length)) then
        exit;
    end;
  end
}
  DoSearchMacro;
end;

procedure TACSearchUtf8Dfa.DoSearch(const s: string; aOnMatch: TNestMatch; aOffset, aCount: SizeInt);
var
  State, Tmp, cLen: SizeInt;
  p, pEnd, pText: PByte;
  c: Ucs4Char;
  Code: Integer;
begin
  DoSearchMacro;
end;
{$UNDEF DoSearchMacro}

procedure TACSearchUtf8Dfa.DoSearchCI(const s: string; aOnMatch: TOnMatch; aOffset, aCount: SizeInt);
var
  State, Tmp, cLen, qTop, qLen, qPos: SizeInt;
  pQueue: PSizeInt;
  p, pEnd, pText: PByte;
  c, Loc: Ucs4Char;
  Code: Integer;
begin
{$DEFINE DoSearchCIMacro :=
  if aOffset < 1 then aOffset := 1;
  if aCount < 1 then
    aCount := System.Length(s)
  else
    aCount := Math.Min(Pred(aOffset + aCount), System.Length(s));
  pText := Pointer(s);
  p := pText + Pred(aOffset);
  pEnd := pText + aCount;
  pQueue := Pointer(FQueue);
  qLen := FMaxPatternLen;
  qTop := 0;
  State := 0;
  while p < pEnd do begin
    pQueue[qTop] := Succ(p - pText);
    Inc(qTop);
    if qTop = qLen then qTop := 0;
    c := CodePointToUcs4Char(p, cLen);
    p += cLen;
    if c > BMP_MAX then begin
      State := 0; continue;
    end;
    Loc := DWord(UnicodeData.GetProps(c)^.SimpleLowerCase);
    if Loc <> 0 then c := Loc;
    Code := FCodeTable[c];
    if Code = -1 then begin
      State := 0; continue;
    end;
    State := FTrie[State].NextMove[Code];
    if State = 0 then continue;
    if FTrie[State].Length <> 0 then begin
      qPos := qTop - FTrie[State].Length;
      if qPos < 0 then qPos += qLen;
      if not aOnMatch(TMatch.Make(pQueue[qPos], Succ(p - pText) - pQueue[qPos])) then exit;
    end;
    Tmp := State;
    while FTrie[Tmp].Output <> 0 do begin
      Tmp := FTrie[Tmp].Output;
      qPos := qTop - FTrie[Tmp].Length;
      if qPos < 0 then qPos += qLen;
      if not aOnMatch(TMatch.Make(pQueue[qPos], Succ(p - pText) - pQueue[qPos])) then exit;
    end;
  end
}
  DoSearchCIMacro;
end;

procedure TACSearchUtf8Dfa.DoSearchCI(const s: string; aOnMatch: TNestMatch; aOffset, aCount: SizeInt);
var
  State, Tmp, cLen, qTop, qLen, qPos: SizeInt;
  pQueue: PSizeInt;
  p, pEnd, pText: PByte;
  c, Loc: Ucs4Char;
  Code: Integer;
begin
  DoSearchCIMacro;
end;
{$UNDEF DoSearchCIMacro}{$POP}

constructor TACSearchUtf8Dfa.Create(aIgnoreCase: Boolean);
begin
  inherited Create;
  FIgnoreCase := aIgnoreCase;
end;

function TACSearchUtf8Dfa.TryBuildFsm(const aList: array of string): Boolean;
var
  s: string;
begin
  if not BuildCodeTable(aList) then exit(False);
  System.SetLength(FTrie, ARRAY_INITIAL_SIZE);
  NewNode;
  for s in aList do
    AddPattern(s);
  System.SetLength(FTrie, FNodeCount);
  if FIgnoreCase then
    System.SetLength(FQueue, FMaxPatternLen);
  BuildLinks;
  Result := True;
end;

function TACSearchUtf8Dfa.GetNodeCount: SizeInt;
begin
  Result := FNodeCount;
end;

function TACSearchUtf8Dfa.GetPatternCount: SizeInt;
begin
  Result := FWordCount;
end;

function TACSearchUtf8Dfa.GetCaseInsensitive: Boolean;
begin
  Result := FIgnoreCase;
end;

function TACSearchUtf8Dfa.ContainsPattern(const aValue: string): Boolean;
var
  Curr, Next, Len: SizeInt;
  p, pEnd: PByte;
  c, Loc: Ucs4Char;
  Code: Integer;
begin
  Curr := 0;
  p := Pointer(aValue);
  pEnd := p + System.Length(aValue);
  while p < pEnd do
    begin
      c := CodePointToUcs4Char(p, Len);
      if c > BMP_MAX then exit(False);
      p += Len;
      if FIgnoreCase then
        begin
          Loc := DWord(UnicodeData.GetProps(c)^.SimpleLowerCase);
          if Loc <> 0 then c := Loc;
        end;
      Code := FCodeTable[c];
      if Code = -1 then exit(False);
      Next := FTrie[Curr].NextMove[Code];
      if Next = 0 then exit(False);
      Curr := Next;
    end;
  Result := FTrie[Curr].Length <> 0;
end;

function TACSearchUtf8Dfa.FindMatches(const aText: string; aOffset, aCount: SizeInt): specialize TGArray<TSeqMatch>;
var
  Matches: array of TMatch = nil;
  mCount: SizeInt = 0;
  function Collect(const m: TMatch): Boolean;
  begin
    if mCount = System.Length(Matches) then
      System.SetLength(Matches, mCount * 2);
    Matches[mCount] := m;
    Inc(mCount);
    Result := True;
  end;
begin
  System.SetLength(Matches, ARRAY_INITIAL_SIZE);
  Search(aText, @Collect, aOffset, aCount);
  System.SetLength(Matches, mCount);
  Result := Matches;
end;

procedure TACSearchUtf8Dfa.Search(const aText: string; aOnMatch: TOnMatch; aOffset, aCount: SizeInt);
begin
  if FIgnoreCase then
    DoSearchCI(aText, aOnMatch, aOffset, aCount)
  else
    DoSearch(aText, aOnMatch, aOffset, aCount);
end;

procedure TACSearchUtf8Dfa.Search(const aText: string; aOnMatch: TNestMatch; aOffset, aCount: SizeInt);
begin
  if FIgnoreCase then
    DoSearchCI(aText, aOnMatch, aOffset, aCount)
  else
    DoSearch(aText, aOnMatch, aOffset, aCount);
end;

function TACSearchUtf8Dfa.ContainsMatch(const aText: string; aOffset: SizeInt;
  aCount: SizeInt): Boolean;
var
  State, cLen: SizeInt;
  p, pEnd: PByte;
  c, Loc: Ucs4Char;
  Code: Integer;
begin
  if aOffset < 1 then aOffset := 1;
  if aCount < 1 then
    aCount := System.Length(aText)
  else
    aCount := Math.Min(Pred(aOffset + aCount), System.Length(aText));
  p := PByte(aText) + Pred(aOffset);
  pEnd := PByte(aText) + aCount;
  State := 0;
  while p < pEnd do
    begin
      c := CodePointToUcs4Char(p, cLen);
      p += cLen;
      if c > BMP_MAX then begin
        State := 0; continue;
      end;
      if FIgnoreCase then begin
        Loc := DWord(UnicodeData.GetProps(c)^.SimpleLowerCase);
        if Loc <> 0 then c := Loc;
      end;
      Code := FCodeTable[c];
      if Code = -1 then begin
        State := 0; continue;
      end;
      State := FTrie[State].NextMove[Code];
      if State = 0 then continue;
      if FTrie[State].Length <> 0 then exit(True);
      if FTrie[State].Output <> 0 then exit(True);
    end;
  Result := False;
end;

{ TACSearchUtf8Nfa }

function TACSearchUtf8Nfa.NewNode: SizeInt;
begin
  if FNodeCount = System.Length(FTrie) then
    System.SetLength(FTrie, FNodeCount * 2);
  Result := FNodeCount;
  Inc(FNodeCount);
end;

procedure TACSearchUtf8Nfa.AddPattern(const aValue: string);
var
  Curr, cLen, LenUtf8: SizeInt;
  pNext: PSizeInt;
  p, pEnd: PByte;
  c, Loc: Ucs4Char;
begin
  if aValue = '' then exit;
  Curr := 0;
  LenUtf8 := 0;
  p := Pointer(aValue);
  pEnd := p + System.Length(aValue);
  while p < pEnd do
    begin
      c := CodePointToUcs4Char(p, cLen);
      p += cLen;
      Inc(LenUtf8);
      if FIgnoreCase then
        begin
          Loc := DWord(UnicodeData.GetProps(c)^.SimpleLowerCase);
          if Loc <> 0 then c := Loc;
        end;
      pNext := FTrie[Curr].NextMove.GetMutValueDef(c, NULL_INDEX);
      if pNext^ = NULL_INDEX then
        pNext^ := NewNode;
      Curr := pNext^;
    end;
  FWordCount += Ord(FTrie[Curr].Length = 0);
  if LenUtf8 > FMaxPatternLen then
    FMaxPatternLen := LenUtf8;
  if FIgnoreCase then
    FTrie[Curr].Length := LenUtf8
  else
    FTrie[Curr].Length := System.Length(aValue);
end;

procedure TACSearchUtf8Nfa.BuildLinks;
var
  Queue: specialize TGLiteQueue<SizeInt>;
  Curr, Fail, Link: SizeInt;
  e: specialize TGMapEntry<Ucs4Char, SizeInt>;
begin
  for Link in FTrie[0].NextMove.Values do
    Queue.Enqueue(Link);
  while Queue.TryDequeue(Curr) do
    for e in FTrie[Curr].NextMove do
      begin
        Queue.Enqueue(e.Value);
        Fail := FTrie[Curr].Failure;
        repeat
          if FTrie[Fail].NextMove.TryGetValue(e.Key, Link) then
            begin
              FTrie[e.Value].Failure := Link;
              if FTrie[Link].Length <> 0 then
                FTrie[e.Value].Output := Link
              else
                FTrie[e.Value].Output := FTrie[Link].Output;
              break;
            end;
          if Fail = 0 then break;
          Fail := FTrie[Fail].Failure;
        until False;
      end;
end;

{$PUSH}{$MACRO ON}
procedure TACSearchUtf8Nfa.DoSearch(const s: string; aOnMatch: TOnMatch; aOffset, aCount: SizeInt);
var
  State, NextState, cLen: SizeInt;
  p, pEnd, pText: PByte;
  c: Ucs4Char;
  Code: Integer;
begin
{$DEFINE DoSearchMacro :=
  if aOffset < 1 then aOffset := 1;
  if aCount < 1 then
    aCount := System.Length(s)
  else
    aCount := Math.Min(Pred(aOffset + aCount), System.Length(s));
  pText := Pointer(s);
  p := pText + Pred(aOffset);
  pEnd := pText + aCount;
  State := 0;
  while p < pEnd do begin
    c := CodePointToUcs4Char(p, cLen);
    p += cLen;
    NextState := 0;
    while not FTrie[State].NextMove.TryGetValue(c, NextState) and (State <> 0) do
      State := FTrie[State].Failure;
    if NextState = 0 then continue;
    State := NextState;
    if FTrie[NextState].Length <> 0 then
      if not aOnMatch(TMatch.Make(Succ(p - pText - FTrie[NextState].Length), FTrie[NextState].Length)) then
        exit;
    while FTrie[NextState].Output <> 0 do begin
      NextState := FTrie[NextState].Output;
      if not aOnMatch(TMatch.Make(Succ(p - pText - FTrie[NextState].Length), FTrie[NextState].Length)) then
        exit;
    end;
  end
}
  DoSearchMacro;
end;

procedure TACSearchUtf8Nfa.DoSearch(const s: string; aOnMatch: TNestMatch; aOffset, aCount: SizeInt);
var
  State, NextState, cLen: SizeInt;
  p, pEnd, pText: PByte;
  c: Ucs4Char;
  Code: Integer;
begin
  DoSearchMacro;
end;
{$UNDEF DoSearchMacro}

procedure TACSearchUtf8Nfa.DoSearchCI(const s: string; aOnMatch: TOnMatch; aOffset, aCount: SizeInt);
var
  State, NextState, cLen, qTop, qLen, qPos: SizeInt;
  pQueue: PSizeInt;
  p, pEnd, pText: PByte;
  c, Loc: Ucs4Char;
  Code: Integer;
begin
{$DEFINE DoSearchCIMacro :=
  if aOffset < 1 then aOffset := 1;
  if aCount < 1 then
    aCount := System.Length(s)
  else
    aCount := Math.Min(Pred(aOffset + aCount), System.Length(s));
  pText := Pointer(s);
  p := pText + Pred(aOffset);
  pEnd := pText + aCount;
  pQueue := Pointer(FQueue);
  qLen := FMaxPatternLen;
  qTop := 0;
  State := 0;
  while p < pEnd do begin
    pQueue[qTop] := Succ(p - pText);
    Inc(qTop);
    if qTop = qLen then qTop := 0;
    c := CodePointToUcs4Char(p, cLen);
    p += cLen;
    if FIgnoreCase then begin
      Loc := DWord(UnicodeData.GetProps(c)^.SimpleLowerCase);
      if Loc <> 0 then c := Loc;
    end;
    NextState := 0;
    while not FTrie[State].NextMove.TryGetValue(c, NextState) and (State <> 0) do
      State := FTrie[State].Failure;
    if NextState = 0 then continue;
    State := NextState;
    if FTrie[NextState].Length <> 0 then begin
      qPos := qTop - FTrie[NextState].Length;
      if qPos < 0 then qPos += qLen;
      if not aOnMatch(TMatch.Make(pQueue[qPos], Succ(p - pText) - pQueue[qPos])) then exit;
    end;
    while FTrie[NextState].Output <> 0 do begin
      NextState := FTrie[NextState].Output;
      qPos := qTop - FTrie[NextState].Length;
      if qPos < 0 then qPos += qLen;
      if not aOnMatch(TMatch.Make(pQueue[qPos], Succ(p - pText) - pQueue[qPos])) then exit;
    end;
  end;
}
  DoSearchCIMacro;
end;

procedure TACSearchUtf8Nfa.DoSearchCI(const s: string; aOnMatch: TNestMatch; aOffset, aCount: SizeInt);
var
  State, NextState, cLen, qTop, qLen, qPos: SizeInt;
  pQueue: PSizeInt;
  p, pEnd, pText: PByte;
  c, Loc: Ucs4Char;
  Code: Integer;
begin
  DoSearchCIMacro;
end;
{$UNDEF DoSearchCIMacro}{$POP}

constructor TACSearchUtf8Nfa.Create(const aPatternList: array of string; aIgnoreCase: Boolean);
var
  s: string;
begin
  inherited Create;
  FIgnoreCase := aIgnoreCase;
  System.SetLength(FTrie, ARRAY_INITIAL_SIZE);
  NewNode;
  for s in aPatternList do
    AddPattern(s);
  System.SetLength(FTrie, FNodeCount);
  if FIgnoreCase then
    System.SetLength(FQueue, FMaxPatternLen);
  BuildLinks;
end;

function TACSearchUtf8Nfa.GetNodeCount: SizeInt;
begin
  Result := FNodeCount;
end;

function TACSearchUtf8Nfa.GetPatternCount: SizeInt;
begin
  Result := FWordCount;
end;

function TACSearchUtf8Nfa.GetCaseInsensitive: Boolean;
begin
  Result := FIgnoreCase;
end;

function TACSearchUtf8Nfa.ContainsPattern(const aValue: string): Boolean;
var
  Curr, Len: SizeInt;
  pNext: PSizeInt;
  p, pEnd: PByte;
  c, Loc: Ucs4Char;
begin
  Curr := 0;
  p := Pointer(aValue);
  pEnd := p + System.Length(aValue);
  while p < pEnd do
    begin
      c := CodePointToUcs4Char(p, Len);
      p += Len;
      if FIgnoreCase then
        begin
          Loc := DWord(UnicodeData.GetProps(c)^.SimpleLowerCase);
          if Loc <> 0 then c := Loc;
        end;
      pNext := FTrie[Curr].NextMove.GetMutValueDef(c, NULL_INDEX);
      if pNext^ = NULL_INDEX then exit(False);
      Curr := pNext^;
    end;
  Result := FTrie[Curr].Length <> 0;
end;

function TACSearchUtf8Nfa.FindMatches(const aText: string; aOffset, aCount: SizeInt): specialize TGArray<TSeqMatch>;
var
  Matches: array of TMatch = nil;
  mCount: SizeInt = 0;
  function Collect(const m: TMatch): Boolean;
  begin
    if mCount = System.Length(Matches) then
      System.SetLength(Matches, mCount * 2);
    Matches[mCount] := m;
    Inc(mCount);
    Result := True;
  end;
begin
  System.SetLength(Matches, ARRAY_INITIAL_SIZE);
  Search(aText, @Collect, aOffset, aCount);
  System.SetLength(Matches, mCount);
  Result := Matches;
end;

procedure TACSearchUtf8Nfa.Search(const aText: string; aOnMatch: TOnMatch; aOffset, aCount: SizeInt);
begin
  if FIgnoreCase then
    DoSearchCI(aText, aOnMatch, aOffset, aCount)
  else
    DoSearch(aText, aOnMatch, aOffset, aCount);
end;

procedure TACSearchUtf8Nfa.Search(const aText: string; aOnMatch: TNestMatch; aOffset, aCount: SizeInt);
begin
  if FIgnoreCase then
    DoSearchCI(aText, aOnMatch, aOffset, aCount)
  else
    DoSearch(aText, aOnMatch, aOffset, aCount);
end;

function TACSearchUtf8Nfa.ContainsMatch(const aText: string; aOffset, aCount: SizeInt): Boolean;
var
  State, NextState, cLen: SizeInt;
  p, pEnd: PByte;
  c, Loc: Ucs4Char;
begin
  if aOffset < 1 then aOffset := 1;
  if aCount < 1 then
    aCount := System.Length(aText)
  else
    aCount := Math.Min(Pred(aOffset + aCount), System.Length(aText));
  p := PByte(aText) + Pred(aOffset);
  pEnd := PByte(aText) + aCount;
  State := 0;
  while p < pEnd do
    begin
      c := CodePointToUcs4Char(p, cLen);
      p += cLen;
      if FIgnoreCase then begin
        Loc := DWord(UnicodeData.GetProps(c)^.SimpleLowerCase);
        if Loc <> 0 then c := Loc;
      end;
      NextState := 0;
      while not FTrie[State].NextMove.TryGetValue(c, NextState) and (State <> 0) do
        State := FTrie[State].Failure;
      if NextState = 0 then continue;
      State := NextState;
      if FTrie[NextState].Length <> 0 then exit(True);
      if FTrie[NextState].Output <> 0 then exit(True);
    end;
  Result := False;
end;

function CreateACSearchFsm(const aPatterns: array of string; aIgnoreCase, aForceNFA: Boolean): IACSearchUtf8;
var
  AcDfa: TACSearchUtf8Dfa;
begin
  if aForceNFA then exit(TACSearchUtf8Nfa.Create(aPatterns, aIgnoreCase));
  AcDfa := TACSearchUtf8Dfa.Create(aIgnoreCase);
  if AcDfa.TryBuildFsm(aPatterns) then
    Result := AcDfa
  else
    begin
      AcDfa.Free;
      Result := TACSearchUtf8Nfa.Create(aPatterns, aIgnoreCase);
    end;
end;

end.
