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
{$MODESWITCH TYPEHELPERS}
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
  lgHashMultiSet,
  lgStrConst;

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
    class function HashCode(aValue: Ucs4Char): SizeInt; static; //inline;
    class function Equal(L, R: Ucs4Char): Boolean; static; //inline;
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
    TDynIntArray  = specialize TGDynArray<SizeInt>;
    TLcsImpl      = function(pL, pR: PItem; aLenL, aLenR: SizeInt): TArray;

  const
    MAX_STATIC = 512;
    BLOCK_SIZE = BitSizeOf(QWord);
    BSIZE_MASK = Pred(BLOCK_SIZE);
    BSIZE_LOG  = 6;

    class function  SkipPrefix(var pL, pR: PItem; var aLenL, aLenR: SizeInt): SizeInt; static; inline;
    class function  SkipSuffix(pL, pR: PItem; var aLenL, aLenR: SizeInt): SizeInt; static; inline;
    class function  GetLis(const a: array of SizeInt; aMaxLen: SizeInt): TSizeIntArray; static;
    class function  LcsGusImpl(pL, pR: PItem; aLenL, aLenR: SizeInt): TArray; static;
    class function  LcsKRImpl(pL, pR: PItem; aLenL, aLenR: SizeInt): TArray; static;
    class function  LcsMyersImpl(pL, pR: PItem; aLenL, aLenR: SizeInt): TArray; static;
    class function  GetLcs(pL, pR: PItem; aLenL, aLenR: SizeInt; aImpl: TLcsImpl): TArray; static;
    class function  EditDistImpl(pL, pR: PItem; aLenL, aLenR: SizeInt; const aCost: TSeqEditCost): SizeInt; static;
    class function  LevDistMbrImpl(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt; static;
    class function  GetLevDistMbr(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt; static;
    class function  LevDistMyersQ(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt; static;
    class function  LevDistMyersQ(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt; static;
    class function  LevDistMyersDQ(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt; static;
    class function  LevDistMyersDQ(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt; static;
    class procedure CreatePeq(aSeq: PItem; aSeqLen: SizeInt; out aPeq: TPeq); static;
    class function  LevDistMyersCutoff(const aPeq: TPeq; pR: PItem; aLenL, aLenR, K: SizeInt): SizeInt; static;
    class function  LevDistMyers(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt; static;
    class function  GetLevDistMyers(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt; static;
    class function  GetLevDistMyers(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt; static;
    class function  LcsDistWmImpl(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt; static;
    class function  LcsDistWmLimImpl(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt; static;
    class function  GetLcsDistWm(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt; static;
    class function  GetLcsDistWmLim(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt; static;
    class function  DumDistMbrImpl(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt; static;
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
  { returns edit distance between L and R; used a dynamic programming algorithm
    with O(mn) time complexity, where n and m are the lengths of L and R respectively,
    and O(Max(m, n)) space complexity }
    class function EditDistance(const L, R: array of T; const aCost: TSeqEditCost): SizeInt; static;
  { returns the Levenshtein distance between L and R; a Pascal translation of
    github.com/vaadin/gwt/dev/util/editdistance/ModifiedBerghelRoachEditDistance.java -
    a modified version of algorithm described by Berghel and Roach with O(min(|L|,|R|))*d)
    worst-case time complexity, where d is the edit distance computed  }
    class function LevDistanceMBR(const L, R: array of T): SizeInt; static;
  { the same as above; the aLimit parameter indicates the maximum expected distance,
    if this value is exceeded when calculating the distance, then the function exits
    immediately and returns -1; a negative value of aLimit is considered as no limit }
    class function LevDistanceMBR(const L, R: array of T; aLimit: SizeInt): SizeInt; static;
  { returns the Levenshtein distance between L and R; uses the Myers bit-vector algorithm
    with O(dn/w) time complexity, where n is Max(Length(L), Length(R)),
    d is edit distance computed, and w is the size of a computer word }
    class function LevDistanceMyers(const L, R: array of T): SizeInt; static;
  { the same as above; the aLimit parameter indicates the maximum expected distance,
    if this value is exceeded when calculating the distance, then the function exits
    immediately and returns -1; a negative value of aLimit is considered as no limit }
    class function LevDistanceMyers(const L, R: array of T; aLimit: SizeInt): SizeInt; static;
  { the LCS edit distance allows only insertions and deletions; based on
    "An O(NP) Sequence Comparison Algorithm" by Sun Wu, Udi Manber, Gene Myers and Webb Miller }
    class function LcsDistanceWM(const L, R: array of T): SizeInt; static;
  { the same as above; the aLimit parameter indicates the maximum expected distance,
    if this value is exceeded when calculating the distance, then the function exits
    immediately and returns -1; a negative value of aLimit is considered as no limit }
    class function LcsDistanceWM(const L, R: array of T; aLimit: SizeInt): SizeInt; static;
  { returns the Damerau-Levenshtein distance(restricted) between L and R using
    modified Berghel-Roach algorithm }
    class function DumDistanceMBR(const L, R: array of T): SizeInt; static;
  { the same as above; the aLimit parameter indicates the maximum expected distance,
    if this value is exceeded when calculating the distance, then the function exits
    immediately and returns -1; if a negative value of aLimit is considered as no limit }
    class function DumDistanceMBR(const L, R: array of T; aLimit: SizeInt): SizeInt; static;
  { returns the longest common subsequence(LCS) of sequences L and R, reducing the task to LIS,
    with O(SLogN) time complexity, where S is the number of the matching pairs in L and R;
    inspired by Dan Gusfield "Algorithms on Strings, Trees and Sequences", section 12.5 }
    class function LcsGus(const L, R: array of T): TArray; static;
  { recursive, returns the longest common subsequence(LCS) of sequences L and R;
    uses Kumar-Rangan algorithm for LCS with O(N(M-|LCS|)) time complexity and linear space complexity }
    class function LcsKR(const L, R: array of T): TArray; static;
  { recursive, returns the longest common subsequence(LCS) of sequences L and R;
    uses O(ND) algorithm from "An O(ND) Difference Algorithm and Its Variations" by Gene Myers }
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

  type
    TLcsEditOp = (leoDelete, leoInsert);
    TLcsEdit = record
      Value: T;
      SourceIndex,
      TargetIndex: SizeInt;
      Operation: TLcsEditOp;
      constructor Del(const aValue: T; aSrcIdx, aTrgIdx: SizeInt);
      constructor Ins(const aValue: T; aSrcIdx, aTrgIdx: SizeInt);
    end;
    TLcsEditScript = array of TLcsEdit;

  { returns an edit script, a sequence of primitive operations that convert aSource to aTarget }
    class function LcsEditScript(const aSource, aTarget: array of T; aLcsAlgo: TLcsAlgo = laMyers): TLcsEditScript; static;
  end;

{*********************************************}

  TUcs4Seq  = array of Ucs4Char;
  TUcs4Less = function(const L, R: array of Ucs4Char): Boolean;

{ the responsibility for the correctness and normalization of the strings lies with the user }
  function IsSubSequenceUtf16(const aStr, aSub: unicodestring): Boolean;
  function Utf16ToUcs4Seq(const s: unicodestring): TUcs4Seq;
  function Ucs4SeqToUtf16(const s: TUcs4Seq): unicodestring;
{ returns the edit distance using DP algorithm }
  function EditDistanceUtf16(const L, R: unicodestring; const aCost: TSeqEditCost): SizeInt; inline;
{ returns the Levenshtein distance using Berghel-Roach algorithm }
  function LevDistanceMbrUtf16(const L, R: unicodestring): SizeInt; inline;
  function LevDistanceMbrUtf16(const L, R: unicodestring; aLimit: SizeInt): SizeInt; inline;
{ returns the Levenshtein distance using Myers bit-vector algorithm }
  function LevDistanceMyersUtf16(const L, R: unicodestring): SizeInt; inline;
  function LevDistanceMyersUtf16(const L, R: unicodestring; aLimit: SizeInt): SizeInt; inline;
{ returns the LCS distance using Wu-Manber-Myers-Miller O(NP) algorithm }
  function LcsDistanceWmUtf16(const L, R: unicodestring): SizeInt; inline;
  function LcsDistanceWmUtf16(const L, R: unicodestring; aLimit: SizeInt): SizeInt; inline;
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
{ returns the similarity ratio using the specified distance algorithm after preprocessing
  the input texts according to the aMode parameter; the elements in aStopChars must be single
  code points, otherwise they will be ignored }
  function SimRatioExUtf16(
    const L, R: unicodestring;
    const aStopChars: array of unicodestring;
    aMode: TSimMode = smSimple;
    const aOptions: TSimOptions = [];
    aLimit: Double = Double(0);
    Algo: TSeqDistanceAlgo = sdaDefault;
    aLess: TUcs4Less = nil
  ): Double;
{ returns the similarity ratio using the specified distance algorithm after preprocessing
  the input texts according to the aMode parameter; all non-word chars are treated as stop chars }
  function SimRatioExUtf16(
    const L, R: unicodestring;
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

{ all these functions expect UTF-8 encoded strings as parameters;
  the responsibility for the correctness and normalization of the strings lies with the user }
  function Utf8StrLen(const s: string): SizeInt; inline;
  function Utf8CodePointLength(p: PAnsiChar; aByteCount: SizeInt): SizeInt;
  function Utf8ToLower(const s: string): string;
  function Utf8ToUpper(const s: string): string;
  function IsSubSequenceUtf8(const aStr, aSub: string): Boolean;
  function Utf8ToUcs4Seq(const s: string): TUcs4Seq; inline;
  function Ucs4SeqToUtf8(const s: TUcs4Seq): string;

type
  { TUtf8Cp: describes a codepoint within some string }
  TUtf8Cp = record
  private
    FOffset,
    FCpOffset,
    FSize: SizeInt;
    FCode: Ucs4Char;
    procedure Init(aOfs, aCpOfs, aSize: SizeInt; c: Ucs4Char); inline;
  public
    class function HashCode(const aCp: TUtf8Cp): SizeInt; static; inline;
    class function Equal(const L, R: TUtf8Cp): Boolean; static; inline;
    function ToString: string;
    function Write(aBuffer: PAnsiChar): Integer;
    property Offset: SizeInt read FOffset;
    property CpOffset: SizeInt read FCpOffset;
    property ByteSize: SizeInt read FSize;
    property Code: Ucs4Char read FCode;
  end;

  TUtf8CpSeq = array of TUtf8Cp;

  { TUtf8CpSeqHelper }
  TUtf8CpSeqHelper = type helper for TUtf8CpSeq
    class function Parse(const s: string): TUtf8CpSeq; static;
    function ToString: string;
  end;

{ returns the sequence of TUtf8Cp of string s }
  function Utf8StrToCpSeq(const s: string): TUtf8CpSeq;
{ builds the string for the TUtf8Cp sequence s }
  function CpSeqToUtf8Str(const s: array of TUtf8Cp): string;

{ returns the edit distance using DP algorithm }
  function EditDistanceUtf8(const L, R: string; const aCost: TSeqEditCost): SizeInt; inline;
{ returns the Levenshtein distance using Berghel-Roach algorithm }
  function LevDistanceMbrUtf8(const L, R: string): SizeInt; inline;
  function LevDistanceMbrUtf8(const L, R: string; aLimit: SizeInt): SizeInt; inline;
{ returns the Levenshtein distance using Myers bit-vector algorithm }
  function LevDistanceMyersUtf8(const L, R: string): SizeInt; inline;
  function LevDistanceMyersUtf8(const L, R: string; aLimit: SizeInt): SizeInt; inline;
{ returns the LCS distance using Wu-Manber-Myers-Miller O(NP) algorithm }
  function LcsDistanceWmUtf8(const L, R: string): SizeInt; inline;
  function LcsDistanceWmUtf8(const L, R: string; aLimit: SizeInt): SizeInt; inline;
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
{ returns the similarity ratio using the specified distance algorithm after preprocessing
  the input texts according to the aMode parameter; the elements in aStopChars must be single
  code points, otherwise they will be ignored }
  function SimRatioExUtf8(
    const L, R: string;
    const aStopChars: array of string;
    aMode: TSimMode = smSimple;
    const aOptions: TSimOptions = [];
    aLimit: Double = Double(0);
    Algo: TSeqDistanceAlgo = sdaDefault;
    aLess: TUcs4Less = nil
  ): Double;
{ returns the similarity ratio using the specified distance algorithm after preprocessing
  the input texts according to the aMode parameter; all non-word chars are treated as stop chars }
  function SimRatioExUtf8(
    const L, R: string;
    aMode: TSimMode = smSimple;
    const aOptions: TSimOptions = [];
    aLimit: Double = Double(0);
    Algo: TSeqDistanceAlgo = sdaDefault;
    aLess: TUcs4Less = nil
  ): Double;
{ returns an array, each element of which contains the similarity ratio between
  aPattern and the corresponding element in the aValues array; the elements in
  aStopChars must be single code points, otherwise they will be ignored }
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
{ returns an array, each element of which contains the similarity ratio between
  aPattern and the corresponding element in the aValues array; all non-word chars
  are treated as stop chars }
  function SimRatioListUtf8(
    const aPattern: string;
    const aValues: array of string;
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
  strings whose similarity ratio is not less than the specified aLimit; the elements in
  aStopChars must be single code points, otherwise they will be ignored }
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
{ returns an array of pairs sorted by descending similarity ratio and containing only those
  strings whose similarity ratio is not less than the specified aLimit; all non-word chars
  are treated as stop chars }
  function SelectSimilarUtf8(
    const aPattern: string;
    const aValues: array of string;
    aLimit: Double;
    aMode: TSimMode = smSimple;
    const aOptions: TSimOptions = [];
    Algo: TSeqDistanceAlgo = sdaDefault;
    aLess: TUcs4Less = nil
  ): specialize TGArray<TStringRatio>;

type
  TBufHash32         = function(aBuffer: Pointer; aCount: SizeInt; aSeed: DWord): DWord;
  TBufHash64         = function(aBuffer: Pointer; aCount: SizeInt; aSeed: QWord): QWord;
  TStrCompareOption  = (scoIgnoreWS, scoIgnoreWSChange, scoIgnoreCase);
  TStrCompareOptions = set of TStrCompareOption;

{ returns a 32-bit hash(using aHash) of the string s, skipping whitespace and converting
  it to lower case if the appropriate options are specified in aOpts }
  function Utf8HashText(const s: string; aHash: TBufHash32; const aOpts: TStrCompareOptions = []; aSeed: DWord = 0): DWord;
{ returns a 64-bit hash(using aHash) of the string s, skipping whitespace and converting
  it to lower case if the appropriate options are specified in aOpts }
  function Utf8HashText64(const s: string; aHash: TBufHash64; const aOpts: TStrCompareOptions = []; aSeed: QWord = 0): QWord;
{ checks for equality of the L and R strings, skipping whitespace characters and
  converting the text to lower case if the appropriate flags are included in aOpts }
  function Utf8SameText(const L, R: string; const aOpts: TStrCompareOptions = []): Boolean;

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
    function GetInitialized: Boolean;
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

  TAcMatch = LgUtils.TIndexMatch;
  EAcFsmError = class(Exception);

{$PUSH}{$INTERFACES COM}
  { IACSearchFsmUtf8: Aho-Corasick automation for exact set matching problem;
    it does not store dictionary elements explicitly, instead storing their indices;
    expects UTF-8 encoded strings as parameters }
  IACSearchFsmUtf8 = interface
  ['{AA85E5D4-5FBA-4D3C-BD16-858FC26B619C}']
    function  GetCaseInsensitive: Boolean;
    function  GetOnlyWholeWords: Boolean;
    procedure SetOnlyWholeWords(aValue: Boolean);
    function  GetStateCount: SizeInt;
    function  GetEmptyCount: SizeInt;
    function  GetPatternCount: SizeInt;
    function  GetAlphabetSize: SizeInt;
    function  IsDfa: Boolean;
  { if the string aText, starting at index aOffset and within aCount bytes, matches
    one of the patterns, returns the index of that pattern, otherwise returns -1 }
    function  IndexOfPattern(const aText: string; aOffset: SizeInt = 1; aCount: SizeInt = 0): SizeInt;
  { returns True if the string aText starting at index aOffset and within aCount bytes,
    matches one of the patterns, otherwise returns False;
    any value of aCount < 1 implies a search to the end of the string }
    function  IsMatch(const aText: string; aOffset: SizeInt = 1; aCount: SizeInt = 0): Boolean;
  { returns the very first match(according to the specified mode), if any, otherwise returns stub (0,0,-1)}
    function  FirstMatch(const aText: string; aMode: TSetMatchMode = smmDefault; aOffset: SizeInt = 1;
                         aCount: SizeInt = 0): TAcMatch;
  { returns an array of all matches found in string aText according to the specified
    matching mode aMode, starting at position aOffset within aCount bytes;
    any value of aCount < 1 implies a search to the end of the string }
    function  FindMatches(const aText: string; aMode: TSetMatchMode = smmDefault; aOffset: SizeInt = 1;
                          aCount: SizeInt = 0): specialize TGArray<TAcMatch>;
  { searches in the string aText starting at position aOffset within aCount bytes, passing
    the found matches(1-based, in bytes) to the callback aOnMatch(); immediately exits the procedure
    if aOnMatch() returns False; any value of aCount < 1 implies a search to the end of the string;
    if aOnMatch is nil, then just immediately exits the procedure }
    procedure Search(const aText: string; aOnMatch: specialize TGOnTest<TAcMatch>;
                     aOffset: SizeInt = 1; aCount: SizeInt = 0);
    procedure Search(const aText: string; aOnMatch: specialize TGNestTest<TAcMatch>;
                     aOffset: SizeInt = 1; aCount: SizeInt = 0);
  { returns True if at least one match is found in the string aText, starting at position aOffset
    within aCount bytes; any value of aCount < 1 implies a search to the end of the string }
    function  ContainsMatch(const aText: string; aOffset: SizeInt = 1; aCount: SizeInt = 0): Boolean;
    function  FilterMatches(const aSource: array of TAcMatch; aMode: TSetMatchMode): specialize TGArray<TAcMatch>;
    property  CaseInsensitive: Boolean read GetCaseInsensitive;
  { if set to True, the word boundary will be examined for each match, i.e. if it is surrounded
    by non-word characters or string boundaries }
    property  OnlyWholeWords: Boolean read GetOnlyWholeWords write SetOnlyWholeWords;
  { some statistics }
    property  StateCount: SizeInt read GetStateCount;
    property  EmptyCellCount: SizeInt read GetEmptyCount;
  { number of accounted patterns }
    property  PatternCount: SizeInt read GetPatternCount;
    property  AlphabetSize: SizeInt read GetAlphabetSize;
  end;

  { IACPersistFsmUtf8: serializable Aho-Corasick FSM(always NFA), stores the dictionary in its original form }
  IACPersistFsmUtf8 = interface
  ['{7601BEAB-9868-4D1A-9A71-B552538EFD2D}']
    function  GetFsm: IACSearchFsmUtf8;
    function  GetPatternListSize: Integer;
    function  GetPattern(aIndex: Integer): string;
  { returns a copy of the original pattern list }
    function  GetPatternList: TStringArray;
  { loads the FSM from a stream, may raise an exception }
    procedure LoadFromStream(aStream: TStream);
  { saves the FSM to a stream in its own binary format(currently quite primitive) }
    procedure SaveToStream(aStream: TStream);
  { loads the FSM from a file, may raise an exception }
    procedure LoadFromFile(const aFileName: string);
  { saves the FSM to a file, may raise an exception }
    procedure SaveToFile(const aFileName: string);
    property  Fsm: IACSearchFsmUtf8 read GetFsm;
  { total number of patterns in the original pattern list }
    property  PatternListSize: Integer read GetPatternListSize;
    property  Patterns[aIndex: Integer]: string read GetPattern; default;
  end;
{$POP}

{ creates an instance of the Aho-Corasick FSM;
  aPatterns(or aPatterEnum) specifies a set of search patterns, in case of duplicate patterns,
  only the first one will be taken into account;
  aIgnoreCase set to True specifies a case-insensitive search;
  by default the function tries to build a DFA first, which may not be possible if the patterns
  contain characters outside the BMP or the patterns alphabet is too large; DFA usually takes
  more memory and in addition, NFA can be faster on reasonably large dictionaries;
  if aForceNFA is set to True the function will immediately build NFA }
  function CreateAcFsmUtf8(const aPatterns: array of string; aIgnoreCase: Boolean = False;
                           aForceNFA: Boolean = False): IACSearchFsmUtf8;
{ creates an instance of a serializable Aho-Corasick FSM }
  function NewAcFsmUtf8(const aPatterns: array of string; aIgnoreCase: Boolean = False): IACPersistFsmUtf8;
  function NewAcFsmUtf8(aPatterEnum: specialize IGEnumerable<string>; aIgnoreCase: Boolean = False): IACPersistFsmUtf8;
{ loads an instance of a serializable Aho-Corasick FSM from a stream }
  function LoadAcFsmUtf8(aStream: TStream): IACPersistFsmUtf8;
{ loads an instance of a serializable Aho-Corasick FSM from a file }
  function LoadAcFsmUtf8(const aFileName: string): IACPersistFsmUtf8;

{ Aho-Corasick NFA is based on Double Array Trie(DAT) and uses the simplest construction
  algorithm, which nevertheless provides acceptable construction time. But if the dictionary
  alphabet is sufficiently large, the load factor of the resulting DAT may be rather low.
  If this flag is set to True, it can improve the load factor, but on the other hand it can
  drastically increase the time taken to create NFA for large dictionaries. }
var
  DatPreferMaxLoad: Boolean = False;

type
  TStrReplaceOption  = (sroOnlyWholeWords, sroIgnoreCase);
  TStrReplaceOptions = set of TStrReplaceOption;
{ a mode of selecting a single match when there are several overlapping matches,
  defines behavior corresponding to TSetMatchMode modes }
  TOverlapsHandleMode = (ohmLeftmostFirst, ohmLeftmostLongest, ohmLeftmostShortest);

{ replaces all found occurrences of search patterns in the string aSource with the specified values
  and returns the resulting string, if there were no replacements, returns the original string;
  parameter aSamples specifies a set of search patterns;
  parameter aSubs specifies a set of substitutions, if the aSubs array has fewer elements
  than the aSamples array, aDefaultSub will be used as a replacement for the missing elements;
  aReplaceCount parameter returns the number of replacements made;
  expects UTF-8 encoded strings as parameters }
  function ACStrReplace(const aSource: string;
                        const aSamples, aSubs: array of string;
                        out aReplaceCount: SizeInt;
                        const aOptions: TStrReplaceOptions = [];
                        aMode: TOverlapsHandleMode = ohmLeftmostFirst;
                        const aDefaultSub: string = ''): string;
{ the same as above for the case where the number of replacements is not of interest }
  function ACStrReplace(const aSource: string;
                        const aSamples, aSubs: array of string;
                        const aOptions: TStrReplaceOptions = [];
                        aMode: TOverlapsHandleMode = ohmLeftmostFirst;
                        const aDefaultSub: string = ''): string;
{ performs replacements for each element of the aSource array }
  function ACStrReplaceList(const aSource, aSamples, aSubs: array of string;
                            out aReplaceCount: SizeInt;
                            const aOptions: TStrReplaceOptions = [];
                            aMode: TOverlapsHandleMode = ohmLeftmostFirst;
                            const aDefaultSub: string = ''): TStringArray;
{ the same as above for the case where the number of replacements is not of interest }
  function ACStrReplaceList(const aSource, aSamples, aSubs: array of string;
                            const aOptions: TStrReplaceOptions = [];
                            aMode: TOverlapsHandleMode = ohmLeftmostFirst;
                            const aDefaultSub: string = ''): TStringArray;
{ uses a previously created automaton for replacements }
  function ACStrReplace(aSearchFsm: IACSearchFsmUtf8;
                        const aSource: string;
                        const aSubs: array of string;
                        out aReplaceCount: SizeInt;
                        aMode: TOverlapsHandleMode = ohmLeftmostFirst;
                        const aDefaultSub: string = ''): string;
{ the same as above for the case where the number of replacements is not of interest }
  function ACStrReplace(aSearchFsm: IACSearchFsmUtf8;
                        const aSource: string;
                        const aSubs: array of string;
                        aMode: TOverlapsHandleMode = ohmLeftmostFirst;
                        const aDefaultSub: string = ''): string;

const
  UC_TBL_HIGH = $1fff;
{$PUSH}{$J-}
  UC_CATEGORY_TBL: array[0..UC_TBL_HIGH] of Byte = ({$I uc_bmp_gcategory.inc});
{$POP}

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}
{$WARN 6058 OFF : Call to subroutine "$1" marked as inline is not inlined }
uses
  BufStream, Character, UnicodeData, lgMiscUtils;

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

class function TGSeqUtil.LcsGusImpl(pL, pR: PItem; aLenL, aLenR: SizeInt): TArray;
var
  MatchList: TMap;
  NodeList: TNodeList;
  Tmp: TSizeIntArray;
  LocLis: TSizeIntArray;
  I, J, NodeIdx: SizeInt;
  p: ^TEntry;
const
  INIT_SIZE = 256;
begin
  //here aLenL <= aLenR and aLenL <> 0

  for I := 0 to Pred(aLenL) do
    if not MatchList.FindOrAdd(pL[I], p) then
      p^ := TEntry.Create(pL[I], NULL_INDEX);

  System.SetLength(NodeList, INIT_SIZE);
  J := 0;
  for I := 0 to Pred(aLenR) do
    begin
      p := MatchList.Find(pR[I]);
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
      NodeIdx := MatchList.Find(pL[I])^.Value;
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

  if Tmp = nil then exit(nil);

  NodeList := nil;
  LocLis := GetLis(Tmp, aLenL);
  Tmp := nil;
  System.SetLength(Result, System.Length(LocLis));
  for I := 0 to System.High(LocLis) do
    Result[I] := pR[LocLis[I]];
end;

{$PUSH}{$WARN 5089 OFF}
class function TGSeqUtil.LcsKRImpl(pL, pR: PItem; aLenL, aLenR: SizeInt): TArray;
var
  LocLcs: TVector;
  R1, R2, LL, LL1, LL2: PSizeInt;
  R, S: SizeInt;
  procedure FillOne(LFirst, RFirst, RLast: SizeInt; aForward: Boolean);
  var
    I, J, LoR, PosR, Tmp: SizeInt;
  begin
    J := 1;
    I := S;
    if aForward then begin
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
  procedure Swap(var pL, pR: Pointer); inline;
  var
    Tmp: Pointer;
  begin
    Tmp := pL;
    pL := pR;
    pR := Tmp;
  end;
  procedure CalMid(LFirst, LLast, RFirst, RLast, Waste: SizeInt; L: PSizeInt; aForward: Boolean);
  var
    P: SizeInt;
  begin
    if aForward then
      S := Succ(LLast - LFirst)
    else
      S := Succ(LFirst - LLast);
    P := S - Waste;
    R := 0;
    while S >= P do begin
      FillOne(LFirst, RFirst, RLast, aForward);
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
var
  StBuf: array[0..Pred(MAX_STATIC)] of SizeInt;
  Buf: array of SizeInt;
begin
  //here aLenL <= aLenR and aLenL <> 0
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

  LocLcs := Default(TVector);
  LocLcs.EnsureCapacity(aLenL);

  Lcs(0, Pred(aLenL), 0, Pred(aLenR), (aLenL + aLenR - LcsDistWmImpl(pL, pR, aLenL, aLenR)) div 2);

  Result := LocLcs.ToArray;
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
    OddDelta := System.Odd(Delta);
    Mid := (LenL + LenR) div 2 + Ord(OddDelta);
    ForV := @V0[Succ(Mid)];
    RevV := @V1[Succ(Mid)];
    ForV[1] := 0;
    RevV[1] := 0;
    aSnake := Default(TSnake); // make compiler happy
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
            while (Row < LenL) and (Col < LenR) and TEqRel.Equal(pL[LFirst + Row], pR[RFirst + Col]) do begin
              Inc(Row); Inc(Col);
            end;
            ForV[K] := Row;
            if OddDelta and (K >= Delta-D+1)and(K <= Delta+D-1)and(Row + RevV[Delta-K] >= LenL)then begin
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
            while (Row < LenL) and (Col < LenR) and TEqRel.Equal(pL[LLast-Row], pR[RLast-Col]) do begin
              Inc(Row); Inc(Col);
            end;
            RevV[K] := Row;
            if not OddDelta and(K <= Delta+D)and(K >= Delta-D)and(Row + ForV[Delta-K] >= LenL) then begin
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
begin
  //here aLenL <= aLenR and aLenL <> 0
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

  LocLcs := Default(TVector);
  LocLcs.EnsureCapacity(aLenL);

  Lcs(0, Pred(aLenL), 0, Pred(aLenR));

  Result := LocLcs.ToArray;
end;
{$POP}

class function TGSeqUtil.GetLcs(pL, pR: PItem; aLenL, aLenR: SizeInt; aImpl: TLcsImpl): TArray;
var
  LocLcs: TArray;
  PrefixLen, SuffixLen: SizeInt;
begin
  //here aLenL <= aLenR
  if pL = pR then
    exit(THelper.CreateCopy(pL[0..Pred(aLenL)]));

  SuffixLen := SkipSuffix(pL, pR, aLenL, aLenR);
  PrefixLen := SkipPrefix(pL, pR, aLenL, aLenR);

  if aLenL = 0 then begin
    System.SetLength(Result, PrefixLen + SuffixLen);
    THelper.CopyItems(pL - PrefixLen, PItem(Result), PrefixLen);
    THelper.CopyItems(pL, PItem(Result) + PrefixLen, SuffixLen);
    exit;
  end;

  if (aLenL = 1) and (aLenR = 1) then begin
    System.SetLength(Result, PrefixLen + SuffixLen);
    THelper.CopyItems(pL - PrefixLen, PItem(Result), PrefixLen);
    THelper.CopyItems(pL + 1, PItem(Result) + PrefixLen, SuffixLen);
    exit;
  end;

  LocLcs := aImpl(pL, pR, aLenL, aLenR);

  System.SetLength(Result, System.Length(LocLcs) + PrefixLen + SuffixLen);
  THelper.CopyItems(pL - PrefixLen, PItem(Result), PrefixLen);
  THelper.CopyItems(Pointer(LocLcs), PItem(Result) + PrefixLen, System.Length(LocLcs));
  THelper.CopyItems(pL + aLenL, PItem(Result) + PrefixLen + System.Length(LocLcs), SuffixLen);
end;

class function TGSeqUtil.EditDistImpl(pL, pR: PItem; aLenL, aLenR: SizeInt; const aCost: TSeqEditCost): SizeInt;
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

  if (aLenL = 1) and (aLenR = 1) then
    if TEqRel.Equal(pL^, pR^) then
      exit(0)
    else
      exit(SizeInt(aCost.Replacement));

  if aLenR < MAX_STATIC then
    begin
      Dist := @StBuf[0];
      Dist[0] := 0;
    end
  else
    begin
      System.SetLength(Buf, Succ(aLenR));
      Dist := Pointer(Buf);
    end;

  for I := 1 to aLenR do
    Dist[I] := Dist[I-1] + SizeInt(aCost.Insertion);

  for I := 1 to aLenL do
    begin
      Prev := Dist[0] + SizeInt(aCost.Deletion);
      v := pL[I-1];
      for J := 1 to aLenR do
        begin
          if TEqRel.Equal(pR[J-1], v) then
            Next := Dist[J-1]
          else
            Next := MinOf3(
              Dist[J-1] + SizeInt(aCost.Replacement),
              Dist[J] + SizeInt(aCost.Deletion),
              Prev + SizeInt(aCost.Insertion)
            );
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
    MaxRow := Math.Min(aLenL - k, aLenR);
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

class function TGSeqUtil.GetLevDistMbr(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt;
begin
  //here aLenL <= aLenR
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

  if (aLenL = 1) and (aLenR = 1) then
    exit(Ord(not TEqRel.Equal(pL^, pR^)));

  if aLimit > aLenR then
    aLimit := aLenR;

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
  aPeq := Default(TPeq);
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

  if (aLenL = 1) and (aLenR = 1) then
    exit(Ord(not TEqRel.Equal(pL^, pR^)));

  case aLenL of
    1..BitSizeOf(QWord):
      Result := LevDistMyersQ(pL, pR, aLenL, aLenR);
    BitSizeOf(QWord)+1..BitSizeOf(QWord)*2:
      Result := LevDistMyersDQ(pL, pR, aLenL, aLenR);
  else
    Result := LevDistMyers(pL, pR, aLenL, aLenR, aLenR);
  end;
end;

class function TGSeqUtil.GetLevDistMyers(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt;
begin
  //here aLenL <= aLenR
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

  if (aLenL = 1) and (aLenR = 1) then
    exit(Ord(not TEqRel.Equal(pL^, pR^)));

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

class function TGSeqUtil.LcsDistWmImpl(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt;
var
  StBuf: array[0..Pred(MAX_STATIC)] of SizeInt;
  Buf: TDynIntArray;
  P, K, X, Y, Delta: SizeInt;
  Fp: PSizeInt;
begin
  if aLenL + aLenR < MAX_STATIC - 2 then
    Fp := @StBuf[0]
  else begin
    Buf.Length := aLenL + aLenR + 3;
    Fp := Buf.Ptr;
  end;
  System.FillChar(Fp^, (aLenL + aLenR + 3) * SizeOf(SizeInt), $ff);
  Fp += Succ(aLenL);
  Delta := aLenR - aLenL;
  for P := 0 to aLenL do begin
    for K := -P to Delta - 1 do begin
      Y := Math.Max(Fp[K - 1] + 1, Fp[K + 1]);
      X := Y - K;
      while (X < aLenL) and (Y < aLenR) and TEqRel.Equal(pL[X], pR[Y]) do begin
        Inc(X); Inc(Y);
      end;
      Fp[K] := Y;
    end;
    for K := P + Delta downto Delta do begin
      Y := Math.Max(Fp[K - 1] + 1, Fp[K + 1]);
      X := Y - K;
      while (X < aLenL) and (Y < aLenR) and TEqRel.Equal(pL[X], pR[Y]) do begin
        Inc(X); Inc(Y);
      end;
      Fp[K] := Y;
    end;
    if Fp[Delta] = aLenR then exit(P + P + Delta);
  end;
  Result := aLenL + aLenR;
end;

class function TGSeqUtil.LcsDistWmLimImpl(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt;
var
  StBuf: array[0..Pred(MAX_STATIC)] of SizeInt;
  Buf: TDynIntArray;
  P, K, X, Y, Delta: SizeInt;
  Fp: PSizeInt;
begin
  if aLimit < MAX_STATIC - 2 then
    Fp := @StBuf[0]
  else begin
    Buf.Length := aLimit + 3;
    Fp := Buf.Ptr;
  end;
  System.FillChar(Fp^, (aLimit + 3) * SizeOf(SizeInt), $ff);
  Delta := aLenR - aLenL;
  aLimit := (aLimit - Delta) div 2;
  Fp += Succ(aLimit);
  for P := 0 to aLimit do begin
    for K := -P to Delta - 1 do begin
      Y := Math.Max(Fp[K - 1] + 1, Fp[K + 1]);
      X := Y - K;
      while (X < aLenL) and (Y < aLenR) and TEqRel.Equal(pL[X], pR[Y]) do begin
        Inc(X); Inc(Y);
      end;
      Fp[K] := Y;
    end;
    for K := P + Delta downto Delta do begin
      Y := Math.Max(Fp[K - 1] + 1, Fp[K + 1]);
      X := Y - K;
      while (X < aLenL) and (Y < aLenR) and TEqRel.Equal(pL[X], pR[Y]) do begin
        Inc(X); Inc(Y);
      end;
      Fp[K] := Y;
    end;
    if Fp[Delta] = aLenR then exit(P + P + Delta);
  end;
  Result := NULL_INDEX;
end;

class function TGSeqUtil.GetLcsDistWm(pL, pR: PItem; aLenL, aLenR: SizeInt): SizeInt;
begin
  //here aLenL <= aLenR
  if pL = pR then
    exit(aLenR - aLenL);

  SkipSuffix(pL, pR, aLenL, aLenR);
  SkipPrefix(pL, pR, aLenL, aLenR);

  if aLenL = 0 then
    exit(aLenR);

  if (aLenL = 1) and (aLenR = 1) then
    exit(Ord(not TEqRel.Equal(pL^, pR^))*2);

  Result := LcsDistWmImpl(pL, pR, aLenL, aLenR);
end;

class function TGSeqUtil.GetLcsDistWmLim(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt;
begin
  //here aLenL <= aLenR
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

  if (aLenL = 1) and (aLenR = 1) then
    if TEqRel.Equal(pL^, pR^) then
      exit(0)
    else
      if aLimit > 1 then
        exit(2)
      else
        exit(NULL_INDEX);

  if aLimit > aLenL + aLenR then
    aLimit := aLenL + aLenR;

  Result := LcsDistWmLimImpl(pL, pR, aLenL, aLenR, aLimit);
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
    MaxRow := Math.Min(aLenL - k, aLenR);
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

class function TGSeqUtil.GetDumDistMbr(pL, pR: PItem; aLenL, aLenR, aLimit: SizeInt): SizeInt;
begin
  //here aLenL <= aLenR
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

  if (aLenL = 1) and (aLenR = 1) then
    exit(Ord(not TEqRel.Equal(pL^, pR^)));

  if aLimit > aLenR then
    aLimit := aLenR;

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

class function TGSeqUtil.EditDistance(const L, R: array of T; const aCost: TSeqEditCost): SizeInt;
begin
  if System.Length(L) = 0 then
    exit(System.Length(R))
  else
    if System.Length(R) = 0 then
      exit(System.Length(L));
  if System.Length(L) <= System.Length(R) then
    Result := EditDistImpl(@L[0], @R[0], System.Length(L), System.Length(R), aCost)
  else
    Result := EditDistImpl(@R[0], @L[0], System.Length(R), System.Length(L), aCost);
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
  if aLimit < 0 then exit(LevDistanceMBR(L, R));
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
  if aLimit < 0 then exit(LevDistanceMyers(L, R));
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

class function TGSeqUtil.LcsDistanceWM(const L, R: array of T): SizeInt;
begin
  if System.Length(L) = 0 then
    exit(System.Length(R))
  else
    if System.Length(R) = 0 then
      exit(System.Length(L));
  if System.Length(L) <= System.Length(R) then
    Result := GetLcsDistWm(@L[0], @R[0], System.Length(L), System.Length(R))
  else
    Result := GetLcsDistWm(@R[0], @L[0], System.Length(R), System.Length(L));
end;

class function TGSeqUtil.LcsDistanceWM(const L, R: array of T; aLimit: SizeInt): SizeInt;
begin
  if aLimit < 0 then exit(LcsDistanceWM(L, R));
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
    Result := GetLcsDistWmLim(@L[0], @R[0], System.Length(L), System.Length(R), aLimit)
  else
    Result := GetLcsDistWmLim(@R[0], @L[0], System.Length(R), System.Length(L), aLimit);
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
  if aLimit < 0 then exit(DumDistanceMBR(L, R));
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
    Result := GetDumDistMbr(@L[0], @R[0], System.Length(L), System.Length(R), aLimit)
  else
    Result := GetDumDistMbr(@R[0], @L[0], System.Length(R), System.Length(L), aLimit);
end;

class function TGSeqUtil.LcsGus(const L, R: array of T): TArray;
begin
  if (System.Length(L) = 0) or (System.Length(R) = 0) then
    exit(nil);
  if System.Length(L) <= System.Length(R) then
    Result := GetLcs(@L[0], @R[0], System.Length(L), System.Length(R), @LcsGusImpl)
  else
    Result := GetLcs(@R[0], @L[0], System.Length(R), System.Length(L), @LcsGusImpl);
end;

class function TGSeqUtil.LcsKR(const L, R: array of T): TArray;
begin
  if (System.Length(L) = 0) or (System.Length(R) = 0) then
    exit(nil);
  if System.Length(L) <= System.Length(R) then
    Result := GetLcs(@L[0], @R[0], System.Length(L), System.Length(R), @LcsKRImpl)
  else
    Result := GetLcs(@R[0], @L[0], System.Length(R), System.Length(L), @LcsKRImpl);
end;

class function TGSeqUtil.LcsMyers(const L, R: array of T): TArray;
begin
  if (System.Length(L) = 0) or (System.Length(R) = 0) then
    exit(nil);
  if System.Length(L) <= System.Length(R) then
    Result := GetLcs(@L[0], @R[0], System.Length(L), System.Length(R), @LcsMyersImpl)
  else
    Result := GetLcs(@R[0], @L[0], System.Length(R), System.Length(L), @LcsMyersImpl);
end;

class function TGSeqUtil.SimRatio(const L, R: array of T; aLimit: Double; Algo: TSeqDistanceAlgo): Double;
var
  Len, Limit, Dist: SizeInt;
begin
  if (System.Length(L) = 0) and (System.Length(R) = 0) then exit(Double(1.0));
  if aLimit > 1 then aLimit := Double(1);
  if Algo = sdaLcsWM then
    Len := System.Length(L) + System.Length(R)
  else
    Len := Math.Max(System.Length(L), System.Length(R));
  Dist := 0;
  if aLimit = Double(0) then begin
    case Algo of
      sdaDefault,
      sdaLevMyers: Dist := LevDistanceMyers(L, R);
      sdaLevMBR:   Dist := LevDistanceMbr(L, R);
      sdaLcsWM:    Dist := LcsDistanceWM(L, R);
      sdaDumMBR:   Dist := DumDistanceMbr(L, R);
    end;
    exit(Double(Len - Dist)/Double(Len));
  end;
  if aLimit > 0 then
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
    sdaLcsWM:    Dist := LcsDistanceWM(L, R, Limit);
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
          Inc(SrcIdx);
        end;
      while not TEqRel.Equal(v, aTarget[TrgIdx]) do
        begin
          Ins[TrgIdx] := True;
          Inc(TrgIdx);
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


{ TGSeqUtil.TLcsEdit }

constructor TGSeqUtil.TLcsEdit.Del(const aValue: T; aSrcIdx, aTrgIdx: SizeInt);
begin
  Value := aValue;
  SourceIndex := aSrcIdx;
  TargetIndex := aTrgIdx;
  Operation := leoDelete;
end;

constructor TGSeqUtil.TLcsEdit.Ins(const aValue: T; aSrcIdx, aTrgIdx: SizeInt);
begin
  Value := aValue;
  SourceIndex := aSrcIdx;
  TargetIndex := aTrgIdx;
  Operation := leoInsert;
end;

class function TGSeqUtil.LcsEditScript(const aSource, aTarget: array of T; aLcsAlgo: TLcsAlgo): TLcsEditScript;
var
  Lcs: TArray;
  r: array of TLcsEdit;
  I, SrcIdx, TrgIdx, ScriptIdx: SizeInt;
  v: T;
begin
  case aLcsAlgo of
    laGus: Lcs := LcsGus(aSource, aTarget);
    laKr:  Lcs := LcsKr(aSource, aTarget);
  else// laMyers
    Lcs := LcsMyers(aSource, aTarget);
  end;

  System.SetLength(r, System.Length(aSource) + System.Length(aTarget) - System.Length(Lcs)*2);
  ScriptIdx := 0;
  SrcIdx := 0;
  TrgIdx := 0;
  for I := 0 to System.High(Lcs) do begin
    v := Lcs[I];
    while not TEqRel.Equal(v, aSource[SrcIdx]) do begin
      r[ScriptIdx] := TLcsEdit.Del(aSource[SrcIdx], SrcIdx, TrgIdx);
      Inc(ScriptIdx);
      Inc(SrcIdx);
    end;
    while not TEqRel.Equal(v, aTarget[TrgIdx]) do begin
      r[ScriptIdx] := TLcsEdit.Ins(aTarget[TrgIdx], SrcIdx, TrgIdx);
      Inc(ScriptIdx);
      Inc(TrgIdx);
    end;
    Inc(SrcIdx);
    Inc(TrgIdx);
  end;
  while SrcIdx < System.Length(aSource) do begin
    r[ScriptIdx] := TLcsEdit.Del(aSource[SrcIdx], SrcIdx, TrgIdx);
    Inc(ScriptIdx);
    Inc(SrcIdx);
  end;
  while TrgIdx < System.Length(aTarget) do begin
    r[ScriptIdx] := TLcsEdit.Ins(aTarget[TrgIdx], SrcIdx, TrgIdx);
    Inc(ScriptIdx);
    Inc(TrgIdx);
  end;
  Result := r;
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
    dfisMbr, dfisMyers, dfisWmLcs, dfisMbrDum, dfisMbrBound, dfisMyersBound,
    dfisWmLcsBound, dfisMbrDumBound);

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
    dfisMbr:        Result := TUcs4Util.LevDistanceMBR(pL[0..Pred(LenL)], pR[0..Pred(LenR)]);
    dfisMyers:      Result := TUcs4Util.LevDistanceMyers(pL[0..Pred(LenL)], pR[0..Pred(LenR)]);
    dfisWmLcs:      Result := TUcs4Util.LcsDistanceWM(pL[0..Pred(LenL)], pR[0..Pred(LenR)]);
    dfisMbrDum:     Result := TUcs4Util.DumDistanceMBR(pL[0..Pred(LenL)], pR[0..Pred(LenR)]);
    dfisMbrBound:   Result := TUcs4Util.LevDistanceMBR(pL[0..Pred(LenL)], pR[0..Pred(LenR)], aLimit);
    dfisMyersBound: Result := TUcs4Util.LevDistanceMyers(pL[0..Pred(LenL)], pR[0..Pred(LenR)], aLimit);
    dfisWmLcsBound: Result := TUcs4Util.LcsDistanceWM(pL[0..Pred(LenL)], pR[0..Pred(LenR)], aLimit);
  else
    //dfisMbrDumBound
    Result := TUcs4Util.DumDistanceMBR(pL[0..Pred(LenL)], pR[0..Pred(LenR)], aLimit);
  end;
end;

function EditDistanceUtf16(const L, R: unicodestring; const aCost: TSeqEditCost): SizeInt;
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
  Result := TUcs4Util.EditDistance(pL[0..Pred(LenL)], pR[0..Pred(LenR)], aCost);
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

function LcsDistanceWmUtf16(const L, R: unicodestring): SizeInt;
begin
  Result := GenericDistanceUtf16(L, R, -1, dfisWmLcs);
end;

function LcsDistanceWmUtf16(const L, R: unicodestring; aLimit: SizeInt): SizeInt;
begin
  Result := GenericDistanceUtf16(L, R, aLimit, dfisWmLcsBound);
end;

function DumDistanceMbrUtf16(const L, R: unicodestring): SizeInt;
begin
  Result := GenericDistanceUtf16(L, R, -1, dfisMbrDum);
end;

function DumDistanceMbrUtf16(const L, R: unicodestring; aLimit: SizeInt): SizeInt;
begin
  Result := GenericDistanceUtf16(L, R, aLimit, dfisMbrDumBound);
end;

function LcsGenericUtf16(const L, R: unicodestring; aSpec: TUcs4Util.TLcsAlgo): unicodestring;
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
  Result := LcsGenericUtf16(L, R, laGus);
end;

function LcsKRUtf16(const L, R: unicodestring): unicodestring;
begin
  Result := LcsGenericUtf16(L, R, laKR);
end;

function LcsMyersUtf16(const L, R: unicodestring): unicodestring;
begin
  Result := LcsGenericUtf16(L, R, laMyers);
end;

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

type
  TUcs4CharTest = function(c: Ucs4Char): Boolean is nested;

{$PUSH}{$WARN 5089 OFF}
function SimRatioGeneric(const L, R: array of Ucs4Char; aIsStopChar: TUcs4CharTest;
  aMode: TSimMode; aPartial: Boolean; aLimit: Double; Algo: TSeqDistanceAlgo; aLess: TUcs4Less): Double;
type
  TWord       = record Start: PUcs4Char; Len: SizeInt end;
  PWord       = ^TWord;
  TWordArray  = array of TWord;
  TSplitFun   = function(const s: array of Ucs4Char; out aCount: SizeInt; out aBuf: TWordArray;
                         aForceDyn: Boolean): PWord is nested;
  THelper     = specialize TGNestedArrayHelper<TWord>;
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
    if System.Length(s) = 0 then exit(nil);
    System.SetLength(Result, System.Length(s));
    pS := @s[0];
    pR := Pointer(Result);
    I := 0;
    while (I < System.Length(s)) and aIsStopChar(pS[I]) do Inc(I);
    J := 0;
    NewWord := False;
    for I := I to System.High(s) do
      if aIsStopChar(pS[I]) then
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
      if aIsStopChar(p[I]) then begin
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
    r: TUcs4Seq;
  begin
    Words := aSplit(s, Count, Buf, False);
    System.SetLength(r, System.Length(s));
    pR := Pointer(r);
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
    System.SetLength(r, Len);
    SplitMerge := r;
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
    r: TUcs4Seq;
  begin
    System.SetLength(r, aSrcLen);
    pR := Pointer(r);
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
    System.SetLength(r, Len);
    Merge := r;
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
    if WordsL = nil then
      if WordsR = nil then
        exit(Double(1))
      else
        exit(Double(0))
    else
      if WordsR = nil then
        exit(Double(0));
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
      if SetL <> nil then begin
        J := System.Length(SetL);
        System.SetLength(SetL, Succ(System.Length(Intersection) + J));
        System.Move(SetL[0], SetL[System.Length(SetL) - J], J*SizeOf(Ucs4Char));
        System.Move(Intersection[0], SetL[0], System.Length(Intersection)*SizeOf(Ucs4Char));
        SetL[System.Length(Intersection)] := UCS4_SPACE;
      end else
        SetL := Intersection;
      if SetR <> nil then begin
        J := System.Length(SetR);
        System.SetLength(SetR, Succ(System.Length(Intersection) + J));
        System.Move(SetR[0], SetR[System.Length(SetR) - J], J*SizeOf(Ucs4Char));
        System.Move(Intersection[0], SetR[0], System.Length(Intersection)*SizeOf(Ucs4Char));
        SetR[System.Length(Intersection)] := UCS4_SPACE;
      end else
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
function SimRatioExUtf16Impl(const L, R: unicodestring; aIsStopChar: TUcs4CharTest; aMode: TSimMode;
  const aOptions: TSimOptions; aLimit: Double; Algo: TSeqDistanceAlgo; aLess: TUcs4Less): Double;
var
  LBufSt, RBufSt: array[0..Pred(MAX_STATIC)] of Ucs4Char;
  LBuf: TUcs4Seq = nil;
  RBuf: TUcs4Seq = nil;
  LenL, LenR: SizeInt;
  pL, pR: PUcs4Char;
  LocL, LocR: unicodestring;
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

  Result := SimRatioGeneric(
    pL[0..Pred(LenL)], pR[0..Pred(LenR)], aIsStopChar, aMode, soPartial in aOptions, aLimit, Algo, aLess);
end;
{$POP}

function SimRatioExUtf16(const L, R: unicodestring; const aStopChars: array of unicodestring; aMode: TSimMode;
  const aOptions: TSimOptions; aLimit: Double; Algo: TSeqDistanceAlgo; aLess: TUcs4Less): Double;
var
  StopChars: TUcs4CharSet;
  function IsStopChar(c: Ucs4Char): Boolean;
  begin
    Result := StopChars.Find(c) <> nil;
  end;
var
  I: SizeInt;
  c: Ucs4Char;
  p: TUcs4CharSet.PEntry;
begin
  for I := 0 to System.High(aStopChars) do
    if IsSingleCodePointUtf16(aStopChars[I], c) and not StopChars.FindOrAdd(c, p) then
      p^.Key := c;
  Result := SimRatioExUtf16Impl(L, R, @IsStopChar, aMode, aOptions, aLimit, Algo, aLess);
end;

const
  LETTER_OR_DIGIT_CATEGORIES = [
     TUnicodeCategory.ucUppercaseLetter, TUnicodeCategory.ucLowercaseLetter,
     TUnicodeCategory.ucTitlecaseLetter, TUnicodeCategory.ucModifierLetter,
     TUnicodeCategory.ucOtherLetter, TUnicodeCategory.ucDecimalNumber,
     TUnicodeCategory.ucLetterNumber, TUnicodeCategory.ucOtherNumber,
     TUnicodeCategory.ucConnectPunctuation];

function SimRatioExUtf16(const L, R: unicodestring; aMode: TSimMode; const aOptions: TSimOptions;
  aLimit: Double; Algo: TSeqDistanceAlgo; aLess: TUcs4Less): Double;
  function IsStopChar(c: Ucs4Char): Boolean;
  begin
    if c <= UC_TBL_HIGH then
      Result := not(TUnicodeCategory(UC_CATEGORY_TBL[c]) in LETTER_OR_DIGIT_CATEGORIES)
    else
      Result := not(TUnicodeCategory(GetProps(c)^.Category) in LETTER_OR_DIGIT_CATEGORIES);
  end;
begin
  Result := SimRatioExUtf16Impl(L, R, @IsStopChar, aMode, aOptions, aLimit, Algo, aLess);
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

function CodePointToUcs4Char(p: PByte; aLen: SizeInt; out aPtSize: SizeInt): Ucs4Char;
begin
  case p^ of
    0..$7f:
      begin
        aPtSize := 1;
        exit(p^);
      end;
    $c2..$df:
      if (aLen > 1) and (p[1] in [$80..$bf]) then begin
        aPtSize := 2;
        exit(Ucs4Char(Ucs4Char(p[0] and $1f) shl 6 or Ucs4Char(p[1] and $3f)));
      end;
    $e0:
      if (aLen > 2) and (p[1] in [$a0..$bf]) and (p[2] in [$80..$bf]) then begin
        aPtSize := 3;
        exit(Ucs4Char(Ucs4Char(p[0]and $f)shl 12 or Ucs4Char(p[1]and $3f)shl 6 or Ucs4Char(p[2] and $3f)));
      end;
    $e1..$ec, $ee..$ef:
      if (aLen > 2) and (p[1] in [$80..$bf]) and (p[2] in [$80..$bf]) then begin
        aPtSize := 3;
        exit(Ucs4Char(Ucs4Char(p[0]and $f)shl 12 or Ucs4Char(p[1]and $3f)shl 6 or Ucs4Char(p[2]and $3f)));
      end;
    $ed:
      if (aLen > 2) and (p[1] in [$80..$9f]) and (p[2] in [$80..$bf]) then begin
        aPtSize := 3;
        exit(Ucs4Char(Ucs4Char(p[0]and $f)shl 12 or Ucs4Char(p[1]and $3f)shl 6 or Ucs4Char(p[2]and $3f)));
      end;
    $f0:
      if (aLen > 3) and (p[1] in [$90..$bf]) and (p[2] in [$80..$bf]) and (p[3] in [$80..$bf]) then begin
        aPtSize := 4;
        exit(Ucs4Char(Ucs4Char(p[0] and $7) shl 18 or Ucs4Char(p[1] and $3f) shl 12 or
             Ucs4Char(p[2] and $3f) shl 6 or Ucs4Char(p[3] and $3f)));
      end;
    $f1..$f3:
      if (aLen > 3) and (p[1] in [$80..$bf]) and (p[2] in [$80..$bf]) and (p[3] in [$80..$bf]) then begin
        aPtSize := 4;
        exit(Ucs4Char(Ucs4Char(p[0] and $7) shl 18 or Ucs4Char(p[1] and $3f) shl 12 or
             Ucs4Char(p[2] and $3f) shl 6 or Ucs4Char(p[3] and $3f)));
      end;
    $f4:
      if (aLen > 3) and (p[1] in [$80..$8f]) and (p[2] in [$80..$bf]) and (p[3] in [$80..$bf]) then begin
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

procedure Ucs4Char2Utf8Buffer(var aBuf: PByte; c: DWord); inline;
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
    $800..$d7ff, $e000..$fffc, $fffe..$ffff:
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

{$PUSH}{$J-}
const
  UC_CASE_TBL: array[0..UC_TBL_HIGH] of DWord = ({$I uc_bmp_case_map.inc});
{$POP}

function Utf8ToLower(const s: string): string;
var
  pv, pr, pEnd: PByte;
  c, LoC: DWord;
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
      c := CodePointToUcs4Char(pv, PtSize);
      if c <= UC_TBL_HIGH then
        LoC := UC_CASE_TBL[c] and $ffff
      else
        begin
          LoC := UnicodeData.GetProps(c)^.SimpleLowerCase;
          if LoC = 0 then
            LoC := c;
        end;
      Ucs4Char2Utf8Buffer(pr, LoC);
      pv += PtSize;
    end;
  System.SetLength(r, pr - PByte(r));
  Result := r;
end;

function Utf8ToUpper(const s: string): string;
var
  pv, pr, pEnd: PByte;
  c, UpC: DWord;
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
      c := CodePointToUcs4Char(pv, PtSize);
      if c <= UC_TBL_HIGH then
        UpC := UC_CASE_TBL[c] shr 16
      else
        begin
          UpC := UnicodeData.GetProps(c)^.SimpleUpperCase;
          if UpC = 0 then
            UpC := c;
        end;
      Ucs4Char2Utf8Buffer(pr, UpC);
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
    dfisMbr:           Result := TUcs4Util.LevDistanceMBR(pL[0..Pred(LenL)], pR[0..Pred(LenR)]);
    dfisMyers:         Result := TUcs4Util.LevDistanceMyers(pL[0..Pred(LenL)], pR[0..Pred(LenR)]);
    dfisWmLcs:         Result := TUcs4Util.LcsDistanceWM(pL[0..Pred(LenL)], pR[0..Pred(LenR)]);
    dfisMbrDum:        Result := TUcs4Util.DumDistanceMBR(pL[0..Pred(LenL)], pR[0..Pred(LenR)]);
    dfisMbrBound:      Result := TUcs4Util.LevDistanceMBR(pL[0..Pred(LenL)], pR[0..Pred(LenR)], aLimit);
    dfisMyersBound:    Result := TUcs4Util.LevDistanceMyers(pL[0..Pred(LenL)], pR[0..Pred(LenR)], aLimit);
    dfisWmLcsBound:    Result := TUcs4Util.LcsDistanceWM(pL[0..Pred(LenL)], pR[0..Pred(LenR)], aLimit);
  else
    //dfisMbrDumBound
    Result := TUcs4Util.DumDistanceMBR(pL[0..Pred(LenL)], pR[0..Pred(LenR)], aLimit);
  end;
end;

function EditDistanceUtf8(const L, R: string; const aCost: TSeqEditCost): SizeInt;
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
  Result := TUcs4Util.EditDistance(pL[0..Pred(LenL)], pR[0..Pred(LenR)], aCost);
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

function LcsDistanceWmUtf8(const L, R: string): SizeInt;
begin
  Result := GenericDistanceUtf8(L, R, -1, dfisWmLcs);
end;

function LcsDistanceWmUtf8(const L, R: string; aLimit: SizeInt): SizeInt;
begin
  Result := GenericDistanceUtf8(L, R, aLimit, dfisWmLcsBound);
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

{ TUtf8Cp }

procedure TUtf8Cp.Init(aOfs, aCpOfs, aSize: SizeInt; c: Ucs4Char);
begin
  FOffset := aOfs;
  FCpOffset := aCpOfs;
  FSize := aSize;
  FCode := c;
end;

class function TUtf8Cp.HashCode(const aCp: TUtf8Cp): SizeInt;
begin
  Result := JdkHash(DWord(aCp.Code));
end;

class function TUtf8Cp.Equal(const L, R: TUtf8Cp): Boolean;
begin
  Result := L.Code = R.Code;
end;

function TUtf8Cp.ToString: string;
var
  b: TByte4;
  Len: Integer;
begin
  Len := Ucs4CharToUtf8Char(Code, b);
  SetString(Result, PAnsiChar(@b), Len);
end;

function TUtf8Cp.Write(aBuffer: PAnsiChar): Integer;
var
  p: PByte;
begin
  p := Pointer(aBuffer);
  Ucs4Char2Utf8Buffer(p, DWord(Code));
  Result := ByteSize;
end;

{ TUtf8CpSeqHelper }

class function TUtf8CpSeqHelper.Parse(const s: string): TUtf8CpSeq;
begin
  Result := Utf8StrToCpSeq(s);
end;

function TUtf8CpSeqHelper.ToString: string;
begin
  Result := CpSeqToUtf8Str(Self);
end;

function Utf8StrToCpSeq(const s: string): TUtf8CpSeq;
var
  r: TUtf8CpSeq;
  I, Ofs, CpOfs, Len: SizeInt;
  c: Ucs4Char;
  p, pEnd: PByte;
begin
  if s = '' then exit(nil);
  System.SetLength(r, System.Length(s));
  p := Pointer(s);
  pEnd := p + System.Length(s);
  Ofs := 1;
  CpOfs := 1;
  I := 0;
  Len := 0;
  while p < pEnd do
    begin
      c := CodePointToUcs4Char(p, Len);
      r[I].Init(Ofs, CpOfs, Len, c);
      Inc(I);
      Inc(CpOfs);
      p += Len;
      Ofs += Len;
    end;
  System.SetLength(r, I);
  Result := r;
end;

function CpSeqToUtf8Str(const s: array of TUtf8Cp): string;
var
  I, Len: SizeInt;
  p: PByte;
begin
  Result := '';
  if System.Length(s) = 0 then exit;
  Len := 0;
  for I := 0 to System.High(s) do
    Len += s[I].ByteSize;
  System.SetLength(Result, Len);
  p := Pointer(Result);
  for I := 0 to System.High(s) do
    Ucs4Char2Utf8Buffer(p, DWord(s[I].Code));
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

function SimRatioExUtf8Impl(const L, R: string; aIsStopChar: TUcs4CharTest; aMode: TSimMode;
  const aOptions: TSimOptions; aLimit: Double; Algo: TSeqDistanceAlgo; aLess: TUcs4Less): Double;
var
  LBufSt, RBufSt: array[0..Pred(MAX_STATIC)] of Ucs4Char;
  LBuf: TUcs4Seq = nil;
  RBuf: TUcs4Seq = nil;
  LenL, LenR: SizeInt;
  pL, pR: PUcs4Char;

var
  LocL, LocR: string;
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

  Result := SimRatioGeneric(
    pL[0..Pred(LenL)], pR[0..Pred(LenR)], aIsStopChar, aMode, soPartial in aOptions, aLimit, Algo, aLess);
end;

function SimRatioExUtf8(const L, R: string; const aStopChars: array of string; aMode: TSimMode;
  const aOptions: TSimOptions; aLimit: Double; Algo: TSeqDistanceAlgo; aLess: TUcs4Less): Double;
var
  StopChars: TUcs4CharSet;

  function IsStopChar(c: Ucs4Char): Boolean;
  begin
    Result := StopChars.Find(c) <> nil;
  end;

var
  I: SizeInt;
  c: Ucs4Char;
  p: TUcs4CharSet.PEntry;
begin
  for I := 0 to System.High(aStopChars) do
    if IsSingleCodePointUtf8(aStopChars[I], c) and not StopChars.FindOrAdd(c, p) then
      p^.Key := c;
  Result := SimRatioExUtf8Impl(L, R, @IsStopChar, aMode, aOptions, aLimit, Algo, aLess);
end;

function SimRatioExUtf8(const L, R: string; aMode: TSimMode; const aOptions: TSimOptions; aLimit: Double;
  Algo: TSeqDistanceAlgo; aLess: TUcs4Less): Double;
  function IsStopChar(c: Ucs4Char): Boolean;
  begin
    if c <= UC_TBL_HIGH then
      Result := not(TUnicodeCategory(UC_CATEGORY_TBL[c]) in LETTER_OR_DIGIT_CATEGORIES)
    else
      Result := not(TUnicodeCategory(GetProps(c)^.Category) in LETTER_OR_DIGIT_CATEGORIES);
  end;
begin
  Result := SimRatioExUtf8Impl(L, R, @IsStopChar, aMode, aOptions, aLimit, Algo, aLess);
end;

function SimRatioListUtf8Impl(const aPattern: string; const aValues: array of string;
  aIsStopChar: TUcs4CharTest; aMode: TSimMode; const aOptions: TSimOptions; aLimit: Double;
  Algo: TSeqDistanceAlgo; aLess: TUcs4Less): specialize TGArray<Double>;
var
  LBufSt, RBufSt: array[0..Pred(MAX_STATIC)] of Ucs4Char;
  LBuf: TUcs4Seq = nil;
  RBuf: TUcs4Seq = nil;
  LenL, LenR, I: SizeInt;
  pL, pR: PUcs4Char;
  Pattern, Value: string;
  r: array of Double;
begin
  if System.Length(aValues) < 1 then exit(nil);

  if soIgnoreCase in aOptions then
    Pattern := Utf8ToLower(aPattern)
  else
    Pattern := aPattern;

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
      pL[0..Pred(LenL)], pR[0..Pred(LenR)], aIsStopChar, aMode, soPartial in aOptions, aLimit, Algo, aLess);
  end;

  Result := r;
end;

function SimRatioListUtf8(const aPattern: string; const aValues: array of string;
  const aStopChars: array of string; aMode: TSimMode; const aOptions: TSimOptions; aLimit: Double;
  Algo: TSeqDistanceAlgo; aLess: TUcs4Less): specialize TGArray<Double>;
var
  StopChars: TUcs4CharSet;

  function IsStopChar(c: Ucs4Char): Boolean;
  begin
    Result := StopChars.Find(c) <> nil;
  end;

var
  I: SizeInt;
  c: Ucs4Char;
  p: TUcs4CharSet.PEntry;
begin
  for I := 0 to System.High(aStopChars) do
    if IsSingleCodePointUtf8(aStopChars[I], c) and not StopChars.FindOrAdd(c, p) then
      p^.Key := c;
  Result := SimRatioListUtf8Impl(aPattern, aValues, @IsStopChar, aMode, aOptions, aLimit, Algo, aLess);
end;

function SimRatioListUtf8(const aPattern: string; const aValues: array of string; aMode: TSimMode;
  const aOptions: TSimOptions; aLimit: Double; Algo: TSeqDistanceAlgo; aLess: TUcs4Less): specialize TGArray<Double>;
  function IsStopChar(c: Ucs4Char): Boolean;
  begin
    if c <= UC_TBL_HIGH then
      Result := not(TUnicodeCategory(UC_CATEGORY_TBL[c]) in LETTER_OR_DIGIT_CATEGORIES)
    else
      Result := not(TUnicodeCategory(GetProps(c)^.Category) in LETTER_OR_DIGIT_CATEGORIES);
  end;
begin
  Result := SimRatioListUtf8Impl(aPattern, aValues, @IsStopChar, aMode, aOptions, aLimit, Algo, aLess);
end;

function SelectSimilarUtf8(const aPattern: string; const aValues: array of string;
  const aStopChars: array of string; aLimit: Double; aMode: TSimMode;
  const aOptions: TSimOptions; Algo: TSeqDistanceAlgo; aLess: TUcs4Less): specialize TGArray<TStringRatio>;
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
    if ratios[I] >= aLimit then begin
      r[J].Value := aValues[I];
      r[J].Ratio := ratios[I];
      Inc(J);
    end;
  System.SetLength(r, J);
  specialize TGNestedArrayHelper<TStringRatio>.Sort(r, @Less);
  Result := r;
end;

function SelectSimilarUtf8(const aPattern: string; const aValues: array of string; aLimit: Double; aMode: TSimMode;
  const aOptions: TSimOptions; Algo: TSeqDistanceAlgo; aLess: TUcs4Less): specialize TGArray<TStringRatio>;
  function IsStopChar(c: Ucs4Char): Boolean;
  begin
    if c <= UC_TBL_HIGH then
      Result := not(TUnicodeCategory(UC_CATEGORY_TBL[c]) in LETTER_OR_DIGIT_CATEGORIES)
    else
      Result := not(TUnicodeCategory(GetProps(c)^.Category) in LETTER_OR_DIGIT_CATEGORIES);
  end;
  function Less(const L, R: TStringRatio): Boolean;
  begin
    Result := R.Ratio < L.Ratio;
  end;
var
  ratios: array of Double;
  r: array of TStringRatio;
  I, J: SizeInt;
begin
  ratios := SimRatioListUtf8Impl(aPattern, aValues, @IsStopChar, aMode, aOptions, aLimit, Algo, aLess);
  System.SetLength(r, System.Length(ratios));
  J := 0;
  for I := 0 to System.High(ratios) do
    if ratios[I] >= aLimit then begin
      r[J].Value := aValues[I];
      r[J].Ratio := ratios[I];
      Inc(J);
    end;
  System.SetLength(r, J);
  specialize TGNestedArrayHelper<TStringRatio>.Sort(r, @Less);
  Result := r;
end;

function IsWhiteSpaceUcs4(aChar: DWord): Boolean; //inline;
const
  WS = [9, 10, 11, 12, 13, 32, 133, 160];
begin
  if aChar < $a1 then
    exit(aChar in WS)
  else
    if aChar > $167f then
      exit(UnicodeData.GetProps(aChar)^.WhiteSpace);
  Result := False;
end;

function Ucs4CharToLower(aChar: DWord): DWord; inline;
begin
  if aChar <= UC_TBL_HIGH then
    Result := UC_CASE_TBL[aChar] and $ffff
  else begin
    Result := UnicodeData.GetProps(aChar)^.SimpleLowerCase;
    if Result = 0 then
      Result := aChar;
  end;
end;

function FilterStringUtf8(const s: string; pBuf: PByte; const aOpts: TStrCompareOptions): SizeInt;
var
  pSrc, pBufStart, pEnd: PByte;
  c: DWord;
  PtSize: SizeInt;
  WsSkipping: Boolean;
begin
  // here is implied ((scoIgnoreWS or scoIgnoreWSChange) or scoIgnoreCase) is set
  pBufStart := pBuf;
  pSrc := PByte(s);
  pEnd := pSrc + System.Length(s);
  PtSize := 0;
  if scoIgnoreWS in aOpts then begin
    if scoIgnoreCase in aOpts then
      while pSrc < pEnd do begin
        c := CodePointToUcs4Char(pSrc, PtSize);
        pSrc += PtSize;
        if IsWhiteSpaceUcs4(c) then continue;
        Ucs4Char2Utf8Buffer(pBuf, Ucs4CharToLower(c));
      end
    else
      while pSrc < pEnd do begin
        c := CodePointToUcs4Char(pSrc, PtSize);
        pSrc += PtSize;
        if IsWhiteSpaceUcs4(c) then continue;
        Ucs4Char2Utf8Buffer(pBuf, c);
      end
  end else
    if scoIgnoreWSChange in aOpts then begin
      WsSkipping := False;
      if scoIgnoreCase in aOpts then
        while pSrc < pEnd do begin
          c := CodePointToUcs4Char(pSrc, PtSize);
          pSrc += PtSize;
          if IsWhiteSpaceUcs4(c) then begin
            if WsSkipping then continue;
            WsSkipping := True;
            Ucs4Char2Utf8Buffer(pBuf, Ucs4CharToLower(c));
          end else begin
            WsSkipping := False;
            Ucs4Char2Utf8Buffer(pBuf, Ucs4CharToLower(c));
          end;
        end
      else
        while pSrc < pEnd do begin
          c := CodePointToUcs4Char(pSrc, PtSize);
          pSrc += PtSize;
          if IsWhiteSpaceUcs4(c) then begin
          if WsSkipping then continue;
            WsSkipping := True;
            Ucs4Char2Utf8Buffer(pBuf, c);
          end else begin
            WsSkipping := False;
            Ucs4Char2Utf8Buffer(pBuf, c);
          end;
        end
    end else // aIgnoreCase
      while pSrc < pEnd do begin
        c := CodePointToUcs4Char(pSrc, PtSize);
        Ucs4Char2Utf8Buffer(pBuf, Ucs4CharToLower(c));
        pSrc += PtSize;
      end;
  Result := pBuf - pBufStart;
end;

function Utf8HashText(const s: string; aHash: TBufHash32; const aOpts: TStrCompareOptions; aSeed: DWord): DWord;
var
  StBuf: array[0..Pred(MAX_STATIC div SizeOf(DWord))] of DWord;
  Buf: specialize TGDynArray<DWord>;
  Len: SizeInt;
  pBuf: PByte;
begin
  if (s = '') or (aOpts = []) then exit(aHash(Pointer(s), System.Length(s), aSeed));
  Len := System.Length(s);
  if scoIgnoreCase in aOpts then Len += Len;
  if Len <= SizeOf(StBuf) then
    pBuf := @StBuf[0]
  else
    begin
      Buf.Length := (Len + Pred(SizeOf(DWord))) div SizeOf(DWord);
      pBuf := Pointer(Buf.Ptr);
    end;
  Len := FilterStringUtf8(s, pBuf, aOpts);
  Result := aHash(pBuf, Len, aSeed);
end;

function Utf8HashText64(const s: string; aHash: TBufHash64; const aOpts: TStrCompareOptions; aSeed: QWord): QWord;
var
  StBuf: array[0..Pred(MAX_STATIC div SizeOf(QWord))] of QWord;
  Buf: specialize TGDynArray<QWord>;
  Len: SizeInt;
  pBuf: PByte;
begin
  if (s = '') or (aOpts = []) then exit(aHash(Pointer(s), System.Length(s), aSeed));
  Len := System.Length(s);
  if scoIgnoreCase in aOpts then Len += Len;
  if Len <= SizeOf(StBuf) then
    pBuf := @StBuf[0]
  else
    begin
      Buf.Length := (Len + Pred(SizeOf(QWord))) div SizeOf(QWord);
      pBuf := Pointer(Buf.Ptr);
    end;
  Len := FilterStringUtf8(s, pBuf, aOpts);
  Result := aHash(pBuf, Len, aSeed);
end;

function Utf8SameText(const L, R: string; const aOpts: TStrCompareOptions): Boolean;
var
  LBuf, RBuf: specialize TGDynArray<Byte>;
  LenL, LenR: SizeInt;
begin
  if aOpts = [] then exit(L = R);
  if scoIgnoreCase in aOpts then
    begin
      LBuf.Length := System.Length(L)*2;
      RBuf.Length := System.Length(R)*2;
    end
  else
    begin
      LBuf.Length := System.Length(L);
      RBuf.Length := System.Length(R);
    end;
  LenL := FilterStringUtf8(L, LBuf.Ptr, aOpts);
  LenR := FilterStringUtf8(R, RBuf.Ptr, aOpts);
  if LenL <> LenR then
    exit(False)
  else
    if LenL = 0 then
      exit(True);
  Result := CompareByte(LBuf.Ptr^, RBuf.Ptr^, LenL) = 0;
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
      c := CodePointToUcs4Char(@FText[FTextIndex], Succ(TextLen - FTextIndex), I);
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
    c := CodePointToUcs4Char(@aText[TextPos], Succ(TextLen - TextPos), cLen);
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
    c := CodePointToUcs4Char(@FText[FTextIndex], Succ(TextLen - FTextIndex), cLen);
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
  TACFsmUtf8Class = class of TACFsmUtf8;

  { TACFsmUtf8 }
  TACFsmUtf8 = class abstract(TInterfacedObject, IACSearchFsmUtf8)
  type
    TMatch         = LgUtils.TIndexMatch;
    TMatchArray    = specialize TGArray<TMatch>;
    TOnMatch       = specialize TGOnTest<TMatch>;
    TNestMatch     = specialize TGNestTest<TMatch>;
    IStrEnumerable = specialize IGEnumerable<string>;
  protected
  type
    TOfsEntry = record
      Offset: SizeInt;
      vChar: Ucs4Char;
    end;
    TSortHelper   = specialize TGRegularTimSort<TMatch>;
    TMatchLess    = specialize TGLessCompare<TMatch>;
    TLeftmostMode = smmLeftmostFirst..smmLeftmostShortest;
  var
    FQueue: array of TOfsEntry;
    FOnMatchHandler: TOnMatch;
    FNestMatchHandler: TNestMatch;
    FOnMatch: TOnMatch;
    FQueueSize,
    FNodeCount,
    FWordCount,
    FAlphabetSize: Int32;
    FIgnoreCase,
    FWholeWordsOnly: Boolean;
    class function  IsWordChar(c: Ucs4Char): Boolean; static; inline;
    class function  CpToUcs4(var p: PByte; aLen: SizeInt): Ucs4Char; static; inline;
    class function  RtlUcs4Lower(c: Ucs4Char): Ucs4Char; static; inline;
    class function  CpToUcs4Lower(var p: PByte; aLen: SizeInt): Ucs4Char; static; inline;
    class function  CpToUcs4Lower(p: PByte; aLen: SizeInt; out aPtSize: SizeInt): Ucs4Char; static; inline;
    class procedure DoFilterMatches(var aMatches: TMatchArray; aMode: TSetMatchMode); static;
    class function  SelectComparer(aMode: TLeftmostMode): TMatchLess; static; inline;
    class function  GetFsmClass: TACFsmUtf8Class; virtual; abstract;
    class function  GetDfaClass(aIgnoreCase: Boolean): TACFsmUtf8Class; inline;
    class function  GetNfaClass(aIgnoreCase: Boolean): TACFsmUtf8Class; inline;
    function  TestOnMatch(const m: TMatch): Boolean;
    function  TestNestMatch(const m: TMatch): Boolean;
    procedure RegisterMatchHandler(h: TOnMatch);
    procedure RegisterMatchHandler(h: TNestMatch);
    function  IsDfa: Boolean; virtual;
    function  GetAlphabetSize: SizeInt; virtual;
    function  GetStateCount: SizeInt;
    function  GetEmptyCount: SizeInt; virtual;
    function  GetPatternCount: SizeInt;
    function  GetCaseInsensitive: Boolean;
    function  GetOnlyWholeWords: Boolean;
    procedure SetOnlyWholeWords(aValue: Boolean);
    function  DoProlog(const s: string; aOfs, aCount: SizeInt; out pText, pStart, pEnd: PByte): Boolean; inline;
    function  DoProlog(const s: string; aOfs, aCount: SizeInt; out pStart, pEnd: PByte): Boolean; inline;
    function  DoFindFirst(const s: string; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch; virtual; abstract;
    function  DoFindFirstOww(const s: string; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch; virtual; abstract;
    procedure DoSearchNoOverlap(const s: string; aOffset, aCount: SizeInt; aHandler: TNestMatch); virtual; abstract;
    procedure DoSearchNoOverlapOww(const s: string; aOffset, aCount: SizeInt; aHandler: TNestMatch); virtual; abstract;
    procedure DoSearch(const s: string; aOffset, aCount: SizeInt); virtual; abstract;
    procedure DoSearchOww(const s: string; aOffset, aCount: SizeInt); virtual; abstract;
    function  GetHasMatch(const s: string; aOffset, aCount: SizeInt): Boolean; virtual; abstract;
    function  GetHasMatchOww(const s: string; aOffset, aCount: SizeInt): Boolean; virtual; abstract;
    function  PushOffset(aQueueTop, aOffs: SizeInt): SizeInt; inline;
    function  PushOffset(aQueueTop, aOffs: SizeInt; aChar: Ucs4Char): SizeInt; inline;
    function  PushChar(aQueueTop: SizeInt; aChar: Ucs4Char): SizeInt; inline;
    function  OnWordBounds(pCurr, pEnd: PByte; aQueueTop, aWordLen: SizeInt): Boolean;
    function  PopOffset(aQueueTop, aWordLen: SizeInt): SizeInt; inline;
    function  PrevCpOffset(const s: string; aOffset: SizeInt): SizeInt; inline;
    function  PushFirstOffset(const s: string; aOffset: SizeInt): SizeInt;
  public
    class function CreateInstance(const aPatternList: array of string; aIgnoreCase, aForceNFA: Boolean): TACFsmUtf8;
    constructor Create; virtual;
    function  TryBuildFsm(const aPatternList: array of string): Boolean; virtual; abstract;
    function  Clone: IACSearchFsmUtf8; virtual; abstract;
    function  IndexOfPattern(const s: string; aOffset: SizeInt = 1; aCount: SizeInt = 0): SizeInt; virtual; abstract;
    function  IsMatch(const s: string; aOffset: SizeInt = 1; aCount: SizeInt = 0): Boolean;
    function  FirstMatch(const aText: string; aMode: TSetMatchMode = smmDefault; aOffset: SizeInt = 1;
                         aCount: SizeInt = 0): TMatch;
    function  FindMatches(const aText: string; aMode: TSetMatchMode = smmDefault; aOffset: SizeInt = 1;
                          aCount: SizeInt = 0): TMatchArray;
    procedure Search(const aText: string; aOnMatch: TOnMatch; aOffset: SizeInt = 1; aCount: SizeInt = 0);
    procedure Search(const aText: string; aOnMatch: TNestMatch; aOffset: SizeInt = 1; aCount: SizeInt = 0);
    function  ContainsMatch(const aText: string; aOffset: SizeInt = 1; aCount: SizeInt = 0): Boolean;
    function  FilterMatches(const aSource: array of TMatch; aMode: TSetMatchMode): TMatchArray;
    property  NodeCount: Int32 read FNodeCount;
    property  PatternCount: Int32 read FWordCount;
    property  OnlyWholeWords: Boolean read FWholeWordsOnly write FWholeWordsOnly;
    property  CaseInsensitive: Boolean read FIgnoreCase;
  end;

  { TACDfaUtf8 }
  TACDfaUtf8 = class(TACFsmUtf8)
  protected
  const
    BMP_MAX  = Int32($ffff);
    MAX_CODE = High(ShortInt);
  type
    TNode = record
      NextMove: array of Int32;// transition table
      Output,                  // output link(singly linked list)
      Index,                   // index in the input list if node is teminal
      Length,                  // length in bytes, Length > 0 indicates a terminal node
      Utf8Len: Int32;          // length in code points
    end;
    TIntQueue = specialize TGLiteQueue<Int32>;
  protected
    FTrie: array of TNode;
    FCharMap: array of Int32;
    function  GetAlphabetSize: SizeInt; override;
    function  BuildCharMap(const aList: array of string): Boolean;
    function  NewNode: SizeInt; inline;
    procedure AddPattern(const aValue: string; aIndex: SizeInt);
    function  NextFsmState(aState: Int32; c: Ucs4Char): Int32; inline;
    procedure BuildFsm;
    function  DoFindFirst(const s: string; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch; override;
    function  DoFindFirstOww(const s: string; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch; override;
    procedure DoSearchNoOverlap(const s: string; aOffset, aCount: SizeInt; aOnMatch: TNestMatch); override;
    procedure DoSearchNoOverlapOww(const s: string; aOffset, aCount: SizeInt; aOnMatch: TNestMatch); override;
    procedure DoSearch(const s: string; aOffset, aCount: SizeInt); override;
    procedure DoSearchOww(const s: string; aOffset, aCount: SizeInt); override;
    function  GetHasMatch(const s: string; aOffset, aCount: SizeInt): Boolean; override;
    function  GetHasMatchOww(const s: string; aOffset, aCount: SizeInt): Boolean; override;
    class function GetFsmClass: TACFsmUtf8Class; override;
  public
    function  TryBuildFsm(const aPatternList: array of string): Boolean; override;
    function  Clone: IACSearchFsmUtf8; override;
    function  IndexOfPattern(const s: string; aOffset: SizeInt = 1; aCount: SizeInt = 0): SizeInt; override;
    property  AlphabetSize: Int32 read FAlphabetSize;
  end;

  { TACDfaCIUtf8 }
  TACDfaCIUtf8 = class(TACDfaUtf8)
  protected
    function  DoFindFirst(const s: string; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch; override;
    function  DoFindFirstOww(const s: string; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch; override;
    procedure DoSearchNoOverlap(const s: string; aOffset, aCount: SizeInt; aOnMatch: TNestMatch); override;
    procedure DoSearchNoOverlapOww(const s: string; aOffset, aCount: SizeInt; aOnMatch: TNestMatch); override;
    procedure DoSearch(const s: string; aOffset, aCount: SizeInt); override;
    procedure DoSearchOww(const s: string; aOffset, aCount: SizeInt); override;
    function  GetHasMatch(const s: string; aOffset, aCount: SizeInt): Boolean; override;
    function  GetHasMatchOww(const s: string; aOffset, aCount: SizeInt): Boolean; override;
    class function GetFsmClass: TACFsmUtf8Class; override;
  public
    constructor Create; override;
  end;

  { TCode2StateMap: for internal use only }
  TCode2StateMap = record
  type
    TEntry = specialize TGMapEntry<Int32, Int32>;
    PEntry = ^TEntry;
    TEnumerator = record
    private
      FCurrent,
      FLast: PEntry;
      function GetCurrent: TEntry; inline;
    public
      function MoveNext: Boolean; inline;
      property Current: TEntry read GetCurrent;
    end;
  strict private
  const
    INITIAL_SIZE = 4;
  var
    FItems: array of TEntry;
    FCount: Int32;
    function DoFind(aKey: Int32; out aIndex: Int32): PEntry;
    function DoAdd(aKey, aIndex: Int32): PEntry; inline;
    function GetKey(aIndex: Int32): Int32; inline;
    function GetValue(aIndex: Int32): Int32; inline;
  public
    function GetEnumerator: TEnumerator; inline;
    function GetMutValueDef(aKey, aDefault: Int32): PInt32; inline;
    property Count: Int32 read FCount;
    property Keys[aIndex: Int32]: Int32 read GetKey;
    property Values[aIndex: Int32]: Int32 read GetValue;
  end;

  TAcMagicSeq = array[0..6] of AnsiChar;
  TAcStreamHeader = record
    Magic: TAcMagicSeq;
    Version,
    PatternListSize,
    RealPatternCount,
    StateCount,
    AlphabetSize,
    QueueSize: Int32;
    CaseInsensitive: Boolean;
    procedure WriteStream(aStream: TStream);
    procedure ReadStream(aStream: TStream);
  end;

{$PUSH}{$INTERFACES COM}
   IAcFsmStreamWriter = interface
   ['{9DC816DE-2DCD-43C3-86BC-497CC87B1FBC}']
     function  GetQueueSize: Int32;
     procedure FsmStreamWrite(aStream: TStream);
   end;
{$POP}

  { TACNfaUtf8: based on double array trie approach }
  TACNfaUtf8 = class(TACFsmUtf8, IAcFsmStreamWriter)
  protected
  type
    TOutput = record
      Len,          // length in bytes
      CpLen,        // length in code points
      Index: Int32; // index in the input list
      procedure ReadStream(aStream: TStream);
      procedure WriteStream(aStream: TStream);
    end;
    TNode = record
      AdjList: TCode2StateMap;
      Output: Int32;
    end;
    TDaNode = record
      Base,
      Check,
      Failure,        // failure link: singly linked list
      Output,         // output link: index in FOutput array
      NextOut: Int32; // next output link: singly linked list
      procedure ReadStream(aStream: TStream);
      procedure WriteStream(aStream: TStream);
    end;
    TPair = record
      Node,
      DaNode: Int32;
      constructor Make(aNode, aDaNode: Int32);
    end;
    TPairQueue = specialize TGLiteQueue<TPair>;
  const
    NULL_NODE         = Low(Int32);
    LEAF_NODE         = Int32(0);
    BIG_ALPHABET_SIZE = 1024; //todo: need some tweaking?
  protected
    FDaTrie: array of TDaNode;
    FOutput: array of TOutput;
    FCharMap: array of Int32;
    FTrie: array of TNode;
    FMaxChar: Ucs4Char;
    FOutCount: Int32;
    function  NewNode: SizeInt; inline;
    function  NewOutput(aIndex, aLen, aCpLen: Int32): Int32; inline;
    procedure BuildCharMap(const aPatternList: array of string);
    function  EncodeChar(c: Ucs4Char): Int32; inline;
    procedure AddPattern(const aValue: string; aIndex: SizeInt);
    function  NextMove(aState, aCode: Int32): Int32; inline;
    function  NextFsmState(aState, aCode: Int32): Int32; inline;
  { uses a slightly modified LINK method to create a double array-like structure }
    procedure BuildFsm;
    function  IsDfa: Boolean; override;
    function  DoFindFirst(const s: string; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch; override;
    function  DoFindFirstOww(const s: string; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch; override;
    procedure DoSearchNoOverlap(const s: string; aOffset, aCount: SizeInt; aOnMatch: TNestMatch); override;
    procedure DoSearchNoOverlapOww(const s: string; aOffset, aCount: SizeInt; aOnMatch: TNestMatch); override;
    procedure DoSearch(const s: string; aOffset, aCount: SizeInt); override;
    procedure DoSearchOww(const s: string; aOffset, aCount: SizeInt); override;
    function  GetHasMatch(const s: string; aOffset, aCount: SizeInt): Boolean; override;
    function  GetHasMatchOww(const s: string; aOffset, aCount: SizeInt): Boolean; override;
    function  GetEmptyCount: SizeInt; override;
    function  GetAlphabetSize: SizeInt; override;
    function  GetQueueSize: Int32;
    procedure FsmStreamWrite(aStream: TStream);
    procedure FsmStreamRead(aStream: TStream; const aHeader: TAcStreamHeader);
    class function GetFsmClass: TACFsmUtf8Class; override;
    class function CreateInstance(aCaseInsensitive: Boolean): TACNfaUtf8;
    property  OutCount: Int32 read FOutCount;
  public
    constructor Create; override;
    function  TryBuildFsm(const aPatternList: array of string): Boolean; override;
    function  Clone: IACSearchFsmUtf8; override;
    function  IndexOfPattern(const s: string; aOffset: SizeInt = 1; aCount: SizeInt = 0): SizeInt; override;
    property  AlphabetSize: Int32 read FAlphabetSize;
  end;

  { TACNfaCIUtf8 }
  TACNfaCIUtf8 = class(TACNfaUtf8)
  protected
    function  DoFindFirst(const s: string; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch; override;
    function  DoFindFirstOww(const s: string; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch; override;
    procedure DoSearchNoOverlap(const s: string; aOffset, aCount: SizeInt; aOnMatch: TNestMatch); override;
    procedure DoSearchNoOverlapOww(const s: string; aOffset, aCount: SizeInt; aOnMatch: TNestMatch); override;
    procedure DoSearch(const s: string; aOffset, aCount: SizeInt); override;
    procedure DoSearchOww(const s: string; aOffset, aCount: SizeInt); override;
    function  GetHasMatch(const s: string; aOffset, aCount: SizeInt): Boolean; override;
    function  GetHasMatchOww(const s: string; aOffset, aCount: SizeInt): Boolean; override;
    class function GetFsmClass: TACFsmUtf8Class; override;
  public
    constructor Create; override;
  end;

  { TACPersistFsmUtf8 }
  TACPersistFsmUtf8 = class(TInterfacedObject, IACPersistFsmUtf8)
  protected
  const
  {$PUSH}{$J-}
    MAGIC_SEQ: TAcMagicSeq = 'IAcFsm8';
  {$POP}
    BUF_SIZE           = $10000;
    STREAM_FMT_VERSION = 1;
  protected
    FInstance: IACSearchFsmUtf8;
    FPatternList: TStringArray;
  public
    constructor Create(const aPatterns: array of string; aIgnoreCase: Boolean); overload;
    function  GetFsm: IACSearchFsmUtf8;
    function  GetPatternListSize: Integer;
    function  GetPattern(aIndex: Integer): string;
    function  GetPatternList: TStringArray;
    procedure LoadFromStream(aStream: TStream);
    procedure SaveToStream(aStream: TStream);
    procedure LoadFromFile(const aFileName: string);
    procedure SaveToFile(const aFileName: string);
  end;

{ TACFsmUtf8 }

class function TACFsmUtf8.IsWordChar(c: Ucs4Char): Boolean;
begin
  if c <= UC_TBL_HIGH then
    Result := TUnicodeCategory(UC_CATEGORY_TBL[c]) in LETTER_OR_DIGIT_CATEGORIES
  else
    Result := TUnicodeCategory(GetProps(c)^.Category) in LETTER_OR_DIGIT_CATEGORIES;
end;

class function TACFsmUtf8.CpToUcs4(var p: PByte; aLen: SizeInt): Ucs4Char;
var
  PtSize: SizeInt;
begin
  Result := CodePointToUcs4Char(p, aLen, PtSize);
  p += PtSize;
end;

class function TACFsmUtf8.RtlUcs4Lower(c: Ucs4Char): Ucs4Char;
begin
  Result := DWord(UnicodeData.GetProps(c)^.SimpleLowerCase);
  if Result = 0 then
    Result := c;
end;

class function TACFsmUtf8.CpToUcs4Lower(var p: PByte; aLen: SizeInt): Ucs4Char;
begin
  Result := CpToUcs4(p, aLen);
  if Result <= UC_TBL_HIGH then
    Result := UC_CASE_TBL[Result] and $ffff
  else
    Result := RtlUcs4Lower(Result);
end;

class function TACFsmUtf8.CpToUcs4Lower(p: PByte; aLen: SizeInt; out aPtSize: SizeInt): Ucs4Char;
begin
  Result := CodePointToUcs4Char(p, aLen, aPtSize);
  if Result <= UC_TBL_HIGH then
    Result := UC_CASE_TBL[Result] and $ffff
  else
    Result := RtlUcs4Lower(Result);
end;

function MatchCompareNO(const L, R: TAcMatch): Boolean;
begin
  Result := L.Offset < R.Offset;
end;

function MatchCompareLF(const L, R: TAcMatch): Boolean;
begin
  if L.Offset = R.Offset then
    Result := L.Index < R.Index
  else
    Result := L.Offset < R.Offset;
end;

function MatchCompareLL(const L, R: TAcMatch): Boolean;
begin
  if L.Offset = R.Offset then
    Result := L.Length > R.Length
  else
    Result := L.Offset < R.Offset;
end;

function MatchCompareLS(const L, R: TAcMatch): Boolean;
begin
  if L.Offset = R.Offset then
    Result := L.Length < R.Length
  else
    Result := L.Offset < R.Offset;
end;

class procedure TACFsmUtf8.DoFilterMatches(var aMatches: TMatchArray; aMode: TSetMatchMode);
var
  Count, I, Len, Ofs: SizeInt;
begin
  if aMatches = nil then exit;
  case aMode of
    smmDefault:          exit;
    smmNonOverlapping:   TSortHelper.Sort(aMatches, @MatchCompareNO);
    smmLeftmostFirst:    TSortHelper.Sort(aMatches, @MatchCompareLF);
    smmLeftmostLongest:  TSortHelper.Sort(aMatches, @MatchCompareLL);
    smmLeftmostShortest: TSortHelper.Sort(aMatches, @MatchCompareLS);
  end;
  Count := 0;
  I := 0;
  Len := System.Length(aMatches);
  repeat
    Ofs := aMatches[Count].Offset + aMatches[Count].Length;
    Inc(Count);
    Inc(I);
    while (I < Len) and (aMatches[I].Offset < Ofs) do Inc(I);
    if I >= Len then break;
    if I <> Count then
      aMatches[Count] := aMatches[I];
  until False;
  System.SetLength(aMatches, Count);
end;

class function TACFsmUtf8.SelectComparer(aMode: TLeftmostMode): TMatchLess;
begin
  case aMode of
    smmLeftmostFirst:    Result := @MatchCompareLF;
    smmLeftmostLongest:  Result := @MatchCompareLL;
  else // smmLeftmostShortest
    Result := @MatchCompareLS;
  end;
end;

class function TACFsmUtf8.GetDfaClass(aIgnoreCase: Boolean): TACFsmUtf8Class;
begin
  if aIgnoreCase then
    Result := TACDfaCIUtf8
  else
    Result := TACDfaUtf8;
end;

class function TACFsmUtf8.GetNfaClass(aIgnoreCase: Boolean): TACFsmUtf8Class;
begin
  if aIgnoreCase then
    Result := TACNfaCIUtf8
  else
    Result := TACNfaUtf8;
end;

function TACFsmUtf8.TestOnMatch(const m: TMatch): Boolean;
begin
  Result := FOnMatchHandler(m);
end;

function TACFsmUtf8.TestNestMatch(const m: TMatch): Boolean;
begin
  Result := FNestMatchHandler(m);
end;

procedure TACFsmUtf8.RegisterMatchHandler(h: TOnMatch);
begin
  FOnMatchHandler := h;
  FOnMatch := @TestOnMatch;
end;

procedure TACFsmUtf8.RegisterMatchHandler(h: TNestMatch);
begin
  FNestMatchHandler := h;
  FOnMatch := @TestNestMatch;
end;

function TACFsmUtf8.IsDfa: Boolean;
begin
  Result := True;
end;

function TACFsmUtf8.GetAlphabetSize: SizeInt;
begin
  Result := -1;
end;

function TACFsmUtf8.GetStateCount: SizeInt;
begin
  Result := FNodeCount;
end;

function TACFsmUtf8.GetEmptyCount: SizeInt;
begin
  Result := 0;
end;

function TACFsmUtf8.GetPatternCount: SizeInt;
begin
  Result := FWordCount;
end;

function TACFsmUtf8.GetCaseInsensitive: Boolean;
begin
  Result := FIgnoreCase;
end;

function TACFsmUtf8.GetOnlyWholeWords: Boolean;
begin
  Result := FWholeWordsOnly;
end;

procedure TACFsmUtf8.SetOnlyWholeWords(aValue: Boolean);
begin
  FWholeWordsOnly := aValue;
end;

function TACFsmUtf8.DoProlog(const s: string; aOfs, aCount: SizeInt; out pText, pStart, pEnd: PByte): Boolean;
begin
  if (s = '') or (PatternCount = 0) then
    exit(False);
  if aOfs < 1 then
    aOfs := 1;
  if aCount < 1 then
    aCount := System.Length(s)
  else
    aCount := Math.Min(Pred(aOfs + aCount), System.Length(s));
  if aOfs > aCount then exit(False);
  pText := Pointer(s);
  pStart := pText + Pred(aOfs);
  pEnd := pText + aCount;
  Result := True;
end;

function TACFsmUtf8.DoProlog(const s: string; aOfs, aCount: SizeInt; out pStart, pEnd: PByte): Boolean;
begin
  if (s = '') or (PatternCount = 0) then
    exit(False);
  if aOfs < 1 then
    aOfs := 1;
  if aCount < 1 then
    aCount := System.Length(s)
  else
    aCount := Math.Min(Pred(aOfs + aCount), System.Length(s));
  if aOfs > aCount then exit(False);
  pStart := PByte(s) + Pred(aOfs);
  pEnd := PByte(s) + aCount;
  Result := True;
end;

function TACFsmUtf8.PushOffset(aQueueTop, aOffs: SizeInt): SizeInt;
begin
  FQueue[aQueueTop].Offset := aOffs;
  Result := Succ(aQueueTop);
  if Result = FQueueSize then Result := 0;
end;

function TACFsmUtf8.PushOffset(aQueueTop, aOffs: SizeInt; aChar: Ucs4Char): SizeInt;
begin
  with FQueue[aQueueTop] do
    begin
      Offset := aOffs;
      vChar := aChar;
    end;
  Result := Succ(aQueueTop);
  if Result = FQueueSize then Result := 0;
end;

function TACFsmUtf8.PushChar(aQueueTop: SizeInt; aChar: Ucs4Char): SizeInt;
begin
  FQueue[aQueueTop].vChar := aChar;
  Result := Succ(aQueueTop);
  if Result = FQueueSize then Result := 0;
end;

function TACFsmUtf8.OnWordBounds(pCurr, pEnd: PByte; aQueueTop, aWordLen: SizeInt): Boolean;
begin
  if (pCurr < pEnd) and IsWordChar(CpToUcs4(pCurr, pEnd - pCurr)) then exit(False);
  aQueueTop := Pred(aQueueTop - aWordLen);
  if aQueueTop < 0 then aQueueTop += FQueueSize;
  Result := not IsWordChar(FQueue[aQueueTop].vChar);
end;

function TACFsmUtf8.PopOffset(aQueueTop, aWordLen: SizeInt): SizeInt;
begin
  aQueueTop := aQueueTop - aWordLen;
  if aQueueTop < 0 then aQueueTop += FQueueSize;
  Result := FQueue[aQueueTop].Offset;
end;

function TACFsmUtf8.PrevCpOffset(const s: string; aOffset: SizeInt): SizeInt;
begin
  if aOffset <= 1 then exit(0);
  Result := aOffset-1;
  if Byte(s[aOffset-1]) > $7f then
    begin
      if (Byte(s[aOffset-1]) and $c0 <> $80) or (aOffset < 3) then exit;
      if Byte(s[aOffset-2]) and $c0 = $c0 then
        Result := aOffset-2
      else
        begin
          if (Byte(s[aOffset-2]) and $c0 <> $80) or (aOffset < 4) then exit;
          if Byte(s[aOffset-3]) and $c0 = $c0 then
            Result := aOffset-3
          else
            begin
              if (Byte(s[aOffset-3]) and $c0 <> $80) or (aOffset < 5) then exit;
              if Byte(s[aOffset-4]) and $c0 = $c0 then
                Result := aOffset-4;
            end;
        end;
    end;
end;

function TACFsmUtf8.PushFirstOffset(const s: string; aOffset: SizeInt): SizeInt;
var
  Len: SizeInt;
begin
  aOffset := PrevCpOffset(s, aOffset);
  with FQueue[0] do
    if aOffset = 0 then
      begin
        Offset := 0;
        vChar := 0;
      end
    else
      begin
        Offset := aOffset;
        vChar := CodePointToUcs4Char(@s[aOffset], Len);
      end;
  Result := 1;
end;

class function TACFsmUtf8.CreateInstance(const aPatternList: array of string; aIgnoreCase,
  aForceNFA: Boolean): TACFsmUtf8;
begin
  if aForceNFA then
    begin
      Result := GetNfaClass(aIgnoreCase).Create;
      Result.TryBuildFsm(aPatternList);
    end
  else
    begin
      Result := GetDfaClass(aIgnoreCase).Create;
      if not Result.TryBuildFsm(aPatternList) then
        begin
          Result.Free;
          Result := GetNfaClass(aIgnoreCase).Create;
          Result.TryBuildFsm(aPatternList);
        end;
    end;
end;

constructor TACFsmUtf8.Create;
begin
  inherited;
end;

function TACFsmUtf8.IsMatch(const s: string; aOffset: SizeInt; aCount: SizeInt): Boolean;
begin
  Result := IndexOfPattern(s, aOffset, aCount) <> NULL_INDEX;
end;

function TACFsmUtf8.FirstMatch(const aText: string; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch;
begin
  if OnlyWholeWords then
    Result := DoFindFirstOww(aText, aMode, aOffset, aCount)
  else
    Result := DoFindFirst(aText, aMode, aOffset, aCount);
end;

function TACFsmUtf8.FindMatches(const aText: string; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatchArray;
var
  Matches: TMatchArray;
  MatchCount: SizeInt;
  function AddMatch(const m: TMatch): Boolean;
  begin
    if MatchCount = System.Length(Matches) then
      System.SetLength(Matches, MatchCount * 2);
    Matches[MatchCount] := m;
    Inc(MatchCount);
    Result := True;
  end;
begin
  System.SetLength(Matches, ARRAY_INITIAL_SIZE);
  MatchCount := 0;
  if aMode = smmNonOverlapping then
    begin
      if OnlyWholeWords then
        DoSearchNoOverlapOww(aText, aOffset, aCount, @AddMatch)
      else
        DoSearchNoOverlap(aText, aOffset, aCount, @AddMatch);
      System.SetLength(Matches, MatchCount);
      exit(Matches);
    end;
  Search(aText, @AddMatch, aOffset, aCount);
  System.SetLength(Matches, MatchCount);
  if aMode <> smmDefault then
    DoFilterMatches(Matches, aMode);
  Result := Matches;
end;

procedure TACFsmUtf8.Search(const aText: string; aOnMatch: TOnMatch; aOffset, aCount: SizeInt);
begin
  if aOnMatch = nil then exit;
  RegisterMatchHandler(aOnMatch);
  if OnlyWholeWords then
    DoSearchOww(aText, aOffset, aCount)
  else
    DoSearch(aText, aOffset, aCount);
end;

procedure TACFsmUtf8.Search(const aText: string; aOnMatch: TNestMatch; aOffset, aCount: SizeInt);
begin
  if aOnMatch = nil then exit;
  RegisterMatchHandler(aOnMatch);
  if OnlyWholeWords then
    DoSearchOww(aText, aOffset, aCount)
  else
    DoSearch(aText, aOffset, aCount);
end;

function TACFsmUtf8.ContainsMatch(const aText: string; aOffset: SizeInt; aCount: SizeInt): Boolean;
begin
  if OnlyWholeWords then
    Result := GetHasMatchOww(aText, aOffset, aCount)
  else
    Result := GetHasMatch(aText, aOffset, aCount);
end;

function TACFsmUtf8.FilterMatches(const aSource: array of TMatch; aMode: TSetMatchMode): TMatchArray;
begin
  Result := specialize TGArrayHelpUtil<TMatch>.CreateCopy(aSource);
  DoFilterMatches(Result, aMode);
end;

{ TACDfaUtf8 }

function TACDfaUtf8.GetAlphabetSize: SizeInt;
begin
  Result := FAlphabetSize;
end;

function TACDfaUtf8.BuildCharMap(const aList: array of string): Boolean;
var
  p, pEnd: PByte;
  I: SizeInt;
  c: Ucs4Char;
begin
  System.SetLength(FCharMap, BMP_MAX + 1);
  System.FillChar(Pointer(FCharMap)^, System.Length(FCharMap)*SizeOf(Int32), $ff);
  for I := 0 to System.High(aList) do
    begin
      if aList[I] = '' then continue;
      p := Pointer(aList[I]);
      pEnd := p + System.Length(aList[I]);
      while p < pEnd do
        begin
          if CaseInsensitive then
            c := CpToUcs4Lower(p, pEnd - p)
          else
            c := CpToUcs4(p, pEnd - p);
          if c > BMP_MAX then exit(False);
          if FCharMap[c] = -1 then
            begin
              if AlphabetSize = MAX_CODE then exit(False);
              FCharMap[c] := AlphabetSize;
              Inc(FAlphabetSize);
            end;
        end;
    end;
  Result := True;
end;

function TACDfaUtf8.NewNode: SizeInt;
begin
{$IFDEF CPU64}
  if NodeCount = MaxInt then
    raise ELGMaxItemsExceed.CreateFmt(SEMaxNodeCountExceedFmt, [MaxInt]);
{$ENDIF CPU64}
  if NodeCount = System.Length(FTrie) then
    System.SetLength(FTrie, NodeCount * 2);
  System.SetLength(FTrie[FNodeCount].NextMove, AlphabetSize);
  Result := NodeCount;
  Inc(FNodeCount);
end;

procedure TACDfaUtf8.AddPattern(const aValue: string; aIndex: SizeInt);
var
  p, pEnd: PByte;
  Curr, Next, LenUtf8, Code: Int32;
begin
  if aValue = '' then exit;
  Curr := 0;
  p := Pointer(aValue);
  pEnd := p + System.Length(aValue);
  LenUtf8 := 0;
  while p < pEnd do
    begin
      if CaseInsensitive then
        Code := FCharMap[CpToUcs4Lower(p, pEnd - p)]
      else
        Code := FCharMap[CpToUcs4(p, pEnd - p)];
      Inc(LenUtf8);
      Next := FTrie[Curr].NextMove[Code];
      if Next = 0 then
        begin
          Next := NewNode;
          FTrie[Curr].NextMove[Code] := Next;
        end;
      Curr := Next;
    end;
  if FTrie[Curr].Length = 0 then
    begin
      Inc(FWordCount);
      if LenUtf8 > FQueueSize then FQueueSize := LenUtf8;
      with FTrie[Curr] do
        begin
          Index := aIndex;
          Length := System.Length(aValue);
          Utf8Len := LenUtf8;
        end;
    end;
end;

function TACDfaUtf8.NextFsmState(aState: Int32; c: Ucs4Char): Int32;
begin
  if (c > BMP_MAX) or (FCharMap[c] = -1) then exit(0);
  Result := FTrie[aState].NextMove[FCharMap[c]];
end;

procedure TACDfaUtf8.BuildFsm;
var
  Queue: specialize TGLiteQueue<Int32>;
  Failure: array of Int32;
  Curr, Next, Fail, Link, c: Int32;
begin
  System.SetLength(Failure, NodeCount);
  for Curr in FTrie[0].NextMove do
    if Curr <> 0 then
      Queue.Enqueue(Curr);
  while Queue.TryDequeue(Curr) do
    for c := 0 to Pred(AlphabetSize) do
      if FTrie[Curr].NextMove[c] <> 0 then
        begin
          Next := FTrie[Curr].NextMove[c];
          Queue.Enqueue(Next);
          Fail := Curr;
          repeat
            Fail := Failure[Fail];
            Link := FTrie[Fail].NextMove[c];
            if Link <> 0 then
              begin
                Failure[Next] := Link;
                if FTrie[Link].Length <> 0 then
                  FTrie[Next].Output := Link
                else
                  FTrie[Next].Output := FTrie[Link].Output;
                break;
              end;
          until Fail = 0;
        end
      else
        FTrie[Curr].NextMove[c] := FTrie[Failure[Curr]].NextMove[c];
end;

{$PUSH}{$WARN 5036 OFF}
function TACDfaUtf8.DoFindFirst(const s: string; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch;
var
  Match: TMatch;
  MatchLess: TMatchLess;
  procedure TestMatch(const m: TMatch); inline;
  begin
    if (Match.Offset = 0) or MatchLess(m, Match) then
      Match := m;
  end;
var
  p, pText, pEnd: PByte;
  State, NextState: Int32;
begin
  Match := TMatch.Make(0, 0, NULL_INDEX);
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit(Match);
  State := 0;
  if aMode < smmLeftmostFirst then begin
    while p < pEnd do begin
      State := NextFsmState(State, CpToUcs4(p, pEnd - p));
      if State = 0 then continue;
      with FTrie[State] do
        if Length <> 0 then
          exit(TMatch.Make(Succ(p - pText - Length), Length, Index));
      if FTrie[State].Output <> 0 then
        with FTrie[FTrie[State].Output] do
          exit(TMatch.Make(Succ(p - pText - Length), Length, Index));
    end;
    exit(Match);
  end;
  MatchLess := SelectComparer(aMode);
  while p < pEnd do begin
    NextState := NextFsmState(State, CpToUcs4(p, pEnd - p));
    if NextState = 0 then
      if State = 0 then
        continue
      else
        if Match.Offset <> 0 then break;
    State := NextState;
    with FTrie[NextState] do
      if Length <> 0 then
        TestMatch(TMatch.Make(Succ(p - pText - Length), Length, Index));
    while FTrie[NextState].Output <> 0 do begin
      NextState := FTrie[NextState].Output;
      with FTrie[NextState] do
        TestMatch(TMatch.Make(Succ(p - pText - Length), Length, Index));
    end;
  end;
  Result := Match;
end;

function TACDfaUtf8.DoFindFirstOww(const s: string; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch;
var
  Match: TMatch;
  MatchLess: TMatchLess;
  procedure TestMatch(const m: TMatch); inline;
  begin
    if (Match.Offset = 0) or MatchLess(m, Match) then
      Match := m;
  end;
var
  p, pText, pEnd: PByte;
  State, NextState, qTop: Int32;
  c: Ucs4Char;
begin
  Match := TMatch.Make(0, 0, NULL_INDEX);
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit(Match);
  qTop := PushFirstOffset(s, Succ(p - pText));
  State := 0;
  if aMode < smmLeftmostFirst then begin
    while p < pEnd do begin
      c := CpToUcs4(p, pEnd - p);
      qTop := PushChar(qTop, c);
      State := NextFsmState(State, c);
      if State = 0 then continue;
      with FTrie[State] do
        if (Utf8Len <> 0) and OnWordBounds(p, pEnd, qTop, Utf8Len) then
          exit(TMatch.Make(Succ(p - pText - Length), Length, Index));
      NextState := State;
      while FTrie[NextState].Output <> 0 do begin
        NextState := FTrie[NextState].Output;
        with FTrie[NextState] do
          if OnWordBounds(p, pEnd, qTop, Utf8Len) then
            exit(TMatch.Make(Succ(p - pText - Length), Length, Index));
      end;
    end;
    exit(Match);
  end;
  MatchLess := SelectComparer(aMode);
  while p < pEnd do begin
    c := CpToUcs4(p, pEnd - p);
    qTop := PushChar(qTop, c);
    NextState := NextFsmState(State, c);
    if NextState = 0 then
      if State = 0 then
        continue
      else
        if Match.Offset <> 0 then break;
    State := NextState;
    with FTrie[NextState] do
      if (Utf8Len <> 0) and OnWordBounds(p, pEnd, qTop, Utf8Len) then
        TestMatch(TMatch.Make(Succ(p - pText - Length), Length, Index));
    while FTrie[NextState].Output <> 0 do begin
      NextState := FTrie[NextState].Output;
      with FTrie[NextState] do
        if OnWordBounds(p, pEnd, qTop, Utf8Len) then
          TestMatch(TMatch.Make(Succ(p - pText - Length), Length, Index));
    end;
  end;
  Result := Match;
end;
{$POP}

procedure TACDfaUtf8.DoSearchNoOverlap(const s: string; aOffset, aCount: SizeInt; aOnMatch: TNestMatch);
var
  p, pEnd, pText: PByte;
  State: Int32;
begin
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit;
  State := 0;
  while p < pEnd do
    begin
      State := NextFsmState(State, CpToUcs4(p, pEnd - p));
      if State = 0 then continue;
      with FTrie[State] do
        if Length <> 0 then
          begin
            aOnMatch(TMatch.Make(Succ(p - pText - Length), Length, Index));
            State := 0;
            continue;
          end;
      if FTrie[State].Output <> 0 then
        with FTrie[FTrie[State].Output] do
          begin
            aOnMatch(TMatch.Make(Succ(p - pText - Length), Length, Index));
            State := 0;
          end;
    end;
end;

procedure TACDfaUtf8.DoSearchNoOverlapOww(const s: string; aOffset, aCount: SizeInt; aOnMatch: TNestMatch);
var
  p, pEnd, pText: PByte;
  State, Tmp, qTop: Int32;
  c: Ucs4Char;
begin
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit;
  qTop := PushFirstOffset(s, Succ(p - pText));
  State := 0;
  while p < pEnd do begin
    c := CpToUcs4(p, pEnd - p);
    qTop := PushChar(qTop, c);
    State := NextFsmState(State, c);
    if State = 0 then continue;
    with FTrie[State] do
      if (Utf8Len <> 0) and OnWordBounds(p, pEnd, qTop, Utf8Len) then
        begin
          aOnMatch(TMatch.Make(Succ(p - pText - Length), Length, Index));
          State := 0;
          continue;
        end;
    Tmp := State;
    while FTrie[Tmp].Output <> 0 do
      begin
        Tmp := FTrie[Tmp].Output;
        with FTrie[Tmp] do
          if OnWordBounds(p, pEnd, qTop, Utf8Len) then
            begin
              aOnMatch(TMatch.Make(Succ(p - pText - Length), Length, Index));
              State := 0;
              break;
            end;
      end;
  end;
end;

procedure TACDfaUtf8.DoSearch(const s: string; aOffset, aCount: SizeInt);
var
  p, pEnd, pText: PByte;
  State, Tmp: Int32;
begin
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit;
  State := 0;
  while p < pEnd do
    begin
      State := NextFsmState(State, CpToUcs4(p, pEnd - p));
      if State = 0 then continue;
      with FTrie[State] do
        if Length <> 0 then
          if not FOnMatch(TMatch.Make(Succ(p - pText - Length), Length, Index)) then exit;
      Tmp := State;
      while FTrie[Tmp].Output <> 0 do
        begin
          Tmp := FTrie[Tmp].Output;
          with FTrie[Tmp] do
            if not FOnMatch(TMatch.Make(Succ(p - pText - Length), Length, Index)) then exit;
        end;
    end;
end;

procedure TACDfaUtf8.DoSearchOww(const s: string; aOffset, aCount: SizeInt);
var
  p, pEnd, pText: PByte;
  State, Tmp, qTop: Int32;
  c: Ucs4Char;
begin
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit;
  qTop := PushFirstOffset(s, Succ(p - pText));
  State := 0;
  while p < pEnd do begin
    c := CpToUcs4(p, pEnd - p);
    qTop := PushChar(qTop, c);
    State := NextFsmState(State, c);
    if State = 0 then continue;
    with FTrie[State] do
      if (Utf8Len <> 0) and OnWordBounds(p, pEnd, qTop, Utf8Len) then
        if not FOnMatch(TMatch.Make(Succ(p - pText - Length), Length, Index)) then exit;
    Tmp := State;
    while FTrie[Tmp].Output <> 0 do
      begin
        Tmp := FTrie[Tmp].Output;
        with FTrie[Tmp] do
          if OnWordBounds(p, pEnd, qTop, Utf8Len) then
            if not FOnMatch(TMatch.Make(Succ(p - pText - Length), Length, Index)) then exit;
      end;
  end;
end;

function TACDfaUtf8.GetHasMatch(const s: string; aOffset, aCount: SizeInt): Boolean;
var
  p, pEnd: PByte;
  State: Int32;
begin
  if not DoProlog(s, aOffset, aCount, p, pEnd) then exit(False);
  State := 0;
  while p < pEnd do
    begin
      State := NextFsmState(State, CpToUcs4(p, pEnd - p));
      if State = 0 then continue;
      if (FTrie[State].Length <> 0) or (FTrie[State].Output <> 0) then
        exit(True);
    end;
  Result := False;
end;

function TACDfaUtf8.GetHasMatchOww(const s: string; aOffset, aCount: SizeInt): Boolean;
var
  p, pText, pEnd: PByte;
  State, Tmp, qTop: Int32;
  c: Ucs4Char;
begin
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit(False);
  State := 0;
  qTop := PushFirstOffset(s, Succ(p - pText));
  State := 0;
  while p < pEnd do
    begin
      c := CpToUcs4(p, pEnd - p);
      qTop := PushChar(qTop, c);
      State := NextFsmState(State, c);
      if State = 0 then continue;
      with FTrie[State] do
        if (Utf8Len <> 0) and OnWordBounds(p, pEnd, qTop, Utf8Len) then exit(True);
      Tmp := State;
      while FTrie[Tmp].Output <> 0 do
        begin
          Tmp := FTrie[Tmp].Output;
          if OnWordBounds(p, pEnd, qTop, FTrie[Tmp].Utf8Len) then exit(True);
        end;
    end;
  Result := False;
end;

class function TACDfaUtf8.GetFsmClass: TACFsmUtf8Class;
begin
  Result := TACDfaUtf8;
end;

function TACDfaUtf8.TryBuildFsm(const aPatternList: array of string): Boolean;
var
  I: SizeInt;
begin
  if not BuildCharMap(aPatternList) then exit(False);
  System.SetLength(FTrie, ARRAY_INITIAL_SIZE);
  NewNode;
  for I := 0 to System.High(aPatternList) do
    AddPattern(aPatternList[I], I);
  System.SetLength(FTrie, FNodeCount);
  Inc(FQueueSize, 2);
  System.SetLength(FQueue, FQueueSize);
  BuildFsm;
  Result := True;
end;

function TACDfaUtf8.Clone: IACSearchFsmUtf8;
var
  Inst: TACDfaUtf8;
begin
  Inst := TACDfaUtf8(GetFsmClass.Create);
  Inst.FTrie := FTrie;
  Inst.FCharMap := FCharMap;
  System.SetLength(Inst.FQueue, FQueueSize);
  Inst.FAlphabetSize := FAlphabetSize;
  Inst.FQueueSize := FQueueSize;
  Inst.FNodeCount := FNodeCount;
  Inst.FWordCount := FWordCount;
  Inst.FIgnoreCase := FIgnoreCase;
  Inst.FWholeWordsOnly := FWholeWordsOnly;
  Result := Inst;
end;

function TACDfaUtf8.IndexOfPattern(const s: string; aOffset: SizeInt; aCount: SizeInt): SizeInt;
var
  p, pS, pEnd: PByte;
  State: Int32;
begin
  Result := NULL_INDEX;
  if not DoProlog(s, aOffset, aCount, p, pEnd) then exit;
  pS := p;
  State := 0;
  while p < pEnd do
    begin
      if CaseInsensitive then
        State := NextFsmState(State, CpToUcs4Lower(p, pEnd - p))
      else
        State := NextFsmState(State, CpToUcs4(p, pEnd - p));
      if State = 0 then exit;
    end;
  with FTrie[State] do
    if Length = pEnd - pS then
      Result := Index;
end;

{ TACDfaCIUtf8 }

{$PUSH}{$WARN 5036 OFF}
function TACDfaCIUtf8.DoFindFirst(const s: string; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch;
var
  Match: TMatch;
  MatchLess: TMatchLess;
  procedure TestMatch(const m: TMatch); inline;
  begin
    if (Match.Offset = 0) or MatchLess(m, Match) then
      Match := m;
  end;
var
  Ofs: SizeInt;
  p, pEnd, pText: PByte;
  State, NextState, qTop: Int32;
begin
  Match := TMatch.Make(0, 0, NULL_INDEX);
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit(Match);
  qTop := 0;
  State := 0;
  if aMode < smmLeftmostFirst then begin
    while p < pEnd do begin
      qTop := PushOffset(qTop, Succ(p - pText));
      State := NextFsmState(State, CpToUcs4Lower(p, pEnd - p));
      if State = 0 then continue;
      with FTrie[State] do
        if Utf8Len <> 0 then begin
          Ofs := PopOffset(qTop, Utf8Len);
          exit(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
        end;
      if FTrie[State].Output <> 0 then
        with FTrie[FTrie[State].Output] do begin
          Ofs := PopOffset(qTop, Utf8Len);
          exit(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
        end;
    end;
    exit(Match);
  end;
  MatchLess := SelectComparer(aMode);
  while p < pEnd do begin
    qTop := PushOffset(qTop, Succ(p - pText));
    NextState := NextFsmState(State, CpToUcs4Lower(p, pEnd - p));
    if NextState = 0 then
      if State = 0 then
        continue
      else
        if Match.Offset <> 0 then break;
    State := NextState;
    with FTrie[NextState] do
      if Utf8Len <> 0 then begin
        Ofs := PopOffset(qTop, Utf8Len);
        TestMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
      end;
    while FTrie[NextState].Output <> 0 do begin
      NextState := FTrie[NextState].Output;
      with FTrie[NextState] do begin
        Ofs := PopOffset(qTop, Utf8Len);
        TestMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
      end;
    end;
  end;
  Result := Match;
end;

function TACDfaCIUtf8.DoFindFirstOww(const s: string; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch;
var
  Match: TMatch;
  MatchLess: TMatchLess;
  procedure TestMatch(const m: TMatch); inline;
  begin
    if (Match.Offset = 0) or MatchLess(m, Match) then
      Match := m;
  end;
var
  Ofs: SizeInt;
  p, pEnd, pText: PByte;
  State, NextState, qTop: Int32;
  c: Ucs4Char;
begin
  Match := TMatch.Make(0, 0, NULL_INDEX);
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit(Match);
  qTop := PushFirstOffset(s, Succ(p - pText));
  State := 0;
  if aMode < smmLeftmostFirst then begin
    while p < pEnd do begin
      c := CpToUcs4Lower(p, pEnd - p, Ofs);
      qTop := PushOffset(qTop, Succ(p - pText), c);
      p += Ofs;
      State := NextFsmState(State, c);
      if State = 0 then continue;
      with FTrie[State] do
        if (Utf8Len <> 0) and OnWordBounds(p, pEnd, qTop, Utf8Len) then begin
          Ofs := PopOffset(qTop, Utf8Len);
          exit(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
        end;
      NextState := State;
      while FTrie[NextState].Output <> 0 do begin
        NextState := FTrie[NextState].Output;
        with FTrie[NextState] do
          if OnWordBounds(p, pEnd, qTop, Utf8Len) then begin
            Ofs := PopOffset(qTop, Utf8Len);
            exit(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
          end;
      end;
    end;
    exit(Match);
  end;
  MatchLess := SelectComparer(aMode);
  while p < pEnd do begin
    c := CpToUcs4Lower(p, pEnd - p, Ofs);
    qTop := PushOffset(qTop, Succ(p - pText), c);
    p += Ofs;
    NextState := NextFsmState(State, c);
    if NextState = 0 then
      if State = 0 then
        continue
      else
        if Match.Offset <> 0 then break;
    State := NextState;
    with FTrie[NextState] do
      if (Utf8Len <> 0) and OnWordBounds(p, pEnd, qTop, Utf8Len) then begin
        Ofs := PopOffset(qTop, Utf8Len);
        TestMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
      end;
    while FTrie[NextState].Output <> 0 do begin
      NextState := FTrie[NextState].Output;
      with FTrie[NextState] do
        if OnWordBounds(p, pEnd, qTop, Utf8Len) then begin
          Ofs := PopOffset(qTop, Utf8Len);
          TestMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
        end;
    end;
  end;
  Result := Match;
end;
{$POP}

procedure TACDfaCIUtf8.DoSearchNoOverlap(const s: string; aOffset, aCount: SizeInt; aOnMatch: TNestMatch);
var
  Ofs: SizeInt;
  p, pEnd, pText: PByte;
  State, qTop: Int32;
begin
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit;
  qTop := 0;
  State := 0;
  while p < pEnd do begin
    qTop := PushOffset(qTop, Succ(p - pText));
    State := NextFsmState(State, CpToUcs4Lower(p, pEnd - p));
    if State = 0 then continue;
    with FTrie[State] do
      if Utf8Len <> 0 then
        begin
          Ofs := PopOffset(qTop, Utf8Len);
          aOnMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
          State := 0;
          continue;
        end;
    if FTrie[State].Output <> 0 then
      with FTrie[FTrie[State].Output] do
        begin
          Ofs := PopOffset(qTop, Utf8Len);
          aOnMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
          State := 0;
        end;
  end;
end;

procedure TACDfaCIUtf8.DoSearchNoOverlapOww(const s: string; aOffset, aCount: SizeInt; aOnMatch: TNestMatch);
var
  Ofs: SizeInt;
  p, pEnd, pText: PByte;
  State, Tmp, qTop: Int32;
  c: Ucs4Char;
begin
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit;
  qTop := PushFirstOffset(s, Succ(p - pText));
  State := 0;
  while p < pEnd do begin
    c := CpToUcs4Lower(p, pEnd - p, Ofs);
    qTop := PushOffset(qTop, Succ(p - pText), c);
    p += Ofs;
    State := NextFsmState(State, c);
    if State = 0 then continue;
    with FTrie[State] do
      if (Utf8Len <> 0) and OnWordBounds(p, pEnd, qTop, Utf8Len) then
        begin
          Ofs := PopOffset(qTop, Utf8Len);
          aOnMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
          State := 0;
          continue;
        end;
    Tmp := State;
    while FTrie[Tmp].Output <> 0 do
      begin
        Tmp := FTrie[Tmp].Output;
        with FTrie[Tmp] do
          if OnWordBounds(p, pEnd, qTop, Utf8Len) then
            begin
              Ofs := PopOffset(qTop, Utf8Len);
              aOnMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
              State := 0;
              break;
            end;
      end;
  end;
end;

procedure TACDfaCIUtf8.DoSearch(const s: string; aOffset, aCount: SizeInt);
var
  Ofs: SizeInt;
  p, pEnd, pText: PByte;
  State, Tmp, qTop: Int32;
begin
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit;
  qTop := 0;
  State := 0;
  while p < pEnd do begin
    qTop := PushOffset(qTop, Succ(p - pText));
    State := NextFsmState(State, CpToUcs4Lower(p, pEnd - p));
    if State = 0 then continue;
    with FTrie[State] do
      if Utf8Len <> 0 then
        begin
          Ofs := PopOffset(qTop, Utf8Len);
          if not FOnMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index)) then exit;
        end;
    Tmp := State;
    while FTrie[Tmp].Output <> 0 do
      begin
        Tmp := FTrie[Tmp].Output;
        with FTrie[Tmp] do
          begin
            Ofs := PopOffset(qTop, Utf8Len);
            if not FOnMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index)) then exit;
          end;
      end;
  end;
end;

procedure TACDfaCIUtf8.DoSearchOww(const s: string; aOffset, aCount: SizeInt);
var
  Ofs: SizeInt;
  p, pEnd, pText: PByte;
  State, Tmp, qTop: Int32;
  c: Ucs4Char;
begin
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit;
  qTop := PushFirstOffset(s, Succ(p - pText));
  State := 0;
  while p < pEnd do begin
    c := CpToUcs4Lower(p, pEnd - p, Ofs);
    qTop := PushOffset(qTop, Succ(p - pText), c);
    p += Ofs;
    State := NextFsmState(State, c);
    if State = 0 then continue;
    with FTrie[State] do
      if (Utf8Len <> 0) and OnWordBounds(p, pEnd, qTop, Utf8Len) then
        begin
          Ofs := PopOffset(qTop, Utf8Len);
          if not FOnMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index)) then exit;
        end;
    Tmp := State;
    while FTrie[Tmp].Output <> 0 do
      begin
        Tmp := FTrie[Tmp].Output;
        with FTrie[Tmp] do
          if OnWordBounds(p, pEnd, qTop, Utf8Len) then
            begin
              Ofs := PopOffset(qTop, Utf8Len);
              if not FOnMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index)) then
                exit;
            end;
      end;
  end;
end;

function TACDfaCIUtf8.GetHasMatch(const s: string; aOffset, aCount: SizeInt): Boolean;
var
  p, pEnd: PByte;
  State: Int32;
begin
  if not DoProlog(s, aOffset, aCount, p, pEnd) then exit(False);
  State := 0;
  while p < pEnd do
    begin
      State := NextFsmState(State, CpToUcs4Lower(p, pEnd - p));
      if State = 0 then continue;
      if (FTrie[State].Length <> 0) or (FTrie[State].Output <> 0) then
        exit(True);
    end;
  Result := False;
end;

function TACDfaCIUtf8.GetHasMatchOww(const s: string; aOffset, aCount: SizeInt): Boolean;
var
  p, pText, pEnd: PByte;
  State, Tmp, qTop: Int32;
  c: Ucs4Char;
begin
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit(False);
  State := 0;
  qTop := PushFirstOffset(s, Succ(p - pText));
  State := 0;
  while p < pEnd do
    begin
      c := CpToUcs4Lower(p, pEnd - p);
      qTop := PushChar(qTop, c);
      State := NextFsmState(State, c);
      if State = 0 then continue;
      with FTrie[State] do
        if (Utf8Len <> 0) and OnWordBounds(p, pEnd, qTop, Utf8Len) then exit(True);
      Tmp := State;
      while FTrie[Tmp].Output <> 0 do
        begin
          Tmp := FTrie[Tmp].Output;
          if OnWordBounds(p, pEnd, qTop, FTrie[Tmp].Utf8Len) then exit(True);
        end;
    end;
  Result := False;
end;

class function TACDfaCIUtf8.GetFsmClass: TACFsmUtf8Class;
begin
  Result := TACDfaCIUtf8;
end;

constructor TACDfaCIUtf8.Create;
begin
  inherited;
  FIgnoreCase := True;
end;

{ TCode2StateMap.TEnumerator }

function TCode2StateMap.TEnumerator.GetCurrent: TEntry;
begin
  Result := FCurrent^;
end;

function TCode2StateMap.TEnumerator.MoveNext: Boolean;
begin
  Result := FCurrent < FLast;
  FCurrent += Ord(Result);
end;

{ TCode2StateMap }

function TCode2StateMap.DoFind(aKey: Int32; out aIndex: Int32): PEntry;
var
  L, R, M: Int32;
begin
  if Count = 0 then begin
    aIndex := 0;
    exit(nil);
  end;
  L := 0;
  R := Pred(Count);
  if aKey < FItems[L].Key then begin
    aIndex := 0;
    exit(nil);
  end else
    if aKey > FItems[R].Key then begin
      aIndex := Count;
      exit(nil);
    end;
  while L < R do begin
    M := {$PUSH}{$Q-}{$R-}(L + R) shr 1;{$POP}
    if FItems[M].Key < aKey then
      L := Succ(M)
    else
      R := M;
  end;
  if FItems[R].Key = aKey then
    Result := @FItems[R]
  else begin
    aIndex := R;
    Result := nil;
  end;
end;

function TCode2StateMap.DoAdd(aKey, aIndex: Int32): PEntry;
begin
  if FItems <> nil then begin
    if Count = System.Length(FItems) then
      System.SetLength(FItems, Count * 2);
    if aIndex <> Count then
      System.Move(FItems[aIndex], FItems[aIndex+1], (Count - aIndex) * SizeOf(TEntry));
    Inc(FCount);
    FItems[aIndex].Key := aKey;
    Result := @FItems[aIndex];
  end else begin
    System.SetLength(FItems, INITIAL_SIZE);
    FItems[0].Key := aKey;
    FCount := 1;
    Result := @FItems[0];
  end;
end;

function TCode2StateMap.GetKey(aIndex: Int32): Int32;
begin
  Result := FItems[aIndex].Key;
end;

function TCode2StateMap.GetValue(aIndex: Int32): Int32;
begin
  Result := FItems[aIndex].Value;
end;

function TCode2StateMap.GetEnumerator: TEnumerator;
begin
  if Count = 0 then
    begin
      Result.FCurrent := nil;
      Result.FLast := nil;
    end
  else
    begin
      Result.FCurrent := PEntry(FItems) - 1;
      Result.FLast := Result.FCurrent + Count;
    end;
end;

function TCode2StateMap.GetMutValueDef(aKey, aDefault: Int32): PInt32;
var
  p: PEntry;
  I: Int32;
begin
  p := DoFind(aKey, I);
  if p = nil then
    begin
      p := DoAdd(aKey, I);
      p^.Value := aDefault;
    end;
  Result := @p^.Value;
end;

{ TAcStreamHeader }

procedure TAcStreamHeader.WriteStream(aStream: TStream);
begin
  aStream.WriteBuffer(Magic, SizeOf(Magic));
  aStream.WriteBuffer(CaseInsensitive, SizeOf(CaseInsensitive));
  aStream.WriteBuffer(NToLE(Version), SizeOf(Version));
  aStream.WriteBuffer(NToLE(PatternListSize), SizeOf(PatternListSize));
  aStream.WriteBuffer(NToLE(RealPatternCount), SizeOf(RealPatternCount));
  aStream.WriteBuffer(NToLE(StateCount), SizeOf(StateCount));
  aStream.WriteBuffer(NToLE(AlphabetSize), SizeOf(AlphabetSize));
  aStream.WriteBuffer(NToLE(QueueSize), SizeOf(QueueSize));
end;

procedure TAcStreamHeader.ReadStream(aStream: TStream);
var
  I: Int32;
begin
  I := 0; // make comliler happy
  aStream.ReadBuffer(Magic, SizeOf(Magic));
  aStream.ReadBuffer(CaseInsensitive, SizeOf(CaseInsensitive));
  aStream.ReadBuffer(I, SizeOf(I));
  Version := LEToN(I);
  aStream.ReadBuffer(I, SizeOf(I));
  PatternListSize := LEToN(I);
  aStream.ReadBuffer(I, SizeOf(I));
  RealPatternCount := LEToN(I);
  aStream.ReadBuffer(I, SizeOf(I));
  StateCount := LEToN(I);
  aStream.ReadBuffer(I, SizeOf(I));
  AlphabetSize := LEToN(I);
  aStream.ReadBuffer(I, SizeOf(I));
  QueueSize := LEToN(I);
end;

{ TACNfaUtf8.TOutput }

procedure TACNfaUtf8.TOutput.ReadStream(aStream: TStream);
var
  I: Int32;
begin
  I := 0; //make compliler happy
  aStream.ReadBuffer(I, SizeOf(I));
  Len := LEToN(I);
  aStream.ReadBuffer(I, SizeOf(I));
  CpLen := LEToN(I);
  aStream.ReadBuffer(I, SizeOf(I));
  Index := LEToN(I);
end;

procedure TACNfaUtf8.TOutput.WriteStream(aStream: TStream);
begin
  aStream.WriteBuffer(NToLE(Len), SizeOf(Len));
  aStream.WriteBuffer(NToLE(CpLen), SizeOf(CpLen));
  aStream.WriteBuffer(NToLE(Index), SizeOf(Index));
end;

{ TACNfaUtf8.TDaNode }

procedure TACNfaUtf8.TDaNode.ReadStream(aStream: TStream);
var
  I: Int32;
begin
  I := 0; //make compliler happy
  aStream.ReadBuffer(I, SizeOf(I));
  Base := LEToN(I);
  aStream.ReadBuffer(I, SizeOf(I));
  Check := LEToN(I);
  aStream.ReadBuffer(I, SizeOf(I));
  Failure := LEToN(I);
  aStream.ReadBuffer(I, SizeOf(I));
  Output := LEToN(I);
  aStream.ReadBuffer(I, SizeOf(I));
  NextOut := LEToN(I);
end;

procedure TACNfaUtf8.TDaNode.WriteStream(aStream: TStream);
begin
  aStream.WriteBuffer(NToLE(Base), SizeOf(Base));
  aStream.WriteBuffer(NToLE(Check), SizeOf(Check));
  aStream.WriteBuffer(NToLE(Failure), SizeOf(Failure));
  aStream.WriteBuffer(NToLE(Output), SizeOf(Output));
  aStream.WriteBuffer(NToLE(NextOut), SizeOf(NextOut));
end;

{ TACNfaUtf8.TPair }

constructor TACNfaUtf8.TPair.Make(aNode, aDaNode: Int32);
begin
  Node := aNode;
  DaNode := aDaNode;
end;

{ TACNfaUtf8 }

function TACNfaUtf8.NewNode: SizeInt;
begin
{$IFDEF CPU64}
  if NodeCount = MaxInt then
    raise ELGMaxItemsExceed.CreateFmt(SEMaxNodeCountExceedFmt, [MaxInt]);
{$ENDIF CPU64}
  if NodeCount = System.Length(FTrie) then
    System.SetLength(FTrie, NodeCount * 2);
  Result := NodeCount;
  Inc(FNodeCount);
end;

function TACNfaUtf8.NewOutput(aIndex, aLen, aCpLen: Int32): Int32;
begin
{$IFDEF CPU64}
  if OutCount = MaxInt then
    raise ELGMaxItemsExceed.CreateFmt(SEMaxNodeCountExceedFmt, [MaxInt]);
{$ENDIF CPU64}
  if OutCount = System.Length(FOutput) then
    System.SetLength(FOutput, OutCount * 2);
  with FOutput[OutCount] do
    begin
      Index := aIndex;
      Len := aLen;
      CpLen := aCpLen;
    end;
  Result := OutCount;
  Inc(FOutCount);
end;

procedure TACNfaUtf8.BuildCharMap(const aPatternList: array of string);
type
  TMapType = specialize TGLiteChainHashMap<Ucs4Char, Int32, TUcs4Hasher>;
  TMap     = TMapType.TMap;
  TEntry   = TMap.TEntry;
  function EntryCmp(const L, R: TEntry): Boolean;
  begin
    Result := L.Value > R.Value;
  end;
var
  Map: TMap;
  a: array of TEntry;
  p, pEnd: PByte;
  I: Int32;
  c, MaxChar: Ucs4Char;
begin
  MaxChar := 0;
  for I := 0 to System.High(aPatternList) do
    begin
      if aPatternList[I] = '' then continue;
      p := Pointer(aPatternList[I]);
      pEnd := p + System.Length(aPatternList[I]);
      while p < pEnd do begin
        if CaseInsensitive then
          c := CpToUcs4Lower(p, pEnd - p)
        else
          c := CpToUcs4(p, pEnd - p);
        if c > MaxChar then
          MaxChar := c;
        Inc(Map.GetMutValueDef(c, 0)^);
      end;
    end;
  FMaxChar := MaxChar;
  System.SetLength(FCharMap, Succ(MaxChar));
  a := Map.ToArray;
  specialize TGNestedArrayHelper<TEntry>.MergeSort(a, @EntryCmp);
  for I := 0 to System.High(a) do
    FCharMap[a[I].Key] := Succ(I);
  FAlphabetSize := System.Length(a);
end;

function TACNfaUtf8.EncodeChar(c: Ucs4Char): Int32;
begin
  if c > FMaxChar then exit(0);
  Result := FCharMap[c];
end;

procedure TACNfaUtf8.AddPattern(const aValue: string; aIndex: SizeInt);
var
  pNext: PInt32;
  p, pEnd: PByte;
  Curr, LenCp, Code: Int32;
begin
  if aValue = '' then exit;
  Curr := 0; // root node has index 0
  p := Pointer(aValue);
  pEnd := p + System.Length(aValue);
  LenCp := 0;
  while p < pEnd do
    begin
      if CaseInsensitive then
        Code := FCharMap[CpToUcs4Lower(p, pEnd - p)]
      else
        Code := FCharMap[CpToUcs4(p, pEnd - p)];
      Inc(LenCp);
      pNext := FTrie[Curr].AdjList.GetMutValueDef(Code, 0);
      if pNext^ = 0 then
        pNext^ := NewNode;
      Curr := pNext^;
    end;
  if FTrie[Curr].Output = 0 then
    begin
      Inc(FWordCount);
      if LenCp > FQueueSize then FQueueSize := LenCp;
      FTrie[Curr].Output := NewOutput(aIndex, System.Length(aValue), LenCp);
    end;
end;

function TACNfaUtf8.NextMove(aState, aCode: Int32): Int32;
begin
  Result := FDaTrie[aState].Base + aCode;
  if (UInt32(Result) > UInt32(System.Length(FDaTrie))) or (FDaTrie[Result].Check <> aState) then
    Result := 0;
end;

function TACNfaUtf8.NextFsmState(aState, aCode: Int32): Int32;
begin
  if aCode = 0 then exit(0);
  Result := FDaTrie[aState].Base + aCode;
  if (UInt32(Result) > UInt32(System.Length(FDaTrie))) or (FDaTrie[Result].Check <> aState) then
    repeat
      aState := FDaTrie[aState].Failure;
      Result := NextMove(aState, aCode);
    until (Result <> 0) or (aState = 0);
end;

{$PUSH}{$WARN 5036 OFF}
procedure TACNfaUtf8.BuildFsm;
var
  VListHead, VListTail, AllVacantBound: Int32;
  procedure VacantListRemove(aNode: Int32); inline;
  var
    Prev, Next: Int32;
  begin
    if aNode >= AllVacantBound then
      AllVacantBound := Succ(aNode);
    Next := FDaTrie[aNode].Base;
    Prev := FDaTrie[aNode].Check;
    if Prev <> NULL_NODE then
      FDaTrie[-Prev].Base := Next;
    if Next <> NULL_NODE then
      FDaTrie[-Next].Check := Prev;
    if aNode = VListHead then
      VListHead := -Next;
    if aNode = VListTail then
      VListTail := -Prev;
  end;
  procedure Expand(aNewSize: SizeInt);
  var
    OldSize, I: Int32;
  begin
    if aNewSize <= System.Length(FDaTrie) then exit;
    OldSize := System.Length(FDaTrie);
  {$IFDEF CPU64}
    if OldSize = MaxInt then
      raise ELGMaxItemsExceed.CreateFmt(SEMaxNodeCountExceedFmt, [MaxInt]);
    System.SetLength(FDaTrie, Math.Min(aNewSize, MaxInt));
  {$ELSE CPU64}
    System.SetLength(FDaTrie, aNewSize);
  {$ENDIF CPU64}
    for I := OldSize to System.High(FDaTrie) do
      begin
        FDaTrie[I].Base := -Succ(I);
        FDaTrie[I].Check := -Pred(I);
      end;
    FDaTrie[System.High(FDaTrie)].Base := NULL_NODE;
    if VListHead = NULL_NODE then ////////////////
      VListHead := OldSize;
    if VListTail <> NULL_NODE then
      FDaTrie[VListTail].Base := -OldSize;
    FDaTrie[OldSize].Check := -VListTail;
    VListTail := System.High(FDaTrie);
  end;
  function NextVacantValueMaxLoad(aTrieNode: Int32): Int32;
  var
    Next, LowKey, HiIdx, Dist, I: Int32;
  begin
    Next := VListHead;
    with FTrie[aTrieNode].AdjList do begin
      LowKey := Keys[0];
      HiIdx := Pred(Count);
      Dist := Keys[HiIdx] - LowKey;
      repeat
        if (Next = NULL_NODE) or (Next + Dist > System.High(FDaTrie)) then begin
          Expand(System.Length(FDaTrie) * 2);
          if Next = NULL_NODE then
            Next := VListHead;
        end;
        Result := Next - LowKey;
        for I := 1 to HiIdx do
          if FDaTrie[Result + Keys[I]].Check >= 0 then begin
            Result := NULL_NODE;
            break;
          end;
        if Result <> NULL_NODE then exit;
        Next := -FDaTrie[Next].Base;
      until False;
    end;
  end;
  function NextVacantValue(aTrieNode: Int32): Int32;
  var
    Next, Dist1, Dist2, Dist3: Int32;
  begin
    with FTrie[aTrieNode].AdjList do
      case Count of
        1:
          begin
            if VListHead = NULL_NODE then
              Expand(System.Length(FDaTrie) * 2);
            Result := VListHead - Keys[0];
          end;
        2:
          begin
            Dist1 := Keys[1] - Keys[0];
            Next := VListHead;
            Result := NULL_NODE;
            repeat
              if (Next = NULL_NODE) or (Next + Dist1 > System.High(FDaTrie)) then begin
                Expand(System.Length(FDaTrie) * 2);
                if Next = NULL_NODE then
                  Next := VListHead;
              end;
              if FDaTrie[Next + Dist1].Check < 0 then
                exit(Next - Keys[0]);
              Next := -FDaTrie[Next].Base;
            until False;
          end;
        3:
          begin
            Dist1 := Keys[1] - Keys[0];
            Dist2 := Keys[2] - Keys[0];
            Next := VListHead;
            Result := NULL_NODE;
            repeat
              if (Next = NULL_NODE) or (Next + Dist2 > System.High(FDaTrie)) then begin
                Expand(System.Length(FDaTrie) * 2);
                if Next = NULL_NODE then
                  Next := VListHead;
              end;
              if (FDaTrie[Next + Dist1].Check < 0) and (FDaTrie[Next + Dist2].Check < 0) then
                exit(Next - Keys[0]);
              Next := -FDaTrie[Next].Base;
            until False;
          end;
        4:
          begin
            Dist1 := Keys[1] - Keys[0];
            Dist2 := Keys[2] - Keys[0];
            Dist3 := Keys[3] - Keys[0];
            Next := VListHead;
            Result := NULL_NODE;
            repeat
              if (Next = NULL_NODE) or (Next + Dist3 > System.High(FDaTrie)) then begin
                Expand(System.Length(FDaTrie) * 2);
                if Next = NULL_NODE then
                  Next := VListHead;
              end;
              if(FDaTrie[Next + Dist1].Check < 0) and (FDaTrie[Next + Dist2].Check < 0) and
                (FDaTrie[Next + Dist3].Check < 0) then
                exit(Next - Keys[0]);
              Next := -FDaTrie[Next].Base;
            until False;
          end;
      else
        if AllVacantBound + Keys[Pred(Count)] - Keys[0] > System.High(FDaTrie) then
          Expand(System.Length(FDaTrie) * 2);
        Result := AllVacantBound - Keys[0];
      end;
  end;

var
  Queue: TPairQueue;
  ParentPair: TPair;
  Next, Fail, Link: Int32;
  e: TCode2StateMap.TEntry;
  TryMaxLoad: Boolean;
begin
  TryMaxLoad := DatPreferMaxLoad and (AlphabetSize >= BIG_ALPHABET_SIZE);
  System.SetLength(FDaTrie, LgUtils.RoundUpTwoPower(NodeCount + AlphabetSize));
  //build a doubly linked list of vacant nodes in FDaTrie
  for Next := 2 to System.High(FDaTrie) do
    with FDaTrie[Next] do begin
      Base := -Succ(Next);
      Check := -Pred(Next);
    end;
  AllVacantBound := 1;
  VListHead := 1;
  FDaTrie[1].Base := -2;
  FDaTrie[1].Check := NULL_NODE;
  FDaTrie[System.High(FDaTrie)].Base := NULL_NODE;
  VListTail := System.High(FDaTrie);

  // root node has index 0 both in FTrie and FDaTrie
  for e in FTrie[0].AdjList do begin
    VacantListRemove(e.Key);
    with FDaTrie[e.Key] do begin
      Base := LEAF_NODE;
      Check := 0;
      Output := FTrie[e.Value].Output;
    end;
    Queue.Enqueue(TPair.Make(e.Value, e.Key));
  end;

  while Queue.TryDequeue(ParentPair) do
    if FTrie[ParentPair.Node].AdjList.Count <> 0 then begin
      if TryMaxLoad then
        FDaTrie[ParentPair.DaNode].Base := NextVacantValueMaxLoad(ParentPair.Node)
      else
        FDaTrie[ParentPair.DaNode].Base := NextVacantValue(ParentPair.Node);
      for e in FTrie[ParentPair.Node].AdjList do begin
        Next := FDaTrie[ParentPair.DaNode].Base + e.Key;
        VacantListRemove(Next);
        with FDaTrie[Next] do begin
          Base := LEAF_NODE;
          Check := ParentPair.DaNode;
          Output := FTrie[e.Value].Output;
        end;
        Queue.Enqueue(TPair.Make(e.Value, Next));
        Fail := ParentPair.DaNode;
        repeat
          Fail := FDaTrie[Fail].Failure;
          Link := NextMove(Fail, e.Key);
          if Link <> 0 then begin
            FDaTrie[Next].Failure := Link;
            if FDaTrie[Link].Output <> 0 then
              FDaTrie[Next].NextOut := Link
            else
              FDaTrie[Next].NextOut := FDaTrie[Link].NextOut;
            break;
          end;
        until Fail = 0;
      end;
    end;
  if System.Length(FDaTrie) > AllVacantBound then
    System.SetLength(FDaTrie, AllVacantBound);
end;

function TACNfaUtf8.IsDfa: Boolean;
begin
  Result := False;
end;

function TACNfaUtf8.DoFindFirst(const s: string; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch;
var
  Match: TMatch;
  MatchLess: TMatchLess;
  procedure TestMatch(const m: TMatch); inline;
  begin
    if (Match.Offset = 0) or MatchLess(m, Match) then
      Match := m;
  end;
var
  p, pEnd, pText: PByte;
  State, NextState: Int32;
begin
  Match := TMatch.Make(0, 0, NULL_INDEX);
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit(Match);
  State := 0;
  if aMode < smmLeftmostFirst then begin
    while p < pEnd do begin
      State := NextFsmState(State, EncodeChar(CpToUcs4(p, pEnd - p)));
      if State = 0 then continue;
      if FDaTrie[State].Output <> 0 then
        with FOutput[FDaTrie[State].Output] do
          exit(TMatch.Make(Succ(p - pText - Len), Len, Index));
      if FDaTrie[State].NextOut <> 0 then
        with FOutput[FDaTrie[FDaTrie[State].NextOut].Output] do
          exit(TMatch.Make(Succ(p - pText - Len), Len, Index));
    end;
    exit(Match);
  end;
  MatchLess := SelectComparer(aMode);
  while p < pEnd do begin
    NextState := NextFsmState(State, EncodeChar(CpToUcs4(p, pEnd - p)));
    if NextState = 0 then
      if State = 0 then
        continue
      else
        if Match.Offset <> 0 then break;
    State := NextState;
    if FDaTrie[NextState].Output <> 0 then
      with FOutput[FDaTrie[NextState].Output] do
        TestMatch(TMatch.Make(Succ(p - pText - Len), Len, Index));
    while FDaTrie[NextState].NextOut <> 0 do begin
      NextState := FDaTrie[NextState].NextOut;
      with FOutput[FDaTrie[NextState].Output] do
        TestMatch(TMatch.Make(Succ(p - pText - Len), Len, Index));
    end;
  end;
  Result := Match;
end;

function TACNfaUtf8.DoFindFirstOww(const s: string; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch;
var
  Match: TMatch;
  MatchLess: TMatchLess;
  procedure TestMatch(const m: TMatch); inline;
  begin
    if (Match.Offset = 0) or MatchLess(m, Match) then
      Match := m;
  end;
var
  p, pEnd, pText: PByte;
  State, NextState, qTop: Int32;
  c: Ucs4Char;
begin
  Match := TMatch.Make(0, 0, NULL_INDEX);
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit(Match);
  qTop := PushFirstOffset(s, Succ(p - pText));
  State := 0;
  if aMode < smmLeftmostFirst then begin
    while p < pEnd do begin
      c := CpToUcs4(p, pEnd - p);
      qTop := PushChar(qTop, c);
      State := NextFsmState(State, EncodeChar(c));
      if State = 0 then continue;
      if FDaTrie[State].Output <> 0 then
        with FOutput[FDaTrie[State].Output] do
          if OnWordBounds(p, pEnd, qTop, CpLen) then
            exit(TMatch.Make(Succ(p - pText - Len), Len, Index));
      NextState := State;
      while FDaTrie[NextState].NextOut <> 0 do begin
        NextState := FDaTrie[NextState].NextOut;
        with FOutput[FDaTrie[NextState].Output] do
          if OnWordBounds(p, pEnd, qTop, CpLen) then
            exit(TMatch.Make(Succ(p - pText - Len), Len, Index));
      end;
    end;
    exit(Match);
  end;
  MatchLess := SelectComparer(aMode);
  while p < pEnd do begin
    c := CpToUcs4(p, pEnd - p);
    qTop := PushChar(qTop, c);
    NextState := NextFsmState(State, EncodeChar(c));
    if NextState = 0 then
      if State = 0 then
        continue
      else
        if Match.Offset <> 0 then break;
    State := NextState;
    if FDaTrie[NextState].Output <> 0 then
      with FOutput[FDaTrie[NextState].Output] do
        if OnWordBounds(p, pEnd, qTop, CpLen) then
          TestMatch(TMatch.Make(Succ(p - pText - Len), Len, Index));
    while FDaTrie[NextState].NextOut <> 0 do begin
      NextState := FDaTrie[NextState].NextOut;
      with FOutput[FDaTrie[NextState].Output] do
        if OnWordBounds(p, pEnd, qTop, CpLen) then
          TestMatch(TMatch.Make(Succ(p - pText - Len), Len, Index));
    end;
  end;
  Result := Match;
end;
{$POP}

procedure TACNfaUtf8.DoSearchNoOverlap(const s: string; aOffset, aCount: SizeInt; aOnMatch: TNestMatch);
var
  p, pEnd, pText: PByte;
  State: Int32;
begin
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit;
  State := 0;
  while p < pEnd do
    begin
      State := NextFsmState(State, EncodeChar(CpToUcs4(p, pEnd - p)));
      if State = 0 then continue;
      if FDaTrie[State].Output <> 0 then
        with FOutput[FDaTrie[State].Output] do
          begin
            aOnMatch(TMatch.Make(Succ(p - pText - Len), Len, Index));
            State := 0;
            continue;
          end;
      if FDaTrie[State].NextOut <> 0 then
        with FOutput[FDaTrie[FDaTrie[State].NextOut].Output] do
          begin
            aOnMatch(TMatch.Make(Succ(p - pText - Len), Len, Index));
            State := 0;
          end;
    end;
end;

procedure TACNfaUtf8.DoSearchNoOverlapOww(const s: string; aOffset, aCount: SizeInt; aOnMatch: TNestMatch);
var
  p, pEnd, pText: PByte;
  State, Tmp, qTop: Int32;
  c: Ucs4Char;
begin
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit;
  qTop := PushFirstOffset(s, Succ(p - pText));
  State := 0;
  while p < pEnd do
    begin
      c := CpToUcs4(p, pEnd - p);
      qTop := PushChar(qTop, c);
      State := NextFsmState(State, EncodeChar(c));
      if State = 0 then continue;
      if FDaTrie[State].Output <> 0 then
        with FOutput[FDaTrie[State].Output] do
          if OnWordBounds(p, pEnd, qTop, CpLen) then
            begin
              aOnMatch(TMatch.Make(Succ(p - pText - Len), Len, Index));
              State := 0;
              continue;
            end;
      Tmp := State;
      while FDaTrie[Tmp].NextOut <> 0 do
        begin
          Tmp := FDaTrie[Tmp].NextOut;
          with FOutput[FDaTrie[Tmp].Output] do
            if OnWordBounds(p, pEnd, qTop, CpLen) then
              begin
                aOnMatch(TMatch.Make(Succ(p - pText - Len), Len, Index));
                State := 0;
                break;
              end;
        end;
    end;
end;

procedure TACNfaUtf8.DoSearch(const s: string; aOffset, aCount: SizeInt);
var
  p, pEnd, pText: PByte;
  State, Tmp: Int32;
begin
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit;
  State := 0;
  while p < pEnd do
    begin
      State := NextFsmState(State, EncodeChar(CpToUcs4(p, pEnd - p)));
      if State = 0 then continue;
      if FDaTrie[State].Output <> 0 then
        with FOutput[FDaTrie[State].Output] do
          if not FOnMatch(TMatch.Make(Succ(p - pText - Len), Len, Index)) then exit;
      Tmp := State;
      while FDaTrie[Tmp].NextOut <> 0 do
        begin
          Tmp := FDaTrie[Tmp].NextOut;
          with FOutput[FDaTrie[Tmp].Output] do
            if not FOnMatch(TMatch.Make(Succ(p - pText - Len), Len, Index)) then exit;
        end;
    end;
end;

procedure TACNfaUtf8.DoSearchOww(const s: string; aOffset, aCount: SizeInt);
var
  p, pEnd, pText: PByte;
  State, Tmp, qTop: Int32;
  c: Ucs4Char;
begin
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit;
  qTop := PushFirstOffset(s, Succ(p - pText));
  State := 0;
  while p < pEnd do
    begin
      c := CpToUcs4(p, pEnd - p);
      qTop := PushChar(qTop, c);
      State := NextFsmState(State, EncodeChar(c));
      if State = 0 then continue;
      if FDaTrie[State].Output <> 0 then
        with FOutput[FDaTrie[State].Output] do
          if OnWordBounds(p, pEnd, qTop, CpLen) then
            if not FOnMatch(TMatch.Make(Succ(p - pText - Len), Len, Index)) then exit;
      Tmp := State;
      while FDaTrie[Tmp].NextOut <> 0 do
        begin
          Tmp := FDaTrie[Tmp].NextOut;
          with FOutput[FDaTrie[Tmp].Output] do
            if OnWordBounds(p, pEnd, qTop, CpLen) then
              if not FOnMatch(TMatch.Make(Succ(p - pText - Len), Len, Index)) then exit;
        end;
    end;
end;

function TACNfaUtf8.GetHasMatch(const s: string; aOffset, aCount: SizeInt): Boolean;
var
  p, pEnd: PByte;
  State: Int32;
begin
  if not DoProlog(s, aOffset, aCount, p, pEnd) then exit(False);
  State := 0;
  while p < pEnd do
    begin
      State := NextFsmState(State, EncodeChar(CpToUcs4(p, pEnd - p)));
      if State = 0 then continue;
      if (FDaTrie[State].Output <> 0) or (FDaTrie[State].NextOut <> 0) then
        exit(True);
    end;
  Result := False;
end;

function TACNfaUtf8.GetHasMatchOww(const s: string; aOffset, aCount: SizeInt): Boolean;
var
  p, pEnd, pText: PByte;
  State, Tmp, qTop: Int32;
  c: Ucs4Char;
begin
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit(False);
  State := 0;
  qTop := PushFirstOffset(s, Succ(p - pText));
  while p < pEnd do
    begin
      c := CpToUcs4(p, pEnd - p);
      qTop := PushChar(qTop, c);
      State := NextFsmState(State, EncodeChar(c));
      if State = 0 then continue;
      if FDaTrie[State].Output <> 0 then
        with FOutput[FDaTrie[State].Output] do
          if OnWordBounds(p, pEnd, qTop, CpLen) then exit(True);
      Tmp := State;
      while FDaTrie[Tmp].NextOut <> 0 do
        begin
          Tmp := FDaTrie[Tmp].NextOut;
          with FOutput[FDaTrie[Tmp].Output] do
            if OnWordBounds(p, pEnd, qTop, CpLen) then exit(True);
        end;
    end;
  Result := False;
end;

function TACNfaUtf8.GetEmptyCount: SizeInt;
begin
  Result := System.Length(FDaTrie) - NodeCount;
end;

function TACNfaUtf8.GetAlphabetSize: SizeInt;
begin
  Result := FAlphabetSize;
end;

function TACNfaUtf8.GetQueueSize: Int32;
begin
  Result := FQueueSize;
end;

procedure TACNfaUtf8.FsmStreamWrite(aStream: TStream);
var
  I, Len: Int32;
begin
  Len := System.Length(FDaTrie);
  aStream.WriteBuffer(NToLE(Len), SizeOf(Len)); // FDaTrie size
  for I := 0 to System.High(FDaTrie) do         // FDaTrie array
    FDaTrie[I].WriteStream(aStream);
  Len := System.Length(FOutput);
  aStream.WriteBuffer(NToLE(Len), SizeOf(Len)); // FOutput size
  for I := 0 to System.High(FOutput) do         // FOutput array
    FOutput[I].WriteStream(aStream);
  Len := System.Length(FCharMap);
  aStream.WriteBuffer(NToLE(Len), SizeOf(Len)); // FCharMap size
  for I := 0 to System.High(FCharMap) do
    if FCharMap[I] <> 0 then begin              // only indices and values of non-zero codes
      aStream.WriteBuffer(NToLE(I), SizeOf(I));
      aStream.WriteBuffer(NToLE(FCharMap[I]), SizeOf(FCharMap[I]));
    end;
end;

procedure TACNfaUtf8.FsmStreamRead(aStream: TStream; const aHeader: TAcStreamHeader);
var
  I, J, C: Int32;
begin
  FWordCount := aHeader.RealPatternCount;
  FNodeCount := aHeader.StateCount;
  FAlphabetSize := aHeader.AlphabetSize;
  FQueueSize := aHeader.QueueSize;
  System.SetLength(FQueue, FQueueSize);
  J := 0; //make compliler happy
  aStream.ReadBuffer(J, SizeOf(J));     // FDaTrie size
  System.SetLength(FDaTrie, LEToN(J));
  for I := 0 to System.High(FDaTrie) do // FDaTrie array
    FDaTrie[I].ReadStream(aStream);
  aStream.ReadBuffer(J, SizeOf(J));     // FOutput size
  FOutCount := LEToN(J);
  System.SetLength(FOutput, FOutCount);
  for I := 0 to System.High(FOutput) do // FOutput array
    FOutput[I].ReadStream(aStream);
  aStream.ReadBuffer(J, SizeOf(J));     // FCharMap size
  FMaxChar := Pred(LEToN(J));
  System.SetLength(FCharMap, Succ(FMaxChar));
  C := 0; //make compliler happy
  for I := 1 to AlphabetSize do begin   // indices and values of non-zero codes
    aStream.ReadBuffer(J, SizeOf(J));
    aStream.ReadBuffer(C, SizeOf(C));
    FCharMap[LEToN(J)] := LEToN(C);
  end;
end;

class function TACNfaUtf8.GetFsmClass: TACFsmUtf8Class;
begin
  Result := TACNfaUtf8;
end;

class function TACNfaUtf8.CreateInstance(aCaseInsensitive: Boolean): TACNfaUtf8;
begin
  if aCaseInsensitive then
    Result := TACNfaCIUtf8.Create
  else
    Result := TACNfaUtf8.Create;
end;

constructor TACNfaUtf8.Create;
begin
  inherited;
  System.SetLength(FTrie, ARRAY_INITIAL_SIZE);
  System.SetLength(FOutput, ARRAY_INITIAL_SIZE);
  FNodeCount := 1;
  FOutCount := 1;
end;

function TACNfaUtf8.TryBuildFsm(const aPatternList: array of string): Boolean;
var
  I: SizeInt;
begin
  BuildCharMap(aPatternList);
  for I := 0 to System.High(aPatternList) do
    AddPattern(aPatternList[I], I);
  Inc(FQueueSize, 2);
  System.SetLength(FOutput, OutCount);
  System.SetLength(FQueue, FQueueSize);
  BuildFsm;
  FTrie := nil;
  Result := True;
end;

function TACNfaUtf8.Clone: IACSearchFsmUtf8;
var
  Inst: TACNfaUtf8;
begin
  Inst := TACNfaUtf8(GetFsmClass.Create);
  Inst.FCharMap := FCharMap;
  Inst.FDaTrie := FDaTrie;
  Inst.FOutput := FOutput;
  System.SetLength(Inst.FQueue, FQueueSize);
  Inst.FQueueSize := FQueueSize;
  Inst.FNodeCount := FNodeCount;
  Inst.FWordCount := FWordCount;
  Inst.FMaxChar := FMaxChar;
  Inst.FOutCount := FOutCount;
  Inst.FAlphabetSize := FAlphabetSize;
  Inst.FIgnoreCase := FIgnoreCase;
  Inst.FWholeWordsOnly := FWholeWordsOnly;
  Result := Inst;
end;

function TACNfaUtf8.IndexOfPattern(const s: string; aOffset: SizeInt; aCount: SizeInt): SizeInt;
var
  p, pEnd: PByte;
  State, c: Int32;
begin
  Result := NULL_INDEX;
  if not DoProlog(s, aOffset, aCount, p, pEnd) then exit;
  State := 0;
  while p < pEnd do
    begin
      if CaseInsensitive then
        c := EncodeChar(CpToUcs4Lower(p, pEnd - p))
      else
        c := EncodeChar(CpToUcs4(p, pEnd - p));
      if c = 0 then exit;
      State := NextMove(State, c);
      if State = 0 then exit;
    end;
  with FDaTrie[State] do
    if Output <> 0 then
      Result := FOutput[Output].Index;
end;

{ TACNfaCIUtf8 }

{$PUSH}{$WARN 5036 OFF}
function TACNfaCIUtf8.DoFindFirst(const s: string; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch;
var
  Match: TMatch;
  MatchLess: TMatchLess;
  procedure TestMatch(const m: TMatch); inline;
  begin
    if (Match.Offset = 0) or MatchLess(m, Match) then
      Match := m;
  end;
var
  Ofs: SizeInt;
  p, pEnd, pText: PByte;
  State, NextState, qTop: Int32;
begin
  Match := TMatch.Make(0, 0, NULL_INDEX);
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit(Match);
  qTop := 0;
  State := 0;
  if aMode < smmLeftmostFirst then begin
    while p < pEnd do begin
      qTop := PushOffset(qTop, Succ(p - pText));
      State := NextFsmState(State, EncodeChar(CpToUcs4Lower(p, pEnd - p)));
      if State = 0 then continue;
      if FDaTrie[State].Output <> 0 then
        with FOutput[FDaTrie[State].Output] do begin
          Ofs := PopOffset(qTop, CpLen);
          exit(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
        end;
      if FDaTrie[State].NextOut <> 0 then
        with FOutput[FDaTrie[FDaTrie[State].NextOut].Output] do begin
          Ofs := PopOffset(qTop, CpLen);
          exit(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
        end;
    end;
    exit(Match);
  end;
  MatchLess := SelectComparer(aMode);
  while p < pEnd do begin
    qTop := PushOffset(qTop, Succ(p - pText));
    NextState := NextFsmState(State, EncodeChar(CpToUcs4Lower(p, pEnd - p)));
    if NextState = 0 then
      if State = 0 then
        continue
      else
        if Match.Offset <> 0 then break;
    State := NextState;
    if FDaTrie[NextState].Output <> 0 then
      with FOutput[FDaTrie[NextState].Output] do begin
        Ofs := PopOffset(qTop, CpLen);
        TestMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
      end;
    while FDaTrie[NextState].NextOut <> 0 do begin
      NextState := FDaTrie[NextState].NextOut;
      with FOutput[FDaTrie[NextState].Output] do begin
        Ofs := PopOffset(qTop, CpLen);
        TestMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
      end;
    end;
  end;
  Result := Match;
end;

function TACNfaCIUtf8.DoFindFirstOww(const s: string; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch;
var
  Match: TMatch;
  MatchLess: TMatchLess;
  procedure TestMatch(const m: TMatch); inline;
  begin
    if (Match.Offset = 0) or MatchLess(m, Match) then
      Match := m;
  end;
var
  Ofs: SizeInt;
  p, pEnd, pText: PByte;
  State, NextState, qTop: Int32;
  c: Ucs4Char;
begin
  Match := TMatch.Make(0, 0, NULL_INDEX);
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit(Match);
  qTop := PushFirstOffset(s, Succ(p - pText));
  State := 0;
  if aMode < smmLeftmostFirst then begin
    while p < pEnd do begin
      c := CpToUcs4Lower(p, pEnd - p, Ofs);
      qTop := PushOffset(qTop, Succ(p - pText), c);
      p += Ofs;
      State := NextFsmState(State, EncodeChar(c));
      if State = 0 then continue;
      if FDaTrie[State].Output <> 0 then
        with FOutput[FDaTrie[State].Output] do
          if OnWordBounds(p, pEnd, qTop, CpLen) then begin
            Ofs := PopOffset(qTop, CpLen);
            exit(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
          end;
      NextState := State;
      while FDaTrie[NextState].NextOut <> 0 do begin
        NextState := FDaTrie[NextState].NextOut;
        with FOutput[FDaTrie[NextState].Output] do
          if OnWordBounds(p, pEnd, qTop, CpLen) then begin
            Ofs := PopOffset(qTop, CpLen);
            exit(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
          end;
      end;
    end;
    exit(Match);
  end;
  MatchLess := SelectComparer(aMode);
  while p < pEnd do begin
    c := CpToUcs4Lower(p, pEnd - p, Ofs);
    qTop := PushOffset(qTop, Succ(p - pText), c);
    p += Ofs;
    NextState := NextFsmState(State, EncodeChar(c));
    if NextState = 0 then
      if State = 0 then
        continue
      else
        if Match.Offset <> 0 then break;
    State := NextState;
    if FDaTrie[State].Output <> 0 then
      with FOutput[FDaTrie[State].Output] do
        if OnWordBounds(p, pEnd, qTop, CpLen) then begin
          Ofs := PopOffset(qTop, CpLen);
          TestMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
        end;
    while FDaTrie[NextState].NextOut <> 0 do begin
      NextState := FDaTrie[NextState].NextOut;
      with FOutput[FDaTrie[NextState].Output] do
        if OnWordBounds(p, pEnd, qTop, CpLen) then begin
          Ofs := PopOffset(qTop, CpLen);
          TestMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
        end;
    end;
  end;
  Result := Match;
end;
{$POP}

procedure TACNfaCIUtf8.DoSearchNoOverlap(const s: string; aOffset, aCount: SizeInt; aOnMatch: TNestMatch);
var
  Ofs: SizeInt;
  p, pEnd, pText: PByte;
  State, qTop: Int32;
begin
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit;
  qTop := 0;
  State := 0;
  while p < pEnd do
    begin
      qTop := PushOffset(qTop, Succ(p - pText));
      State := NextFsmState(State, EncodeChar(CpToUcs4Lower(p, pEnd - p)));
      if State = 0 then continue;
      if FDaTrie[State].Output <> 0 then
        with FOutput[FDaTrie[State].Output] do
          begin
            Ofs := PopOffset(qTop, CpLen);
            aOnMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
            State := 0;
            continue;
          end;
      if FDaTrie[State].NextOut <> 0 then
        with FOutput[FTrie[FDaTrie[State].NextOut].Output] do
          begin
            Ofs := PopOffset(qTop, CpLen);
            aOnMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
            State := 0;
          end;
    end;
end;

procedure TACNfaCIUtf8.DoSearchNoOverlapOww(const s: string; aOffset, aCount: SizeInt; aOnMatch: TNestMatch);
var
  Ofs: SizeInt;
  p, pEnd, pText: PByte;
  State, Tmp, qTop: Int32;
  c: Ucs4Char;
begin
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit;
  qTop := PushFirstOffset(s, Succ(p - pText));
  State := 0;
  while p < pEnd do
    begin
      c := CpToUcs4Lower(p, pEnd - p, Ofs);
      qTop := PushOffset(qTop, Succ(p - pText), c);
      p += Ofs;
      State := NextFsmState(State, EncodeChar(c));
      if State = 0 then continue;
      if FDaTrie[State].Output <> 0 then
        with FOutput[FDaTrie[State].Output] do
          if OnWordBounds(p, pEnd, qTop, CpLen) then
            begin
              Ofs := PopOffset(qTop, CpLen);
              aOnMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
              State := 0;
              continue;
            end;
      Tmp := State;
      while FDaTrie[Tmp].NextOut <> 0 do
        begin
          Tmp := FDaTrie[Tmp].NextOut;
          with FOutput[FDaTrie[Tmp].Output] do
            if OnWordBounds(p, pEnd, qTop, CpLen) then
              begin
                Ofs := PopOffset(qTop, CpLen);
                aOnMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index));
                State := 0;
                break;
              end;
        end;
    end;
end;

procedure TACNfaCIUtf8.DoSearch(const s: string; aOffset, aCount: SizeInt);
var
  Ofs: SizeInt;
  p, pEnd, pText: PByte;
  State, Tmp, qTop: Int32;
begin
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit;
  qTop := 0;
  State := 0;
  while p < pEnd do
    begin
      qTop := PushOffset(qTop, Succ(p - pText));
      State := NextFsmState(State, EncodeChar(CpToUcs4Lower(p, pEnd - p)));
      if State = 0 then continue;
      if FDaTrie[State].Output <> 0 then
        with FOutput[FDaTrie[State].Output] do
          begin
            Ofs := PopOffset(qTop, CpLen);
            if not FOnMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index)) then exit;
          end;
      Tmp := State;
      while FDaTrie[Tmp].NextOut <> 0 do
        begin
          Tmp := FDaTrie[Tmp].NextOut;
          with FOutput[FDaTrie[Tmp].Output] do
            begin
              Ofs := PopOffset(qTop, CpLen);
              if not FOnMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index)) then exit;
            end;
        end;
    end;
end;

procedure TACNfaCIUtf8.DoSearchOww(const s: string; aOffset, aCount: SizeInt);
var
  Ofs: SizeInt;
  p, pEnd, pText: PByte;
  State, Tmp, qTop: Int32;
  c: Ucs4Char;
begin
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit;
  qTop := PushFirstOffset(s, Succ(p - pText));
  State := 0;
  while p < pEnd do
    begin
      c := CpToUcs4Lower(p, pEnd - p, Ofs);
      qTop := PushOffset(qTop, Succ(p - pText), c);
      p += Ofs;
      State := NextFsmState(State, EncodeChar(c));
      if State = 0 then continue;
      if FDaTrie[State].Output <> 0 then
        with FOutput[FDaTrie[State].Output] do
          if OnWordBounds(p, pEnd, qTop, CpLen) then
            begin
              Ofs := PopOffset(qTop, CpLen);
              if not FOnMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index)) then exit;
            end;
      Tmp := State;
      while FDaTrie[Tmp].NextOut <> 0 do
        begin
          Tmp := FDaTrie[Tmp].NextOut;
          with FOutput[FDaTrie[Tmp].Output] do
            if OnWordBounds(p, pEnd, qTop, CpLen) then
              begin
                Ofs := PopOffset(qTop, CpLen);
                if not FOnMatch(TMatch.Make(Ofs, Succ(p - pText) - Ofs, Index)) then exit;
              end;
        end;
    end;
end;

function TACNfaCIUtf8.GetHasMatch(const s: string; aOffset, aCount: SizeInt): Boolean;
var
  p, pEnd: PByte;
  State: Int32;
begin
  if not DoProlog(s, aOffset, aCount, p, pEnd) then exit(False);
  State := 0;
  while p < pEnd do
    begin
      State := NextFsmState(State, EncodeChar(CpToUcs4Lower(p, pEnd - p)));
      if State = 0 then continue;
      if (FDaTrie[State].Output <> 0) or (FDaTrie[State].NextOut <> 0) then
        exit(True);
    end;
  Result := False;
end;

function TACNfaCIUtf8.GetHasMatchOww(const s: string; aOffset, aCount: SizeInt): Boolean;
var
  p, pEnd, pText: PByte;
  State, Tmp, qTop: Int32;
  c: Ucs4Char;
begin
  if not DoProlog(s, aOffset, aCount, pText, p, pEnd) then exit(False);
  State := 0;
  qTop := PushFirstOffset(s, Succ(p - pText));
  while p < pEnd do
    begin
      c := CpToUcs4Lower(p, pEnd - p);
      qTop := PushChar(qTop, c);
      State := NextFsmState(State, EncodeChar(c));
      if State = 0 then continue;
      if FDaTrie[State].Output <> 0 then
        with FOutput[FDaTrie[State].Output] do
          if OnWordBounds(p, pEnd, qTop, CpLen) then
            exit(True);
      Tmp := State;
      while FDaTrie[Tmp].NextOut <> 0 do
        begin
          Tmp := FDaTrie[Tmp].NextOut;
          with FOutput[FDaTrie[Tmp].Output] do
            if OnWordBounds(p, pEnd, qTop, CpLen) then
              exit(True);
        end;
    end;
  Result := False;
end;

class function TACNfaCIUtf8.GetFsmClass: TACFsmUtf8Class;
begin
  Result := TACNfaCIUtf8;
end;

constructor TACNfaCIUtf8.Create;
begin
  inherited;
  FIgnoreCase := True;
end;

{ TACPersistFsmUtf8 }

constructor TACPersistFsmUtf8.Create(const aPatterns: array of string; aIgnoreCase: Boolean);
var
  Inst: TACNfaUtf8;
begin
  FPatternList := specialize TGArrayHelpUtil<string>.CreateCopy(aPatterns);
  Inst := TACNfaUtf8.CreateInstance(aIgnoreCase);
  Inst.TryBuildFsm(FPatternList);
  FInstance := Inst;
end;

function TACPersistFsmUtf8.GetFsm: IACSearchFsmUtf8;
begin
  Result := FInstance;
end;

function TACPersistFsmUtf8.GetPatternListSize: Integer;
begin
  Result := System.Length(FPatternList);
end;

function TACPersistFsmUtf8.GetPattern(aIndex: Integer): string;
begin
  if DWord(aIndex) >= DWord(System.Length(FPatternList)) then
    raise EAcFsmError.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
  Result := FPatternList[aIndex];
end;

function TACPersistFsmUtf8.GetPatternList: TStringArray;
begin
  Result := System.Copy(FPatternList);
end;

procedure TACPersistFsmUtf8.LoadFromStream(aStream: TStream);
var
  Header: TAcStreamHeader;
  rbs: TReadBufStream;
  Inst: TACNfaUtf8;
  List: TStringArray;
  s: string;
  I, Len: Int32;
begin
  Inst := nil;
  try
    rbs := TReadBufStream.Create(aStream, BUF_SIZE);
    try
      Header.ReadStream(rbs); // header
      if Header.Magic <> MAGIC_SEQ then
        raise EAcFsmError.Create(SEUnknownAcStreamFormat); //
      if Header.Version <> STREAM_FMT_VERSION then
        raise EAcFsmError.Create(SEUnsupportAcFmtVersion); //
      System.SetLength(List, Header.PatternListSize);
      for I := 0 to System.High(List) do begin // pattern list
        rbs.ReadBuffer(Len, SizeOf(Len));
        System.SetLength(s, LEToN(Len));
        rbs.ReadBuffer(Pointer(s)^, System.Length(s));
        List[I] := s;
        s := '';
      end;
      Inst := TACNfaUtf8.CreateInstance(Header.CaseInsensitive);
      Inst.FsmStreamRead(rbs, Header); // FSM
      FInstance := Inst;
      FPatternList := List;
    finally
      rbs.Free;
    end;
  except
    Inst.Free;
    raise;
  end;
end;

procedure TACPersistFsmUtf8.SaveToStream(aStream: TStream);
var
  Header: TAcStreamHeader;
  wbs: TWriteBufStream;
  s: string;
  I, Len: Int32;
begin
  if FInstance = nil then
    raise EAcFsmError.Create(SEAcFsmNotAssigned);
  Header.Magic := MAGIC_SEQ;
  Header.Version := STREAM_FMT_VERSION;
  Header.PatternListSize := System.Length(FPatternList);
  Header.RealPatternCount := FInstance.PatternCount;
  Header.StateCount := FInstance.StateCount;
  Header.AlphabetSize := FInstance.AlphabetSize;
  Header.QueueSize := (FInstance as IAcFsmStreamWriter).GetQueueSize;
  Header.CaseInsensitive := FInstance.CaseInsensitive;
  wbs := TWriteBufStream.Create(aStream, BUF_SIZE);
  try
    Header.WriteStream(wbs); // header
    for I := 0 to System.High(FPatternList) do // pattern list
      begin
        s := FPatternList[I];
        Len := System.Length(s);
        wbs.WriteBuffer(NToLE(Len), SizeOf(Len));
        wbs.WriteBuffer(Pointer(s)^, Len);
      end;
    (FInstance as IAcFsmStreamWriter).FsmStreamWrite(wbs); // FSM
  finally
    wbs.Free;
  end;
end;

procedure TACPersistFsmUtf8.LoadFromFile(const aFileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TACPersistFsmUtf8.SaveToFile(const aFileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;


function CreateAcFsmUtf8(const aPatterns: array of string; aIgnoreCase: Boolean;
  aForceNFA: Boolean): IACSearchFsmUtf8;
begin
  Result := TACFsmUtf8.CreateInstance(aPatterns, aIgnoreCase, aForceNFA);
end;

function NewAcFsmUtf8(const aPatterns: array of string; aIgnoreCase: Boolean): IACPersistFsmUtf8;
begin
  Result := TACPersistFsmUtf8.Create(aPatterns, aIgnoreCase);
end;

function NewAcFsmUtf8(aPatterEnum: specialize IGEnumerable<string>; aIgnoreCase: Boolean): IACPersistFsmUtf8;
begin
  Result := TACPersistFsmUtf8.Create(aPatterEnum.ToArray, aIgnoreCase);
end;

function LoadAcFsmUtf8(aStream: TStream): IACPersistFsmUtf8;
var
  r: TACPersistFsmUtf8;
begin
  Result := nil;
  r := TACPersistFsmUtf8.Create;
  try
    r.LoadFromStream(aStream);
    Result := r;
  except
    r.Free;
    raise;
  end;
end;

function LoadAcFsmUtf8(const aFileName: string): IACPersistFsmUtf8;
var
  r: TACPersistFsmUtf8;
begin
  Result := nil;
  r := TACPersistFsmUtf8.Create;
  try
    r.LoadFromFile(aFileName);
    Result := r;
  except
    r.Free;
    raise;
  end;
end;

function ACStrReplace(const aSource: string; const aSamples, aSubs: array of string; out aReplaceCount: SizeInt;
  const aOptions: TStrReplaceOptions; aMode: TOverlapsHandleMode; const aDefaultSub: string): string;
var
  IFsm: IACSearchFsmUtf8;
begin
  aReplaceCount := 0;
  if aSource = '' then exit('');
  if System.Length(aSamples) = 0 then exit(aSource);
  IFsm := TACFsmUtf8.CreateInstance(aSamples, sroIgnoreCase in aOptions, False);
  IFsm.OnlyWholeWords := sroOnlyWholeWords in aOptions;
  Result := ACStrReplace(IFsm, aSource, aSubs, aReplaceCount, aMode, aDefaultSub);
end;

function ACStrReplace(const aSource: string; const aSamples, aSubs: array of string;
  const aOptions: TStrReplaceOptions; aMode: TOverlapsHandleMode; const aDefaultSub: string): string;
var
  Dummy: SizeInt;
begin
  Result := ACStrReplace(aSource, aSamples, aSubs, Dummy, aOptions, aMode, aDefaultSub);
end;

function ACStrReplaceList(const aSource, aSamples, aSubs: array of string; out aReplaceCount: SizeInt;
  const aOptions: TStrReplaceOptions; aMode: TOverlapsHandleMode; const aDefaultSub: string): TStringArray;
var
  IFsm: IACSearchFsmUtf8;
  r: TStringArray;
  I, Count: SizeInt;
begin
  aReplaceCount := 0;
  if System.Length(aSource) = 0 then exit(nil);
  if System.Length(aSamples) = 0 then
    exit(specialize TGArrayHelpUtil<string>.CreateCopy(aSource));
  IFsm := TACFsmUtf8.CreateInstance(aSamples, sroIgnoreCase in aOptions, False);
  IFsm.OnlyWholeWords := sroOnlyWholeWords in aOptions;
  System.SetLength(r, System.Length(aSource));
  for I := 0 to System.High(aSource) do
    begin
      r[I] := ACStrReplace(IFsm, aSource[I], aSubs, Count, aMode, aDefaultSub);
      aReplaceCount += Count;
    end;
  Result := r;
end;

function ACStrReplaceList(const aSource, aSamples, aSubs: array of string; const aOptions: TStrReplaceOptions;
  aMode: TOverlapsHandleMode; const aDefaultSub: string): TStringArray;
var
  Dummy: SizeInt;
begin
  Result := ACStrReplaceList(aSource, aSamples, aSubs, Dummy, aOptions, aMode, aDefaultSub);
end;

{$PUSH}{$J-}
const
   OverlapMode2MatchMode: array[TOverlapsHandleMode] of TSetMatchMode = (
     smmLeftmostFirst, smmLeftmostLongest, smmLeftmostShortest
   );
{$POP}

function ACStrReplace(aSearchFsm: IACSearchFsmUtf8; const aSource: string; const aSubs: array of string;
  out aReplaceCount: SizeInt; aMode: TOverlapsHandleMode; const aDefaultSub: string): string;
var
  Matches: array of lgUtils.TIndexMatch;
  r: string;
  pSrc, p, pEnd, pR: PAnsiChar;
  I, RLen, CopyCount: SizeInt;
begin
  aReplaceCount := 0;
  if aSource = '' then exit('');
  Matches := aSearchFsm.FindMatches(aSource, OverlapMode2MatchMode[aMode]);
  if Matches = nil then exit(aSource);
  aReplaceCount := System.Length(Matches);
  RLen := System.Length(aSource);
  for I := 0 to System.High(Matches) do
    with Matches[I] do begin
      RLen -= Length;
      if Index < System.Length(aSubs) then
        RLen += System.Length(aSubs[Index])
      else
        RLen += System.Length(aDefaultSub);
    end;
  if RLen = 0 then exit('');
  System.SetLength(r, RLen);
  pR := Pointer(r);
  pSrc := Pointer(aSource);
  p := pSrc;
  pEnd := pSrc + System.Length(aSource);
  for I := 0 to System.High(Matches) do
    with Matches[I] do begin
      CopyCount := Offset - Succ(p - pSrc);
      System.Move(p^, pR^, CopyCount);
      pR += CopyCount;
      p += CopyCount + Length;
      if Index < System.Length(aSubs) then begin
        System.Move(Pointer(aSubs[Index])^, pR^, System.Length(aSubs[Index]));
        pR += System.Length(aSubs[Index]);
      end else begin
        System.Move(Pointer(aDefaultSub)^, pR^, System.Length(aDefaultSub));
        pR += System.Length(aDefaultSub);
      end;
    end;
  if p < pEnd then
    System.Move(p^, pR^, pEnd - p);
  Result := r;
end;

function ACStrReplace(aSearchFsm: IACSearchFsmUtf8; const aSource: string; const aSubs: array of string;
  aMode: TOverlapsHandleMode; const aDefaultSub: string): string;
var
  Dummy: SizeInt;
begin
  Result := ACStrReplace(aSearchFsm, aSource, aSubs, Dummy, aMode, aDefaultSub);
end;

end.
