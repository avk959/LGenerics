{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Some useful string routines.                                             *
*                                                                           *
*   Copyright(c) 2018-2024 A.Koverdyaev(avk)                                *
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
{$MODESWITCH NESTEDPROCVARS}
{$INLINE ON}

interface

uses

  Classes, SysUtils, Math,
  lgUtils,
  lgHelpers,
  lgArrayHelpers,
  lgAbstractContainer,
  lgVector,
  lgQueue,
  lgMiscUtils,
  lgStrConst;


type
  TStrSlice = record
    Ptr: PAnsiChar;
    Count: SizeInt;
    constructor Init(p: PAnsiChar; aCount: SizeInt);
    class operator := (const s: string): TStrSlice; inline;
    class operator := (const s: TStrSlice): string; inline;
    class operator = (const L, R: TStrSlice): Boolean; inline;
    class operator = (const L: TStrSlice; const R: string): Boolean; inline;
  end;

  TAnsiStrHelper = type helper(TAStrHelper) for string
  private
  type
    TStrEnumerable = class(specialize TGAutoEnumerable<string>)
    strict private
      FValue: string;
      FStartIndex,
      FLastIndex: SizeInt;
      FStopChars: TSysCharSet;
    protected
      function  GetCurrent: string; override;
    public
      constructor Create(const aValue: string; const aStopChars: TSysCharSet);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TSliceEnumerable = class(specialize TGAutoEnumerable<TStrSlice>)
    strict private
      FValue: string;
      FStartIndex,
      FLastIndex: SizeInt;
      FStopChars: TSysCharSet;
    protected
      function  GetCurrent: TStrSlice; override;
    public
      constructor Create(const aValue: string; const aStopChars: TSysCharSet);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

  public
  type
    IStrEnumerable   = specialize IGEnumerable<string>;
    ISliceEnumerable = specialize IGEnumerable<TStrSlice>;

    TWordSliceEnumerator = record
    strict private
      FValue: string;
      FStartIndex,
      FLastIndex: SizeInt;
      FStopChars: TSysCharSet;
    private
      procedure Init(const aValue: string; const aStopChars: TSysCharSet); inline;
      function  GetCurrent: TStrSlice; inline;
    public
      function  MoveNext: Boolean; inline;
      property  Current: TStrSlice read GetCurrent;
    end;

    TWordSliceEnum = record
    strict private
      FValue: string;
      FStopChars: TSysCharSet;
    private
      procedure Init(const aValue: string; const aStopChars: TSysCharSet); inline;
    public
      function GetEnumerator: TWordSliceEnumerator; inline;
      function ToArray: specialize TGArray<TStrSlice>;
    end;

  const
    WhiteSpaces     = [#9, #10, #11, #12, #13, ' '];
    AsciiDelimiters = [#0..#255] - ['a'..'z', 'A'..'Z', '0'..'9', '_'];
  { Join2 is similar to Join() from SysUtils, but does not raise exceptions,
    returning an empty string in such cases }
    class function Join2(const aSeparator: string; const aValues: array of string): string; static;
    class function Join2(const aSeparator: string; const aValues: array of string;
                         aFrom, aCount: SizeInt): string; static;
    class function Join(const aSeparator: string; const aValues: array of TStrSlice): string; static; overload;
    class function Join(const aSeparator: string; const aValues: array of TStrSlice;
                        aFrom, aCount: SizeInt): string; static; overload;
    class function Join(const aSeparator: string; aValues: IStrEnumerable): string; static; overload;
    class function Join(const aSeparator: string; aValues: ISliceEnumerable): string; static; overload;
    function StripWhiteSpaces: string; inline;
    function StripChar(aChar: AnsiChar): string;
    function StripChars(const aChars: TSysCharSet): string;
    function ToBytes: TBytes;
    // only single byte delimiters allowed
    function Words(const aStopChars: TSysCharSet = AsciiDelimiters): IStrEnumerable; inline;
    function WordSlices(const aStopChars: TSysCharSet = AsciiDelimiters): ISliceEnumerable; inline;
    function WordSliceEnum(const aStopChars: TSysCharSet = AsciiDelimiters): TWordSliceEnum; inline;
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
      FHeap: rawbytestring;
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
    FNeedle: rawbytestring;
    function  DoFind(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
    function  FindNext(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
    function  Find(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
  public
  type
    TIntArray = array of SizeInt;

    TStrMatches = record
    private
      FHeap: rawbytestring;
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
    constructor Create(const aPattern: rawbytestring);
    constructor Create(const aPattern: array of Byte);
  { returns an enumerator of indices(1-based) of all occurrences of pattern in s }
    function Matches(const s: rawbytestring): TStrMatches; inline;
  { returns an enumerator of indices(0-based) of all occurrences of pattern in a }
    function Matches(const a: array of Byte): TByteMatches;
  { returns the index of the next occurrence of the pattern in s,
    starting at index aOffset(1-based) or 0 if there is no occurrence;
    to get the index of the next occurrence, you need to pass in aOffset
    the index of the previous occurrence, increased by one }
    function NextMatch(const s: rawbytestring; aOffset: SizeInt = 1): SizeInt;
  { returns the index of the next occurrence of the pattern in a,
    starting at index aOffset(0-based) or -1 if there is no occurrence;
    to get the index of the next occurrence, you need to pass in aOffset
    the index of the previous occurrence, increased by one }
    function NextMatch(const a: array of Byte; aOffset: SizeInt = 0): SizeInt;
  { returns in an array the indices(1-based) of all occurrences of the pattern in s }
    function FindMatches(const s: rawbytestring): TIntArray;
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
      FHeap: rawbytestring;
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
    FNeedle: rawbytestring;
    function  Find(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
    function  FindNext(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
  public
  type
    TIntArray = array of SizeInt;

    TStrMatches = record
    private
      FHeap: rawbytestring;
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
    constructor Create(const aPattern: rawbytestring);
    constructor Create(const aPattern: array of Byte);
  { returns an enumerator of indices(1-based) of all occurrences of pattern in s }
    function Matches(const s: rawbytestring): TStrMatches; inline;
  { returns an enumerator of indices(0-based) of all occurrences of pattern in a }
    function Matches(const a: array of Byte): TByteMatches;
  { returns the index of the next occurrence of the pattern in s,
    starting at index aOffset(1-based) or 0 if there is no occurrence;
    to get the index of the next occurrence, you need to pass in aOffset
    the index of the previous occurrence, increased by one }
    function NextMatch(const s: rawbytestring; aOffset: SizeInt = 1): SizeInt;
  { returns the index of the next occurrence of the pattern in a,
    starting at index aOffset(0-based) or -1 if there is no occurrence;
    to get the index of the next occurrence, you need to pass in aOffset
    the index of the previous occurrence, increased by one }
    function NextMatch(const a: array of Byte; aOffset: SizeInt = 0): SizeInt;
  { returns in an array the indices(1-based) of all occurrences of the pattern in s }
    function FindMatches(const s: rawbytestring): TIntArray;
  { returns in an array the indices(0-based) of all occurrences of the pattern in a }
    function FindMatches(const a: array of Byte): TIntArray;
  end;

  TCaseMapTable = array[Byte] of Byte;
{ must convert the chars to a single case, no matter which one }
  TCaseMapFun = function(c: Char): Char;

  { TBmSearchCI implements case insensitive variant of TBmSearch;
    for single-byte encodings only }
  TBmSearchCI = record
  private
  type
    PMatcher = ^TBmSearchCI;

    TEnumerator = record
    private
      FCurrIndex: SizeInt;
      FHeap: rawbytestring;
      FMatcher: PMatcher;
      function GetCurrent: SizeInt; inline;
    public
      function MoveNext: Boolean;
      property Current: SizeInt read GetCurrent;
    end;

  var
    FCaseMap: TCaseMapTable;
    FBcShift: array[Byte] of Integer; //bad character shifts
    FGsShift: array of Integer;       //good suffix shifts
    FNeedle: rawbytestring;
    procedure FillMap;
    procedure FillMap(aMap: TCaseMapFun);
    procedure FillMap(const aTable: TCaseMapTable);
    function  DoFind(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
    function  FindNext(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
    function  Find(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
  public
  type
    TIntArray = array of SizeInt;

    TMatches = record
    private
      FHeap: rawbytestring;
      FMatcher: PMatcher;
    public
      function GetEnumerator: TEnumerator; inline;
    end;
  { initializes the algorithm with a search pattern }
    constructor Create(const aPattern: rawbytestring);
  { initializes the algorithm with a search pattern and custom case map }
    constructor Create(const aPattern: rawbytestring; aMap: TCaseMapFun);
    constructor Create(const aPattern: rawbytestring; const aTable: TCaseMapTable);
  { sets a new search pattern; it is assumed that the algorithm was previously initialized }
    procedure Update(const aPattern: rawbytestring);
  { returns an enumerator of indices(1-based) of all occurrences of pattern in s }
    function Matches(const s: rawbytestring): TMatches; inline;
  { returns the index of the next occurrence of the pattern in s,
    starting at index aOffset(1-based) or 0 if there is no occurrence;
    to get the index of the next occurrence, you need to pass in aOffset
    the index of the previous occurrence, increased by one }
    function NextMatch(const s: rawbytestring; aOffset: SizeInt = 1): SizeInt;
  { returns in an array the indices(1-based) of all occurrences of the pattern in s }
    function FindMatches(const s: rawbytestring): TIntArray;
  end;

  { TACAbstractFsm }
  TACAbstractFsm = class abstract
  type
    TMatch      = LgUtils.TIndexMatch;
    TOnMatch    = specialize TGOnTest<TMatch>;
    TNestMatch  = specialize TGNestTest<TMatch>;
    TMatchArray = specialize TGArray<TMatch>;
  protected
  type
    TSortHelper = specialize TGRegularTimSort<TMatch>;
  protected
    FOnMatchHandler: TOnMatch;
    FNestMatchHandler: TNestMatch;
    FOnMatch: TOnMatch;
    FNodeCount,
    FWordCount: SizeInt;
    FAlphabetSize: SizeInt;
    function  TestOnMatch(const m: TMatch): Boolean;
    function  TestNestMatch(const m: TMatch): Boolean;
    procedure RegisterMatchHandler(h: TOnMatch);
    procedure RegisterMatchHandler(h: TNestMatch);
    function  TestInput(const s: rawbytestring; var aOffset, aCount: SizeInt): Boolean; inline;
    function  TestInput(const a: array of Byte; var aOffset, aCount: SizeInt): Boolean;
    function  GetPatternIndex(p: PByte; aCount: SizeInt): SizeInt; virtual; abstract;
    function  GetFirstMatch(p: PByte; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch; virtual; abstract;
    function  DoFindNoOverlap(p: PByte; aOffset, aCount: SizeInt): TMatchArray; virtual; abstract;
    function  DoFind(p: PByte; aOffset, aCount: SizeInt): TMatchArray; virtual; abstract;
    procedure DoSearch(p: PByte; aOffset, aCount: SizeInt); virtual; abstract;
    function  HasMatch(p: PByte; aCount: SizeInt): Boolean; virtual; abstract;
    class procedure DoFilterMatches(var aMatches: TMatchArray; aMode: TSetMatchMode); static;
  public
    class function FilterMatches(const aSource: array of TMatch; aMode: TSetMatchMode): TMatchArray; static;
  { returns the index of the pattern in the input list if the instance contains such a pattern,
    otherwise returns -1 }
    function IndexOfPattern(const aText: rawbytestring; aOffset: SizeInt = 1; aCount: SizeInt = 0): SizeInt;
    function IndexOfPattern(const aText: array of Byte; aOffset: SizeInt = 0; aCount: SizeInt = 0): SizeInt;
  { returns True if the instance contains such a pattern, False otherwise }
    function IsMatch(const aText: rawbytestring; aOffset: SizeInt = 1; aCount: SizeInt = 0): Boolean;
    function IsMatch(const aText: array of Byte; aOffset: SizeInt = 0; aCount: SizeInt = 0): Boolean;
  { returns the first match(according to the specified mode), if any, otherwise returns stub(0,0,-1) }
    function FirstMatch(const aText: rawbytestring; aMode: TSetMatchMode = smmDefault;
                        aOffset: SizeInt = 1; aCount: SizeInt = 0): TMatch;
    function FirstMatch(const aText: array of Byte; aMode: TSetMatchMode = smmDefault;
                        aOffset: SizeInt = 0; aCount: SizeInt = 0): TMatch;
  { returns an array of all matches found in the string aText(according to the specified mode),
    starting at position aOffset within aCount bytes;
    any value of aCount < 1 implies a search to the end of the text }
    function FindMatches(const aText: rawbytestring; aMode: TSetMatchMode = smmDefault;
                         aOffset: SizeInt = 1; aCount: SizeInt = 0): TMatchArray;
    function FindMatches(const aText: array of Byte; aMode: TSetMatchMode = smmDefault;
                         aOffset: SizeInt = 0; aCount: SizeInt = 0): TMatchArray;
  { searches in the string aText starting at position aOffset within aCount bytes, passing
    all found matches to the callback aOnMatch(); immediately exits the procedure if aOnMatch()
    returns False; any value of aCount < 1 implies a search to the end of the text;
    if aOnMatch is nil, it just immediately exits the procedure }
    procedure Search(const aText: rawbytestring; aOnMatch: TOnMatch; aOffset: SizeInt = 1; aCount: SizeInt = 0);
    procedure Search(const aText: rawbytestring; aOnMatch: TNestMatch; aOffset: SizeInt = 1; aCount: SizeInt = 0);
    procedure Search(const aText: array of Byte; aOnMatch: TOnMatch; aOffset: SizeInt = 0; aCount: SizeInt = 0);
    procedure Search(const aText: array of Byte; aOnMatch: TNestMatch; aOffset: SizeInt = 0; aCount: SizeInt = 0);
  { returns True if at least one match is found in the string aText, starting at position aOffset
    within aCount bytes; any value of aCount < 1 implies a search to the end of the text }
    function ContainsMatch(const aText: rawbytestring; aOffset: SizeInt = 1; aCount: SizeInt = 0): Boolean;
    function ContainsMatch(const aText: array of Byte; aOffset: SizeInt = 0; aCount: SizeInt = 0): Boolean;
    property StateCount: SizeInt read FNodeCount;
    property PatternCount: SizeInt read FWordCount;
    property AlphabetSize: SizeInt read FAlphabetSize;
  end;

  { TACSearchDfa: Aho-Corasick DFA for multiple string matching; does not store dictionary
    elements explicitly, instead storing their indices in the initializing pattern list }
  TACSearchDfa = class(TACAbstractFsm)
  protected
  type
    TNode = record
      NextMove: array of Int32;// transition table
      Output,                  // output link
      Index,                   // index in the input list(if node is teminal)
      Length: Int32;           // length in bytes(Length > 0 indicates a terminal node)
    end;
  protected
    FCodeMap: array[Byte] of Int32;
    FTrie: array of TNode;
    function  NewNode: SizeInt;
    procedure BuildCodeMap(const aList: array of rawbytestring);
    procedure BuildCodeMap(const aList: array of TBytes);
    procedure DoAddPattern(p: PByte; aCount, aIndex: Integer);
    procedure AddPattern(const s: rawbytestring; aIndex: SizeInt);
    procedure AddPattern(const a: array of Byte; aIndex: SizeInt);
    procedure BuildFsm;
    function  NextFsmState(aState, aCode: Int32): Int32; inline;
    function  GetPatternIndex(p: PByte; aCount: SizeInt): SizeInt; override;
    function  GetFirstMatch(p: PByte; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch; override;
    function  DoFindNoOverlap(p: PByte; aOffset, aCount: SizeInt): TMatchArray; override;
    function  DoFind(p: PByte; aOffset, aCount: SizeInt): TMatchArray; override;
    procedure DoSearch(p: PByte; aOffset, aCount: SizeInt); override;
    function  HasMatch(p: PByte; aCount: SizeInt): Boolean; override;
  public
    constructor Create;
    constructor Create(const aPatternList: array of rawbytestring);
    constructor CreateBytes(const aPatternList: array of TBytes);
    function Clone: TACSearchDfa;
  end;

  { TDaACSearch: Aho-Corasick NFA based on double array trie; does not store dictionary
    elements explicitly, instead storing their indices in the initializing pattern list }
  TDaACSearch = class(TACAbstractFsm)
  protected
  type
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
      function DoAdd(aKey, aIndex: Int32): PEntry;
      function GetKey(aIndex: Int32): Int32; inline;
      function GetValue(aIndex: Int32): Int32; inline;
    public
      function GetEnumerator: TEnumerator; inline;
      function GetMutValueDef(aKey, aDefault: Int32): PInt32; inline;
      property Count: Int32 read FCount;
      property Keys[aIndex: Int32]: Int32 read GetKey;
      property Values[aIndex: Int32]: Int32 read GetValue;
    end;
    TOutput = record
      Length,       // length in bytes
      Index: Int32; // index in the input list
    end;
    TNode = record
      AdjList: TCode2StateMap;
      Output: Int32;
    end;
    TDaNode = record
      Base,
      Check,
      Failure,        // failure link(singly linked list)
      Output,         // output link
      NextOut: Int32; // next output link(singly linked list)
    end;
    TPair = record
      Node,
      DaNode: Int32;
      constructor Make(aNode, aDaNode: Int32);
    end;
    TCountRec = record
      Key: Byte;
      Count: Int32;
    end;
  const
    NULL_NODE         = Low(Int32);
    LEAF_NODE         = Int32(0);
    BIG_ALPHABET_SIZE = 128;//todo: need some tweaking?
  var
    FCodeMap: array of Int32;
    FOutput: array of TOutput;
    FTrie: array of TNode;
    FDaTrie: array of TDaNode;
    FOutCount: Int32;
    function  GetEmptyCount: Int32; inline;
    function  NewOutput(aIndex, aLen: Int32): Int32; inline;
    function  NewNode: Int32; inline;
    procedure BuildCodeMap(const aList: array of string);
    procedure BuildCodeMap(const aList: array of TBytes);
    procedure DoAddPattern(p: PByte; aCount, aIndex: Integer);
    procedure AddPattern(const s: rawbytestring; aIndex: Int32);
    procedure AddPattern(const a: array of Byte; aIndex: Int32);
    function  NextMove(aState, aCode: Int32): Int32; inline;
    function  NextFsmState(aState, aCode: Int32): Int32; inline;
    procedure BuildFsm;
    function  GetPatternIndex(p: PByte; aCount: SizeInt): SizeInt; override;
    function  GetFirstMatch(p: PByte; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch; override;
    function  DoFindNoOverlap(p: PByte; aOffset, aCount: SizeInt): TMatchArray; override;
    function  DoFind(p: PByte; aOffset, aCount: SizeInt): TMatchArray; override;
    procedure DoSearch(p: PByte; aOffset, aCount: SizeInt); override;
    function  HasMatch(p: PByte; aCount: SizeInt): Boolean; override;
    property  OutCount: Int32 read FOutCount;
    class function CountRecCmp(const L, R: TCountRec): Boolean; static;
  public
    constructor Create;
    constructor Create(const aPatternList: array of rawbytestring);
    constructor CreateBytes(const aPatternList: array of TBytes);
    function Clone: TDaACSearch;
    property EmptyCellCount: Int32 read GetEmptyCount;
  end;

{ the following functions are only suitable for single-byte encodings }

{ returns True if aSub is a subsequence of aStr, False otherwise }
  function IsSubSequence(const aStr, aSub: rawbytestring): Boolean; inline;
{ returns the longest common subsequence(LCS) of sequences L and R, reducing the task to LIS,
  with O(SLogN) time complexity, where S is the number of the matching pairs in L and R;
  inspired by Dan Gusfield "Algorithms on Strings, Trees and Sequences", section 12.5 }
  function LcsGus(const L, R: rawbytestring): rawbytestring;
  function LcsGus(const L, R: array of Byte): TBytes;
{ recursive, returns the longest common subsequence(LCS) of sequences L and R;
  uses Kumar-Rangan algorithm for LCS with O(N(M-|LCS|)) time complexity and linear space complexity }
  function LcsKR(const L, R: rawbytestring): rawbytestring;
  function LcsKR(const L, R: array of Byte): TBytes;
{ recursive, returns the longest common subsequence(LCS) of sequences L and R;
  uses O(ND) algorithm from "An O(ND) Difference Algorithm and Its Variations" by Gene Myers }
  function LcsMyers(const L, R: rawbytestring): rawbytestring;
  function LcsMyers(const L, R: array of Byte): TBytes;
{ returns the edit distance between L and R; used a dynamic programming algorithm
  with O(mn) time complexity, where m and n are the lengths of L and R respectively,
  and O(Max(m, n)) space complexity }
  function EditDistance(const L, R: rawbytestring; const aCost: TSeqEditCost): SizeInt;
  function EditDistance(const L, R: array of Byte; const aCost: TSeqEditCost): SizeInt;
{ returns the Levenshtein distance between L and R; a Pascal translation(well, almost :))
  of github.com/vaadin/gwt/dev/util/editdistance/ModifiedBerghelRoachEditDistance.java -
  a modified version of algorithm described by Berghel and Roach with O(min(n, m)*d)
  worst-case time complexity, where n and m are the lengths of L and R respectively
  and d is the edit distance computed }
  function LevDistanceMbr(const L, R: rawbytestring): SizeInt;
  function LevDistanceMbr(const L, R: array of Byte): SizeInt;
{ the same as above; the aLimit parameter indicates the maximum expected distance,
  if this value is exceeded when calculating the distance, then the function exits
  immediately and returns -1; a negative value of aLimit is considered as no limit }
  function LevDistanceMbr(const L, R: rawbytestring; aLimit: SizeInt): SizeInt;
  function LevDistanceMbr(const L, R: array of Byte; aLimit: SizeInt): SizeInt;
{ returns the Levenshtein distance between L and R; uses the Myers bit-vector algorithm
  with O(dn/w) time complexity, where n is Max(|L|, |R|), d is edit distance computed,
  and w is the size of a computer word }
  function LevDistanceMyers(const L, R: rawbytestring): SizeInt;
  function LevDistanceMyers(const L, R: array of Byte): SizeInt;
{ the same as above; the aLimit parameter indicates the maximum expected distance,
  if this value is exceeded when calculating the distance, then the function exits
  immediately and returns -1; a negative value of aLimit is considered as no limit }
  function LevDistanceMyers(const L, R: rawbytestring; aLimit: SizeInt): SizeInt;
  function LevDistanceMyers(const L, R: array of Byte; aLimit: SizeInt): SizeInt;
{ the LCS edit distance allows only insertions and deletions; based on
  "An O(NP) Sequence Comparison Algorithm" by Sun Wu, Udi Manber, Gene Myers and Webb Miller }
  function LcsDistanceWM(const L, R: rawbytestring): SizeInt;
  function LcsDistanceWM(const L, R: array of Byte): SizeInt;
{ the same as above; the aLimit parameter indicates the maximum expected distance,
  if this value is exceeded when calculating the distance, then the function exits
  immediately and returns -1; a negative value of aLimit is considered as no limit }
  function LcsDistanceWM(const L, R: rawbytestring; aLimit: SizeInt): SizeInt;
  function LcsDistanceWM(const L, R: array of Byte; aLimit: SizeInt): SizeInt;
{ returns the Damerau-Levenshtein distance(restricted) between L and R using modified Berghel-Roach algorithm }
  function DumDistanceMbr(const L, R: rawbytestring): SizeInt;
  function DumDistanceMbr(const L, R: array of Byte): SizeInt;
{ the same as above; the aLimit parameter indicates the maximum expected distance,
  if this value is exceeded when calculating the distance, then the function exits
  immediately and returns -1; a negative value of aLimit is considered as no limit }
  function DumDistanceMbr(const L, R: rawbytestring; aLimit: SizeInt): SizeInt;
  function DumDistanceMbr(const L, R: array of Byte; aLimit: SizeInt): SizeInt;
{ returns similarity ratio using specified distance algorithm;
  aLimit specifies the lower bound of the required similarity(0.0 < aLimit <= 1.0),
  if the obtained value is less than the specified one, zero will be returned;
  aLimit <= 0 does not impose any restrictions on the obtained values }
  function SimRatio(const L, R: rawbytestring; aLimit: Double = Double(0); Algo: TSeqDistanceAlgo = sdaDefault): Double;
  function SimRatio(const L, R: array of Byte; aLimit: Double = Double(0); Algo: TSeqDistanceAlgo = sdaDefault): Double;

type
{ must convert the string to a single case, no matter which one }
  TSimCaseMap = function(const s: rawbytestring): rawbytestring;
  TSimLess    = function(const L, R: array of Char): Boolean;

const
  DEF_STOP_CHARS = [#0..#32];

{ returns the similarity ratio computed using the specified distance algorithm with some
  preprocessing of the input text; inspired by FuzzyWuzzy }
  function SimRatioEx(
    const L, R: rawbytestring;
    aMode: TSimMode = smSimple;
    const aStopChars: TSysCharSet = DEF_STOP_CHARS;
    const aOptions: TSimOptions = [];
    aLimit: Double = Double(0);
    Algo: TSeqDistanceAlgo = sdaDefault;
    aCaseMap: TSimCaseMap = nil;
    aLess: TSimLess = nil
  ): Double;
{ returns an array, each element of which contains the similarity ratio between
  aPattern and the corresponding element in the aValues array }
  function SimRatioList(
    const aPattern: rawbytestring;
    const aValues: array of rawbytestring;
    aMode: TSimMode = smSimple;
    const aStopChars: TSysCharSet = DEF_STOP_CHARS;
    const aOptions: TSimOptions = [];
    aLimit: Double = Double(0);
    Algo: TSeqDistanceAlgo = sdaDefault;
    aCaseMap: TSimCaseMap = nil;
    aLess: TSimLess = nil
  ): specialize TGArray<Double>;

type
  TRbStrRatio = record
    Value: rawbytestring;
    Ratio: Double;
  end;
{ returns an array of pairs sorted by descending similarity ratio and containing only those
  strings whose similarity ratio is not less than the specified boundary aLimit }
  function SelectSimilar(
    const aPattern: rawbytestring;
    const aValues: array of rawbytestring;
    aLimit: Double;
    aMode: TSimMode = smSimple;
    const aStopChars: TSysCharSet = DEF_STOP_CHARS;
    const aOptions: TSimOptions = [];
    Algo: TSeqDistanceAlgo = sdaDefault;
    aCaseMap: TSimCaseMap = nil;
    aLess: TSimLess = nil
  ): specialize TGArray<TRbStrRatio>;

  function IsValidDotQuadIPv4(const s: rawbytestring): Boolean;
  function IsValidDotDecIPv4(const s: rawbytestring): Boolean;

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}

function IsSubSequence(const aStr, aSub: rawbytestring): Boolean;
begin
  Result := specialize TGSimpleArrayHelper<Byte>.IsSubSequence(
    PByte(aStr)[0..Pred(System.Length(aStr))], PByte(aSub)[0..Pred(System.Length(aSub))]);
end;

function SkipPrefix(var pL, pR: PByte; var aLenL, aLenR: SizeInt): SizeInt; inline;
begin
  //implied aLenL <= aLenR
  Result := 0;

  while (Result < aLenL) and (pL[Result] = pR[Result]) do
    Inc(Result);

  pL += Result;
  pR += Result;
  aLenL -= Result;
  aLenR -= Result;
end;

function SkipSuffix(pL, pR: PByte; var aLenL, aLenR: SizeInt): SizeInt; inline;
begin
  //implied aLenL <= aLenR
  Result := 0;
  while (aLenL > 0) and (pL[Pred(aLenL)] = pR[Pred(aLenR)]) do
    begin
      Dec(aLenL);
      Dec(aLenR);
      Inc(Result);
    end;
end;

type
  TLcsFunImpl = function(pL, pR: PByte; aLenL, aLenR: SizeInt): TBytes;

const
  MAX_STATIC = 512;

{$PUSH}{$WARN 5057 OFF}
function LcsGusImpl(pL, pR: PByte; aLenL, aLenR: SizeInt): TBytes;
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
  I, J, NodeIdx: SizeInt;
const
  INIT_SIZE = 256; //???
begin
  //here aLenL <= aLenR and aLenL <> 0
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

  if Tmp = nil then exit(nil);

  NodeList := nil;

  LocLis := TSizeIntHelper.Lis(Tmp);

  if LocLis = nil then exit([pR[Tmp[0]]]);

  System.SetLength(Result, System.Length(LocLis));
  for I := 0 to System.High(LocLis) do
    Result[I] := pR[LocLis[I]];
end;
{$POP}

function GetLcs(pL, pR: PByte; aLenL, aLenR: SizeInt; aImpl: TLcsFunImpl): TBytes;
var
  LocLcs: TBytes;
  PrefixLen, SuffixLen: SizeInt;
begin
  //here aLenL <= aLenR
  if pL = pR then
    exit(specialize TGArrayHelpUtil<Byte>.CreateCopy(pL[0..Pred(aLenL)]));

  SuffixLen := SkipSuffix(pL, pR, aLenL, aLenR);
  PrefixLen := SkipPrefix(pL, pR, aLenL, aLenR);

  if aLenL = 0 then begin
    System.SetLength(Result, PrefixLen + SuffixLen);
    System.Move((pL - PrefixLen)^, PByte(Result)^, PrefixLen);
    System.Move(pL^, (PByte(Result) + PrefixLen)^, SuffixLen);
    exit;
  end;

  if (aLenL = 1) and (aLenR = 1) then begin
    System.SetLength(Result, PrefixLen + SuffixLen);
    System.Move((pL - PrefixLen)^, PByte(Result)^, PrefixLen);
    System.Move((pL + 1)^, (PByte(Result) + PrefixLen)^, SuffixLen);
    exit;
  end;

  LocLcs := aImpl(pL, pR, aLenL, aLenR);

  System.SetLength(Result, System.Length(LocLcs) + PrefixLen + SuffixLen);
  System.Move((pL - PrefixLen)^, PByte(Result)^, PrefixLen);
  System.Move(PByte(LocLcs)^, (PByte(Result) + PrefixLen)^, System.Length(LocLcs));
  System.Move((pL + aLenL)^, (PByte(Result) + PrefixLen + System.Length(LocLcs))^, SuffixLen);
end;

function LcsGus(const L, R: rawbytestring): rawbytestring;
var
  b: TBytes;
begin
  Result := '';
  if (L = '') or (R = '') then
    exit;
  if System.Length(L) <= System.Length(R) then
    b := GetLcs(Pointer(L), Pointer(R), System.Length(L), System.Length(R), @LcsGusImpl)
  else
    b := GetLcs(Pointer(R), Pointer(L), System.Length(R), System.Length(L), @LcsGusImpl);
  System.SetLength(Result, System.Length(b));
  System.Move(Pointer(b)^, Pointer(Result)^, System.Length(b));
end;

function LcsGus(const L, R: array of Byte): TBytes;
begin
  if (System.Length(L) = 0) or (System.Length(R) = 0) then
    exit(nil);
  if System.Length(L) <= System.Length(R) then
    Result := GetLcs(@L[0], @R[0], System.Length(L), System.Length(R), @LcsGusImpl)
  else
    Result := GetLcs(@R[0], @L[0], System.Length(R), System.Length(L), @LcsGusImpl);
end;

function LcsDistWmImpl(pL, pR: PByte; aLenL, aLenR: SizeInt): SizeInt; forward;
{$PUSH}{$WARN 5089 OFF}
{
 S. Kiran Kumar and C. Pandu Rangan(1987) "A Linear Space Algorithm for the LCS Problem"
}
function LcsKrImpl(pL, pR: PByte; aLenL, aLenR: SizeInt): TBytes;
var
  LocLcs: specialize TGLiteVector<Byte>;
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
        if J > R then LoR := 0 else LoR := R1[J];
        PosR := R2[J - 1] - 1;
        while (PosR > LoR) and (pL[LFirst+(I-1)] <> pR[RFirst+(PosR-1)]) do
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
        if J > R then LoR := 0 else LoR := R1[J];
        PosR := R2[J - 1] - 1;
        while (PosR > LoR) and (pL[LFirst-(I-1)] <> pR[RFirst-(PosR-1)]) do
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
    while (I < LcsLen) and (pL[LFirst+I] = pR[RFirst+LL[LcsLen-I]-1]) do begin
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
  //here aLenL <= aLenR
  if pL = pR then
    exit(specialize TGArrayHelpUtil<Byte>.CreateCopy(pL[0..Pred(aLenL)]));

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

  Lcs(0, Pred(aLenL), 0, Pred(aLenR), (aLenL + aLenR - LcsDistWmImpl(pL, pR, aLenL, aLenR)) div 2);

  Result := LocLcs.ToArray;
end;
{$POP}

function LcsKR(const L, R: rawbytestring): rawbytestring;
var
  b: TBytes;
begin
  Result := '';
  if (L = '') or (R = '') then
    exit;
  if System.Length(L) <= System.Length(R) then
    b := GetLcs(Pointer(L), Pointer(R), System.Length(L), System.Length(R), @LcsKrImpl)
  else
    b := GetLcs(Pointer(R), Pointer(L), System.Length(R), System.Length(L), @LcsKrImpl);
  System.SetLength(Result, System.Length(b));
  System.Move(Pointer(b)^, Pointer(Result)^, System.Length(b));
end;

function LcsKR(const L, R: array of Byte): TBytes;
begin
  if (System.Length(L) = 0) or (System.Length(R) = 0) then
    exit(nil);
  if System.Length(L) <= System.Length(R) then
    Result := GetLcs(@L[0], @R[0], System.Length(L), System.Length(R), @LcsKrImpl)
  else
    Result := GetLcs(@R[0], @L[0], System.Length(R), System.Length(L), @LcsKrImpl);
end;

type
  TSnake = record
    StartRow, StartCol,
    EndRow, EndCol: SizeInt;
    procedure SetStartCell(aRow, aCol: SizeInt); inline;
    procedure SetEndCell(aRow, aCol: SizeInt); inline;
  end;

procedure TSnake.SetStartCell(aRow, aCol: SizeInt);
begin
  StartRow := aRow;
  StartCol := aCol;
end;

procedure TSnake.SetEndCell(aRow, aCol: SizeInt);
begin
  EndRow := aRow;
  EndCol := aCol;
end;

{$PUSH}{$WARN 5089 OFF}{$WARN 5037 OFF}
{
  Eugene W. Myers(1986) "An O(ND) Difference Algorithm and Its Variations"
}
function LcsMyersImpl(pL, pR: PByte; aLenL, aLenR: SizeInt): TBytes;
var
  LocLcs: specialize TGLiteVector<Byte>;
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
            while (Row < LenL) and (Col < LenR) and (pL[LFirst + Row] = pR[RFirst + Col]) do
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
            while (Row < LenL) and (Col < LenR) and (pL[LLast-Row] = pR[RLast-Col]) do
              begin
                Inc(Row);
                Inc(Col);
              end;
            RevV[K] := Row;
            if not OddDelta and (K <= Delta + D) and (K >= Delta - D) and
              (Row + ForV[Delta - K] >= LenL) then
              begin
                aSnake.SetStartCell(Succ(LLast - Row), Succ(RLast - Col));
                exit(D * 2);
              end;
            K += 2;
          end;
      end;
    raise Exception.Create('Internal error in ' + {$I %CURRENTROUTINE%});
    Result := NULL_INDEX;
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
  //here aLenL <= aLenR
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

  Result := LocLcs.ToArray;
end;
{$POP}

function LcsMyers(const L, R: rawbytestring): rawbytestring;
var
  b: TBytes;
begin
  Result := '';
  if (L = '') or (R = '') then
    exit;
  if System.Length(L) <= System.Length(R) then
    b := GetLcs(Pointer(L), Pointer(R), System.Length(L), System.Length(R), @LcsMyersImpl)
  else
    b := GetLcs(Pointer(R), Pointer(L), System.Length(R), System.Length(L), @LcsMyersImpl);
  System.SetLength(Result, System.Length(b));
  System.Move(Pointer(b)^, Pointer(Result)^, System.Length(b));
end;

function LcsMyers(const L, R: array of Byte): TBytes;
begin
  if (System.Length(L) = 0) or (System.Length(R) = 0) then
    exit(nil);
  if System.Length(L) <= System.Length(R) then
    Result := GetLcs(@L[0], @R[0], System.Length(L), System.Length(R), @LcsMyersImpl)
  else
    Result := GetLcs(@R[0], @L[0], System.Length(R), System.Length(L), @LcsMyersImpl);
end;

function EditDistanceImpl(pL, pR: PByte; aLenL, aLenR: SizeInt; const aCost: TSeqEditCost): SizeInt;
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

  SkipSuffix(pL, pR, aLenL, aLenR);
  SkipPrefix(pL, pR, aLenL, aLenR);

  if aLenL = 0 then
    exit(aLenR);

  if (aLenL = 1) and (aLenR = 1) then
    if pL^ = pR^ then
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
      b := pL[I-1];
      for J := 1 to aLenR do
        begin
          if pR[J-1] = b then
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

function EditDistance(const L, R: rawbytestring; const aCost: TSeqEditCost): SizeInt;
begin
  if System.Length(L) = 0 then
    exit(System.Length(R))
  else
    if System.Length(R) = 0 then
      exit(System.Length(L));
  if System.Length(L) <= System.Length(R) then
    Result := EditDistanceImpl(Pointer(L), Pointer(R), System.Length(L), System.Length(R), aCost)
  else
    Result := EditDistanceImpl(Pointer(R), Pointer(L), System.Length(R), System.Length(L), aCost);
end;

function EditDistance(const L, R: array of Byte; const aCost: TSeqEditCost): SizeInt;
begin
  if System.Length(L) = 0 then
    exit(System.Length(R))
  else
    if System.Length(R) = 0 then
      exit(System.Length(L));
  if System.Length(L) <= System.Length(R) then
    Result := EditDistanceImpl(@L[0], @R[0], System.Length(L), System.Length(R), aCost)
  else
    Result := EditDistanceImpl(@R[0], @L[0], System.Length(R), System.Length(L), aCost);
end;

function LevDistanceMbrImpl(pL, pR: PByte; aLenL, aLenR, aLimit: SizeInt): SizeInt;

  function FindRow(k, aDist, aLeft, aAbove, aRight: SizeInt): SizeInt; inline;
  var
    I, MaxRow: SizeInt;
  begin
    if aDist = 0 then
      I := 0
    else
      I := MaxOf3(aLeft, aAbove + 1, aRight + 1);
    MaxRow := Math.Min(aLenL - k, aLenR);
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

function GetLevDistanceMbr(pL, pR: PByte; aLenL, aLenR, aLimit: SizeInt): SizeInt;
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
    exit(Ord(pL^ <> pR^));

  if aLimit > aLenR then
    aLimit := aLenR;

  Result := LevDistanceMbrImpl(pL, pR, aLenL, aLenR, aLimit);
end;

function LevDistanceMbr(const L, R: rawbytestring): SizeInt;
begin
  if L = '' then
    exit(System.Length(R))
  else
    if R = '' then
      exit(System.Length(L));
  if System.Length(L) <= System.Length(R) then
    Result := GetLevDistanceMbr(Pointer(L), Pointer(R), System.Length(L), System.Length(R), System.Length(R))
  else
    Result := GetLevDistanceMbr(Pointer(R), Pointer(L), System.Length(R), System.Length(L), System.Length(L));
end;

function LevDistanceMbr(const L, R: array of Byte): SizeInt;
begin
  if System.Length(L) = 0 then
    exit(System.Length(R))
  else
    if System.Length(R) = 0 then
      exit(System.Length(L));
  if System.Length(L) <= System.Length(R) then
    Result := GetLevDistanceMbr(@L[0], @R[0], System.Length(L), System.Length(R), System.Length(R))
  else
    Result := GetLevDistanceMbr(@R[0], @L[0], System.Length(R), System.Length(L), System.Length(L));
end;

function LevDistanceMbr(const L, R: rawbytestring; aLimit: SizeInt): SizeInt;
begin
  if aLimit < 0 then exit(LevDistanceMbr(L, R));
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
    Result := GetLevDistanceMbr(Pointer(L), Pointer(R), System.Length(L), System.Length(R), aLimit)
  else
    Result := GetLevDistanceMbr(Pointer(R), Pointer(L), System.Length(R), System.Length(L), aLimit);
end;

function LevDistanceMbr(const L, R: array of Byte; aLimit: SizeInt): SizeInt;
begin
  if aLimit < 0 then exit(LevDistanceMbr(L, R));
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
    Result := GetLevDistanceMbr(@L[0], @R[0], System.Length(L), System.Length(R), aLimit)
  else
    Result := GetLevDistanceMbr(@R[0], @L[0], System.Length(R), System.Length(L), aLimit);
end;

{$PUSH}{$WARN 5057 OFF}{$WARN 5036 OFF}{$Q-}{$R-}
{
  Myers, G.(1999) "A fast bit-vector algorithm for approximate string matching based on dynamic programming"
  Heikki Hyyrö(2001) "Explaining and extending the bit-parallel approximate string matching algorithm of Myers"
  Martin Šošić, Mile Šikić(2017) "Edlib: a C/C++ library for fast, exact sequence alignment using edit distance"
  }
{ in terms of Hyyrö }
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
    end;
end;

function LevDistMyersD(pL, pR: PByte; aLenL, aLenR, aLimit: SizeInt): SizeInt;
var
  Pm: array[Byte] of DWord;
  PmI, Hp, Hn, Vp, Vn, D0: DWord;
  I: SizeInt;
begin
  System.FillChar(Pm, SizeOf(Pm), 0);
  for I := 0 to Pred(aLenL) do
    Pm[pL[I]] := Pm[pL[I]] or QWord(1) shl I;

  Result := aLenL;
  aLimit += aLenR - aLenL;
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
        begin
          if Hp and (DWord(1) shl Pred(aLenL)) <> 0 then
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
  PmI, Hp, Hn, Vp, Vn, D0: QWord;
  I: SizeInt;
begin
  System.FillChar(Pm, SizeOf(Pm), 0);
  for I := 0 to Pred(aLenL) do
    Pm[pL[I]] := Pm[pL[I]] or QWord(1) shl I;

  Result := aLenL;
  aLimit += aLenR - aLenL;
  Vn := 0;
  Vp := High(QWord);

  for I := 0 to Pred(aLenR) do
    begin
      PmI := Pm[pR[I]];
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
  returns the size of the new alphabet and recoded sequences in aBuffer }
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
  I, J, BCount, LastRow: SizeInt;
  Pad: QWord;
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

{
  with some imrovements from Martin Šošić, Mile Šikić:
    "Edlib: a C/C 11 library for fast, exact sequence alignment using edit distance"
}
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
begin
  //here aLenL <= aLenR
  if pL = pR then
    exit(aLenR - aLenL);

  SkipSuffix(pL, pR, aLenL, aLenR);
  SkipPrefix(pL, pR, aLenL, aLenR);

  if aLenL = 0 then
    exit(aLenR);

  if (aLenL = 1) and (aLenR = 1) then
    exit(Ord(pL^ <> pR^));

  case aLenL of
    1..BitSizeOf(DWord):
      Result := LevDistMyersD(pL, pR, aLenL, aLenR);
    BitSizeOf(DWord)+1..BitSizeOf(QWord):
      Result := LevDistMyersQ(pL, pR, aLenL, aLenR);
    BitSizeOf(QWord)+1..BitSizeOf(QWord)*2:
      Result := LevDistMyersDQ(pL, pR, aLenL, aLenR);
  else
    Result := LevDistMyers(pL, pR, aLenL, aLenR, aLenR);
  end;
end;

function LevDistanceMyers(const L, R: rawbytestring): SizeInt;
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

  if aLimit = 0 then  //////////
    exit(NULL_INDEX); //////////

  if (aLenL = 1) and (aLenR = 1) then
    exit(Ord(pL^ <> pR^));

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

function LevDistanceMyers(const L, R: rawbytestring; aLimit: SizeInt): SizeInt;
begin
  if aLimit < 0 then exit(LevDistanceMyers(L, R));
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

{$PUSH}{$WARN 5057 OFF}
function LcsDistWmImpl(pL, pR: PByte; aLenL, aLenR: SizeInt): SizeInt;
var
  StBuf: array[0..Pred(MAX_STATIC)] of SizeInt;
  Buf: specialize TGDynArray<SizeInt>;
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
      while (X < aLenL) and (Y < aLenR) and (pL[X] = pR[Y]) do begin
        Inc(X); Inc(Y);
      end;
      Fp[K] := Y;
    end;
    for K := P + Delta downto Delta do begin
      Y := Math.Max(Fp[K - 1] + 1, Fp[K + 1]);
      X := Y - K;
      while (X < aLenL) and (Y < aLenR) and (pL[X] = pR[Y]) do begin
        Inc(X); Inc(Y);
      end;
      Fp[K] := Y;
    end;
    if Fp[Delta] = aLenR then exit(P + P + Delta);
  end;
  Result := aLenL + aLenR;
end;
{$POP}

function GetLcsDistWm(pL, pR: PByte; aLenL, aLenR: SizeInt): SizeInt;
begin
  //here aLenL <= aLenR
  if pL = pR then exit(aLenR - aLenL);

  SkipSuffix(pL, pR, aLenL, aLenR);
  SkipPrefix(pL, pR, aLenL, aLenR);

  if aLenL = 0 then exit(aLenR);

  if (aLenL = 1) and (aLenR = 1) then
    exit(Ord(pL^ <> pR^)*2);

  Result := LcsDistWmImpl(pL, pR, aLenL, aLenR);
end;

function LcsDistanceWM(const L, R: rawbytestring): SizeInt;
begin
  if System.Length(L) = 0 then
    exit(System.Length(R))
  else
    if System.Length(R) = 0 then
      exit(System.Length(L));
  if System.Length(L) <= System.Length(R) then
    Result := GetLcsDistWm(PByte(L), PByte(R), System.Length(L), System.Length(R))
  else
    Result := GetLcsDistWm(PByte(R), PByte(L), System.Length(R), System.Length(L));
end;

function LcsDistanceWM(const L, R: array of Byte): SizeInt;
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

{$PUSH}{$WARN 5057 OFF}
function LcsDistWmLimImpl(pL, pR: PByte; aLenL, aLenR, aLimit: SizeInt): SizeInt;
var
  StBuf: array[0..Pred(MAX_STATIC)] of SizeInt;
  Buf: specialize TGDynArray<SizeInt>;
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
      while (X < aLenL) and (Y < aLenR) and (pL[X] = pR[Y]) do begin
        Inc(X); Inc(Y);
      end;
      Fp[K] := Y;
    end;
    for K := P + Delta downto Delta do begin
      Y := Math.Max(Fp[K - 1] + 1, Fp[K + 1]);
      X := Y - K;
      while (X < aLenL) and (Y < aLenR) and (pL[X] = pR[Y]) do begin
        Inc(X); Inc(Y);
      end;
      Fp[K] := Y;
    end;
    if Fp[Delta] = aLenR then exit(P + P + Delta);
  end;
  Result := NULL_INDEX;
end;
{$POP}

function GetLcsDistWmLim(pL, pR: PByte; aLenL, aLenR, aLimit: SizeInt): SizeInt;
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
    if pL^ = pR^ then
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

function LcsDistanceWM(const L, R: rawbytestring; aLimit: SizeInt): SizeInt;
begin
  if aLimit < 0 then exit(LcsDistanceWM(L, R));
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
    Result := GetLcsDistWmLim(PByte(L), PByte(R), System.Length(L), System.Length(R), aLimit)
  else
    Result := GetLcsDistWmLim(PByte(R), PByte(L), System.Length(R), System.Length(L), aLimit);
end;

function LcsDistanceWM(const L, R: array of Byte; aLimit: SizeInt): SizeInt;
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

function DumDistanceMbrImpl(pL, pR: PByte; aLenL, aLenR, aLimit: SizeInt): SizeInt;

  function FindRow(k, aDist, aLeft, aAbove, aRight: SizeInt): SizeInt; inline;
  var
    I, MaxRow: SizeInt;
  begin
    if aDist = 0 then
      I := 0
    else
      I := MaxOf3(aLeft,aAbove+Ord((pR[aAbove+1]=pL[aAbove+k])and(pR[aAbove]=pL[aAbove+k+1]))+1,aRight+1);
    MaxRow := Math.Min(aLenL - k, aLenR);
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

function GetDumDistanceMbr(pL, pR: PByte; aLenL, aLenR, aLimit: SizeInt): SizeInt;
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

  if aLimit = 0 then  //////////
    exit(NULL_INDEX); //////////

  if (aLenL = 1) and (aLenR = 1) then
    exit(Ord(pL^ <> pR^));

  if aLimit > aLenR then
    aLimit := aLenR;

  Result := DumDistanceMbrImpl(pL, pR, aLenL, aLenR, aLimit);
end;

function DumDistanceMbr(const L, R: rawbytestring): SizeInt;
begin
  if L = '' then
    exit(System.Length(R))
  else
    if R = '' then
      exit(System.Length(L));
  if System.Length(L) <= System.Length(R) then
    Result := GetDumDistanceMbr(Pointer(L), Pointer(R), System.Length(L), System.Length(R), System.Length(R))
  else
    Result := GetDumDistanceMbr(Pointer(R), Pointer(L), System.Length(R), System.Length(L), System.Length(L));
end;

function DumDistanceMbr(const L, R: array of Byte): SizeInt;
begin
  if System.Length(L) = 0 then
    exit(System.Length(R))
  else
    if System.Length(R) = 0 then
      exit(System.Length(L));
  if System.Length(L) <= System.Length(R) then
    Result := GetDumDistanceMbr(@L[0], @R[0], System.Length(L), System.Length(R), System.Length(R))
  else
    Result := GetDumDistanceMbr(@R[0], @L[0], System.Length(R), System.Length(L), System.Length(L));
end;

function DumDistanceMbr(const L, R: rawbytestring; aLimit: SizeInt): SizeInt;
begin
  if aLimit < 0 then exit(DumDistanceMbr(L, R));
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
    Result := GetDumDistanceMbr(Pointer(L), Pointer(R), System.Length(L), System.Length(R), aLimit)
  else
    Result := GetDumDistanceMbr(Pointer(R), Pointer(L), System.Length(R), System.Length(L), aLimit);
end;

function DumDistanceMbr(const L, R: array of Byte; aLimit: SizeInt): SizeInt;
begin
  if aLimit < 0 then exit(DumDistanceMbr(L, R));
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
    Result := GetDumDistanceMbr(@L[0], @R[0], System.Length(L), System.Length(R), aLimit)
  else
    Result := GetDumDistanceMbr(@R[0], @L[0], System.Length(R), System.Length(L), aLimit);
end;

function SimRatio(const L, R: rawbytestring; aLimit: Double; Algo: TSeqDistanceAlgo): Double;
var
  Len, Limit, Dist: SizeInt;
begin
  if (L = '') and (R = '') then exit(Double(1.0));
  if aLimit < 0 then aLimit := Double(0);
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
    else // sdaDumMBR
      Dist := DumDistanceMbr(L, R);
    end;
    exit(Double(Len - Dist)/Double(Len));
  end;
  if aLimit > 0 then
    Limit := Len - {$IFDEF CPU64}Ceil64{$ELSE}Ceil{$ENDIF}(aLimit*Len)
  else
    Limit := -1;
  case Algo of
    sdaDefault:
      if aLimit > Double(0.90) then  //todo: more precise ???
        Dist := LevDistanceMbr(L, R, Limit)
      else
        Dist := LevDistanceMyers(L, R, Limit);
    sdaLevMBR:   Dist := LevDistanceMbr(L, R, Limit);
    sdaLevMyers: Dist := LevDistanceMyers(L, R, Limit);
    sdaLcsWM:    Dist := LcsDistanceWM(L, R, Limit);
  else // sdaDumMBR
    Dist := DumDistanceMbr(L, R, Limit);
  end;
  if Dist <> NULL_INDEX then
    Result := Double(Len - Dist)/Double(Len)
  else
    Result := Double(0);
end;

function SimRatio(const L, R: array of Byte; aLimit: Double; Algo: TSeqDistanceAlgo): Double;
var
  Len, Limit, Dist: SizeInt;
begin
  if (System.Length(L) = 0) and (System.Length(R) = 0) then exit(Double(1.0));
  if aLimit < 0 then aLimit := Double(0);
  if aLimit > 1 then aLimit := Double(1);
  if Algo = sdaLcsWM then
    Len := System.Length(L) + System.Length(R)
  else
    Len := Math.Max(System.Length(L), System.Length(R));
  if aLimit = Double(0) then begin
    case Algo of
      sdaDefault,
      sdaLevMyers: Dist := LevDistanceMyers(L, R);
      sdaLevMBR:   Dist := LevDistanceMbr(L, R);
      sdaLcsWM:    Dist := LcsDistanceWM(L, R);
    else // sdaDumMBR
      Dist := DumDistanceMbr(L, R);
    end;
    exit(Double(Len - Dist)/Double(Len));
  end;
  if aLimit > 0 then
    Limit := Len - {$IFDEF CPU64}Ceil64{$ELSE}Ceil{$ENDIF}(aLimit*Len)
  else
    Limit := -1;
  case Algo of
    sdaDefault:
      if aLimit > Double(0.90) then //todo: more precise ???
        Dist := LevDistanceMbr(L, R, Limit)
      else
        Dist := LevDistanceMyers(L, R, Limit);
    sdaLevMBR:   Dist := LevDistanceMbr(L, R, Limit);
    sdaLevMyers: Dist := LevDistanceMyers(L, R, Limit);
    sdaLcsWM:    Dist := LcsDistanceWM(L, R, Limit);
  else // sdaDumMBR
    Dist := DumDistanceMbr(L, R, Limit);
  end;
  if Dist <> NULL_INDEX then
    Result := Double(Len - Dist)/Double(Len)
  else
    Result := Double(0);
end;

{$PUSH}{$WARN 5089 OFF}
function SimRatioEx(const L, R: rawbytestring; aMode: TSimMode; const aStopChars: TSysCharSet;
  const aOptions: TSimOptions; aLimit: Double; Algo: TSeqDistanceAlgo; aCaseMap: TSimCaseMap;
  aLess: TSimLess): Double;
type
  TWord      = record Start: PChar; Len: SizeInt end;
  PWord      = ^TWord;
  TWordArray = array of TWord;
  TSplitFun  = function(const s: rawbytestring; out aCount: SizeInt; out aBuf: TWordArray;
                        aForceDyn: Boolean): PWord is nested;
  THelper    = specialize TGNestedArrayHelper<TWord>;
var
  StBuf: array[0..Pred(MAX_STATIC)] of TWord;

  function SplitMerge(const s: rawbytestring): rawbytestring;
  var
    I, J: SizeInt;
    pS, pR: PChar;
    NewWord: Boolean;
  begin
    if aStopChars = [] then exit(s);
    System.SetLength(Result, System.Length(s));
    pS := Pointer(s);
    pR := Pointer(Result);
    I := 0;
    while (I < System.Length(s)) and (pS[I] in aStopChars) do Inc(I);
    J := 0;
    NewWord := False;
    for I := I to Pred(System.Length(s)) do
      if pS[I] in aStopChars then
        NewWord := True
      else begin
        if NewWord then begin
          pR[J] := ' ';
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
    c: SizeInt;
  begin
    c := CompareByte(L.Start^, R.Start^, Math.Min(L.Len, R.Len));
    if c = 0 then exit(L.Len < R.Len);
    LessDef := c < 0;
  end;

  function Equal(const L, R: TWord): Boolean;
  begin
    if L.Len <> R.Len then exit(False);
    Result := CompareByte(L.Start^, R.Start^, L.Len) = 0;
  end;

  function SplitAndSort(const s: rawbytestring; out aCount: SizeInt; out aBuf: TWordArray; aForceDyn: Boolean): PWord;
  var
    p: PChar absolute s;
    Words: PWord;
    I, Count, CurrLen: SizeInt;
    CurrStart: PChar;
  begin
    if aForceDyn or (System.Length(s) div 2 + System.Length(s) and 1 > MAX_STATIC) then begin
      System.SetLength(aBuf, System.Length(s) div 2 + System.Length(s) and 1);
      Words := Pointer(aBuf);
    end else
      Words := @StBuf[0];

    CurrStart := p;
    CurrLen := 0;
    Count := 0;
    for I := 0 to Pred(System.Length(s)) do
      if p[I] in aStopChars then begin
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

  function SplitMerge(const s: rawbytestring; aSplit: TSplitFun): rawbytestring;
  var
    Words: PWord;
    Buf: TWordArray = nil;
    I, J, Count, Len: SizeInt;
    pR: PChar;
  begin
    Words := aSplit(s, Count, Buf, False);
    System.SetLength(Result, System.Length(s));
    pR := Pointer(Result);
    Len := 0;
    for I := 0 to Pred(Count) do begin
      if I > 0 then begin
        Len += Words[I].Len + 1;
        pR^ := ' ';
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

  function SplitMergeSorted(const s: rawbytestring): rawbytestring; inline;
  begin
    Result := SplitMerge(s, @SplitAndSort);
  end;

  function SplitSortedSet(const s: rawbytestring; out aCount: SizeInt; out aBuf: TWordArray; aForceDyn: Boolean): PWord;
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

  function SplitMergeSortedSet(const s: rawbytestring): rawbytestring; inline;
  begin
    Result := SplitMerge(s, @SplitSortedSet);
  end;

  function SimPartial(const L, R: rawbytestring): Double;
  var
    I: SizeInt;
  begin
    if L = '' then
      if R = '' then exit(Double(1.0))
      else exit(Double(0.0))
    else
      if R = '' then exit(Double(0.0));

    Result := Double(0.0);
    if System.Length(L) <= System.Length(R) then
      for I := 0 to System.Length(R) - System.Length(L) do begin
        Result := Math.Max(
          Result,
          SimRatio(PByte(L)[0..Pred(System.Length(L))], PByte(R)[I..I+Pred(System.Length(L))], aLimit, Algo));
        if Result = Double(1.0) then break;
      end
    else
      for I := 0 to System.Length(L) - System.Length(R) do begin
        Result := Math.Max(
          Result,
          SimRatio(PByte(R)[0..Pred(System.Length(R))], PByte(L)[I..I+Pred(System.Length(R))], aLimit, Algo));
        if Result = Double(1.0) then break;
      end;
  end;

  function Merge(aSrcLen: SizeInt; aWords: PWord; const aIndices: TBoolVector): rawbytestring;
  var
    I, J, Len: SizeInt;
    pR: PChar;
    NotFirst: Boolean;
  begin
    System.SetLength(Result, aSrcLen);
    pR := Pointer(Result);
    NotFirst := False;
    Len := 0;
    for I in aIndices do begin
      if NotFirst then begin
        Len += aWords[I].Len + 1;
        pR^ := ' ';
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

  function SimWordSetPairwise(const L, R: rawbytestring): Double;
  var
    WordsL, WordsR: PWord;
    BufL, BufR: TWordArray;
    IntersectIdx, DiffIdxL, DiffIdxR: TBoolVector;
    I, J, CountL, CountR: SizeInt;
    Intersection, SetL, SetR: rawbytestring;
  begin
    WordsL := SplitSortedSet(L, CountL, BufL, False);
    WordsR := SplitSortedSet(R, CountR, BufR, True);
    if CountL = 0 then
      if CountR = 0 then
        exit(Double(1))
      else
        exit(Double(0))
    else
      if CountR = 0 then
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
    if (Intersection <> '') and (soPartial in aOptions) then exit(Double(1.0)); ///////
    SetL := Merge(System.Length(L), WordsL, DiffIdxL);
    SetR := Merge(System.Length(R), WordsR, DiffIdxR);

    if Intersection <> '' then begin
      if SetL <> '' then
        SetL := Intersection + ' ' + SetL
      else
        SetL := Intersection;
      if SetR <> '' then
        SetR := Intersection + ' ' + SetR
      else
        SetR := Intersection;
    end;

    if soPartial in aOptions then
      Result := SimPartial(SetL, SetR) /////////
    else begin
      Result := SimRatio(Intersection, SetL, aLimit, Algo);
      if Result = Double(1.0) then exit;
      Result := Math.Max(Result, SimRatio(Intersection, SetR, aLimit, Algo));
      if Result = Double(1.0) then exit;
      Result := Math.Max(Result, SimRatio(SetL, SetR, aLimit, Algo));
    end;
  end;

var
  LocL, LocR: rawbytestring;
begin

  if soIgnoreCase in aOptions then
    if aCaseMap <> nil then begin
      LocL := aCaseMap(L);
      LocR := aCaseMap(R);
    end else begin
      LocL := LowerCase(L);
      LocR := LowerCase(R);
    end
  else begin
    LocL := L;
    LocR := R;
  end;

  case aMode of
    smSimple:
      if soPartial in aOptions then
        Result := SimPartial(SplitMerge(LocL), SplitMerge(LocR))
      else
        Result := SimRatio(SplitMerge(LocL), SplitMerge(LocR), aLimit, Algo);
    smTokenSort:
      if soPartial in aOptions then
        Result := SimPartial(SplitMergeSorted(LocL), SplitMergeSorted(LocR))
      else
        Result := SimRatio(SplitMergeSorted(LocL), SplitMergeSorted(LocR), aLimit, Algo);
    smTokenSet:
      if soPartial in aOptions then
        Result := SimPartial(SplitMergeSortedSet(LocL), SplitMergeSortedSet(LocR))
      else
        Result := SimRatio(SplitMergeSortedSet(LocL), SplitMergeSortedSet(LocR), aLimit, Algo);
  else // smTokenSetEx
    Result := SimWordSetPairwise(LocL, LocR);
  end;
end;

function SimRatioList(const aPattern: rawbytestring; const aValues: array of rawbytestring; aMode: TSimMode;
  const aStopChars: TSysCharSet; const aOptions: TSimOptions; aLimit: Double; Algo: TSeqDistanceAlgo;
  aCaseMap: TSimCaseMap; aLess: TSimLess): specialize TGArray<Double>;
type
  TWord      = record Start: PChar; Len: SizeInt end;
  PWord      = ^TWord;
  TWordArray = array of TWord;
  TSplitFun  = function(const s: rawbytestring; out aCount: SizeInt; out aBuf: TWordArray;
                        aForceDyn: Boolean): PWord is nested;
  THelper    = specialize TGNestedArrayHelper<TWord>;
var
  StBuf: array[0..Pred(MAX_STATIC)] of TWord;

  function SplitMerge(const s: rawbytestring): rawbytestring;
  var
    I, J: SizeInt;
    pS, pR: PChar;
    NewWord: Boolean;
  begin
    if aStopChars = [] then exit(s);
    System.SetLength(Result, System.Length(s));
    pS := Pointer(s);
    pR := Pointer(Result);
    I := 0;
    while (I < System.Length(s)) and (pS[I] in aStopChars) do Inc(I);
    J := 0;
    NewWord := False;
    for I := I to Pred(System.Length(s)) do
      if pS[I] in aStopChars then
        NewWord := True
      else begin
        if NewWord then begin
          pR[J] := ' ';
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
    c: SizeInt;
  begin
    c := CompareByte(L.Start^, R.Start^, Math.Min(L.Len, R.Len));
    if c = 0 then exit(L.Len < R.Len);
    LessDef := c < 0;
  end;

  function Equal(const L, R: TWord): Boolean;
  begin
    if L.Len <> R.Len then exit(False);
    Result := CompareByte(L.Start^, R.Start^, L.Len) = 0;
  end;

  function SplitAndSort(const s: rawbytestring; out aCount: SizeInt; out aBuf: TWordArray; aForceDyn: Boolean): PWord;
  var
    p: PChar absolute s;
    Words: PWord;
    I, Count, CurrLen: SizeInt;
    CurrStart: PChar;
  begin
    if aForceDyn or (System.Length(s) div 2 + System.Length(s) and 1 > MAX_STATIC) then begin
      System.SetLength(aBuf, System.Length(s) div 2 + System.Length(s) and 1);
      Words := Pointer(aBuf);
    end else
      Words := @StBuf[0];

    CurrStart := p;
    CurrLen := 0;
    Count := 0;
    for I := 0 to Pred(System.Length(s)) do
      if p[I] in aStopChars then begin
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

  function SplitMerge(const s: rawbytestring; aSplit: TSplitFun): rawbytestring;
  var
    Words: PWord;
    Buf: TWordArray = nil;
    I, J, Count, Len: SizeInt;
    pR: PChar;
    r: rawbytestring;
  begin
    Words := aSplit(s, Count, Buf, False);
    System.SetLength(r, System.Length(s));
    pR := Pointer(r);
    Len := 0;
    for I := 0 to Pred(Count) do begin
      if I > 0 then begin
        Len += Words[I].Len + 1;
        pR^ := ' ';
        Inc(pR);
      end else
        Len += Words[I].Len;
      for J := 0 to Pred(Words[I].Len) do
        pR[J] := Words[I].Start[J];
      pR += Words[I].Len;
    end;
    System.SetLength(r, Len);
    SplitMerge := r;
  end;

  function SplitMergeSorted(const s: rawbytestring): rawbytestring; inline;
  begin
    Result := SplitMerge(s, @SplitAndSort);
  end;

  function SplitSortedSet(const s: rawbytestring; out aCount: SizeInt; out aBuf: TWordArray; aForceDyn: Boolean): PWord;
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

  function SplitMergeSortedSet(const s: rawbytestring): rawbytestring; inline;
  begin
    Result := SplitMerge(s, @SplitSortedSet);
  end;

  function SimPartial(const L, R: rawbytestring): Double;
  var
    I: SizeInt;
  begin
    if L = '' then
      if R = '' then exit(Double(1.0))
      else exit(Double(0.0))
    else
      if R = '' then exit(Double(0.0));

    Result := Double(0.0);
    if System.Length(L) <= System.Length(R) then
      for I := 0 to System.Length(R) - System.Length(L) do begin
        Result := Math.Max(
          Result,
          SimRatio(PByte(L)[0..Pred(System.Length(L))], PByte(R)[I..I+Pred(System.Length(L))], aLimit, Algo));
        if Result = Double(1.0) then break;
      end
    else
      for I := 0 to System.Length(L) - System.Length(R) do begin
        Result := Math.Max(
          Result,
          SimRatio(PByte(R)[0..Pred(System.Length(R))], PByte(L)[I..I+Pred(System.Length(R))], aLimit, Algo));
        if Result = Double(1.0) then break;
      end;
  end;

  function Merge(aSrcLen: SizeInt; aWords: PWord; const aIndices: TBoolVector): rawbytestring;
  var
    I, J, Len: SizeInt;
    pR: PChar;
    NotFirst: Boolean;
    r: rawbytestring;
  begin
    System.SetLength(r, aSrcLen);
    pR := Pointer(r);
    NotFirst := False;
    Len := 0;
    for I in aIndices do begin
      if NotFirst then begin
        Len += aWords[I].Len + 1;
        pR^ := ' ';
        Inc(pR);
      end else begin
        Len += aWords[I].Len;
        NotFirst := True;
      end;
      for J := 0 to Pred(aWords[I].Len) do
        pR[J] := aWords[I].Start[J];
      pR += aWords[I].Len;
    end;
    System.SetLength(r, Len);
    Merge := r;
  end;

  function ToProperCase(const s: rawbytestring): rawbytestring;
  begin
    if soIgnoreCase in aOptions then
      if aCaseMap <> nil then
        Result := aCaseMap(s)
      else
        Result := LowerCase(s)
    else
      Result := s;
  end;

var
  r: array of Double;

  procedure SimWordSetsPairwise;
  var
    WordsL, WordsR: PWord;
    BufL, BufR: TWordArray;
    IntersectIdx, DiffIdxL, DiffIdxR: TBoolVector;
    I, J, K, CountL, CountR: SizeInt;
    Pattern, Value, Intersection, SetL, SetR: rawbytestring;
  begin
    Pattern := ToProperCase(aPattern);
    WordsL := SplitSortedSet(Pattern, CountL, BufL, False);
    IntersectIdx.EnsureCapacity(CountL);

    for K := 0 to System.High(aValues) do begin
      Value := ToProperCase(aValues[K]);
      WordsR := SplitSortedSet(Value, CountR, BufR, True);

      if CountL = 0 then begin
        if CountR = 0 then
          r[K] := Double(1)
        else
          r[K] := Double(0);
        continue;
      end else
        if CountR = 0 then begin
          r[K] := Double(0);
          continue;
        end;

      IntersectIdx.ClearBits;
      DiffIdxL.ClearBits;
      DiffIdxL.InitRange(CountL);
      DiffIdxR.ClearBits;
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

      Intersection := Merge(System.Length(Pattern), WordsL, IntersectIdx);
      if (Intersection <> '') and (soPartial in aOptions) then begin
        r[K] := Double(1.0);
        continue;
      end;

      SetL := Merge(System.Length(Pattern), WordsL, DiffIdxL);
      SetR := Merge(System.Length(Value), WordsR, DiffIdxR);

      if Intersection <> '' then begin
        if SetL <> '' then
          SetL := Intersection + ' ' + SetL
        else
          SetL := Intersection;
        if SetR <> '' then
          SetR := Intersection + ' ' + SetR
        else
          SetR := Intersection;
      end;

      if soPartial in aOptions then
        r[K] := SimPartial(SetL, SetR)
      else begin
        r[K] := SimRatio(Intersection, SetL, aLimit, Algo);
        if r[K] < Double(1.0) then begin
          r[K] := Math.Max(r[K], SimRatio(Intersection, SetR, aLimit, Algo));
          if r[K] < Double(1.0) then
            r[K] := Math.Max(r[K], SimRatio(SetL, SetR, aLimit, Algo));
        end;
      end;
    end;
  end;

var
  LPattern: rawbytestring;
  I: SizeInt;
begin
  if System.Length(aValues) = 0 then exit(nil);
  System.SetLength(r, System.Length(aValues));

  if aMode in [smSimple..smTokenSet] then begin
    case aMode of
      smSimple:     LPattern := SplitMerge(ToProperCase(aPattern));
      smTokenSort:  LPattern := SplitMergeSorted(ToProperCase(aPattern));
      smTokenSet:   LPattern := SplitMergeSortedSet(ToProperCase(aPattern));
    else
    end;
    for I := 0 to System.High(aValues) do begin
      case aMode of
        smSimple:
          if soPartial in aOptions then
            r[I] := SimPartial(LPattern, SplitMerge(ToProperCase(aValues[I])))
          else
            r[I] := SimRatio(LPattern, SplitMerge(ToProperCase(aValues[I])), aLimit, Algo);
        smTokenSort:
          if soPartial in aOptions then
            r[I] := SimPartial(LPattern, SplitMergeSorted(ToProperCase(aValues[I])))
          else
            r[I] := SimRatio(LPattern, SplitMergeSorted(ToProperCase(aValues[I])), aLimit, Algo);
        smTokenSet:
          if soPartial in aOptions then
            r[I] := SimPartial(LPattern, SplitMergeSortedSet(ToProperCase(aValues[I])))
          else
            r[I] := SimRatio(LPattern, SplitMergeSortedSet(ToProperCase(aValues[I])), aLimit, Algo);
      else
      end;
    end;
  end else
    SimWordSetsPairwise;

  Result := r;
end;
{$POP}

{$PUSH}{$WARN 5036 OFF}
function SelectSimilar(const aPattern: rawbytestring; const aValues: array of rawbytestring; aLimit: Double;
  aMode: TSimMode; const aStopChars: TSysCharSet; const aOptions: TSimOptions; Algo: TSeqDistanceAlgo;
  aCaseMap: TSimCaseMap; aLess: TSimLess): specialize TGArray<TRbStrRatio>;
  function Less(const L, R: TRbStrRatio): Boolean;
  begin
    Result := R.Ratio < L.Ratio;
  end;
var
  ratios: array of Double;
  r: array of TRbStrRatio;
  I, J: SizeInt;
begin
  ratios := SimRatioList(aPattern, aValues, aMode, aStopChars, aOptions, aLimit, Algo, aCaseMap, aLess);
  System.SetLength(r, System.Length(ratios));
  J := 0;
  for I := 0 to System.High(ratios) do
    if ratios[I] >= aLimit then begin
      r[J].Value := aValues[I];
      r[J].Ratio := ratios[I];
      Inc(J);
    end;
  System.SetLength(r, J);
  specialize TGNestedArrayHelper<TRbStrRatio>.Sort(r, @Less);
  Result := r;
end;

function IsValidDotQuadIPv4(const s: rawbytestring): Boolean;
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

function IsValidDotDecIPv4(const s: rawbytestring): Boolean;
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

{ TStrSlice }

constructor TStrSlice.Init(p: PAnsiChar; aCount: SizeInt);
begin
  Ptr := p;
  Count := aCount;
end;

class operator TStrSlice.:=(const s: string): TStrSlice;
begin
  Result := TStrSlice.Init(Pointer(s), System.length(s));
end;

class operator TStrSlice.:=(const s: TStrSlice): string;
begin
  System.SetLength(Result, s.Count);
  System.Move(s.Ptr^, Pointer(Result)^, s.Count);
end;

class operator TStrSlice.=(const L, R: TStrSlice): Boolean;
begin
  if L.Count <> R.Count then
    exit(False);
  Result := CompareByte(L.Ptr^, R.Ptr^, L.Count) = 0;
end;

class operator TStrSlice.=(const L: TStrSlice; const R: string): Boolean;
begin
  if L.Count <> System.Length(R) then
    exit(False);
  Result := CompareByte(L.Ptr^, Pointer(R)^, L.Count) = 0;
end;

{ TAnsiStrHelper.TStrEnumerable }

function TAnsiStrHelper.TStrEnumerable.GetCurrent: string;
begin
  Result := System.Copy(FValue, FStartIndex, FLastIndex - FStartIndex);
end;

constructor TAnsiStrHelper.TStrEnumerable.Create(const aValue: string; const aStopChars: TSysCharSet);
begin
  inherited Create;
  FValue := aValue;
  FStopChars := aStopChars;
  FStartIndex := 1;
  FLastIndex := 0;
end;

{$PUSH}{$MACRO ON}
function TAnsiStrHelper.TStrEnumerable.MoveNext: Boolean;
var
  I, Start: SizeInt;
begin
{$DEFINE MoveBodyMacro :=
  Start := 0;
  for I := Succ(FLastIndex) to System.Length(FValue) do
    begin
      if FValue[I] in FStopChars then
        if Start <> 0 then
           break else
      else
        if Start = 0 then
          Start := I;
      Inc(FLastIndex);
    end;
  if Start <> 0 then
    begin
      Inc(FLastIndex);
      FStartIndex := Start;
      exit(True);
    end;
  Result := False
}
  MoveBodyMacro;
end;

procedure TAnsiStrHelper.TStrEnumerable.Reset;
begin
  FStartIndex := 1;
  FLastIndex := 0;
end;

{ TAnsiStrHelper.TSliceEnumerable }

function TAnsiStrHelper.TSliceEnumerable.GetCurrent: TStrSlice;
begin
  Result.Init(@FValue[FStartIndex], FLastIndex - FStartIndex);
end;

constructor TAnsiStrHelper.TSliceEnumerable.Create(const aValue: string; const aStopChars: TSysCharSet);
begin
  inherited Create;
  FValue := aValue;
  FStopChars := aStopChars;
  FStartIndex := 1;
  FLastIndex := 0;
end;

function TAnsiStrHelper.TSliceEnumerable.MoveNext: Boolean;
var
  I, Start: SizeInt;
begin
  MoveBodyMacro;
end;

procedure TAnsiStrHelper.TSliceEnumerable.Reset;
begin
  FStartIndex := 1;
  FLastIndex := 0;
end;

{ TAnsiStrHelper.TWordSliceEnumerator }

procedure TAnsiStrHelper.TWordSliceEnumerator.Init(const aValue: string; const aStopChars: TSysCharSet);
begin
  FValue := aValue;
  FStopChars := aStopChars;
  FStartIndex := 1;
  FLastIndex := 0;
end;

function TAnsiStrHelper.TWordSliceEnumerator.GetCurrent: TStrSlice;
begin
  Result.Init(@FValue[FStartIndex], FLastIndex - FStartIndex);
end;

function TAnsiStrHelper.TWordSliceEnumerator.MoveNext: Boolean;
var
  I, Start: SizeInt;
begin
  MoveBodyMacro;
end;
{$POP}

{ TAnsiStrHelper.TWordSliceEnum }

procedure TAnsiStrHelper.TWordSliceEnum.Init(const aValue: string; const aStopChars: TSysCharSet);
begin
  FValue := aValue;
  FStopChars := aStopChars;
end;

{$PUSH}{$WARN 5092 OFF}
function TAnsiStrHelper.TWordSliceEnum.GetEnumerator: TWordSliceEnumerator;
begin
  Result.Init(FValue, FStopChars);
end;
{$POP}

function TAnsiStrHelper.TWordSliceEnum.ToArray: specialize TGArray<TStrSlice>;
var
  I: SizeInt;
begin
  I := 0;
  System.SetLength(Result, ARRAY_INITIAL_SIZE);
  with GetEnumerator do
    while MoveNext do
      begin
        if I = System.Length(Result) then
          System.SetLength(Result, I + I);
        Result[I] := Current;
        Inc(I);
      end;
  System.SetLength(Result, I);
end;

{ TAnsiStrHelper }

class function TAnsiStrHelper.Join2(const aSeparator: string; const aValues: array of string): string;
begin
  Result := Join2(aSeparator, aValues, 0, System.Length(aValues));
end;

class function TAnsiStrHelper.Join2(const aSeparator: string; const aValues: array of string;
  aFrom, aCount: SizeInt): string;
var
  I, Len, Last: SizeInt;
  p: PAnsiChar;
begin
  if (System.High(aValues) < 0) or (aFrom > System.High(aValues)) or (aCount <= 0) then
    exit('');
  if aFrom < 0 then
    aFrom := 0;
  Last := Math.Min(Pred(aFrom + aCount), System.High(aValues));
  Len := 0;
  for I := aFrom to Last do
    Len += System.Length(aValues[I]);
  System.SetLength(Result, Len + System.Length(aSeparator) * (Last - aFrom));
  Len := System.Length(aSeparator);
  p := Pointer(Result);
  System.Move(Pointer(aValues[aFrom])^, p^, System.Length(aValues[aFrom]));
  p += System.Length(aValues[aFrom]);
  for I := Succ(aFrom) to Last do
    begin
      System.Move(Pointer(aSeparator)^, p^, Len);
      p += Len;
      System.Move(Pointer(aValues[I])^, p^, System.Length(aValues[I]));
      p += System.Length(aValues[I]);
    end;
end;

class function TAnsiStrHelper.Join(const aSeparator: string; const aValues: array of TStrSlice): string;
begin
  Result := Join(aSeparator, aValues, 0, System.Length(aValues));
end;

class function TAnsiStrHelper.Join(const aSeparator: string; const aValues: array of TStrSlice;
  aFrom, aCount: SizeInt): string;
var
  I, Len, Last: SizeInt;
  p: PAnsiChar;
begin
  if (System.High(aValues) < 0) or (aFrom > System.High(aValues)) or (aCount <= 0) then
    exit('');
  if aFrom < 0 then
    aFrom := 0;
  Last := Math.Min(Pred(aFrom + aCount), System.High(aValues));
  Len := 0;
  for I := aFrom to Last do
    Len += aValues[I].Count;
  System.SetLength(Result, Len + System.Length(aSeparator) * (Last - aFrom));
  Len := System.Length(aSeparator);
  p := Pointer(Result);
  System.Move(aValues[aFrom].Ptr^, p^, aValues[aFrom].Count);
  p += aValues[aFrom].Count;
  for I := Succ(aFrom) to Last do
    begin
      System.Move(Pointer(aSeparator)^, p^, Len);
      p += Len;
      System.Move(aValues[I].Ptr^, p^, aValues[I].Count);
      p += aValues[I].Count;
    end;
end;

class function TAnsiStrHelper.Join(const aSeparator: string; aValues: IStrEnumerable): string;
var
  s: string;
  p: PAnsiChar;
  CharCount: SizeInt;
  procedure EnsureCapacity(aValue: SizeInt); inline;
  begin
    if aValue > System.Length(s) then
      begin
        System.SetLength(s, lgUtils.RoundUpTwoPower(aValue));
        p := Pointer(s);
      end;
  end;
  procedure Append(const s: string); inline;
  begin
    EnsureCapacity(CharCount + System.Length(s));
    System.Move(Pointer(s)^, p[CharCount], System.Length(s));
    CharCount += System.Length(s);
  end;
begin
  CharCount := 0;
  with aValues.GetEnumerator do
    try
      if MoveNext then
        begin
          Append(Current);
          while MoveNext do
            begin
              Append(aSeparator);
              Append(Current);
            end;
        end;
    finally
      Free;
    end;
  System.SetLength(s, CharCount);
  Result := s;
end;

class function TAnsiStrHelper.Join(const aSeparator: string; aValues: ISliceEnumerable): string;
var
  s: string;
  p: PAnsiChar;
  CharCount: SizeInt;
  procedure EnsureCapacity(aValue: SizeInt); inline;
  begin
    if aValue > System.Length(s) then
      begin
        System.SetLength(s, lgUtils.RoundUpTwoPower(aValue));
        p := Pointer(s);
      end;
  end;
  procedure Append(const s: TStrSlice); inline;
  begin
    EnsureCapacity(CharCount + s.Count);
    System.Move(s.Ptr^, p[CharCount], s.Count);
    CharCount += s.Count;
  end;
begin
  CharCount := 0;
  with aValues.GetEnumerator do
    try
      if MoveNext then
        begin
          Append(Current);
          while MoveNext do
            begin
              Append(aSeparator);
              Append(Current);
            end;
        end;
    finally
      Free;
    end;
  System.SetLength(s, CharCount);
  Result := s;
end;

function TAnsiStrHelper.StripWhiteSpaces: string;
begin
  Result := StripChars(WhiteSpaces);
end;

function TAnsiStrHelper.StripChar(aChar: AnsiChar): string;
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

function TAnsiStrHelper.StripChars(const aChars: TSysCharSet): string;
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

function TAnsiStrHelper.ToBytes: TBytes;
begin
  System.SetLength(Result, System.Length(Self));
  System.Move(Pointer(Self)^, Pointer(Result)^, System.Length(Self));
end;

function TAnsiStrHelper.Words(const aStopChars: TSysCharSet): IStrEnumerable;
begin
  Result := TStrEnumerable.Create(Self, aStopChars);
end;

function TAnsiStrHelper.WordSlices(const aStopChars: TSysCharSet): ISliceEnumerable;
begin
  Result := TSliceEnumerable.Create(Self, aStopChars);
end;

{$PUSH}{$WARN 5092 OFF}
function TAnsiStrHelper.WordSliceEnum(const aStopChars: TSysCharSet): TWordSliceEnum;
begin
  Result.Init(Self, aStopChars);
end;
{$POP}

{ TSringListHelper }

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

type
  TBcTableType = array[Byte] of Integer;

procedure FillBcTable(pNeedle: PByte; aLen: Integer; var aTable: TBcTableType);
var
  I: Integer;
begin
  specialize TGArrayHelpUtil<Integer>.Fill(aTable, aLen);
  for I := 0 to aLen - 2 do
    aTable[pNeedle[I]] := Pred(aLen - I);
end;

procedure FillGsTable(pNeedle: PByte; aLen: Integer; out aTable: specialize TGArray<Integer>);
var
  I, J, LastPrefix: Integer;
  IsPrefix: Boolean;
begin
  SetLength(aTable, aLen);
  LastPrefix := Pred(aLen);
  for I := Pred(aLen) downto 0 do
    begin
      IsPrefix := True;
      for J := 0 to aLen - I - 2 do
        if (pNeedle[J] <> pNeedle[J + Succ(I)]) then
          begin
            IsPrefix := False;
            break;
          end;
      if IsPrefix then
        LastPrefix := Succ(I);
      aTable[I] := LastPrefix + aLen - Succ(I);
    end;
  for I := 0 to aLen - 2 do
    begin
      J := 0;
      while (pNeedle[I - J] = pNeedle[Pred(aLen - J)]) and (J < I) do
        Inc(J);
      if pNeedle[I - J] <> pNeedle[Pred(aLen - J)] then
        aTable[Pred(aLen - J)] := Pred(aLen + J - I);
    end;
end;

{ TBmSearch }

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

constructor TBmSearch.Create(const aPattern: rawbytestring);
begin
  FGsShift := nil;
  if aPattern <> '' then
    begin
      FNeedle := aPattern;
      FillBcTable(Pointer(FNeedle), System.Length(FNeedle), FBcShift);
      FillGsTable(Pointer(FNeedle), System.Length(FNeedle), FGsShift);
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
      FillBcTable(Pointer(FNeedle), System.Length(FNeedle), FBcShift);
      FillGsTable(Pointer(FNeedle), System.Length(FNeedle), FGsShift);
    end;
end;

function TBmSearch.Matches(const s: rawbytestring): TStrMatches;
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

function TBmSearch.NextMatch(const s: rawbytestring; aOffset: SizeInt): SizeInt;
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

function TBmSearch.FindMatches(const s: rawbytestring): TIntArray;
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

constructor TBmhrSearch.Create(const aPattern: rawbytestring);
begin
  if aPattern <> '' then
    begin
      FNeedle := aPattern;
      FillBcTable(Pointer(FNeedle), System.Length(FNeedle), FBcShift);
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
      FillBcTable(Pointer(FNeedle), System.Length(FNeedle), FBcShift);
    end;
end;

function TBmhrSearch.Matches(const s: rawbytestring): TStrMatches;
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

function TBmhrSearch.NextMatch(const s: rawbytestring; aOffset: SizeInt): SizeInt;
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

function TBmhrSearch.FindMatches(const s: rawbytestring): TIntArray;
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
    FCaseMap[I] := Ord(LowerCase(Char(I)));
end;

procedure TBmSearchCI.FillMap(aMap: TCaseMapFun);
var
  I: Integer;
begin
  for I := 0 to 255 do
    FCaseMap[I] := Ord(aMap(Char(I)));
end;

procedure TBmSearchCI.FillMap(const aTable: TCaseMapTable);
begin
  FCaseMap := aTable;
end;

function TBmSearchCI.DoFind(aHeap: PByte; const aHeapLen: SizeInt; I: SizeInt): SizeInt;
var
  J, NeedLast: SizeInt;
  p: PByte absolute FNeedle;
begin
  NeedLast := Pred(System.Length(FNeedle));
  while I < aHeapLen do
    begin
      while (I < aHeapLen) and (FCaseMap[aHeap[I]] <> p[NeedLast]) do
        I += FBcShift[FCaseMap[aHeap[I]]];
      if I >= aHeapLen then break;
      J := Pred(NeedLast);
      Dec(I);
      while (J <> NULL_INDEX) and (FCaseMap[aHeap[I]] = p[J]) do
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

constructor TBmSearchCI.Create(const aPattern: rawbytestring);
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
        p[Pred(I)] := FCaseMap[Ord(aPattern[I])];
      FillBcTable(Pointer(FNeedle), System.Length(FNeedle), FBcShift);
      FillGsTable(Pointer(FNeedle), System.Length(FNeedle), FGsShift);
    end;
end;

constructor TBmSearchCI.Create(const aPattern: rawbytestring; aMap: TCaseMapFun);
var
  I: Integer;
  p: PByte;
begin
  FGsShift := nil;
  FNeedle := '';
  FillMap(aMap);
  if aPattern <> '' then
    begin
      System.SetLength(FNeedle, System.Length(aPattern));
      p := PByte(FNeedle);
      for I := 1 to System.Length(aPattern) do
        p[Pred(I)] := FCaseMap[Ord(aPattern[I])];
      FillBcTable(Pointer(FNeedle), System.Length(FNeedle), FBcShift);
      FillGsTable(Pointer(FNeedle), System.Length(FNeedle), FGsShift);
    end;
end;

constructor TBmSearchCI.Create(const aPattern: rawbytestring; const aTable: TCaseMapTable);
var
  I: Integer;
  p: PByte;
begin
  FGsShift := nil;
  FNeedle := '';
  FillMap(aTable);
  if aPattern <> '' then
    begin
      System.SetLength(FNeedle, System.Length(aPattern));
      p := PByte(FNeedle);
      for I := 1 to System.Length(aPattern) do
        p[Pred(I)] := FCaseMap[Ord(aPattern[I])];
      FillBcTable(Pointer(FNeedle), System.Length(FNeedle), FBcShift);
      FillGsTable(Pointer(FNeedle), System.Length(FNeedle), FGsShift);
    end;
end;

procedure TBmSearchCI.Update(const aPattern: rawbytestring);
var
  I: Integer;
  p: PByte;
begin
  FGsShift := nil;
  FNeedle := '';
  if aPattern <> '' then
    begin
      System.SetLength(FNeedle, System.Length(aPattern));
      p := PByte(FNeedle);
      for I := 1 to System.Length(aPattern) do
        p[Pred(I)] := FCaseMap[Ord(aPattern[I])];
      FillBcTable(Pointer(FNeedle), System.Length(FNeedle), FBcShift);
      FillGsTable(Pointer(FNeedle), System.Length(FNeedle), FGsShift);
    end;
end;

function TBmSearchCI.Matches(const s: rawbytestring): TMatches;
begin
  if FNeedle <> '' then
    Result.FHeap := s
  else
    Result.FHeap := '';
  Result.FMatcher := @Self;
end;

function TBmSearchCI.NextMatch(const s: rawbytestring; aOffset: SizeInt): SizeInt;
begin
  if (FNeedle = '') or (s = '') then exit(0);
  if aOffset < 1 then
    aOffset := 1;
  Result := Succ(Find(PByte(s), System.Length(s), Pred(aOffset)));
end;

function TBmSearchCI.FindMatches(const s: rawbytestring): TIntArray;
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

{ TACAbstractFsm }

function TACAbstractFsm.TestOnMatch(const m: TMatch): Boolean;
begin
  Result := FOnMatchHandler(m);
end;

function TACAbstractFsm.TestNestMatch(const m: TMatch): Boolean;
begin
  Result := FNestMatchHandler(m);
end;

procedure TACAbstractFsm.RegisterMatchHandler(h: TOnMatch);
begin
  FOnMatchHandler := h;
  FOnMatch := @TestOnMatch;
end;

procedure TACAbstractFsm.RegisterMatchHandler(h: TNestMatch);
begin
  FNestMatchHandler := h;
  FOnMatch := @TestNestMatch;
end;

function TACAbstractFsm.TestInput(const s: rawbytestring; var aOffset, aCount: SizeInt): Boolean;
begin
  if (s = '') or (PatternCount = 0) or (aOffset > System.Length(s)) then exit(False);
  if aOffset < 1 then aOffset := 1;
  if aCount > 0 then
    aCount := Math.Min(aCount, Succ(System.Length(s) - aOffset))
  else
    aCount := Succ(System.Length(s) - aOffset);
  Result := True;
end;

function TACAbstractFsm.TestInput(const a: array of Byte; var aOffset, aCount: SizeInt): Boolean;
begin
  if (System.Length(a) = 0) or (PatternCount = 0) or (aOffset > System.High(a)) then exit(False);
  if aOffset < 0 then aOffset := 0;
  if aCount > 0 then
    aCount := Math.Min(aCount, System.Length(a) - aOffset)
  else
    aCount := System.Length(a) - aOffset;
  Result := True;
end;

function MatchCompareNO(const L, R: TIndexMatch): Boolean;
begin
  Result := L.Offset < R.Offset;
end;

function MatchCompareLF(const L, R: TIndexMatch): Boolean;
begin
  if L.Offset = R.Offset then
    Result := L.Index < R.Index
  else
    Result := L.Offset < R.Offset;
end;

function MatchCompareLL(const L, R: TIndexMatch): Boolean;
begin
  if L.Offset = R.Offset then
    Result := L.Length > R.Length
  else
    Result := L.Offset < R.Offset;
end;

function MatchCompareLS(const L, R: TIndexMatch): Boolean;
begin
  if L.Offset = R.Offset then
    Result := L.Length < R.Length
  else
    Result := L.Offset < R.Offset;
end;

class procedure TACAbstractFsm.DoFilterMatches(var aMatches: TMatchArray; aMode: TSetMatchMode);
var
  Count, I, Len, Ofs: SizeInt;
begin
  if aMatches = nil then exit;
  case aMode of
    smmDefault: exit;
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

class function TACAbstractFsm.FilterMatches(const aSource: array of TMatch; aMode: TSetMatchMode): TMatchArray;
begin
  Result := specialize TGArrayHelpUtil<TMatch>.CreateCopy(aSource);
  DoFilterMatches(Result, aMode);
end;

function TACAbstractFsm.IndexOfPattern(const aText: rawbytestring; aOffset: SizeInt; aCount: SizeInt): SizeInt;
begin
  if not TestInput(aText, aOffset, aCount) then exit(NULL_INDEX);
  Result := GetPatternIndex(@aText[aOffset], aCount);
end;

function TACAbstractFsm.IndexOfPattern(const aText: array of Byte; aOffset: SizeInt; aCount: SizeInt): SizeInt;
begin
  if not TestInput(aText, aOffset, aCount) then exit(NULL_INDEX);
  Result := GetPatternIndex(@aText[aOffset], aCount);
end;

function TACAbstractFsm.IsMatch(const aText: rawbytestring; aOffset: SizeInt; aCount: SizeInt): Boolean;
begin
  Result := IndexOfPattern(aText, aOffset, aCount) <> NULL_INDEX;
end;

function TACAbstractFsm.IsMatch(const aText: array of Byte; aOffset: SizeInt; aCount: SizeInt): Boolean;
begin
  Result := IndexOfPattern(aText, aOffset, aCount) <> NULL_INDEX;
end;

//{$PUSH}{$WARN 5036 OFF}
function TACAbstractFsm.FirstMatch(const aText: rawbytestring; aMode: TSetMatchMode; aOffset: SizeInt;
  aCount: SizeInt): TMatch;
begin
  if not TestInput(aText, aOffset, aCount) then exit(TMatch.Make(0, 0, NULL_INDEX));
  Result := GetFirstMatch(@aText[aOffset], aMode, aOffset, aCount);
end;

function TACAbstractFsm.FirstMatch(const aText: array of Byte; aMode: TSetMatchMode; aOffset: SizeInt;
  aCount: SizeInt): TMatch;
begin
  if not TestInput(aText, aOffset, aCount) then exit(TMatch.Make(0, 0, NULL_INDEX));
  Result := GetFirstMatch(@aText[aOffset], aMode, aOffset, aCount);
end;
//{$POP}

function TACAbstractFsm.FindMatches(const aText: rawbytestring; aMode: TSetMatchMode; aOffset: SizeInt;
  aCount: SizeInt): TMatchArray;
begin
  if not TestInput(aText, aOffset, aCount) then exit(nil);
  if aMode = smmNonOverlapping then exit(DoFindNoOverlap(@aText[aOffset], aOffset, aCount));
  Result := DoFind(@aText[aOffset], aOffset, aCount);
  if aMode <> smmDefault then
    DoFilterMatches(Result, aMode);
end;

function TACAbstractFsm.FindMatches(const aText: array of Byte; aMode: TSetMatchMode; aOffset: SizeInt;
  aCount: SizeInt): TMatchArray;
begin
  if not TestInput(aText, aOffset, aCount) then exit(nil);
  if aMode = smmNonOverlapping then exit(DoFindNoOverlap(@aText[aOffset], aOffset, aCount));
  Result := DoFind(@aText[aOffset], aOffset, aCount);
  if aMode <> smmDefault then
    DoFilterMatches(Result, aMode);
end;

procedure TACAbstractFsm.Search(const aText: rawbytestring; aOnMatch: TOnMatch; aOffset: SizeInt; aCount: SizeInt);
begin
  if not TestInput(aText, aOffset, aCount) then exit;
  RegisterMatchHandler(aOnMatch);
  DoSearch(@aText[aOffset], aOffset, aCount);
end;

procedure TACAbstractFsm.Search(const aText: rawbytestring; aOnMatch: TNestMatch; aOffset: SizeInt; aCount: SizeInt);
begin
  if not TestInput(aText, aOffset, aCount) then exit;
  RegisterMatchHandler(aOnMatch);
  DoSearch(@aText[aOffset], aOffset, aCount);
end;

procedure TACAbstractFsm.Search(const aText: array of Byte; aOnMatch: TOnMatch; aOffset: SizeInt; aCount: SizeInt);
begin
  if not TestInput(aText, aOffset, aCount) then exit;
  RegisterMatchHandler(aOnMatch);
  DoSearch(@aText[aOffset], aOffset, aCount);
end;

procedure TACAbstractFsm.Search(const aText: array of Byte; aOnMatch: TNestMatch; aOffset: SizeInt; aCount: SizeInt);
begin
  if not TestInput(aText, aOffset, aCount) then exit;
  RegisterMatchHandler(aOnMatch);
  DoSearch(@aText[aOffset], aOffset, aCount);
end;

function TACAbstractFsm.ContainsMatch(const aText: rawbytestring; aOffset: SizeInt; aCount: SizeInt): Boolean;
begin
  if not TestInput(aText, aOffset, aCount) then exit(False);
  Result := HasMatch(@aText[aOffset], aCount);
end;

function TACAbstractFsm.ContainsMatch(const aText: array of Byte; aOffset: SizeInt; aCount: SizeInt): Boolean;
begin
  if not TestInput(aText, aOffset, aCount) then exit(False);
  Result := HasMatch(@aText[aOffset], aCount);
end;

{ TACSearchDfa }

function TACSearchDfa.NewNode: SizeInt;
begin
{$IFDEF CPU64}
  if StateCount = MaxInt then
    raise ELGMaxItemsExceed.CreateFmt(SEMaxNodeCountExceedFmt, [MaxInt]);
{$ENDIF CPU64}
  if FNodeCount = System.Length(FTrie) then
    System.SetLength(FTrie, StateCount * 2);
  Result := StateCount;
  Inc(FNodeCount);
  System.SetLength(FTrie[Result].NextMove, AlphabetSize);
end;

procedure TACSearchDfa.BuildCodeMap(const aList: array of rawbytestring);
var
  I: SizeInt;
  s: string;
begin
  System.FillChar(FCodeMap, SizeOf(FCodeMap), $ff);
  for s in aList do
    for I := 1 to System.Length(s) do
      if FCodeMap[Ord(s[I])] = -1 then
        begin
          FCodeMap[Ord(s[I])] := AlphabetSize;
          Inc(FAlphabetSize);
        end;
end;

procedure TACSearchDfa.BuildCodeMap(const aList: array of TBytes);
var
  I: SizeInt;
  b: TBytes;
begin
  System.FillChar(FCodeMap, SizeOf(FCodeMap), $ff);
  for b in aList do
    for I := 0 to System.High(b) do
      if FCodeMap[b[I]] = -1 then
        begin
          FCodeMap[b[I]] := AlphabetSize;
          Inc(FAlphabetSize);
        end;
end;

procedure TACSearchDfa.DoAddPattern(p: PByte; aCount, aIndex: Integer);
var
  I: SizeInt;
  Curr, Next, Code: Int32;
begin
  Curr := 0;
  for I := 0 to Pred(aCount) do
    begin
      Code := FCodeMap[p[I]];
      Next := FTrie[Curr].NextMove[Code];
      // if no transition is found for current character, just add a new one
      if Next = 0 then
        begin
          Next := NewNode;
          FTrie[Curr].NextMove[Code] := Next;
        end
      else;
      Curr := Next;
    end;
  with FTrie[Curr] do
    if Length = 0 then
      begin
        Inc(FWordCount);
        Index := aIndex;
        Length := aCount;
      end;
end;

procedure TACSearchDfa.AddPattern(const s: rawbytestring; aIndex: SizeInt);
begin
  if s = '' then exit;
  DoAddPattern(PByte(s), System.Length(s), aIndex);
end;

procedure TACSearchDfa.AddPattern(const a: array of Byte; aIndex: SizeInt);
begin
  if System.Length(a) = 0 then exit;
  DoAddPattern(@a[0], System.Length(a), aIndex);
end;

procedure TACSearchDfa.BuildFsm;
var
  Queue: specialize TGLiteQueue<Int32>;
  Failure: array of Int32;
  Curr, Next, Fail, Link, Code: Int32;
begin // simple BFS
  System.SetLength(Failure, StateCount);
  for Curr in FTrie[0].NextMove do
    if Curr <> 0 then
      Queue.Enqueue(Curr);
  while Queue.TryDequeue(Curr) do
    for Code := 0 to Pred(AlphabetSize) do
      if FTrie[Curr].NextMove[Code] <> 0 then
        begin
          Next := FTrie[Curr].NextMove[Code];
          Queue.Enqueue(Next);
          Fail := Curr;
          repeat
            Fail := Failure[Fail];
            Link := FTrie[Fail].NextMove[Code];
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
        FTrie[Curr].NextMove[Code] := FTrie[Failure[Curr]].NextMove[Code];
end;

function TACSearchDfa.NextFsmState(aState, aCode: Int32): Int32;
begin
  if aCode = -1 then exit(0);
  Result := FTrie[aState].NextMove[aCode];
end;

function TACSearchDfa.GetPatternIndex(p: PByte; aCount: SizeInt): SizeInt;
var
  I, State: Int32;
begin
  Result := NULL_INDEX;
  State := 0;
  for I := 0 to Pred(aCount) do
    begin
      State := NextFsmState(State, FCodeMap[p[I]]);
      if State = 0 then exit;
    end;
  with FTrie[State] do
    if Length = aCount then
      Result := Index;
end;

{$PUSH}{$WARN 5036 OFF : Local variable "$1" does not seem to be initialized }
function TACSearchDfa.GetFirstMatch(p: PByte; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch;
var
  Match: TMatch;
  MatchLess: specialize TGLessCompare<TMatch>;
  procedure TestMatch(const m: TMatch); inline;
  begin
    if (Match.Offset = 0) or MatchLess(m, Match) then
      Match := m;
  end;
var
  I: SizeInt;
  State, NextState: Int32;
begin
  Match := TMatch.Make(0, 0, NULL_INDEX);
  State := 0;
  if aMode < smmLeftmostFirst then begin
    for I := 0 to Pred(aCount) do begin
      State := NextFsmState(State, FCodeMap[p[I]]);
      if State = 0 then continue;
      with FTrie[State] do
        if Length <> 0 then
          exit(TMatch.Make(Succ(aOffset + I - Length), Length, Index));
      if FTrie[State].Output <> 0 then
        with FTrie[FTrie[State].Output] do
          exit(TMatch.Make(Succ(aOffset + I - Length), Length, Index));
    end;
    exit(Match);
  end;
  case aMode of
    smmLeftmostFirst:    MatchLess := @MatchCompareLF;
    smmLeftmostLongest:  MatchLess := @MatchCompareLL;
  else // smmLeftmostShortest
    MatchLess := @MatchCompareLS;
  end;
  for I := 0 to Pred(aCount) do begin
    NextState := NextFsmState(State, FCodeMap[p[I]]);
    if NextState = 0 then
      if State = 0 then
        continue
      else
        if Match.Offset <> 0 then break;
    State := NextState;
    with FTrie[NextState] do
      if Length <> 0 then
        TestMatch(TMatch.Make(Succ(aOffset + I - Length), Length, Index));
    while FTrie[NextState].Output <> 0 do begin
      NextState := FTrie[NextState].Output;
      with FTrie[NextState] do
        TestMatch(TMatch.Make(Succ(aOffset + I - Length), Length, Index));
    end;
  end;
  Result := Match;
end;
{$POP}

function TACSearchDfa.DoFindNoOverlap(p: PByte; aOffset, aCount: SizeInt): TMatchArray;
var
  Matches: array of TMatch;
  MatchCount: SizeInt;
  procedure AddMatch(const m: TMatch);
  begin
    if MatchCount = System.Length(Matches) then
      System.SetLength(Matches, MatchCount * 2);
    Matches[MatchCount] := m;
    Inc(MatchCount);
  end;
var
  I: SizeInt;
  State: Int32;
begin
  System.SetLength(Matches, ARRAY_INITIAL_SIZE);
  MatchCount := 0;
  State := 0;
  for I := 0 to Pred(aCount) do
    begin
      State := NextFsmState(State, FCodeMap[p[I]]);
      if State = 0 then continue;
      with FTrie[State] do
        if Length <> 0 then
          begin
            AddMatch(TMatch.Make(Succ(aOffset + I - Length), Length, Index));
            State := 0;
            continue;
          end;
      if FTrie[State].Output <> 0 then
        with FTrie[FTrie[State].Output] do
          begin
            AddMatch(TMatch.Make(Succ(aOffset + I - Length), Length, Index));
            State := 0;
          end;
    end;
  System.SetLength(Matches, MatchCount);
  Result := Matches;
end;

function TACSearchDfa.DoFind(p: PByte; aOffset, aCount: SizeInt): TMatchArray;
var
  Matches: array of TMatch;
  MatchCount: SizeInt;
  procedure AddMatch(const m: TMatch);
  begin
    if MatchCount = System.Length(Matches) then
      System.SetLength(Matches, MatchCount * 2);
    Matches[MatchCount] := m;
    Inc(MatchCount);
  end;
var
  I: SizeInt;
  State, Tmp: Int32;
begin
  System.SetLength(Matches, ARRAY_INITIAL_SIZE);
  MatchCount := 0;
  State := 0;
  for I := 0 to Pred(aCount) do
    begin
      State := NextFsmState(State, FCodeMap[p[I]]);
      if State = 0 then continue;
      with FTrie[State] do
        if Length <> 0 then
          AddMatch(TMatch.Make(Succ(aOffset + I - Length), Length, Index));
      Tmp := State;
      while FTrie[Tmp].Output <> 0 do
        begin
          Tmp := FTrie[Tmp].Output;
          with FTrie[Tmp] do
            AddMatch(TMatch.Make(Succ(aOffset + I - Length), Length, Index));
        end;
    end;
  System.SetLength(Matches, MatchCount);
  Result := Matches;
end;

procedure TACSearchDfa.DoSearch(p: PByte; aOffset, aCount: SizeInt);
var
  I: SizeInt;
  State, Tmp: Int32;
begin
  State := 0;
  for I := 0 to Pred(aCount) do
    begin
      State := NextFsmState(State, FCodeMap[p[I]]);
      if State = 0 then continue;
      with FTrie[State] do
        if Length <> 0 then
          if not FOnMatch(TMatch.Make(Succ(aOffset + I - Length), Length, Index)) then exit;
      Tmp := State;
      while FTrie[Tmp].Output <> 0 do
        begin
          Tmp := FTrie[Tmp].Output;
          with FTrie[Tmp] do
            if not FOnMatch(TMatch.Make(Succ(aOffset + I - Length), Length, Index)) then exit;
        end;
    end;
end;

function TACSearchDfa.HasMatch(p: PByte; aCount: SizeInt): Boolean;
var
  I: SizeInt;
  State: Int32;
begin
  State := 0;
  for I := 0 to Pred(aCount) do
    begin
      State := NextFsmState(State, FCodeMap[p[I]]);
      if State = 0 then continue;
      if FTrie[State].Length <> 0 then exit(True);
      if FTrie[State].Output <> 0 then exit(True);
    end;
  Result := False;
end;

constructor TACSearchDfa.Create;
begin
  inherited;
end;

constructor TACSearchDfa.Create(const aPatternList: array of rawbytestring);
var
  I: SizeInt;
begin
  //todo: any failure conditions depending on the size or (???) of the input?
  BuildCodeMap(aPatternList);
  System.SetLength(FTrie, ARRAY_INITIAL_SIZE);
  NewNode;
  for I := 0 to System.High(aPatternList) do
    AddPattern(aPatternList[I], I);
  System.SetLength(FTrie, FNodeCount);
  BuildFsm;
end;

constructor TACSearchDfa.CreateBytes(const aPatternList: array of TBytes);
var
  I: SizeInt;
begin
  BuildCodeMap(aPatternList);
  System.SetLength(FTrie, ARRAY_INITIAL_SIZE);
  NewNode;
  for I := 0 to System.High(aPatternList) do
    AddPattern(aPatternList[I], I);
  System.SetLength(FTrie, FNodeCount);
  BuildFsm;
end;

function TACSearchDfa.Clone: TACSearchDfa;
begin
  Result := TACSearchDfa.Create;
  Result.FTrie := FTrie;
  Result.FCodeMap := FCodeMap;
  Result.FNodeCount := StateCount;
  Result.FWordCount := PatternCount;
  Result.FAlphabetSize := AlphabetSize;
end;

{ TDaACSearch.TCode2StateMap.TEnumerator }

function TDaACSearch.TCode2StateMap.TEnumerator.GetCurrent: TEntry;
begin
  Result := FCurrent^;
end;

function TDaACSearch.TCode2StateMap.TEnumerator.MoveNext: Boolean;
begin
  Result := FCurrent < FLast;
  FCurrent += Ord(Result);
end;

{ TDaACSearch.TCode2StateMap }

function TDaACSearch.TCode2StateMap.DoFind(aKey: Int32; out aIndex: Int32): PEntry;
var
  L, R, M: Int32;
begin
  if Count = 0 then
    begin
      aIndex := 0;
      exit(nil);
    end;
  L := 0;
  R := Pred(Count);
  if aKey < FItems[L].Key then
    begin
      aIndex := 0;
      exit(nil);
    end
  else
    if aKey > FItems[R].Key then
      begin
        aIndex := Count;
        exit(nil);
      end;
  while L < R do begin
    M := (L + R) shr 1;
    if FItems[M].Key < aKey then
      L := Succ(M)
    else
      R := M;
  end;
  if FItems[R].Key = aKey then
    Result := @FItems[R]
  else
    begin
      aIndex := R;
      Result := nil;
    end;
end;

function TDaACSearch.TCode2StateMap.DoAdd(aKey, aIndex: Int32): PEntry;
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

function TDaACSearch.TCode2StateMap.GetKey(aIndex: Int32): Int32;
begin
  Result := FItems[aIndex].Key;
end;

function TDaACSearch.TCode2StateMap.GetValue(aIndex: Int32): Int32;
begin
  Result := FItems[aIndex].Value;
end;

function TDaACSearch.TCode2StateMap.GetEnumerator: TEnumerator;
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

function TDaACSearch.TCode2StateMap.GetMutValueDef(aKey, aDefault: Int32): PInt32;
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

{ TDaACSearch.TPair }

constructor TDaACSearch.TPair.Make(aNode, aDaNode: Int32);
begin
  Node := aNode;
  DaNode := aDaNode;
end;

{ TDaACSearch }

function TDaACSearch.GetEmptyCount: Int32;
begin
  Result := System.Length(FDaTrie) - FNodeCount;
end;

function TDaACSearch.NewOutput(aIndex, aLen: Int32): Int32;
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
      Length := aLen;
    end;
  Result := OutCount;
  Inc(FOutCount);
end;

function TDaACSearch.NewNode: Int32;
begin
{$IFDEF CPU64}
  if StateCount = MaxInt then
    raise ELGMaxItemsExceed.CreateFmt(SEMaxNodeCountExceedFmt, [MaxInt]);
{$ENDIF CPU64}
  if StateCount = System.Length(FTrie) then
    System.SetLength(FTrie, StateCount * 2);
  Result := StateCount;
  Inc(FNodeCount);
end;

{$PUSH}{$WARN 5036 OFF}
procedure TDaACSearch.BuildCodeMap(const aList: array of string);
var
  Counter: array[Byte] of TCountRec;
  I, CodeCount: Int32;
  s: string;
begin
  for I := 0 to System.High(Counter) do
    with Counter[I] do
      begin
        Count := 0;
        Key := I;
      end;
  CodeCount := 0;
  for s in aList do
    for I := 1 to System.Length(s) do
      with Counter[Ord(s[I])] do
        begin
          if Count = 0 then
            Inc(CodeCount);
          Inc(Count);
        end;
  specialize TGRegularArrayHelper<TCountRec>.MergeSort(Counter, @CountRecCmp);
  FAlphabetSize := CodeCount;
  System.SetLength(FCodeMap, Int32(High(Byte))+1);
  for I := 0 to Pred(CodeCount)  do
    with Counter[I] do
      FCodeMap[Key] := Succ(I);
end;

procedure TDaACSearch.BuildCodeMap(const aList: array of TBytes);
var
  Counter: array[Byte] of TCountRec;
  I, CodeCount: Int32;
  a: TBytes;
begin
  for I := 0 to System.High(Counter) do
    with Counter[I] do
      begin
        Count := 0;
        Key := I;
      end;
  CodeCount := 0;
  for a in aList do
    for I := 0 to System.High(a) do
      with Counter[a[I]] do
        begin
          if Count = 0 then
            Inc(CodeCount);
          Inc(Count);
        end;
  specialize TGRegularArrayHelper<TCountRec>.MergeSort(Counter, @CountRecCmp);
  FAlphabetSize := CodeCount;
  System.SetLength(FCodeMap, Int32(High(Byte))+1);
  for I := 0 to Pred(CodeCount)  do
    with Counter[I] do
      FCodeMap[Key] := Succ(I);
end;
{$POP}

procedure TDaACSearch.DoAddPattern(p: PByte; aCount, aIndex: Integer);
var
  pNext: PInt32;
  I, Curr: Int32;
begin
  Curr := 0;
  for I := 0 to Pred(aCount) do
    begin
      pNext := FTrie[Curr].AdjList.GetMutValueDef(FCodeMap[p[I]], 0);
      if pNext^ = 0 then
        pNext^ := NewNode;
      Curr := pNext^;
    end;
  if FTrie[Curr].Output = 0 then
    begin
      Inc(FWordCount);
      FTrie[Curr].Output := NewOutput(aIndex, aCount);
    end;
end;

procedure TDaACSearch.AddPattern(const s: rawbytestring; aIndex: Int32);
begin
  if s = '' then exit;
  DoAddPattern(PByte(s), System.Length(s), aIndex);
end;

procedure TDaACSearch.AddPattern(const a: array of Byte; aIndex: Int32);
begin
  if System.Length(a) = 0 then exit;
  DoAddPattern(@a[0], System.Length(a), aIndex);
end;

function TDaACSearch.NextMove(aState, aCode: Int32): Int32;
begin
  Result := FDaTrie[aState].Base + aCode;
  if(UInt32(Result) > UInt32(System.Length(FDaTrie)))or(FDaTrie[Result].Check <> aState)then
    Result := 0;
end;

function TDaACSearch.NextFsmState(aState, aCode: Int32): Int32;
begin
  if aCode = 0 then exit(0);
  Result := FDaTrie[aState].Base + aCode;
  if(UInt32(Result) > UInt32(System.Length(FDaTrie)))or(FDaTrie[Result].Check <> aState)then
    repeat
      aState := FDaTrie[aState].Failure;
      Result := NextMove(aState, aCode);
    until (Result <> 0) or (aState = 0);
end;

{$PUSH}{$WARN 5036 OFF}
procedure TDaACSearch.BuildFsm;
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
    if VListHead = NULL_NODE then
      VListHead := OldSize;
    if VListTail <> NULL_NODE then
      FDaTrie[VListTail].Base := -OldSize;
    FDaTrie[OldSize].Check := -VListTail;
    VListTail := System.High(FDaTrie);
  end;
  function NextBaseValue(aTrieNode: Int32): Int32;
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
      else
        if (AlphabetSize >= BIG_ALPHABET_SIZE) and (Count = 4) then begin
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
        end else begin
          if AllVacantBound + Keys[Count - 1] - Keys[0] > System.High(FDaTrie) then
            Expand(System.Length(FDaTrie) * 2);
          Result := AllVacantBound - Keys[0];
        end;
      end;
  end;
var
  Queue: specialize TGLiteQueue<TPair>;
  ParentPair: TPair;
  Next, Fail, Link: Int32;
  e: TCode2StateMap.TEntry;
begin
  System.SetLength(FDaTrie, LgUtils.RoundUpTwoPower(StateCount + 256));
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
      FDaTrie[ParentPair.DaNode].Base := NextBaseValue(ParentPair.Node);
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
{$POP}

function TDaACSearch.GetPatternIndex(p: PByte; aCount: SizeInt): SizeInt;
var
  I, State, c: Int32;
begin
  Result := NULL_INDEX;
  State := 0;
  for I := 0 to Pred(aCount) do
    begin
      c := FCodeMap[p[I]];
      if c = 0 then exit;
      State := NextMove(State, c);
      if State = 0 then exit;
    end;
  with FDaTrie[State] do
    if (Output <> 0) and (FOutput[Output].Length = aCount) then
      Result := FOutput[Output].Index;
end;

{$PUSH}{$WARN 5036 OFF}
function TDaACSearch.GetFirstMatch(p: PByte; aMode: TSetMatchMode; aOffset, aCount: SizeInt): TMatch;
var
  Match: TMatch;
  MatchLess: specialize TGLessCompare<TMatch>;
  procedure TestMatch(const m: TMatch); inline;
  begin
    if (Match.Offset = 0) or MatchLess(m, Match) then
      Match := m;
  end;
var
  I: SizeInt;
  State, NextState: Int32;
begin
  Match := TMatch.Make(0, 0, NULL_INDEX);
  State := 0;
  if aMode < smmLeftmostFirst then begin
    for I := 0 to Pred(aCount) do begin
      State := NextFsmState(State, FCodeMap[p[I]]);
      if State = 0 then continue;
      if FDaTrie[State].Output <> 0 then
        with FOutput[FDaTrie[State].Output] do
          exit(TMatch.Make(Succ(aOffset + I - Length), Length, Index));
      if FDaTrie[State].NextOut <> 0 then
        with FOutput[FDaTrie[FDaTrie[State].NextOut].Output] do
          exit(TMatch.Make(Succ(aOffset + I - Length), Length, Index));
    end;
    exit(Match);
  end;
  case aMode of
    smmLeftmostFirst:    MatchLess := @MatchCompareLF;
    smmLeftmostLongest:  MatchLess := @MatchCompareLL;
  else // smmLeftmostShortest
    MatchLess := @MatchCompareLS;
  end;
  for I := 0 to Pred(aCount) do begin
    NextState := NextFsmState(State, FCodeMap[p[I]]);
    if NextState = 0 then
      if State = 0 then
        continue
      else
        if Match.Offset <> 0 then break;
    State := NextState;
    if FDaTrie[NextState].Output <> 0 then
      with FOutput[FDaTrie[NextState].Output] do
        TestMatch(TMatch.Make(Succ(aOffset + I - Length), Length, Index));
    while FDaTrie[NextState].NextOut <> 0 do
      begin
        NextState := FDaTrie[NextState].NextOut;
        with FOutput[FDaTrie[NextState].Output] do
          TestMatch(TMatch.Make(Succ(aOffset + I - Length), Length, Index));
      end;
  end;
  Result := Match;
end;
{$POP}

function TDaACSearch.DoFindNoOverlap(p: PByte; aOffset, aCount: SizeInt): TMatchArray;
var
  Matches: array of TMatch;
  MatchCount: SizeInt;
  procedure AddMatch(const m: TMatch);
  begin
    if MatchCount = System.Length(Matches) then
      System.SetLength(Matches, MatchCount * 2);
    Matches[MatchCount] := m;
    Inc(MatchCount);
  end;
var
  I: SizeInt;
  State: Int32;
begin
  System.SetLength(Matches, ARRAY_INITIAL_SIZE);
  MatchCount := 0;
  State := 0;
  for I := 0 to Pred(aCount) do
    begin
      State := NextFsmState(State, FCodeMap[p[I]]);
      if State = 0 then continue;
      if FDaTrie[State].Output <> 0 then
        with FOutput[FDaTrie[State].Output] do
          begin
            AddMatch(TMatch.Make(Succ(aOffset + I - Length), Length, Index));
            State := 0;
            continue;
          end;
      if FDaTrie[State].NextOut <> 0 then
        begin
          with FOutput[FDaTrie[FDaTrie[State].NextOut].Output] do
            AddMatch(TMatch.Make(Succ(aOffset + I - Length), Length, Index));
          State := 0;
        end;
    end;
  System.SetLength(Matches, MatchCount);
  Result := Matches;
end;

function TDaACSearch.DoFind(p: PByte; aOffset, aCount: SizeInt): TMatchArray;
var
  Matches: array of TMatch;
  MatchCount: SizeInt;
  procedure AddMatch(const m: TMatch);
  begin
    if MatchCount = System.Length(Matches) then
      System.SetLength(Matches, MatchCount * 2);
    Matches[MatchCount] := m;
    Inc(MatchCount);
  end;
var
  I: SizeInt;
  State, Tmp: Int32;
begin
  System.SetLength(Matches, ARRAY_INITIAL_SIZE);
  MatchCount := 0;
  State := 0;
  for I := 0 to Pred(aCount) do
    begin
      State := NextFsmState(State, FCodeMap[p[I]]);
      if State = 0 then continue;
      if FDaTrie[State].Output <> 0 then
        with FOutput[FDaTrie[State].Output] do
          AddMatch(TMatch.Make(Succ(aOffset + I - Length), Length, Index));
      Tmp := State;
      while FDaTrie[Tmp].NextOut <> 0 do
        begin
          Tmp := FDaTrie[Tmp].NextOut;
          with FOutput[FDaTrie[Tmp].Output] do
            AddMatch(TMatch.Make(Succ(aOffset + I - Length), Length, Index));
        end;
    end;
  System.SetLength(Matches, MatchCount);
  Result := Matches;
end;

procedure TDaACSearch.DoSearch(p: PByte; aOffset, aCount: SizeInt);
var
  I: SizeInt;
  State, Tmp: Int32;
begin
  State := 0;
  for I := 0 to Pred(aCount) do
    begin
      State := NextFsmState(State, FCodeMap[p[I]]);
      if State = 0 then continue;
      if FDaTrie[State].Output <> 0 then
        with FOutput[FDaTrie[State].Output] do
          if not FOnMatch(TMatch.Make(Succ(aOffset + I - Length), Length, Index)) then exit;
      Tmp := State;
      while FDaTrie[Tmp].NextOut <> 0 do
        begin
          Tmp := FDaTrie[Tmp].NextOut;
          with FOutput[FDaTrie[Tmp].Output] do
            if not FOnMatch(TMatch.Make(Succ(aOffset + I - Length), Length, Index)) then exit;
        end;
    end;
end;

function TDaACSearch.HasMatch(p: PByte; aCount: SizeInt): Boolean;
var
  I: SizeInt;
  State: Int32;
begin
  State := 0;
  for I := 0 to Pred(aCount) do
    begin
      State := NextFsmState(State, FCodeMap[p[I]]);
      if State = 0 then continue;
      if FDaTrie[State].Output <> 0 then exit(True);
      if FDaTrie[State].NextOut <> 0 then exit(True);
    end;
  Result := False;
end;

class function TDaACSearch.CountRecCmp(const L, R: TCountRec): Boolean;
begin
  Result := L.Count > R.Count;
end;

constructor TDaACSearch.Create;
begin
  inherited;
end;

constructor TDaACSearch.Create(const aPatternList: array of rawbytestring);
var
  I: SizeInt;
begin
  System.SetLength(FTrie, ARRAY_INITIAL_SIZE);
  System.SetLength(FOutput, ARRAY_INITIAL_SIZE);
  FNodeCount := 1;
  FOutCount := 1;
  BuildCodeMap(aPatternList);
  for I := 0 to System.High(aPatternList) do
    AddPattern(aPatternList[I], I);
  System.SetLength(FOutput, OutCount);
  BuildFsm;
  FTrie := nil;
end;

constructor TDaACSearch.CreateBytes(const aPatternList: array of TBytes);
var
  I: SizeInt;
begin
  System.SetLength(FTrie, ARRAY_INITIAL_SIZE);
  System.SetLength(FOutput, ARRAY_INITIAL_SIZE);
  FNodeCount := 1;
  FOutCount := 1;
  BuildCodeMap(aPatternList);
  for I := 0 to System.High(aPatternList) do
    AddPattern(aPatternList[I], I);
  System.SetLength(FOutput, OutCount);
  BuildFsm;
  FTrie := nil;
end;

function TDaACSearch.Clone: TDaACSearch;
begin
  Result := TDaACSearch.Create;
  Result.FCodeMap := FCodeMap;
  Result.FDaTrie := FDaTrie;
  Result.FOutput := FOutput;
  Result.FOutCount := OutCount;
  Result.FNodeCount := StateCount;
  Result.FWordCount := PatternCount;
  Result.FAlphabetSize := AlphabetSize;
end;

end.

