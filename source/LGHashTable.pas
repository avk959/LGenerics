{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generic hash table implementations for internal use.                    *
*                                                                           *
*   Copyright(c) 2018 A.Koverdyaev(avk)                                     *
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
unit LGHashTable;

{$mode objfpc}{$H+}
{$INLINE ON}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses

  SysUtils,
  math,
  LGUtils,
  {%H-}LGHelpers,
  LGStrConst;

type

  { TGCustomHashTable}

  generic TGCustomHashTable<TKey, TEntry> = class abstract
  strict protected
  const
    MIN_LOAD_FACTOR  = 0.25;
    MAX_SIZE         = Succ(High(SizeInt) shr 1);
    //ENTRY_SIZE       = SizeOf(TEntry); - does not compiles with TMapEntry
    //workaround :
    /////////////////////////////////////////////
    E_SIZE           = SizeOf(TEntry);
    ENTRY_SIZE       = E_SIZE or Ord(E_SIZE = 0);
    /////////////////////////////////////////////

  type
    TCustomHashTable = specialize TGCustomHashTable<TKey, TEntry>;
    THashTableClass  = class of TGCustomHashTable;
    //to supress unnecessary refcounting:
    TFakeEntry = {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}array[0..Pred(ENTRY_SIZE)] of Byte{$ELSE}TEntry{$ENDIF};
  var
    FCount,
    FExpandTreshold: SizeInt;
    FLoadFactor: Single;
    procedure AllocList(aCapacity: SizeInt); virtual; abstract;
    procedure SetLoadFactor(aValue: Single); virtual; abstract;
    function  GetTableSize: SizeInt;  virtual; abstract;
    function  GetFillRatio: Single;
    function  RestrictLoadFactor(aValue: Single): Single; inline;
    function  GetCapacity: SizeInt; inline;
    property  ExpandTreshold: SizeInt read FExpandTreshold;
    class function EstimateCapacity(aCount: SizeInt; aLoadFactor: Single): SizeInt; virtual; abstract;
  public
  type
    PEntry           = ^TEntry;
    TEntryEvent      = procedure(p: PEntry) of object;
    TTest            = specialize TGTest<TKey>;
    TOnTest          = specialize TGOnTest<TKey>;
    TNestTest        = specialize TGNestTest<TKey>;
    TEntryTest       = function(p: PEntry): Boolean of object;
    TEntryEnumerator = specialize TGCustomEnumerator<PEntry>;

    TSearchResult = record
      case Integer of
        0: (FoundIndex, InsertIndex: SizeInt);
        1: (Node, PrevNode: Pointer);
    end;

    class function DefaultLoadFactor: Single; virtual; abstract;
    class function MaxLoadFactor: Single; virtual; abstract;
    class function MinLoadFactor: Single; static; inline;
    constructor CreateEmpty; virtual;
    constructor CreateEmpty(aLoadFactor: Single); virtual;
    constructor Create; virtual;
    constructor Create(aCapacity: SizeInt); virtual;
    constructor Create(aLoadFactor: Single); virtual;
    constructor Create(aCapacity: SizeInt; aLoadFactor: Single); virtual;
    function  GetEnumerator: TEntryEnumerator; virtual; abstract;
    function  Clone: TCustomHashTable; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure EnsureCapacity(aValue: SizeInt); virtual; abstract;
    procedure TrimToFit; virtual; abstract;
    //return True if aKey found, otherwise insert garbage entry and return False;
    function  FindOrAdd(constref aKey: TKey; out e: PEntry; out aRes: TSearchResult): Boolean; virtual;abstract; overload;
    function  Find(constref aKey: TKey; out aPos: TSearchResult): PEntry; virtual; abstract;
    function  Remove(constref aKey: TKey): Boolean; virtual; abstract;
    procedure RemoveAt(constref aPos: TSearchResult); virtual; abstract;
    function  RemoveIf(aTest: TTest; aOnRemove: TEntryEvent = nil): SizeInt; virtual; abstract;
    function  RemoveIf(aTest: TOnTest; aOnRemove: TEntryEvent = nil): SizeInt; virtual; abstract;
    function  RemoveIf(aTest: TNestTest; aOnRemove: TEntryEvent = nil): SizeInt; virtual; abstract;
    function  RemoveIf(aTest: TEntryTest; aOnRemove: TEntryEvent = nil): SizeInt; virtual; abstract;
    property  Count: SizeInt read FCount;
  { The capacity of the table is the number of elements that can be written without rehashing,
    so real capacity is ExpandTreshold, isn't it? }
    property  Capacity: SizeInt read GetCapacity;
    property  LoadFactor: Single read FLoadFactor write SetLoadFactor;
    property  FillRatio: Single read GetFillRatio;
    property  TableSize: SizeInt read GetTableSize;
  end;

  { TGOpenAddressing }

  generic TGOpenAddressing<TKey, TEntry, TEqRel, TProbeSeq> = class abstract(
    specialize TGCustomHashTable<TKey, TEntry>)
  strict protected
  const
    SLOT_NOT_FOUND: SizeInt = SizeInt(-1);
    USED_FLAG: SizeInt      = SizeInt(SizeInt(1) shl Pred(BitSizeOf(SizeInt)));

  type
    TNode = record
      Hash: SizeInt;
      Data: TEntry;
    end;

    TNodeList = array of TNode;

    TEnumerator = class(TEntryEnumerator)
    private
      FList: TNodeList;
      FCurrIndex,
      FLastIndex: SizeInt;
    protected
      function  GetCurrent: PEntry; override;
    public
      constructor Create(aList: TNodeList);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

  const
    NODE_SIZE             = SizeOf(TNode);
    MAX_CAPACITY: SizeInt = MAX_SIZE div NODE_SIZE;

  type
    TFakeNode = {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}array[0..Pred(NODE_SIZE)] of Byte{$ELSE}TNode{$ENDIF};

  var
    FList: TNodeList;
    class function NodeUsed(constref aNode: TNode): Boolean; static; inline;
    class function NewList(aCapacity: SizeInt): TNodeList; static; inline;
    procedure AllocList(aCapacity: SizeInt); override;
    function  GetTableSize: SizeInt; override;
    function  ListCapacity: SizeInt; inline;
    procedure SetLoadFactor(aValue: Single); override;
    procedure UpdateExpandTreshold; inline;
    procedure Rehash(var aTarget: TNodeList); virtual;
    procedure Resize(aNewCapacity: SizeInt);
    procedure Expand;
    function  DoFind(constref aKey: TKey; aKeyHash: SizeInt): TSearchResult; virtual;
    procedure DoRemove(aIndex: SizeInt); virtual; abstract;
    class function EstimateCapacity(aCount: SizeInt; aLoadFactor: Single): SizeInt; override;
    class constructor Init;
  public
    class function DefaultLoadFactor: Single; override;
    class function MaxLoadFactor: Single; override;
    function  GetEnumerator: TEntryEnumerator; override;
    procedure Clear; override;
    procedure EnsureCapacity(aValue: SizeInt); override;
    procedure TrimToFit; override;
    function  FindOrAdd(constref aKey: TKey; out e: PEntry; out aRes: TSearchResult): Boolean; override;
    function  Find(constref aKey: TKey; out aPos: TSearchResult): PEntry; override;
    function  Remove(constref aKey: TKey): Boolean; override;
    procedure RemoveAt(constref aPos: TSearchResult); override;
  end;

  TLPSeq = class
  const
    DEFAULT_LOAD_FACTOR: Single = 0.55;
    MAX_LOAD_FACTOR: Single     = 0.90;

    class function NextProbe(aPrevPos, {%H-}aIndex: SizeInt): SizeInt; static; inline;
  end;

  { TGOpenAddrLP implements open addressing hash table with linear probing(step = 1) }
  generic TGOpenAddrLP<TKey, TEntry, TEqRel> = class(specialize TGOpenAddressing<TKey, TEntry, TEqRel, TLPSeq>)
  strict protected
    procedure DoRemove(aIndex: SizeInt); override;
  public
    function  Clone: TCustomHashTable; override;
    function  RemoveIf(aTest: TTest; aOnRemove: TEntryEvent = nil): SizeInt; override;
    function  RemoveIf(aTest: TOnTest; aOnRemove: TEntryEvent = nil): SizeInt; override;
    function  RemoveIf(aTest: TNestTest; aOnRemove: TEntryEvent = nil): SizeInt; override;
    function  RemoveIf(aTest: TEntryTest; aOnRemove: TEntryEvent = nil): SizeInt; override;
  end;

  { TGOpenAddrTombstones }

  generic TGOpenAddrTombstones<TKey, TEntry, TEqRel, TProbeSeq> = class abstract(
    specialize TGOpenAddressing<TKey, TEntry, TEqRel, TProbeSeq>)
  strict protected
  const
    TOMBSTONE: SizeInt = SizeInt(1);

  var
    FTombstonesCount: SizeInt;
    function  BusyCount: SizeInt; inline;
    procedure Rehash(var aTarget: TNodeList); override;
    function  DoFind(constref aKey: TKey; aKeyHash: SizeInt): TSearchResult; override;
    procedure DoRemove(aIndex: SizeInt); override;
  public
    procedure Clear; override;
    procedure ClearTombstones; inline;
    function  FindOrAdd(constref aKey: TKey; out e: PEntry; out aRes: TSearchResult): Boolean; override;
    function  RemoveIf(aTest: TTest; aOnRemove: TEntryEvent = nil): SizeInt; override;
    function  RemoveIf(aTest: TOnTest; aOnRemove: TEntryEvent = nil): SizeInt; override;
    function  RemoveIf(aTest: TNestTest; aOnRemove: TEntryEvent = nil): SizeInt; override;
    function  RemoveIf(aTest: TEntryTest; aOnRemove: TEntryEvent = nil): SizeInt; override;
    property  TombstonesCount: SizeInt read FTombstonesCount;
  end;

  { TGOpenAddrLPT implements open addressing tombstones hash table with linear probing and lazy deletion}
  generic TGOpenAddrLPT<TKey, TEntry, TEqRel> = class(
    specialize TGOpenAddrTombstones<TKey, TEntry, TEqRel, TLPSeq>)
    function Clone: TCustomHashTable; override;
  end;

  TQP12Seq = class
  const
    DEFAULT_LOAD_FACTOR: Single = 0.50;
    MAX_LOAD_FACTOR: Single     = 0.75;
    class function NextProbe(aPrevPos, aIndex: SizeInt): SizeInt; static; inline;
  end;

  { TGOpenAddrQP implements open addressing hash table with quadratic probing(c1 = 1/2, c2 = 1/2) }
  generic TGOpenAddrQP<TKey, TEntry, TEqRel> = class(
    specialize TGOpenAddrTombstones<TKey, TEntry, TEqRel, TQP12Seq>)
    function Clone: TCustomHashTable; override;
  end;

{.$DEFINE ORDEREDHASHTABLE_ENABLE_PAGEDNODEMANAGER}{ if uncomment define, TGOrderedHashTable
                                                     will use TGPageNodeManager }
  { TGOrderedHashTable }

  generic TGOrderedHashTable<TKey, TEntry, TEqRel> = class(specialize TGCustomHashTable<TKey, TEntry>)
  public
  type
    PNode = ^TNode;

    TNode = record
      ChainNext,
      Prior,
      Next: PNode;
      Hash: SizeInt;
      Data: TEntry;
      //for node manager
      property  NextLink: PNode read ChainNext write ChainNext;
    end;

  strict protected
  type
    TChainList = array of PNode;

    TEnumerator = class(TEntryEnumerator)
    private
      FHead,
      FCurrNode: PNode;
      FInCycle: Boolean;
    protected
      function  GetCurrent: PEntry; override;
    public
      constructor Create(aHead: PNode);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

  public
  type
    TReverseEnumerator = class(TEntryEnumerator)
    protected
      FHead,
      FTail,
      FCurrNode: PNode;
      FInCycle: Boolean;
      function  GetCurrent: PEntry; override;
    public
      constructor Create(aTable: TGOrderedHashTable);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
      property  Head: PNode read FHead;
      property  Tail: PNode read FTail;
    end;

  strict protected
  type
{$IFDEF ORDEREDHASHTABLE_ENABLE_PAGEDNODEMANAGER}
    TNodeManager = specialize TGPageNodeManager<TNode>;
{$ELSE ORDEREDHASHTABLE_ENABLE_PAGEDNODEMANAGER}
    TNodeManager = specialize TGNodeManager<TNode>;
{$ENDIF ORDEREDHASHTABLE_ENABLE_PAGEDNODEMANAGER}

  const
    DEFAULT_LOAD_FACTOR: Single = 0.75;
    MAX_LOAD_FACTOR: Single     = 4.0;
    MAX_CAPACITY                = (MAX_SIZE shr 2) div SizeOf(Pointer);

  var
    FList: TChainList;
    FHead,
    FTail: PNode;
    FNodeManager: TNodeManager;
    procedure AllocList(aCapacity: SizeInt); override;
    function  GetTableSize: SizeInt; override;
    function  ListCapacity: SizeInt; inline;
    procedure SetLoadFactor(aValue: Single); override;
    function  NewNode: PNode; inline;
    procedure DisposeNode(aNode: PNode); inline;
    procedure ClearChainList;
    procedure UpdateExpandTreshold; inline;
    procedure Rehash(var aTarget: TChainList);
    procedure Resize(aNewCapacity: SizeInt);
    procedure Expand;
    function  DoAdd(aKeyHash: SizeInt): PNode;
    function  DoFind(constref aKey: TKey; aKeyHash: SizeInt): TSearchResult;
    procedure RemoveNode(aNode: PNode);
    class function EstimateCapacity(aCount: SizeInt; aLoadFactor: Single): SizeInt; override;
  public
    class function DefaultLoadFactor: Single; override;
    class function MaxLoadFactor: Single;  override;
    constructor CreateEmpty; override;
    constructor CreateEmpty(aLoadFactor: Single); override;
    constructor Create; override;
    constructor Create(aCapacity: SizeInt); override;
    constructor Create(aLoadFactor: Single); override;
    constructor Create(aCapacity: SizeInt; aLoadFactor: Single); override;
    destructor Destroy; override;
    procedure Clear; override;
    function  Clone: TCustomHashTable; override;
    procedure EnsureCapacity(aValue: SizeInt); override;
    procedure TrimToFit; override;
    function  GetEnumerator: TEntryEnumerator; override;
    function  GetReverseEnumerator: TReverseEnumerator;
    //return True if aKey found, otherwise insert empty Entry and return False;
    function  FindOrAdd(constref aKey: TKey; out e: PEntry; out aRes: TSearchResult): Boolean; override;
    function  Find(constref aKey: TKey; out aPos: TSearchResult): PEntry; override;
    function  Remove(constref aKey: TKey): Boolean; override;
    procedure RemoveAt(constref aPos: TSearchResult); override;
    function  RemoveIf(aTest: TTest; aOnRemove: TEntryEvent = nil): SizeInt; override;
    function  RemoveIf(aTest: TOnTest; aOnRemove: TEntryEvent = nil): SizeInt; override;
    function  RemoveIf(aTest: TNestTest; aOnRemove: TEntryEvent = nil): SizeInt; override;
    function  RemoveIf(aTest: TEntryTest; aOnRemove: TEntryEvent = nil): SizeInt; override;
    function  GetFirst: PEntry;
    function  GetLast: PEntry;
    property  Head: PNode read FHead;
    property  Tail: PNode read FTail;
  end;

{.$DEFINE CHAINHASHTABLE_ENABLE_PAGEDNODEMANAGER}{ if uncomment define, TGChainHashTable
                                                   will use TGPageNodeManager }
  { TGChainHashTable }

  generic TGChainHashTable<TKey, TEntry, TEqRel> = class(specialize TGCustomHashTable<TKey, TEntry>)
  public
  type
    PNode = ^TNode;
    TNode = record
      Next: PNode;
      Hash: SizeInt;
      Data: TEntry;
      //for node manager
      property  NextLink: PNode read Next write Next;
    end;

  strict protected
  type
    TChainList = array of PNode;

    TEnumerator = class(TEntryEnumerator)
    private
      FList: TChainList;
      FCurrNode: PNode;
      FLastIndex,
      FCurrIndex: SizeInt;
    protected
      function  GetCurrent: PEntry; override;
    public
      constructor Create(aList: TChainList);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;
{$IFDEF CHAINHASHTABLE_ENABLE_PAGEDNODEMANAGER}
    TNodeManager = specialize TGPageNodeManager<TNode>;
{$ELSE CHAINHASHTABLE_ENABLE_PAGEDNODEMANAGER}
    TNodeManager = specialize TGNodeManager<TNode>;
{$ENDIF CHAINHASHTABLE_ENABLE_PAGEDNODEMANAGER}
  const
    DEFAULT_LOAD_FACTOR: Single = 0.75;
    MAX_LOAD_FACTOR: Single     = 4.0;
    MAX_CAPACITY                = (MAX_SIZE shr 1) div SizeOf(Pointer);

  var
    FList: TChainList;
    FNodeManager: TNodeManager;
    procedure AllocList(aCapacity: SizeInt); override;
    function  GetTableSize: SizeInt; override;
    function  ListCapacity: SizeInt; inline;
    procedure SetLoadFactor(aValue: Single); override;
    function  NewNode: PNode; inline;
    procedure DisposeNode(aNode: PNode); inline;
    procedure ClearList;
    procedure UpdateExpandTreshold; inline;
    procedure Rehash(var aTarget: TChainList);
    procedure Resize(aNewCapacity: SizeInt);
    procedure Expand;
    function  DoAdd(aKeyHash: SizeInt): PNode;
    function  DoFind(constref aKey: TKey; aKeyHash: SizeInt): TSearchResult;
    procedure DoRemove(constref aPos: TSearchResult); inline;
    class function EstimateCapacity(aCount: SizeInt; aLoadFactor: Single): SizeInt; override;
  public
    class function DefaultLoadFactor: Single; override;
    class function MaxLoadFactor: Single; override;
    constructor CreateEmpty; override;
    constructor CreateEmpty(aLoadFactor: Single); override;
    constructor Create; override;
    constructor Create(aCapacity: SizeInt); override;
    constructor Create(aLoadFactor: Single); override;
    constructor Create(aCapacity: SizeInt; aLoadFactor: Single); override;
    destructor Destroy; override;
    procedure Clear; override;
    function  Clone: TCustomHashTable; override;
    procedure EnsureCapacity(aValue: SizeInt); override;
    procedure TrimToFit; override;
    function  GetEnumerator: TEntryEnumerator; override;
    //return True if aKey found, otherwise insert empty Entry and return False;
    function  FindOrAdd(constref aKey: TKey; out e: PEntry; out aRes: TSearchResult): Boolean; override;
    function  Find(constref aKey: TKey; out aPos: TSearchResult): PEntry; override;
    function  Add(constref aKey: TKey): PNode; inline;
    function  Remove(constref aKey: TKey): Boolean; override;
    procedure RemoveAt(constref aPos: TSearchResult); override;
    function  RemoveIf(aTest: TTest; aOnRemove: TEntryEvent = nil): SizeInt; override;
    function  RemoveIf(aTest: TOnTest; aOnRemove: TEntryEvent = nil): SizeInt; override;
    function  RemoveIf(aTest: TNestTest; aOnRemove: TEntryEvent = nil): SizeInt; override;
    function  RemoveIf(aTest: TEntryTest; aOnRemove: TEntryEvent = nil): SizeInt; override;
  end;

  { TGHashTableLP: simplified version TGOpenAddrLP }
  generic TGHashTableLP<TKey, TEntry, TEqRel> = class
  strict protected
  const
    MAX_SIZE         = Succ(High(SizeInt) shr 1);
    //ENTRY_SIZE       = SizeOf(TEntry); does not compiles with TMapEntry
    //workaround :
    /////////////////////////////////////////////
    E_SIZE           = SizeOf(TEntry);
    ENTRY_SIZE       = E_SIZE or Ord(E_SIZE = 0);
    /////////////////////////////////////////////
  type
    TFakeEntry = {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}array[0..Pred(ENTRY_SIZE)] of Byte{$ELSE}TEntry{$ENDIF};
    TNode = record
      Hash: SizeInt;
      Data: TEntry;
    end;

    TNodeList = array of TNode;

  const
    NODE_SIZE               = SizeOf(TNode);
    SLOT_NOT_FOUND: SizeInt = Low(SizeInt);
    USED_FLAG: SizeInt      = SizeInt(SizeInt(1) shl Pred(BitSizeOf(SizeInt)));
    MAX_CAPACITY: SizeInt   = MAX_SIZE div NODE_SIZE;

  type
    TFakeNode = {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}array[0..Pred(NODE_SIZE)] of Byte{$ELSE}TNode{$ENDIF};

  public
  type
    PEntry = ^TEntry;

    TEnumerator = record
    private
      FList: TNodeList;
      FCurrIndex,
      FLastIndex: SizeInt;
      function  GetCurrent: PEntry; inline;
      procedure Init(aList: TNodeList); inline;
    public
      function  MoveNext: Boolean;
      procedure Reset; inline;
      property  Current: PEntry read GetCurrent;
    end;

  strict protected
    FList: TNodeList;
    FCount,
    FExpandTreshold: SizeInt;
    FLoadFactor: Single;
    function  RestrictLoadFactor(aValue: Single): Single; inline;
    function  ListCapacity: SizeInt; inline;
    procedure UpdateExpandTreshold; inline;
    procedure SetLoadFactor(aValue: Single);
    function  GetCapacity: SizeInt; inline;
    function  GetFillRatio: Single; inline;
    function  GetTableSize: SizeInt; inline;
    procedure AllocList(aCapacity: SizeInt);
    procedure Rehash(var aTarget: TNodeList);
    procedure Resize(aNewCapacity: SizeInt);
    procedure Expand;
    function  DoFind(constref aKey: TKey; aKeyHash: SizeInt): SizeInt;
    procedure DoRemove(aIndex: SizeInt);
    property  ExpandTreshold: SizeInt read FExpandTreshold;
    class function NewList(aCapacity: SizeInt): TNodeList; static; inline;
    class function EstimateCapacity(aCount: SizeInt; aLoadFactor: Single): SizeInt; static; inline;
    class constructor Init;
  public
  const
    DEFAULT_LOAD_FACTOR: Single = 0.55;
    MAX_LOAD_FACTOR: Single     = 0.90;
    MIN_LOAD_FACTOR: Single     = 0.25;
    class function DefaultLoadFactor: Single; static; inline;
    class function MaxLoadFactor: Single; static; inline;
    class function MinLoadFactor: Single; static; inline;
    constructor CreateEmpty;
    constructor CreateEmpty(aLoadFactor: Single);
    constructor Create;
    constructor Create(aCapacity: SizeInt);
    constructor Create(aLoadFactor: Single);
    constructor Create(aCapacity: SizeInt; aLoadFactor: Single);
    function  GetEnumerator: TEnumerator;
    procedure Clear;
    function  EnsureCapacity(aValue: SizeInt): Boolean;
    procedure TrimToFit;
    function  FindOrAdd(constref aKey: TKey; out e: PEntry; out aPos: SizeInt): Boolean;
    function  Find(constref aKey: TKey; out aPos: SizeInt): PEntry;
    function  Remove(constref aKey: TKey): Boolean;
    procedure RemoveAt(constref aPos: SizeInt); inline;
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
    property  LoadFactor: Single read FLoadFactor write SetLoadFactor;
    property  FillRatio: Single read GetFillRatio;
    property  TableSize: SizeInt read GetTableSize;
  end;

  { TGLiteHashTableLP }

  generic TGLiteHashTableLP<TKey, TEntry, TEqRel> = record
  private
  const

    MAX_SIZE         = Succ(High(SizeInt) shr 1);
    //ENTRY_SIZE       = SizeOf(TEntry); - does not compiles with TMapEntry
    //workaround :
    /////////////////////////////////////////////
    E_SIZE           = SizeOf(TEntry);
    ENTRY_SIZE       = E_SIZE or Ord(E_SIZE = 0);
    /////////////////////////////////////////////

  type
    //to supress unnecessary refcounting:
    TFakeEntry = {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}array[0..Pred(ENTRY_SIZE)] of Byte{$ELSE}TEntry{$ENDIF};

    TNode = record
      Hash: SizeInt;
      Data: TEntry;
    end;

    TNodeList = array of TNode;

  const
    NODE_SIZE               = SizeOf(TNode);
    SLOT_NOT_FOUND: SizeInt = Low(SizeInt);
    USED_FLAG: SizeInt      = SizeInt(SizeInt(1) shl Pred(BitSizeOf(SizeInt)));
    MAX_CAPACITY: SizeInt   = MAX_SIZE div NODE_SIZE;

  type
    TFakeNode = {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}array[0..Pred(NODE_SIZE)] of Byte{$ELSE}TNode{$ENDIF};
    PLiteHashTableLP = ^TGLiteHashTableLP;

  public
  type
    PEntry = ^TEntry;

    TEnumerator = record
    private
      FList: TNodeList;
      FCurrIndex,
      FLastIndex: SizeInt;
      function  GetCurrent: PEntry; inline;
      procedure Init(aList: TNodeList); inline;
    public
      function  MoveNext: Boolean;
      procedure Reset; inline;
      property  Current: PEntry read GetCurrent;
    end;

    TRemovableEnumerator = record
    private
      FEnum: TEnumerator;
      FTable: PLiteHashTableLP;
      function  GetCurrent: PEntry; inline;
      procedure Init(aTable: PLiteHashTableLP); inline;
    public
      function  MoveNext: Boolean;
      procedure RemoveCurrent; inline;
      procedure Reset; inline;
      property  Current: PEntry read GetCurrent;
    end;

  private
    FList: TNodeList;
    FCount,
    FExpandTreshold: SizeInt;
    FLoadFactor: Single;
    function  RestrictLoadFactor(aValue: Single): Single; inline;
    procedure UpdateExpandTreshold; inline;
    procedure SetLoadFactor(aValue: Single);
    function  GetCapacity: SizeInt; inline;
    function  GetFillRatio: Single; inline;
    function  GetSize: SizeInt; inline;
    procedure AllocList(aCapacity: SizeInt);
    procedure Rehash(var aTarget: TNodeList);
    procedure Resize(aNewCapacity: SizeInt);
    procedure Expand;
    function  DoFind(constref aKey: TKey; aKeyHash: SizeInt): SizeInt;
    procedure DoRemove(aIndex: SizeInt);
    property  ExpandTreshold: SizeInt read FExpandTreshold;
    class function NewList(aCapacity: SizeInt): TNodeList; static; inline;
    class function EstimateSize(aCount: SizeInt; aLoadFactor: Single): SizeInt; static; inline;
    class constructor Init;
    class operator Initialize(var ht: TGLiteHashTableLP);
    class operator Finalize(var ht: TGLiteHashTableLP);
    class operator Copy(constref aSrc: TGLiteHashTableLP; var aDst: TGLiteHashTableLP); inline;
  public
  const
    DEFAULT_LOAD_FACTOR: Single = 0.55;
    MAX_LOAD_FACTOR: Single     = 0.90;
    MIN_LOAD_FACTOR: Single     = 0.25;

    function  GetEnumerator: TEnumerator; inline;
    function  RemovableEnumerator: TRemovableEnumerator; inline;
    procedure Clear;
    procedure EnsureCapacity(aValue: SizeInt);
    procedure TrimToFit;
    //return True if aKey found, otherwise insert garbage entry and return False;
    function  FindOrAdd(constref aKey: TKey; out e: PEntry; out aPos: SizeInt): Boolean;
    function  Find(constref aKey: TKey; out aPos: SizeInt): PEntry;
    function  Remove(constref aKey: TKey): Boolean;
    procedure RemoveAt(constref aPos: SizeInt);
    property  Count: SizeInt read FCount;
  { The capacity of the table is the number of elements that can be written without rehashing,
    so real capacity is ExpandTreshold }
    property  Capacity: SizeInt read GetCapacity;
    property  LoadFactor: Single read FLoadFactor write SetLoadFactor;
    property  FillRatio: Single read GetFillRatio;
    property  Size: SizeInt read GetSize; //todo: Split(predicate): TGLiteHashTableLP; ???
  end;

  { TGLiteHashList: node based hash table with alter access by index(or vice versa);
      functor TEqRel(equality relation) must provide:
        class function HashCode([const[ref]] aValue: T): SizeInt;
        class function Equal([const[ref]] L, R: T): Boolean; }
  generic TGLiteHashList<T, TEqRel> = record
  private
  type
    TNode = record
      Hash,
      Next: SizeInt;
      Data: T;
    end;

    TNodeList     = array of TNode;
    THashList     = array of SizeInt;
    PLiteHashList = ^TGLiteHashList;

  const
    NULL_INDEX  = SizeInt(-1);
    MAX_SIZE    = Succ(High(SizeInt) shr 1);
    NODE_SIZE   = SizeOf(TNode);
    MAX_CAPACITY: SizeInt  = MAX_SIZE div NODE_SIZE;

  public
  type
    IEnumerable = specialize IGEnumerable<T>;
    TArray      = array of T;

    TEnumerator = record
    private
      FList: TNodeList;
      FLastIndex,
      FCurrIndex: SizeInt;
      function  GetCurrent: T; inline;
      procedure Init(constref aList: TGLiteHashList);
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: T read GetCurrent;
    end;

    TReverseEnumerator = record
    private
      FList: TNodeList;
      FCount,
      FCurrIndex: SizeInt;
      function  GetCurrent: T; inline;
      procedure Init(constref aList: TGLiteHashList);
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: T read GetCurrent;
    end;

    TReverse = record
    private
      FList: PLiteHashList;
      procedure Init(aList: PLiteHashList); inline;
    public
      function  GetEnumerator: TReverseEnumerator; inline;
    end;

  private
    FNodeList: TNodeList;
    FHashList: THashList;
    FCount: SizeInt;
    function  GetCapacity: SizeInt; inline;
    function  GetItem(aIndex: SizeInt): T;
    procedure SetItem(aIndex: SizeInt; const aValue: T);
    procedure InitialAlloc; inline;
    procedure Rehash;
    procedure Resize(aNewCapacity: SizeInt);
    procedure Expand;
    function  Find(constref aValue: T): SizeInt;
    function  GetCount(constref aValue: T): SizeInt;
    function  DoAdd(constref aValue: T): SizeInt;
    procedure DoInsert(aIndex: SizeInt; constref aValue: T);
    procedure DoDelete(aIndex: SizeInt);
    procedure RemoveFromChain(aIndex: SizeInt);
    function  DoRemove(constref aValue: T): Boolean;
    function  GetReverseEnumerator: TReverseEnumerator; inline;
    function  IndexInRange(aIndex: SizeInt): Boolean; inline;
    function  IndexInInsertRange(aIndex: SizeInt): Boolean; inline;
    procedure CheckIndexRange(aIndex: SizeInt); inline;
    procedure CheckInsertIndexRange(aIndex: SizeInt); inline;
    class constructor Init;
    class function  NewHashList(aCapacity: SizeInt): THashList; static; inline;
    class procedure IndexOutOfBoundError(aIndex: SizeInt); static; inline;
    class procedure CapacityExceedError(aValue: SizeInt); static; inline;
    class operator Initialize(var hl: TGLiteHashList);
    class operator Finalize(var hl: TGLiteHashList);
    class operator Copy(constref aSrc: TGLiteHashList; var aDst: TGLiteHashList);
  public
    function  GetEnumerator: TEnumerator;
    function  ToArray: TArray;
    function  Reverse: TReverse;
    procedure Clear;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure EnsureCapacity(aValue: SizeInt);
    procedure TrimToFit;
    function  Contains(constref aValue: T): Boolean; inline;
    function  NonContains(constref aValue: T): Boolean; inline;
    function  IndexOf(constref aValue: T): SizeInt; inline;
    function  CountOf(constref aValue: T): SizeInt; inline;
    function  Add(constref aValue: T): SizeInt; inline;
    function  AddAll(constref a: array of T): SizeInt;
    function  AddAll(e: IEnumerable): SizeInt;
    procedure Insert(aIndex: SizeInt; constref aValue: T); inline;
    procedure Delete(aIndex: SizeInt); inline;
    function  Remove(constref aValue: T): Boolean; inline;
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
    property  Items[aIndex: SizeInt]: T read GetItem write SetItem; default;
  end;

implementation
{$Q-}{$B-}{$COPERATORS ON}

{ TGCustomHashTable }

function TGCustomHashTable.GetFillRatio: Single;
var
  c: SizeInt;
begin
  c := GetTableSize;
  if c > 0 then
    Result := Count / c
  else
    Result := 0.0;
end;

function TGCustomHashTable.RestrictLoadFactor(aValue: Single): Single;
begin
  Result := Math.Min(Math.Max(aValue, MinLoadFactor), MaxLoadFactor);
end;

function TGCustomHashTable.GetCapacity: SizeInt;
begin
  Result := Trunc(TableSize * LoadFactor);
end;

class function TGCustomHashTable.MinLoadFactor: Single;
begin
  Result := MIN_LOAD_FACTOR;
end;

constructor TGCustomHashTable.CreateEmpty;
begin
  FLoadFactor := DefaultLoadFactor;
end;

constructor TGCustomHashTable.CreateEmpty(aLoadFactor: Single);
begin
  FLoadFactor := RestrictLoadFactor(aLoadFactor);
end;

constructor TGCustomHashTable.Create;
begin
  FLoadFactor := DefaultLoadFactor;
  AllocList(DEFAULT_CONTAINER_CAPACITY);
end;

constructor TGCustomHashTable.Create(aCapacity: SizeInt);
begin
  FLoadFactor := DefaultLoadFactor;
  AllocList(EstimateCapacity(aCapacity, LoadFactor));
end;

constructor TGCustomHashTable.Create(aLoadFactor: Single);
begin
  FLoadFactor := RestrictLoadFactor(aLoadFactor);
  AllocList(DEFAULT_CONTAINER_CAPACITY);
end;

constructor TGCustomHashTable.Create(aCapacity: SizeInt; aLoadFactor: Single);
begin
  FLoadFactor := RestrictLoadFactor(aLoadFactor);
  AllocList(EstimateCapacity(aCapacity, LoadFactor));
end;

{ TGOpenAddressing.TEnumerator }

function TGOpenAddressing.TEnumerator.GetCurrent: PEntry;
begin
  Result := @FList[FCurrIndex].Data;
end;

constructor TGOpenAddressing.TEnumerator.Create(aList: TNodeList);
begin
  FList := aList;
  FLastIndex := System.High(aList);
  FCurrIndex := -1;
end;

function TGOpenAddressing.TEnumerator.MoveNext: Boolean;
begin
  repeat
    if FCurrIndex >= FLastIndex then
      exit(False);
    Inc(FCurrIndex);
    Result := NodeUsed(FList[FCurrIndex]);
  until Result;
end;

procedure TGOpenAddressing.TEnumerator.Reset;
begin
  FCurrIndex := -1;
end;

{ TGOpenAddressing }

class function TGOpenAddressing.NodeUsed(constref aNode: TNode): Boolean;
begin
  Result := (aNode.Hash and USED_FLAG) <> 0;
end;

class function TGOpenAddressing.NewList(aCapacity: SizeInt): TNodeList;
begin
  System.SetLength(Result, aCapacity);
  System.FillChar(Result[0], aCapacity * NODE_SIZE, 0);
end;

procedure TGOpenAddressing.AllocList(aCapacity: SizeInt);
begin
  if aCapacity > 0 then
    begin
      aCapacity := Math.Min(aCapacity, MAX_CAPACITY);
      if not IsTwoPower(aCapacity) then
        aCapacity := LGUtils.RoundUpTwoPower(aCapacity);
    end
  else
    aCapacity := DEFAULT_CONTAINER_CAPACITY;
  FList := NewList(aCapacity);
  UpdateExpandTreshold;
end;

function TGOpenAddressing.GetTableSize: SizeInt;
begin
  Result := ListCapacity;
end;

function TGOpenAddressing.ListCapacity: SizeInt;
begin
  Result := System.Length(FList);
end;

procedure TGOpenAddressing.SetLoadFactor(aValue: Single);
begin
  aValue := RestrictLoadFactor(aValue);
  if aValue <> LoadFactor then
    begin
      FLoadFactor := aValue;
      UpdateExpandTreshold;
      if Count >= ExpandTreshold then
        Expand;
    end;
end;

procedure TGOpenAddressing.UpdateExpandTreshold;
begin
  if ListCapacity < MAX_CAPACITY then
    FExpandTreshold := Trunc(ListCapacity * FLoadFactor)
  else
    FExpandTreshold := High(SizeInt);
end;

procedure TGOpenAddressing.Rehash(var aTarget: TNodeList);
var
  h, I, J, Mask: SizeInt;
begin
  if Count > 0 then
    begin
      Mask := System.High(aTarget);
      for I := 0 to System.High(FList) do
        begin
          if NodeUsed(FList[I]) then
            begin
              h := FList[I].Hash and Mask;
              for J := 0 to Mask do
                begin
                  if aTarget[h].Hash = 0 then // -> target node is empty
                    begin
                      TFakeNode(aTarget[h]) := TFakeNode(FList[I]);
                      TFakeEntry(FList[I].Data) := Default(TFakeEntry);
                      break;
                    end;
                  h := TProbeSeq.NextProbe(h, J) and Mask;// probe sequence
                end;
            end;
        end;
    end;
end;

procedure TGOpenAddressing.Resize(aNewCapacity: SizeInt);
var
  List: TNodeList;
begin
  List := NewList(aNewCapacity);
  Rehash(List);
  FList := List;
  UpdateExpandTreshold;
end;

procedure TGOpenAddressing.Expand;
var
  NewCapacity, OldCapacity: SizeInt;
begin
  OldCapacity := ListCapacity;
  if OldCapacity > 0 then
    begin
      NewCapacity := Math.Min(MAX_CAPACITY, OldCapacity shl 1);
      if NewCapacity > OldCapacity then
        Resize(NewCapacity);
    end
  else
    AllocList(DEFAULT_CONTAINER_CAPACITY);
end;

function TGOpenAddressing.DoFind(constref aKey: TKey; aKeyHash: SizeInt): TSearchResult;
var
  I, Pos, h, Mask: SizeInt;
begin
  Mask := System.High(FList);
  aKeyHash := aKeyHash or USED_FLAG;
  Result.FoundIndex := SLOT_NOT_FOUND;
  Result.InsertIndex := SLOT_NOT_FOUND;
  Pos := aKeyHash and Mask;
  for I := 0 to Mask do
    begin
      h := FList[Pos].Hash;
      if h = 0 then                               // node empty => key not found
        begin
          Result.InsertIndex := Pos;
          exit;
        end
      else
        if (h = aKeyHash) and TEqRel.Equal(FList[Pos].Data.Key, aKey) then
          begin
            Result.FoundIndex := Pos;             // key found
            exit;
          end;
      Pos := TProbeSeq.NextProbe(Pos, I) and Mask;// probe sequence
    end;
end;

class function TGOpenAddressing.EstimateCapacity(aCount: SizeInt; aLoadFactor: Single): SizeInt;
begin
  //aCount := Math.Min(Math.Max(aCount, 0), MaxCapacity);
  if aCount > 0 then
    Result := LGUtils.RoundUpTwoPower(Math.Min(Ceil64(Double(aCount) / aLoadFactor), MAX_CAPACITY))
  else
    Result := DEFAULT_CONTAINER_CAPACITY;
end;

class constructor TGOpenAddressing.Init;
begin
{$PUSH}{$J+}
  MAX_CAPACITY := LGUtils.RoundUpTwoPower(MAX_CAPACITY);
{$POP}
end;

class function TGOpenAddressing.DefaultLoadFactor: Single;
begin
  Result := TProbeSeq.DEFAULT_LOAD_FACTOR;
end;

class function TGOpenAddressing.MaxLoadFactor: Single;
begin
  Result := TProbeSeq.MAX_LOAD_FACTOR;
end;

function TGOpenAddressing.GetEnumerator: TEntryEnumerator;
begin
  Result := TEnumerator.Create(FList);
end;

procedure TGOpenAddressing.Clear;
begin
  FList := nil;
  FCount := 0;
  FExpandTreshold := 0;
end;

procedure TGOpenAddressing.EnsureCapacity(aValue: SizeInt);
var
  NewCapacity: SizeInt;
begin
  if aValue > ExpandTreshold then
    begin
      NewCapacity := EstimateCapacity(aValue, LoadFactor);
      if NewCapacity <> ListCapacity then
        Resize(NewCapacity);
    end;
end;

procedure TGOpenAddressing.TrimToFit;
var
  NewCapacity: SizeInt;
begin
  if Count > 0 then
    begin
      NewCapacity := EstimateCapacity(Count, LoadFactor);
      if NewCapacity < ListCapacity then
        Resize(NewCapacity);
    end
  else
    Clear;
end;

function TGOpenAddressing.FindOrAdd(constref aKey: TKey; out e: PEntry; out aRes: TSearchResult): Boolean;
var
  h: SizeInt;
begin
  if FList = nil then
    AllocList(DEFAULT_CONTAINER_CAPACITY);
  h := TEqRel.HashCode(aKey);
  aRes := DoFind(aKey, h);
  Result := aRes.FoundIndex >= 0; // key found?
  if not Result then              // key not found
    begin
      if Count >= ExpandTreshold then
        begin
          Expand;
          aRes := DoFind(aKey, h);
        end;
      if aRes.InsertIndex > SLOT_NOT_FOUND then
        begin
          FList[aRes.InsertIndex].Hash := h or USED_FLAG;
          aRes.FoundIndex := aRes.InsertIndex;
          Inc(FCount);
        end
      else
        raise ELGCapacityExceed.CreateFmt(SEClassCapacityExceedFmt, [ClassName, Succ(Count)]);
    end;
  e := @FList[aRes.FoundIndex].Data;
end;

function TGOpenAddressing.Find(constref aKey: TKey; out aPos: TSearchResult): PEntry;
begin
  Result := nil;
  if Count > 0 then
    begin
      aPos := DoFind(aKey, TEqRel.HashCode(aKey));
      if aPos.FoundIndex >= 0 then
        Result := @FList[aPos.FoundIndex].Data;
    end;
end;

function TGOpenAddressing.Remove(constref aKey: TKey): Boolean;
var
  p: TSearchResult;
begin
  if Count > 0 then
    begin
      p := DoFind(aKey, TEqRel.HashCode(aKey));
      Result := p.FoundIndex >= 0;
      if Result then
        DoRemove(p.FoundIndex);
    end
  else
    Result := False;
end;

procedure TGOpenAddressing.RemoveAt(constref aPos: TSearchResult);
begin
  if (aPos.FoundIndex >= 0) and (aPos.FoundIndex <= System.High(FList)) then
    DoRemove(aPos.FoundIndex);
end;

{ TLPSeq }

class function TLPSeq.NextProbe(aPrevPos, aIndex: SizeInt): SizeInt;
begin
  Result := Succ(aPrevPos);
end;

{ TGOpenAddrLP }

procedure TGOpenAddrLP.DoRemove(aIndex: SizeInt);
var
  I, h, Gap, Mask: SizeInt;
begin
  Mask := System.High(FList);
  FList[aIndex].Hash := 0;
  FList[aIndex].Data := Default(TEntry);
  Gap := aIndex;
  aIndex := Succ(aIndex) and Mask;
  Dec(FCount);
  for I := 0 to Mask do
    begin
      h := FList[aIndex].Hash;
      if h <> 0 then
        begin
          h := h and Mask;
          if (h <> aIndex) and (Succ(aIndex - h + Mask) and Mask >= Succ(aIndex - Gap + Mask) and Mask) then
            begin
              TFakeNode(FList[Gap]) := TFakeNode(FList[aIndex]);
              TFakeNode(FList[aIndex]) := Default(TFakeNode);
              Gap := aIndex;
            end;
          aIndex := Succ(aIndex) and Mask;
        end
      else
        break;
    end;
end;

function TGOpenAddrLP.Clone: TCustomHashTable;
var
  c: TGOpenAddrLP;
begin
  c := TGOpenAddrLP.CreateEmpty(LoadFactor);
  c.FList := System.Copy(FList);
  c.FCount := Count;
  c.FExpandTreshold := ExpandTreshold;
  Result := c;
end;
{$PUSH}{$MACRO ON}
function TGOpenAddrLP.RemoveIf(aTest: TTest; aOnRemove: TEntryEvent): SizeInt;
var
  I: SizeInt;
begin
{$DEFINE TestMacro := aTest(FList[I].Data.Key)}
{$DEFINE RemoveIfMacro :=
  Result := 0;
  if Count > 0 then
    begin
      I := 0;
      while I <= Pred(ListCapacity) do
        if (FList[I].Hash <> 0) and TestMacro then
          begin
            if aOnRemove <> nil then
              aOnRemove(@FList[I].Data);
            DoRemove(I);
            Inc(Result);
          end
        else
          Inc(I);
    end}
  RemoveIfMacro;
end;

function TGOpenAddrLP.RemoveIf(aTest: TOnTest; aOnRemove: TEntryEvent): SizeInt;
var
  I: SizeInt;
begin
  RemoveIfMacro;
end;

function TGOpenAddrLP.RemoveIf(aTest: TNestTest; aOnRemove: TEntryEvent): SizeInt;
var
  I: SizeInt;
begin
  RemoveIfMacro;
end;

function TGOpenAddrLP.RemoveIf(aTest: TEntryTest; aOnRemove: TEntryEvent): SizeInt;
var
  I: SizeInt;
begin
  {$DEFINE TestMacro := aTest(@FList[I].Data)}
  RemoveIfMacro;
end;
{$POP}

{ TGOpenAddrTombstones }

function TGOpenAddrTombstones.BusyCount: SizeInt;
begin
  Result := Count + TombstonesCount;
end;

procedure TGOpenAddrTombstones.Rehash(var aTarget: TNodeList);
begin
  inherited;
  FTombstonesCount := 0;
end;

procedure TGOpenAddrTombstones.DoRemove(aIndex: SizeInt);
begin
  FList[aIndex].Hash := TOMBSTONE;
  FList[aIndex].Data := Default(TEntry);
  Inc(FTombstonesCount);
  Dec(FCount);
end;

procedure TGOpenAddrTombstones.Clear;
begin
  inherited;
  FTombstonesCount := 0;
end;

procedure TGOpenAddrTombstones.ClearTombstones;
begin
  Resize(ListCapacity);
end;

function TGOpenAddrTombstones.FindOrAdd(constref aKey: TKey; out e: PEntry; out aRes: TSearchResult): Boolean;
var
  h: SizeInt;
begin
  if FList = nil then
    AllocList(DEFAULT_CONTAINER_CAPACITY);
  h := TEqRel.HashCode(aKey);
  aRes := DoFind(aKey, h);
  Result := aRes.FoundIndex >= 0; // key found?
  if not Result then              // key not found
    begin
      if BusyCount >= ExpandTreshold then
        begin
          if TombstonesCount >= Count shr 1 then  //todo: Count shr 1 ??? why ???
            ClearTombstones
          else
            Expand;
          aRes := DoFind(aKey, h);
        end;
      if aRes.InsertIndex >= 0 then
        begin
          if FList[aRes.InsertIndex].Hash = TOMBSTONE then
            Dec(FTombstonesCount);
          FList[aRes.InsertIndex].Hash := h or USED_FLAG;
          aRes.FoundIndex := aRes.InsertIndex;
          Inc(FCount);
        end
      else
        raise Exception.CreateFmt(SEClassCapacityExceedFmt, [ClassName, Succ(Count)]);
    end;
  e := @FList[aRes.FoundIndex].Data;
end;
{$PUSH}{$MACRO ON}
function TGOpenAddrTombstones.RemoveIf(aTest: TTest; aOnRemove: TEntryEvent): SizeInt;
var
  I: SizeInt;
begin
{$DEFINE TestMacro := aTest(FList[I].Data.Key)}
{$DEFINE RemoveIfMacro :=
  Result := 0;
  if Count > 0 then
    begin
      I := 0;
      while I <= Pred(ListCapacity) do
        begin
          if NodeUsed(FList[I]) and TestMacro then
            begin
              if aOnRemove <> nil then
                aOnRemove(@FList[I].Data);
              DoRemove(I);
              Inc(Result);
            end;
          Inc(I);
        end;
    end}
  RemoveIfMacro;
end;

function TGOpenAddrTombstones.RemoveIf(aTest: TOnTest; aOnRemove: TEntryEvent): SizeInt;
var
  I: SizeInt;
begin
  RemoveIfMacro;
end;

function TGOpenAddrTombstones.RemoveIf(aTest: TNestTest; aOnRemove: TEntryEvent): SizeInt;
var
  I: SizeInt;
begin
  RemoveIfMacro;
end;

function TGOpenAddrTombstones.RemoveIf(aTest: TEntryTest; aOnRemove: TEntryEvent): SizeInt;
var
  I: SizeInt;
begin
  {$DEFINE TestMacro := aTest(@FList[I].Data)}
  RemoveIfMacro;
end;
{$POP}

function TGOpenAddrTombstones.DoFind(constref aKey: TKey; aKeyHash: SizeInt): TSearchResult;
var
  I, Pos, h, Mask: SizeInt;
begin
  Mask := System.High(FList);
  aKeyHash := aKeyHash or USED_FLAG;
  Result.FoundIndex := SLOT_NOT_FOUND;
  Result.InsertIndex := SLOT_NOT_FOUND;
  Pos := aKeyHash and Mask;
  for I := 0 to Mask do
    begin
      h := FList[Pos].Hash;
      if h = 0 then                                    // node empty => key not found
        begin
          if Result.InsertIndex = SLOT_NOT_FOUND then  // if none tombstone found, remember first empty
            Result.InsertIndex := Pos;
          exit;
        end
      else
        if h = TOMBSTONE then
          begin
            if Result.InsertIndex = SLOT_NOT_FOUND then// remember first tombstone position
              Result.InsertIndex := Pos;
          end
        else
          if (h = aKeyHash) and TEqRel.Equal(FList[Pos].Data.Key, aKey) then
            begin
              Result.FoundIndex := Pos;                // key found
              exit;
            end;
      Pos := TProbeSeq.NextProbe(Pos, I) and Mask;     // probe sequence
    end;
end;

{ TGOpenAddrLPT }

function TGOpenAddrLPT.Clone: TCustomHashTable;
var
  c: TGOpenAddrLPT;
begin
  c := TGOpenAddrLPT.CreateEmpty(LoadFactor);
  c.FList := System.Copy(FList);
  c.FCount := Count;
  c.FExpandTreshold := ExpandTreshold;
  c.FTombstonesCount := TombstonesCount;
  Result := c;
end;

{ TQP12Seq }

class function TQP12Seq.NextProbe(aPrevPos, aIndex: SizeInt): SizeInt;
begin
  Result := Succ(aPrevPos + aIndex);
end;


{ TGOpenAddrQP }

function TGOpenAddrQP.Clone: TCustomHashTable;
var
  c: TGOpenAddrQP;
begin
  c := TGOpenAddrQP.CreateEmpty(LoadFactor);
  c.FList := System.Copy(FList);
  c.FCount := Count;
  c.FExpandTreshold := ExpandTreshold;
  c.FTombstonesCount := TombstonesCount;
  Result := c;
end;

{ TGOrderedHashTable.TEnumerator }

function TGOrderedHashTable.TEnumerator.GetCurrent: PEntry;
begin
  Result := @FCurrNode^.Data;
end;

constructor TGOrderedHashTable.TEnumerator.Create(aHead: PNode);
begin
  FHead := aHead;
end;

function TGOrderedHashTable.TEnumerator.MoveNext: Boolean;
var
  NextNode: PNode = nil;
begin
  if FCurrNode <> nil then
    NextNode := FCurrNode^.Next
  else
    if not FInCycle then
      begin
        NextNode := FHead;
        FInCycle := True;
      end;
  Result := NextNode <> nil;
  if Result then
    FCurrNode := NextNode;
end;

procedure TGOrderedHashTable.TEnumerator.Reset;
begin
  FCurrNode := nil;
  FInCycle := False;
end;

{ TGOrderedHashTable.TReverseEnumerator }

function TGOrderedHashTable.TReverseEnumerator.GetCurrent: PEntry;
begin
  Result := @FCurrNode^.Data;
end;

constructor TGOrderedHashTable.TReverseEnumerator.Create(aTable: TGOrderedHashTable);
begin
  FHead := aTable.Head;
  FTail := aTable.Tail;
  FCurrNode := nil;
end;

function TGOrderedHashTable.TReverseEnumerator.MoveNext: Boolean;
var
  NextNode: PNode = nil;
begin
  if FCurrNode <> nil then
    NextNode := FCurrNode^.Prior
  else
    if not FInCycle then
      begin
        NextNode := FTail;
        FInCycle := True;
      end;
  Result := NextNode <> nil;
  if Result then
    FCurrNode := NextNode;
end;

procedure TGOrderedHashTable.TReverseEnumerator.Reset;
begin
  FCurrNode := nil;
  FInCycle := False;
end;

{ TGOrderedHashTable }

procedure TGOrderedHashTable.AllocList(aCapacity: SizeInt);
begin
  if aCapacity > 0 then
    begin
      aCapacity := Math.Min(aCapacity, MAX_CAPACITY);
      if not IsTwoPower(aCapacity) then
        aCapacity := LGUtils.RoundUpTwoPower(aCapacity);
    end
  else
    aCapacity := DEFAULT_CONTAINER_CAPACITY;
  System.SetLength(FList, aCapacity);
  UpdateExpandTreshold;
end;

function TGOrderedHashTable.GetTableSize: SizeInt;
begin
  Result := ListCapacity;
end;

function TGOrderedHashTable.ListCapacity: SizeInt;
begin
  Result := System.Length(FList);
end;

procedure TGOrderedHashTable.SetLoadFactor(aValue: Single);
begin
  aValue := RestrictLoadFactor(aValue);
  if aValue <> LoadFactor then
    begin
      FLoadFactor := aValue;
      UpdateExpandTreshold;
      if Count >= ExpandTreshold then
        Expand;
    end;
end;

function TGOrderedHashTable.NewNode: PNode;
begin
  Result := FNodeManager.NewNode;
  Inc(FCount);
end;

procedure TGOrderedHashTable.DisposeNode(aNode: PNode);
begin
  if aNode <> nil then
    begin
      aNode^ := Default(TNode);
      FNodeManager.FreeNode(aNode);
      Dec(FCount);
    end;
end;

procedure TGOrderedHashTable.ClearChainList;
var
  CurrNode, NextNode: PNode;
begin
  CurrNode := Head;
  while CurrNode <> nil do
    begin
      NextNode := CurrNode^.Next;
      CurrNode^ := Default(TNode);
      FNodeManager.DisposeNode(CurrNode);
      CurrNode := NextNode;
    end;
  FHead := nil;
  FTail := nil;
  FList := nil;
  FCount := 0;
end;

procedure TGOrderedHashTable.UpdateExpandTreshold;
begin
  if ListCapacity < MAX_CAPACITY then
    FExpandTreshold := Trunc(ListCapacity * LoadFactor)
  else
    FExpandTreshold := High(SizeInt);
end;

procedure TGOrderedHashTable.Rehash(var aTarget: TChainList);
var
  Node, CurrNode, NextNode: PNode;
  I, Mask: SizeInt;
begin
  if Count > 0 then
    begin
      Mask := System.High(aTarget);
      for Node in FList do
        begin
          CurrNode := Node;
          while CurrNode <> nil do
            begin
              I := CurrNode^.Hash and Mask;
              NextNode := CurrNode^.ChainNext;
              CurrNode^.ChainNext := aTarget[I];
              aTarget[I] := CurrNode;
              CurrNode := NextNode;
            end;
        end;
    end;
end;

procedure TGOrderedHashTable.Resize(aNewCapacity: SizeInt);
var
  NewList: TChainList;
begin
  System.SetLength(NewList, aNewCapacity);
  Rehash(NewList);
  FList := NewList;
  UpdateExpandTreshold;
end;

procedure TGOrderedHashTable.Expand;
var
  NewCapacity, OldCapacity: SizeInt;
begin
  OldCapacity := ListCapacity;
  if OldCapacity > 0 then
    begin
      NewCapacity := Math.Min(MAX_CAPACITY, OldCapacity shl 1);
      if NewCapacity > OldCapacity then
        Resize(NewCapacity);
    end
  else
    AllocList(DEFAULT_CONTAINER_CAPACITY);
end;

function TGOrderedHashTable.DoAdd(aKeyHash: SizeInt): PNode;
var
  I: SizeInt;
begin
  //add node to chain
  I := aKeyHash and System.High(FList);
  Result := NewNode;
  Result^.Hash := aKeyHash;
  Result^.ChainNext := FList[I];
  FList[I] := Result;
  //add node to inorder list
  if Head = nil then
    FHead := Result;
  if Tail <> nil then
    Tail^.Next := Result;
  Result^.Prior := Tail;
  FTail := Result;
end;

function TGOrderedHashTable.DoFind(constref aKey: TKey; aKeyHash: SizeInt): TSearchResult;
var
  CurrNode, PrevNode: PNode;
begin
  CurrNode := FList[aKeyHash and System.High(FList)];
  PrevNode := nil;
  while CurrNode <> nil do
    begin
      if (CurrNode^.Hash = aKeyHash) and TEqRel.Equal(CurrNode^.Data.Key, aKey) then
        break;
      PrevNode := CurrNode;
      CurrNode := CurrNode^.ChainNext;
    end;
  Result.Node := CurrNode;
  Result.PrevNode := PrevNode;
end;

procedure TGOrderedHashTable.RemoveNode(aNode: PNode);
var
  CurrNode, PrevNode: PNode;
  Pos: TSearchResult;
begin
  CurrNode := FList[aNode^.Hash and System.High(FList)];
  PrevNode := nil;
  while CurrNode <> nil do
    begin
      if CurrNode = aNode then
        break;
      PrevNode := CurrNode;
      CurrNode := CurrNode^.ChainNext;
    end;
  Pos.Node := CurrNode;
  Pos.PrevNode := PrevNode;
  RemoveAt(Pos);
end;

class function TGOrderedHashTable.EstimateCapacity(aCount: SizeInt; aLoadFactor: Single): SizeInt;
begin
  //aCount := Math.Min(Math.Max(aCount, 0), MAX_CAPACITY);
  if aCount > 0 then
    Result := LGUtils.RoundUpTwoPower(Math.Min(Ceil64(Double(aCount) / aLoadFactor), MAX_CAPACITY))
  else
    Result := DEFAULT_CONTAINER_CAPACITY;
end;

class function TGOrderedHashTable.DefaultLoadFactor: Single;
begin
  Result := DEFAULT_LOAD_FACTOR;
end;

class function TGOrderedHashTable.MaxLoadFactor: Single;
begin
  Result := MAX_LOAD_FACTOR;
end;

constructor TGOrderedHashTable.CreateEmpty;
begin
  inherited;
  FNodeManager := TNodeManager.Create;
end;

constructor TGOrderedHashTable.CreateEmpty(aLoadFactor: Single);
begin
  inherited CreateEmpty(aLoadFactor);
  FNodeManager := TNodeManager.Create;
end;

constructor TGOrderedHashTable.Create;
begin
  inherited Create;
  FNodeManager := TNodeManager.Create;
  FNodeManager.EnsureFreeCount(Capacity);
end;

constructor TGOrderedHashTable.Create(aCapacity: SizeInt);
begin
  inherited Create(aCapacity);
  FNodeManager := TNodeManager.Create;
  FNodeManager.EnsureFreeCount(Capacity);
end;

constructor TGOrderedHashTable.Create(aLoadFactor: Single);
begin
  inherited Create(aLoadFactor);
  FNodeManager := TNodeManager.Create;
  FNodeManager.EnsureFreeCount(Capacity);
end;

constructor TGOrderedHashTable.Create(aCapacity: SizeInt; aLoadFactor: Single);
begin
  inherited Create(aCapacity, aLoadFactor);
  FNodeManager := TNodeManager.Create;
  FNodeManager.EnsureFreeCount(Capacity);
end;

destructor TGOrderedHashTable.Destroy;
begin
  ClearChainList;
  FNodeManager.Free;
  inherited;
end;

procedure TGOrderedHashTable.Clear;
begin
  ClearChainList;
  FNodeManager.Clear;
  FExpandTreshold := 0;
end;

function TGOrderedHashTable.Clone: TCustomHashTable;
var
  CurrNode, AddedNode: PNode;
begin
  Result := TGOrderedHashTable.Create(ListCapacity, LoadFactor);
  CurrNode := FHead;
  while CurrNode <> nil do
    begin
      AddedNode := TGOrderedHashTable(Result).DoAdd(CurrNode^.Hash);
      AddedNode^.Data := CurrNode^.Data;
      CurrNode := CurrNode^.Next;
    end;
end;

procedure TGOrderedHashTable.EnsureCapacity(aValue: SizeInt);
var
  NewCapacity: SizeInt;
begin
  if aValue > ExpandTreshold then
    begin
      FNodeManager.EnsureFreeCount(aValue - Count);
      NewCapacity := EstimateCapacity(aValue, LoadFactor);
      if NewCapacity <> ListCapacity then
        Resize(NewCapacity);
    end;
end;

procedure TGOrderedHashTable.TrimToFit;
var
  NewCapacity: SizeInt;
begin
  if Count > 0 then
    begin
      NewCapacity := EstimateCapacity(Count, LoadFactor);
      if NewCapacity < ListCapacity then
        Resize(NewCapacity);
      FNodeManager.ClearFreeList;
    end
  else
    Clear;
end;

function TGOrderedHashTable.GetEnumerator: TEntryEnumerator;
begin
  Result := TEnumerator.Create(Head);
end;

function TGOrderedHashTable.GetReverseEnumerator: TReverseEnumerator;
begin
  Result := TReverseEnumerator.Create(Self);
end;

function TGOrderedHashTable.FindOrAdd(constref aKey: TKey; out e: PEntry; out aRes: TSearchResult): Boolean;
var
  h: SizeInt;
begin
  if FList = nil then
    AllocList(DEFAULT_CONTAINER_CAPACITY);
  h := TEqRel.HashCode(aKey);
  aRes := DoFind(aKey, h);
  Result := aRes.Node <> nil; // key found?
  if not Result then          // key not found
    begin
      if Count >= ExpandTreshold then
        Expand;
      aRes.Node := DoAdd(h);
    end;
  e := @PNode(aRes.Node)^.Data;
end;

function TGOrderedHashTable.Find(constref aKey: TKey; out aPos: TSearchResult): PEntry;
begin
  if Count > 0 then
    begin
      aPos := DoFind(aKey, TEqRel.HashCode(aKey));
      if aPos.Node <> nil then
        Result := @PNode(aPos.Node)^.Data
      else
        Result := nil;
    end
  else
    Result := nil;
end;

function TGOrderedHashTable.Remove(constref aKey: TKey): Boolean;
var
  sr: TSearchResult;
begin
  sr := DoFind(aKey, TEqRel.HashCode(aKey));
  Result := sr.Node <> nil;
  if Result then
    RemoveAt(sr);
end;

procedure TGOrderedHashTable.RemoveAt(constref aPos: TSearchResult);
var
  CurrNode, PrevNode: PNode;
begin
  if aPos.Node <> nil then
    begin
      PrevNode := aPos.PrevNode;
      CurrNode := aPos.Node;
      if PrevNode <> nil then  //is not head of chain
        PrevNode^.ChainNext := CurrNode^.ChainNext
      else
        FList[CurrNode^.Hash and System.High(FList)] := CurrNode^.ChainNext;

      if CurrNode^.Prior <> nil then //is not in order head
        CurrNode^.Prior^.Next := CurrNode^.Next
      else
        FHead := CurrNode^.Next;

      if CurrNode^.Next <> nil then //is not in order tail
        CurrNode^.Next^.Prior := CurrNode^.Prior;

      DisposeNode(aPos.Node);
    end;
end;
{$PUSH}{$MACRO ON}
function TGOrderedHashTable.RemoveIf(aTest: TTest; aOnRemove: TEntryEvent): SizeInt;
var
  CurrNode, NextNode: PNode;
begin
{$DEFINE TestMacro := aTest(CurrNode^.Data.Key)}
{$DEFINE RemoveIfMacro :=
  Result := 0;
  CurrNode := FHead;
  while CurrNode <> nil do
    begin
      NextNode := CurrNode^.Next;
      if TestMacro then
        begin
          if aOnRemove <> nil then
            aOnRemove(@CurrNode^.Data);
          RemoveNode(CurrNode);
          Inc(Result);
        end;
      CurrNode := NextNode;
    end}
  RemoveIfMacro;
end;

function TGOrderedHashTable.RemoveIf(aTest: TOnTest; aOnRemove: TEntryEvent): SizeInt;
var
  CurrNode, NextNode: PNode;
begin
  RemoveIfMacro;
end;

function TGOrderedHashTable.RemoveIf(aTest: TNestTest; aOnRemove: TEntryEvent): SizeInt;
var
  CurrNode, NextNode: PNode;
begin
  RemoveIfMacro;
end;

function TGOrderedHashTable.RemoveIf(aTest: TEntryTest; aOnRemove: TEntryEvent): SizeInt;
var
  CurrNode, NextNode: PNode;
begin
  {$DEFINE TestMacro := aTest(@CurrNode^.Data)}
  RemoveIfMacro;
end;
{$POP}

function TGOrderedHashTable.GetFirst: PEntry;
begin
  if Head <> nil then
    Result := @Head^.Data
  else
    Result := nil;
end;

function TGOrderedHashTable.GetLast: PEntry;
begin
  if Tail <> nil then
    Result := @Tail^.Data
  else
    Result := nil;
end;

{ TGChainHashTable.TEnumerator }

function TGChainHashTable.TEnumerator.GetCurrent: PEntry;
begin
  Result := @FCurrNode^.Data;
end;

constructor TGChainHashTable.TEnumerator.Create(aList: TChainList);
begin
  FList := aList;
  FLastIndex := High(aList);
  FCurrIndex := -1;
end;

function TGChainHashTable.TEnumerator.MoveNext: Boolean;
var
  NextNode: PNode = nil;
begin
  if FCurrNode <> nil then
    NextNode := FCurrNode^.Next;
  while NextNode = nil do
    begin
      if FCurrIndex >= FLastIndex then
        exit(False);
      Inc(FCurrIndex);
      NextNode := FList[FCurrIndex];
    end;
  FCurrNode := NextNode;
  Result := True;
end;

procedure TGChainHashTable.TEnumerator.Reset;
begin
  FCurrNode := nil;
  FCurrIndex := -1;
end;

{ TGChainHashTable }

procedure TGChainHashTable.AllocList(aCapacity: SizeInt);
begin
  if aCapacity > 0 then
    begin
      aCapacity := Math.Min(aCapacity, MAX_CAPACITY);
      if not IsTwoPower(aCapacity) then
        aCapacity := LGUtils.RoundUpTwoPower(aCapacity);
    end
  else
    aCapacity := DEFAULT_CONTAINER_CAPACITY;
  System.SetLength(FList, aCapacity);
  UpdateExpandTreshold;
end;

function TGChainHashTable.GetTableSize: SizeInt;
begin
  Result := ListCapacity;
end;

function TGChainHashTable.ListCapacity: SizeInt;
begin
  Result := System.Length(FList);
end;

procedure TGChainHashTable.SetLoadFactor(aValue: Single);
begin
  aValue := RestrictLoadFactor(aValue);
  if aValue <> LoadFactor then
    begin
      FLoadFactor := aValue;
      UpdateExpandTreshold;
      if Count >= ExpandTreshold then
        Expand;
    end;
end;

function TGChainHashTable.NewNode: PNode;
begin
  Result := FNodeManager.NewNode;
  Inc(FCount);
end;

procedure TGChainHashTable.DisposeNode(aNode: PNode);
begin
  if aNode <> nil then
    begin
      aNode^ := Default(TNode);
      FNodeManager.FreeNode(aNode);
      Dec(FCount);
    end;
end;

procedure TGChainHashTable.ClearList;
var
  Node, CurrNode, NextNode: PNode;
begin
  for Node in FList do
    begin
      CurrNode := Node;
      while CurrNode <> nil do
        begin
          NextNode := CurrNode^.Next;
          CurrNode^.Data := Default(TEntry);
          FNodeManager.DisposeNode(CurrNode);
          CurrNode := NextNode;
        end;
    end;
  FList := nil;
  FCount := 0;
end;

procedure TGChainHashTable.UpdateExpandTreshold;
begin
  if ListCapacity < MAX_CAPACITY then
    FExpandTreshold := Trunc(ListCapacity * LoadFactor)
  else
    FExpandTreshold := High(SizeInt);
end;

procedure TGChainHashTable.Rehash(var aTarget: TChainList);
var
  Node, CurrNode, NextNode: PNode;
  I, Mask: SizeInt;
begin
  if Count > 0 then
    begin
      Mask := System.High(aTarget);
      for Node in FList do
        begin
          CurrNode := Node;
          while CurrNode <> nil do
            begin
              I := CurrNode^.Hash and Mask;
              NextNode := CurrNode^.Next;
              CurrNode^.Next := aTarget[I];
              aTarget[I] := CurrNode;
              CurrNode := NextNode;
            end;
        end;
    end;
end;

procedure TGChainHashTable.Resize(aNewCapacity: SizeInt);
var
  NewList: TChainList;
begin
  System.SetLength(NewList, aNewCapacity);
  Rehash(NewList);
  FList := NewList;
  UpdateExpandTreshold;
end;

procedure TGChainHashTable.Expand;
var
  NewCapacity, OldCapacity: SizeInt;
begin
  OldCapacity := ListCapacity;
  if OldCapacity > 0 then
    begin
      NewCapacity := Math.Min(MAX_CAPACITY, OldCapacity shl 1);
      if NewCapacity > OldCapacity then
        Resize(NewCapacity);
    end
  else
    AllocList(DEFAULT_CONTAINER_CAPACITY);
end;

function TGChainHashTable.DoAdd(aKeyHash: SizeInt): PNode;
var
  I: SizeInt;
begin
  //add node to chain
  I := aKeyHash and System.High(FList);
  Result := NewNode;
  Result^.Hash := aKeyHash;
  Result^.Next := FList[I];
  FList[I] := Result;
end;

function TGChainHashTable.DoFind(constref aKey: TKey; aKeyHash: SizeInt): TSearchResult;
var
  CurrNode, PrevNode: PNode;
begin
  CurrNode := FList[aKeyHash and System.High(FList)];
  PrevNode := nil;

  while CurrNode <> nil do
    begin
      if (CurrNode^.Hash = aKeyHash) and TEqRel.Equal(CurrNode^.Data.Key, aKey) then
        break;
      PrevNode := CurrNode;
      CurrNode := CurrNode^.Next;
    end;

  Result.Node := CurrNode;
  Result.PrevNode := PrevNode;
end;

procedure TGChainHashTable.DoRemove(constref aPos: TSearchResult);
begin
  if aPos.Node <> nil then
    begin
      if aPos.PrevNode <> nil then  //is not head of chain
        PNode(aPos.PrevNode)^.Next := PNode(aPos.Node)^.Next
      else
        FList[PNode(aPos.Node)^.Hash and System.High(FList)] := PNode(aPos.Node)^.Next;
      DisposeNode(aPos.Node);
    end;
end;

class function TGChainHashTable.EstimateCapacity(aCount: SizeInt; aLoadFactor: Single): SizeInt;
begin
  //aCount := Math.Min(Math.Max(aCount, 0), MAX_CAPACITY);
  if aCount > 0 then
    Result := LGUtils.RoundUpTwoPower(Math.Min(Ceil64(Double(aCount) / aLoadFactor), MAX_CAPACITY))
  else
    Result := DEFAULT_CONTAINER_CAPACITY;
end;

class function TGChainHashTable.DefaultLoadFactor: Single;
begin
  Result := DEFAULT_LOAD_FACTOR;
end;

class function TGChainHashTable.MaxLoadFactor: Single;
begin
  Result := MAX_LOAD_FACTOR;
end;

constructor TGChainHashTable.CreateEmpty;
begin
  inherited CreateEmpty;
  FNodeManager := TNodeManager.Create;
end;

constructor TGChainHashTable.CreateEmpty(aLoadFactor: Single);
begin
  inherited CreateEmpty(aLoadFactor);
  FNodeManager := TNodeManager.Create;
end;

constructor TGChainHashTable.Create;
begin
  inherited Create;
  FNodeManager := TNodeManager.Create;
  FNodeManager.EnsureFreeCount(Capacity);
end;

constructor TGChainHashTable.Create(aCapacity: SizeInt);
begin
  inherited Create(aCapacity);
  FNodeManager := TNodeManager.Create;
  FNodeManager.EnsureFreeCount(Capacity);
end;

constructor TGChainHashTable.Create(aLoadFactor: Single);
begin
  inherited Create(aLoadFactor);
  FNodeManager := TNodeManager.Create;
  FNodeManager.EnsureFreeCount(Capacity);
end;

constructor TGChainHashTable.Create(aCapacity: SizeInt; aLoadFactor: Single);
begin
  inherited Create(aCapacity, aLoadFactor);
  FNodeManager := TNodeManager.Create;
  FNodeManager.EnsureFreeCount(Capacity);
end;

destructor TGChainHashTable.Destroy;
begin
  ClearList;
  FNodeManager.Free;
  inherited;
end;

procedure TGChainHashTable.Clear;
begin
  ClearList;
  FNodeManager.Clear;
  FExpandTreshold := 0;
end;

function TGChainHashTable.Clone: TCustomHashTable;
var
  AddedNode, CurrNode: PNode;
  I: SizeInt;
begin
  Result := TGChainHashTable.Create(ListCapacity, LoadFactor);
  for I := 0 to System.High(FList) do
    begin
      CurrNode := FList[I];
      while CurrNode <> nil do
        begin
          AddedNode := TGChainHashTable(Result).DoAdd(CurrNode^.Hash);
          AddedNode^.Data := CurrNode^.Data;
          CurrNode := CurrNode^.Next;
        end;
    end;
end;

procedure TGChainHashTable.EnsureCapacity(aValue: SizeInt);
var
  NewCapacity: SizeInt;
begin
  if aValue > ExpandTreshold then
    begin
      FNodeManager.EnsureFreeCount(aValue - Count);
      NewCapacity := EstimateCapacity(aValue, LoadFactor);
      if NewCapacity <> ListCapacity then
        Resize(NewCapacity);
    end;
end;

procedure TGChainHashTable.TrimToFit;
var
  NewCapacity: SizeInt;
begin
  if Count > 0 then
    begin
      NewCapacity := EstimateCapacity(Count, LoadFactor);
      if NewCapacity < ListCapacity then
        Resize(NewCapacity);
      FNodeManager.ClearFreeList;
    end
  else
    Clear;
end;

function TGChainHashTable.GetEnumerator: TEntryEnumerator;
begin
  Result := TEnumerator.Create(FList);
end;

function TGChainHashTable.FindOrAdd(constref aKey: TKey; out e: PEntry; out aRes: TSearchResult): Boolean;
var
  h: SizeInt;
begin
  if FList = nil then
    AllocList(DEFAULT_CONTAINER_CAPACITY);
  h := TEqRel.HashCode(aKey);
  aRes := DoFind(aKey, h);
  Result := aRes.Node <> nil; // key found ?
  if not Result then          // key not found
    begin
      if Count >= ExpandTreshold then
        Expand;
      aRes.Node := DoAdd(h);
    end;
  e := @PNode(aRes.Node)^.Data;
end;

function TGChainHashTable.Find(constref aKey: TKey; out aPos: TSearchResult): PEntry;
begin
  if Count > 0 then
    begin
      aPos := DoFind(aKey, TEqRel.HashCode(aKey));
      if aPos.Node <> nil then
        Result := @PNode(aPos.Node)^.Data
      else
        Result := nil;
    end
  else
    Result := nil;
end;

function TGChainHashTable.Add(constref aKey: TKey): PNode;
begin
  if FList = nil then
    AllocList(DEFAULT_CONTAINER_CAPACITY);
  if Count >= ExpandTreshold then
    Expand;
  Result := DoAdd(TEqRel.HashCode(aKey));
end;

function TGChainHashTable.Remove(constref aKey: TKey): Boolean;
var
  p: TSearchResult;
begin
  p := DoFind(aKey, TEqRel.HashCode(aKey));
  Result := p.Node <> nil;
  if Result then
    DoRemove(p);
end;

procedure TGChainHashTable.RemoveAt(constref aPos: TSearchResult);
begin
  DoRemove(aPos);
end;
{$PUSH}{$MACRO ON}
function TGChainHashTable.RemoveIf(aTest: TTest; aOnRemove: TEntryEvent): SizeInt;
var
  PrevNode, CurrNode, NextNode: PNode;
  I: SizeInt;
begin
{$DEFINE TestMacro := aTest(CurrNode^.Data.Key)}
{$DEFINE RemoveIfMacro :=
  Result := 0;
  if Count > 0 then
    for I := 0 to Pred(ListCapacity) do
      begin
        CurrNode := FList[I];
        PrevNode := nil;
        while CurrNode <> nil do
          begin
            NextNode := CurrNode^.Next;
            if TestMacro then
              begin
                if PrevNode <> nil then
                  PrevNode^.Next := NextNode
                else
                  FList[I] := NextNode;
                if aOnRemove <> nil then
                  aOnRemove(@CurrNode^.Data);
                DisposeNode(CurrNode);
                Inc(Result);
              end
            else
              PrevNode := CurrNode;
            CurrNode := NextNode;
          end;
      end}
  RemoveIfMacro;
end;

function TGChainHashTable.RemoveIf(aTest: TOnTest; aOnRemove: TEntryEvent): SizeInt;
var
  PrevNode, CurrNode, NextNode: PNode;
  I: SizeInt;
begin
  RemoveIfMacro;
end;

function TGChainHashTable.RemoveIf(aTest: TNestTest; aOnRemove: TEntryEvent): SizeInt;
var
  PrevNode, CurrNode, NextNode: PNode;
  I: SizeInt;
begin
  RemoveIfMacro;
end;

function TGChainHashTable.RemoveIf(aTest: TEntryTest; aOnRemove: TEntryEvent): SizeInt;
var
  PrevNode, CurrNode, NextNode: PNode;
  I: SizeInt;
begin
  {$DEFINE TestMacro := aTest(@CurrNode^.Data)}
  RemoveIfMacro;
end;
{$POP}

{ TGHashTableLP.TEnumerator }

function TGHashTableLP.TEnumerator.GetCurrent: PEntry;
begin
  Result := @FList[FCurrIndex].Data;
end;

procedure TGHashTableLP.TEnumerator.Init(aList: TNodeList);
begin
  FList := aList;
  FLastIndex := System.High(aList);
  FCurrIndex := -1;
end;

function TGHashTableLP.TEnumerator.MoveNext: Boolean;
begin
  repeat
    if FCurrIndex >= FLastIndex then
      exit(False);
    Inc(FCurrIndex);
    Result := FList[FCurrIndex].Hash <> 0;
  until Result;
end;

procedure TGHashTableLP.TEnumerator.Reset;
begin
  FCurrIndex := -1;
end;

{ TGHashTableLP }

function TGHashTableLP.RestrictLoadFactor(aValue: Single): Single;
begin
  Result := Math.Min(Math.Max(aValue, MIN_LOAD_FACTOR), MAX_LOAD_FACTOR);
end;

function TGHashTableLP.ListCapacity: SizeInt;
begin
  Result := System.Length(FList);
end;

procedure TGHashTableLP.UpdateExpandTreshold;
begin
  if ListCapacity < MAX_CAPACITY then
    FExpandTreshold := Trunc(ListCapacity * LoadFactor)
  else
    FExpandTreshold := MAX_CAPACITY;
end;

procedure TGHashTableLP.SetLoadFactor(aValue: Single);
begin
  aValue := RestrictLoadFactor(aValue);
  if aValue <> LoadFactor then
    begin
      FLoadFactor := aValue;
      UpdateExpandTreshold;
      if Count >= ExpandTreshold then
        Expand;
    end;
end;

function TGHashTableLP.GetCapacity: SizeInt;
begin
  Result := Trunc(ListCapacity * LoadFactor);
end;

function TGHashTableLP.GetFillRatio: Single;
var
  c: SizeInt;
begin
  c := ListCapacity;
  if c > 0 then
    Result := Count / c
  else
    Result := 0.0;
end;

function TGHashTableLP.GetTableSize: SizeInt;
begin
  Result := ListCapacity;
end;

procedure TGHashTableLP.AllocList(aCapacity: SizeInt);
begin
  if aCapacity > 0 then
    begin
      aCapacity := Math.Min(aCapacity, MAX_CAPACITY);
      if not IsTwoPower(aCapacity) then
        aCapacity := LGUtils.RoundUpTwoPower(aCapacity);
    end
  else
    aCapacity := DEFAULT_CONTAINER_CAPACITY;
  FList := NewList(aCapacity);
  UpdateExpandTreshold;
end;

procedure TGHashTableLP.Rehash(var aTarget: TNodeList);
var
  h, I, J, Mask: SizeInt;
begin
  if Count > 0 then
    begin
      Mask := System.High(aTarget);
      for I := 0 to System.High(FList) do
        begin
          if FList[I].Hash <> 0 then
            begin
              h := FList[I].Hash and Mask;
              for J := 0 to Mask do
                begin
                  if aTarget[h].Hash = 0 then // -> target node is empty
                    begin
                      TFakeNode(aTarget[h]) := TFakeNode(FList[I]);
                      TFakeEntry(FList[I].Data) := Default(TFakeEntry);
                      break;
                    end;
                  h := Succ(h) and Mask;     // probe sequence
                end;
            end;
        end;
    end;
end;

procedure TGHashTableLP.Resize(aNewCapacity: SizeInt);
var
  List: TNodeList;
begin
  List := NewList(aNewCapacity);
  Rehash(List);
  FList := List;
  UpdateExpandTreshold;
end;

procedure TGHashTableLP.Expand;
var
  NewCapacity, OldCapacity: SizeInt;
begin
  OldCapacity := ListCapacity;
  if OldCapacity > 0 then
    begin
      NewCapacity := Math.Min(MAX_CAPACITY, OldCapacity shl 1);
      if NewCapacity > OldCapacity then
        Resize(NewCapacity);
    end
  else
    AllocList(DEFAULT_CONTAINER_CAPACITY);
end;

function TGHashTableLP.DoFind(constref aKey: TKey; aKeyHash: SizeInt): SizeInt;
var
  I, Pos, h, Mask: SizeInt;
begin
  Mask := System.High(FList);
  aKeyHash := aKeyHash or USED_FLAG;
  Result := SLOT_NOT_FOUND;
  Pos := aKeyHash and Mask;
  for I := 0 to Mask do
    begin
      h := FList[Pos].Hash;
      if h = 0 then               // node empty => key not found
        exit(not Pos)
      else
        if (h = aKeyHash) and TEqRel.Equal(FList[Pos].Data.Key, aKey) then
          exit(Pos);              // key found
      Pos := Succ(Pos) and Mask;  // probe sequence
    end;
end;

procedure TGHashTableLP.DoRemove(aIndex: SizeInt);
var
  I, h, Gap, Mask: SizeInt;
begin
  Mask := System.High(FList);
  FList[aIndex].Hash := 0;
  FList[aIndex].Data := Default(TEntry);
  Gap := aIndex;
  aIndex := Succ(aIndex) and Mask;
  Dec(FCount);
  for I := 0 to Mask do
    begin
      h := FList[aIndex].Hash;
      if h <> 0 then
        begin
          h := h and Mask;
          if (h <> aIndex) and (Succ(aIndex - h + Mask) and Mask >= Succ(aIndex - Gap + Mask) and Mask) then
            begin
              TFakeNode(FList[Gap]) := TFakeNode(FList[aIndex]);
              TFakeNode(FList[aIndex]) := Default(TFakeNode);
              Gap := aIndex;
            end;
          aIndex := Succ(aIndex) and Mask;
        end
      else
        break;
    end;
end;

class function TGHashTableLP.NewList(aCapacity: SizeInt): TNodeList;
begin
  System.SetLength(Result, aCapacity);
  System.FillChar(Result[0], aCapacity * NODE_SIZE, 0);
end;

class function TGHashTableLP.EstimateCapacity(aCount: SizeInt; aLoadFactor: Single): SizeInt;
begin
  if aCount > 0 then
    Result := LGUtils.RoundUpTwoPower(Math.Min(Ceil64(Double(aCount) / aLoadFactor), MAX_CAPACITY))
  else
    Result := DEFAULT_CONTAINER_CAPACITY;
end;

class constructor TGHashTableLP.Init;
begin
{$PUSH}{$J+}
  MAX_CAPACITY := LGUtils.RoundUpTwoPower(MAX_CAPACITY);
{$POP}
end;

class function TGHashTableLP.DefaultLoadFactor: Single;
begin
  Result := DEFAULT_LOAD_FACTOR;
end;

class function TGHashTableLP.MaxLoadFactor: Single;
begin
  Result := MAX_LOAD_FACTOR;
end;

class function TGHashTableLP.MinLoadFactor: Single;
begin
  Result := MIN_LOAD_FACTOR;
end;

constructor TGHashTableLP.CreateEmpty;
begin
  FLoadFactor := DefaultLoadFactor;
end;

constructor TGHashTableLP.CreateEmpty(aLoadFactor: Single);
begin
  FLoadFactor := RestrictLoadFactor(aLoadFactor);
end;

constructor TGHashTableLP.Create;
begin
  FLoadFactor := DefaultLoadFactor;
  AllocList(DEFAULT_CONTAINER_CAPACITY);
end;

constructor TGHashTableLP.Create(aCapacity: SizeInt);
begin
  FLoadFactor := DefaultLoadFactor;
  AllocList(EstimateCapacity(aCapacity, LoadFactor));
end;

constructor TGHashTableLP.Create(aLoadFactor: Single);
begin
  FLoadFactor := RestrictLoadFactor(aLoadFactor);
  AllocList(DEFAULT_CONTAINER_CAPACITY);
end;

constructor TGHashTableLP.Create(aCapacity: SizeInt; aLoadFactor: Single);
begin
  FLoadFactor := RestrictLoadFactor(aLoadFactor);
  AllocList(EstimateCapacity(aCapacity, LoadFactor));
end;

function TGHashTableLP.GetEnumerator: TEnumerator;
begin
  Result.Init(FList);
end;

procedure TGHashTableLP.Clear;
begin
  FList := nil;
  FCount := 0;
  FExpandTreshold := 0;
end;

function TGHashTableLP.EnsureCapacity(aValue: SizeInt): Boolean;
var
  NewCapacity: SizeInt;
begin
  if aValue > ExpandTreshold then
    begin
      NewCapacity := EstimateCapacity(aValue, LoadFactor);
      if NewCapacity <> ListCapacity then
        try
          Resize(NewCapacity);
          Result := True;
        except
          Result := False;
        end;
    end
  else
    Result := True;
end;

procedure TGHashTableLP.TrimToFit;
var
  NewCapacity: SizeInt;
begin
  if Count > 0 then
    begin
      NewCapacity := EstimateCapacity(Count, LoadFactor);
      if NewCapacity < ListCapacity then
        Resize(NewCapacity);
    end
  else
    Clear;
end;

function TGHashTableLP.FindOrAdd(constref aKey: TKey; out e: PEntry; out aPos: SizeInt): Boolean;
var
  h: SizeInt;
begin
  if FList = nil then
    AllocList(DEFAULT_CONTAINER_CAPACITY);
  h := TEqRel.HashCode(aKey);
  aPos := DoFind(aKey, h);
  Result := aPos >= 0; // key found?
  if not Result then   // key not found, will add new slot
    begin
      if Count >= ExpandTreshold then
        begin
          Expand;
          aPos := DoFind(aKey, h);
        end;
      if aPos > SLOT_NOT_FOUND then
        begin
          aPos := not aPos;
          FList[aPos].Hash := h or USED_FLAG;
          Inc(FCount);
        end
      else
        raise ELGCapacityExceed.CreateFmt(SEClassCapacityExceedFmt, [ClassName, Succ(Count)]);
    end;
  e := @FList[aPos].Data;
end;

function TGHashTableLP.Find(constref aKey: TKey; out aPos: SizeInt): PEntry;
begin
  Result := nil;
  if Count > 0 then
    begin
      aPos := DoFind(aKey, TEqRel.HashCode(aKey));
      if aPos >= 0 then
        Result := @FList[aPos].Data;
    end;
end;

function TGHashTableLP.Remove(constref aKey: TKey): Boolean;
var
  Pos: SizeInt;
begin
  if Count > 0 then
    begin
      Pos := DoFind(aKey, TEqRel.HashCode(aKey));
      Result := Pos >= 0;
      if Result then
        DoRemove(Pos);
    end
  else
    Result := False;
end;

procedure TGHashTableLP.RemoveAt(constref aPos: SizeInt);
begin
  if (aPos >= 0) and (aPos <= System.High(FList)) then
    DoRemove(aPos);
end;

{ TGLiteHashTableLP.TEnumerator }

function TGLiteHashTableLP.TEnumerator.GetCurrent: PEntry;
begin
  Result := @FList[FCurrIndex].Data;
end;

procedure TGLiteHashTableLP.TEnumerator.Init(aList: TNodeList);
begin
  FList := aList;
  FLastIndex := System.High(aList);
  FCurrIndex := -1;
end;

function TGLiteHashTableLP.TEnumerator.MoveNext: Boolean;
begin
  repeat
    if FCurrIndex >= FLastIndex then
      exit(False);
    Inc(FCurrIndex);
    Result := FList[FCurrIndex].Hash <> 0;
  until Result;
end;

procedure TGLiteHashTableLP.TEnumerator.Reset;
begin
  FCurrIndex := -1;
end;

{ TGLiteHashTableLP.TRemovableEnumerator }

function TGLiteHashTableLP.TRemovableEnumerator.GetCurrent: PEntry;
begin
  Result := FEnum.Current;
end;

procedure TGLiteHashTableLP.TRemovableEnumerator.Init(aTable: PLiteHashTableLP);
begin
  FTable := aTable;
  FEnum := aTable^.GetEnumerator;
end;

function TGLiteHashTableLP.TRemovableEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGLiteHashTableLP.TRemovableEnumerator.RemoveCurrent;
begin
  FTable^.DoRemove(FEnum.FCurrIndex);
  Dec(FEnum.FCurrIndex);
end;

procedure TGLiteHashTableLP.TRemovableEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGLiteHashTableLP }

function TGLiteHashTableLP.RestrictLoadFactor(aValue: Single): Single;
begin
  Result := Math.Min(Math.Max(aValue, MIN_LOAD_FACTOR), MAX_LOAD_FACTOR);
end;

procedure TGLiteHashTableLP.UpdateExpandTreshold;
begin
  if Size < MAX_CAPACITY then
    FExpandTreshold := Trunc(Size * LoadFactor)
  else
    FExpandTreshold := MAX_CAPACITY;
end;

procedure TGLiteHashTableLP.SetLoadFactor(aValue: Single);
begin
  aValue := RestrictLoadFactor(aValue);
  if aValue <> LoadFactor then
    begin
      FLoadFactor := aValue;
      UpdateExpandTreshold;
      if Count >= ExpandTreshold then
        Expand;
    end;
end;

function TGLiteHashTableLP.GetCapacity: SizeInt;
begin
  Result := ExpandTreshold;
end;

function TGLiteHashTableLP.GetFillRatio: Single;
var
  sz: SizeInt;
begin
  sz := Size;
  if sz > 0 then
    Result := Count / sz
  else
    Result := 0.0;
end;

function TGLiteHashTableLP.GetSize: SizeInt;
begin
  Result := System.Length(FList);
end;

procedure TGLiteHashTableLP.AllocList(aCapacity: SizeInt);
begin
  if aCapacity > 0 then
    begin
      aCapacity := Math.Min(aCapacity, MAX_CAPACITY);
      if not IsTwoPower(aCapacity) then
        aCapacity := LGUtils.RoundUpTwoPower(aCapacity);
    end
  else
    aCapacity := DEFAULT_CONTAINER_CAPACITY;
  FList := NewList(aCapacity);
  UpdateExpandTreshold;
end;

procedure TGLiteHashTableLP.Rehash(var aTarget: TNodeList);
var
  h, I, J, Mask: SizeInt;
begin
  if Count > 0 then
    begin
      Mask := System.High(aTarget);
      for I := 0 to System.High(FList) do
        begin
          if FList[I].Hash <> 0 then
            begin
              h := FList[I].Hash and Mask;
              for J := 0 to Mask do
                begin
                  if aTarget[h].Hash = 0 then // -> target node is empty
                    begin
                      TFakeNode(aTarget[h]) := TFakeNode(FList[I]);
                      TFakeEntry(FList[I].Data) := Default(TFakeEntry);
                      break;
                    end;
                  h := Succ(h) and Mask;     // probe sequence
                end;
            end;
        end;
    end;
end;

procedure TGLiteHashTableLP.Resize(aNewCapacity: SizeInt);
var
  List: TNodeList;
begin
  List := NewList(aNewCapacity);
  Rehash(List);
  FList := List;
  UpdateExpandTreshold;
end;

procedure TGLiteHashTableLP.Expand;
var
  NewSize, OldSize: SizeInt;
begin
  OldSize := Size;
  if OldSize > 0 then
    begin
      NewSize := Math.Min(MAX_CAPACITY, OldSize shl 1);
      if NewSize > OldSize then
        Resize(NewSize);
    end
  else
    AllocList(DEFAULT_CONTAINER_CAPACITY);
end;

function TGLiteHashTableLP.DoFind(constref aKey: TKey; aKeyHash: SizeInt): SizeInt;
var
  I, Pos, h, Mask: SizeInt;
begin
  Mask := System.High(FList);
  aKeyHash := aKeyHash or USED_FLAG;
  Result := SLOT_NOT_FOUND;
  Pos := aKeyHash and Mask;
  for I := 0 to Mask do
    begin
      h := FList[Pos].Hash;
      if h = 0 then               // node empty => key not found
        exit(not Pos)
      else
        if (h = aKeyHash) and TEqRel.Equal(FList[Pos].Data.Key, aKey) then
          exit(Pos);              // key found
      Pos := Succ(Pos) and Mask;  // probe sequence
    end;
end;

procedure TGLiteHashTableLP.DoRemove(aIndex: SizeInt);
var
  I, h, Gap, Mask: SizeInt;
begin
  Mask := System.High(FList);
  FList[aIndex].Hash := 0;
  FList[aIndex].Data := Default(TEntry);
  Gap := aIndex;
  aIndex := Succ(aIndex) and Mask;
  Dec(FCount);
  for I := 0 to Mask do
    begin
      h := FList[aIndex].Hash;
      if h <> 0 then
        begin
          h := h and Mask;
          if (h <> aIndex) and (Succ(aIndex - h + Mask) and Mask >= Succ(aIndex - Gap + Mask) and Mask) then
            begin
              TFakeNode(FList[Gap]) := TFakeNode(FList[aIndex]);
              TFakeNode(FList[aIndex]) := Default(TFakeNode);
              Gap := aIndex;
            end;
          aIndex := Succ(aIndex) and Mask;
        end
      else
        break;
    end;
end;

class function TGLiteHashTableLP.NewList(aCapacity: SizeInt): TNodeList;
begin
  System.SetLength(Result, aCapacity);
  System.FillChar(Result[0], aCapacity * NODE_SIZE, 0);
end;

class function TGLiteHashTableLP.EstimateSize(aCount: SizeInt; aLoadFactor: Single): SizeInt;
begin
  if aCount > 0 then
    Result := LGUtils.RoundUpTwoPower(Math.Min(Ceil64(Double(aCount) / aLoadFactor), MAX_CAPACITY))
  else
    Result := DEFAULT_CONTAINER_CAPACITY;
end;

class constructor TGLiteHashTableLP.Init;
begin
{$PUSH}{$J+}
  MAX_CAPACITY := LGUtils.RoundUpTwoPower(MAX_CAPACITY);
{$POP}
end;

class operator TGLiteHashTableLP.Initialize(var ht: TGLiteHashTableLP);
begin
  ht.FList := nil;
  ht.FCount := 0;
  ht.FExpandTreshold := 0;
  ht.FLoadFactor := DEFAULT_LOAD_FACTOR;
end;

class operator TGLiteHashTableLP.Finalize(var ht: TGLiteHashTableLP);
begin
  ht.Clear;
end;

class operator TGLiteHashTableLP.Copy(constref aSrc: TGLiteHashTableLP; var aDst: TGLiteHashTableLP);
begin
  aDst.FList := System.Copy(aSrc.FList);
end;

function TGLiteHashTableLP.GetEnumerator: TEnumerator;
begin
  Result.Init(FList);
end;

function TGLiteHashTableLP.RemovableEnumerator: TRemovableEnumerator;
begin
  Result.Init(@Self);
end;

procedure TGLiteHashTableLP.Clear;
begin
  FList := nil;
  FCount := 0;
  FExpandTreshold := 0;
end;

procedure TGLiteHashTableLP.EnsureCapacity(aValue: SizeInt);
var
  NewSize: SizeInt;
begin
  if aValue > ExpandTreshold then
    begin
      NewSize := EstimateSize(aValue, LoadFactor);
      if NewSize <> Size then
        Resize(NewSize);
    end;
end;

procedure TGLiteHashTableLP.TrimToFit;
var
  NewSize: SizeInt;
begin
  if Count > 0 then
    begin
      NewSize := EstimateSize(Count, LoadFactor);
      if NewSize < Size then
        Resize(NewSize);
    end
  else
    Clear;
end;

function TGLiteHashTableLP.FindOrAdd(constref aKey: TKey; out e: PEntry; out aPos: SizeInt): Boolean;
var
  h: SizeInt;
begin
  if FList = nil then
    AllocList(DEFAULT_CONTAINER_CAPACITY);
  h := TEqRel.HashCode(aKey);
  aPos := DoFind(aKey, h);
  Result := aPos >= 0; // key found?
  if not Result then   // key not found, will add new slot
    begin
      if Count >= ExpandTreshold then
        begin
          Expand;
          aPos := DoFind(aKey, h);
        end;
      if aPos > SLOT_NOT_FOUND then
        begin
          aPos := not aPos;
          FList[aPos].Hash := h or USED_FLAG;
          Inc(FCount);
        end
      else
        raise ELGCapacityExceed.CreateFmt(SECapacityExceedFmt, [Succ(Count)]);
    end;
  e := @FList[aPos].Data;
end;

function TGLiteHashTableLP.Find(constref aKey: TKey; out aPos: SizeInt): PEntry;
begin
  Result := nil;
  if Count > 0 then
    begin
      aPos := DoFind(aKey, TEqRel.HashCode(aKey));
      if aPos >= 0 then
        Result := @FList[aPos].Data;
    end;
end;

function TGLiteHashTableLP.Remove(constref aKey: TKey): Boolean;
var
  Pos: SizeInt;
begin
  if Count > 0 then
    begin
      Pos := DoFind(aKey, TEqRel.HashCode(aKey));
      Result := Pos >= 0;
      if Result then
        DoRemove(Pos);
    end
  else
    Result := False;
end;

procedure TGLiteHashTableLP.RemoveAt(constref aPos: SizeInt);
begin
  if (aPos >= 0) and (aPos <= System.High(FList)) then
    DoRemove(aPos);
end;

{ TGLiteHashList.TEnumerator }

function TGLiteHashList.TEnumerator.GetCurrent: T;
begin
  Result := FList[FCurrIndex].Data;
end;

procedure TGLiteHashList.TEnumerator.Init(constref aList: TGLiteHashList);
begin
  FList := aList.FNodeList;
  FLastIndex := System.High(FList);
  FCurrIndex := -1;
end;

function TGLiteHashList.TEnumerator.MoveNext: Boolean;
begin
  Result := FCurrIndex < FLastIndex;
  FCurrIndex += Ord(Result);
end;

procedure TGLiteHashList.TEnumerator.Reset;
begin
  FCurrIndex := -1;
end;

{ TGLiteHashList.TReverseEnumerator }

function TGLiteHashList.TReverseEnumerator.GetCurrent: T;
begin
  Result := FList[FCurrIndex].Data;//
end;

procedure TGLiteHashList.TReverseEnumerator.Init(constref aList: TGLiteHashList);
begin
  FList := aList.FNodeList;
  FCount := aList.Count;
  FCurrIndex := FCount;
end;

function TGLiteHashList.TReverseEnumerator.MoveNext: Boolean;
begin
  Result := FCurrIndex > 0;
  FCurrIndex -= Ord(Result);
end;

procedure TGLiteHashList.TReverseEnumerator.Reset;
begin
  FCurrIndex := FCount;
end;

{ TGLiteHashList.TReverse }

procedure TGLiteHashList.TReverse.Init(aList: PLiteHashList);
begin
  FList := aList;
end;

function TGLiteHashList.TReverse.GetEnumerator: TReverseEnumerator;
begin
  Result := FList^.GetReverseEnumerator;
end;

{ TGLiteHashList }

function TGLiteHashList.GetCapacity: SizeInt;
begin
  Result := System.Length(FNodeList);
end;

function TGLiteHashList.GetItem(aIndex: SizeInt): T;
begin
  CheckIndexRange(aIndex);
  Result := FNodeList[aIndex].Data;
end;

procedure TGLiteHashList.SetItem(aIndex: SizeInt; const aValue: T);
var
  I: SizeInt;
begin
  CheckIndexRange(aIndex);
  if TEqRel.Equal(aValue, FNodeList[aIndex].Data) then
    exit;
  RemoveFromChain(aIndex);
  //add to new chain
  FNodeList[aIndex].Data := aValue;
  FNodeList[aIndex].Hash := TEqRel.HashCode(aValue);
  I := FNodeList[aIndex].Hash and Pred(Capacity);
  FNodeList[aIndex].Next := FHashList[I];
  FHashList[I] := aIndex;
end;

procedure TGLiteHashList.InitialAlloc;
begin
  if Capacity = 0 then
    begin
      System.SetLength(FNodeList, DEFAULT_CONTAINER_CAPACITY);
      FHashList := NewHashList(DEFAULT_CONTAINER_CAPACITY);
    end;
end;

procedure TGLiteHashList.Rehash;
var
  I, J, Mask: SizeInt;
begin
  Mask := Pred(Capacity);
  for I := 0 to Pred(Count) do
    begin
      J := FNodeList[I].Hash and Mask;
      FNodeList[I].Next := FHashList[J];
      FHashList[J] := I;
    end;
end;

procedure TGLiteHashList.Resize(aNewCapacity: SizeInt);
begin
  System.SetLength(FNodeList, aNewCapacity);
  FHashList := NewHashList(aNewCapacity);
  Rehash;
end;

procedure TGLiteHashList.Expand;
var
  NewSize, OldSize: SizeInt;
begin
  OldSize := Capacity;
  if OldSize > 0 then
    begin
      NewSize := Math.Min(MAX_CAPACITY, OldSize shl 1);
      if NewSize > OldSize then
        Resize(NewSize);
    end
  else
    InitialAlloc;
end;

function TGLiteHashList.Find(constref aValue: T): SizeInt;
var
  h, I: SizeInt;
begin
  h := TEqRel.HashCode(aValue);
  I := FHashList[h and Pred(Capacity)];
  while I <> NULL_INDEX do
    begin
      if (FNodeList[I].Hash = h) and TEqRel.Equal(FNodeList[I].Data, aValue) then
        exit(I);
      I := FNodeList[I].Next;
    end;
  Result := NULL_INDEX;
end;

function TGLiteHashList.GetCount(constref aValue: T): SizeInt;
var
  h, I: SizeInt;
begin
  h := TEqRel.HashCode(aValue);
  I := FHashList[h and Pred(Capacity)];
  Result := 0;
  while I <> NULL_INDEX do
    begin
      if (FNodeList[I].Hash = h) and TEqRel.Equal(FNodeList[I].Data, aValue) then
        Inc(Result);
      I := FNodeList[I].Next;
    end;
end;

function TGLiteHashList.DoAdd(constref aValue: T): SizeInt;
var
  I: SizeInt;
begin
  FNodeList[Count].Hash := TEqRel.HashCode(aValue);
  I := FNodeList[Count].Hash and Pred(Capacity);
  FNodeList[Count].Data := aValue;
  FNodeList[Count].Next := FHashList[I];
  FHashList[I] := Count;
  Inc(FCount);
end;

procedure TGLiteHashList.DoInsert(aIndex: SizeInt; constref aValue: T);
begin
  System.Move(FNodeList[aIndex], FNodeList[Succ(aIndex)], (Count - aIndex) * NODE_SIZE);
  System.FillChar(FNodeList[aIndex].Data, SizeOf(T), 0);
  FNodeList[aIndex].Hash := TEqRel.HashCode(aValue);
  FNodeList[aIndex].Data := aValue;
  Inc(FCount);
  System.FillChar(FHashList[0], Capacity * SizeOf(SizeInt), $ff);
  Rehash;
end;

procedure TGLiteHashList.DoDelete(aIndex: SizeInt);
begin
  Dec(FCount);
  FNodeList[aIndex].Data := Default(T);
  System.Move(FNodeList[Succ(aIndex)], FNodeList[aIndex], (Count - aIndex) * NODE_SIZE);
  System.FillChar(FNodeList[Count].Data, SizeOf(T), 0);
  System.FillChar(FHashList[0], Capacity * SizeOf(SizeInt), $ff);
  Rehash;
end;

procedure TGLiteHashList.RemoveFromChain(aIndex: SizeInt);
var
  I, Curr, Prev: SizeInt;
begin
  I := FNodeList[aIndex].Hash and Pred(Capacity);
  Curr := FHashList[I];
  Prev := NULL_INDEX;
  while Curr <> NULL_INDEX do
    begin
      if Curr = aIndex then
        begin
          if Prev <> NULL_INDEX then
            FNodeList[Prev].Next := FNodeList[Curr].Next
          else
            FHashList[I] := FNodeList[Curr].Next;
          exit;
        end;
      Prev := Curr;
      Curr := FNodeList[Curr].Next;
    end;
end;

function TGLiteHashList.DoRemove(constref aValue: T): Boolean;
var
  Removed: SizeInt;
begin
  Removed := Find(aValue);
  Result := Removed >= 0;
  if Result then
    DoDelete(Removed);
end;

function TGLiteHashList.GetReverseEnumerator: TReverseEnumerator;
begin
  Result.Init(Self);
end;

function TGLiteHashList.IndexInRange(aIndex: SizeInt): Boolean;
begin
  Result := (aIndex >= 0) and (aIndex < Count);
end;

function TGLiteHashList.IndexInInsertRange(aIndex: SizeInt): Boolean;
begin
  Result := (aIndex >= 0) and (aIndex <= Count);
end;

procedure TGLiteHashList.CheckIndexRange(aIndex: SizeInt);
begin
  if not IndexInRange(aIndex) then
    IndexOutOfBoundError(aIndex);
end;

procedure TGLiteHashList.CheckInsertIndexRange(aIndex: SizeInt);
begin
  if not IndexInInsertRange(aIndex) then
    IndexOutOfBoundError(aIndex);
end;

class constructor TGLiteHashList.Init;
begin
{$PUSH}{$J+}
  MAX_CAPACITY := LGUtils.RoundUpTwoPower(MAX_CAPACITY);
{$POP}
end;

class function TGLiteHashList.NewHashList(aCapacity: SizeInt): THashList;
begin
  System.SetLength(Result, aCapacity);
  System.FillChar(Result[0], aCapacity * SizeOf(SizeInt), $ff);
end;

class procedure TGLiteHashList.IndexOutOfBoundError(aIndex: SizeInt);
begin
  raise ELGListError.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
end;

class procedure TGLiteHashList.CapacityExceedError(aValue: SizeInt);
begin
  raise ELGCapacityExceed.CreateFmt(SECapacityExceedFmt, [aValue]);
end;

class operator TGLiteHashList.Initialize(var hl: TGLiteHashList);
begin
  hl.FCount := 0;
end;

class operator TGLiteHashList.Finalize(var hl: TGLiteHashList);
begin
  hl.Clear;
end;

class operator TGLiteHashList.Copy(constref aSrc: TGLiteHashList; var aDst: TGLiteHashList);
begin
  aDst.FNodeList := System.Copy(aSrc.FNodeList);
  aDst.FHashList := System.Copy(aSrc.FHashList);
  aDst.FCount := aSrc.FCount;
end;

function TGLiteHashList.GetEnumerator: TEnumerator;
begin
  Result.Init(Self);
end;

function TGLiteHashList.ToArray: TArray;
var
  I: SizeInt;
begin
  System.SetLength(Result, Count);
  for I := 0 to Pred(Count) do
    Result[I] := FNodeList[I].Data;
end;

function TGLiteHashList.Reverse: TReverse;
begin
  Result.Init(@Self);
end;

procedure TGLiteHashList.Clear;
begin
  FNodeList := nil;
  FHashList := nil;
  FCount := 0;
end;

function TGLiteHashList.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TGLiteHashList.NonEmpty: Boolean;
begin
  Result := Count <> 0;
end;

procedure TGLiteHashList.EnsureCapacity(aValue: SizeInt);
begin
  if aValue <= Capacity then
    exit;
  if aValue <= DEFAULT_CONTAINER_CAPACITY then
    aValue := DEFAULT_CONTAINER_CAPACITY
  else
    if aValue < MAX_CAPACITY then
      aValue := LGUtils.RoundUpTwoPower(aValue)
    else
      CapacityExceedError(aValue);
  Resize(aValue);
end;

procedure TGLiteHashList.TrimToFit;
var
  NewSize: SizeInt;
begin
  if NonEmpty then
    begin
      NewSize := LGUtils.RoundUpTwoPower(Count);
      if NewSize < Capacity then
        Resize(NewSize);
    end
  else
    Clear;
end;

function TGLiteHashList.Contains(constref aValue: T): Boolean;
begin
  Result := IndexOf(aValue) >= 0;
end;

function TGLiteHashList.NonContains(constref aValue: T): Boolean;
begin
  Result := IndexOf(aValue) < 0;
end;

function TGLiteHashList.IndexOf(constref aValue: T): SizeInt;
begin
  if NonEmpty then
    Result := Find(aValue)
  else
    Result := NULL_INDEX;
end;

function TGLiteHashList.CountOf(constref aValue: T): SizeInt;
begin
  if NonEmpty then
    Result := GetCount(aValue)
  else
    Result := 0;
end;

function TGLiteHashList.Add(constref aValue: T): SizeInt;
begin
  if Count = Capacity then
    Expand;
  Result := DoAdd(aValue);
end;

function TGLiteHashList.AddAll(constref a: array of T): SizeInt;
var
  v: T;
begin
  Result := System.Length(a);
  EnsureCapacity(Count + Result);
  for v in a do
    DoAdd(v);
end;

function TGLiteHashList.AddAll(e: IEnumerable): SizeInt;
var
  v: T;
begin
  Result := 0;
  for v in e do
    begin
      Add(v);
      Inc(Result);
    end;
end;

procedure TGLiteHashList.Insert(aIndex: SizeInt; constref aValue: T);
begin
  CheckInsertIndexRange(aIndex);
  if Count = Capacity then
    Expand;
  DoInsert(aIndex, aValue);
end;

procedure TGLiteHashList.Delete(aIndex: SizeInt);
begin
  CheckIndexRange(aIndex);
  DoDelete(aIndex);
end;

function TGLiteHashList.Remove(constref aValue: T): Boolean;
begin
  if NonEmpty then
    Result := DoRemove(aValue)
  else
    Result := False;
end;

end.

