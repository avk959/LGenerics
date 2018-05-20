{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generic prority queue implementations.                                  *
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
unit LGPriorityQueue;

{$mode objfpc}{$H+}
{$INLINE ON}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses

  SysUtils,
  math,
  LGUtils,
  LGHelpers,
  LGCustomContainer,
  LGStrConst;

type

  { TGCustomBinHeap: abstract ancestor class of priority queue with queue interface on top of binary heap }
  generic TGCustomBinHeap<T> = class abstract(specialize TGCustomArrayContainer<T>, specialize IGQueue<T>)
  protected
    procedure BuildHeap; virtual; abstract;
    procedure FixHeap(aFrom: SizeInt); inline;
    procedure SiftDown; virtual; abstract;
    procedure FloatUp(aIndex: SizeInt); virtual; abstract;
    function  DequeueItem: T;
    procedure FixAfterEnqueue(aOldCount: SizeInt); inline;
  public
    procedure AfterConstruction; override;
    procedure Enqueue(constref aValue: T);// O(logN)
    function  EnqueueAll(constref a: array of T): SizeInt;
    function  EnqueueAll(e: IEnumerable): SizeInt;
    function  Dequeue: T;                 // O(logN)
    function  TryDequeue(out aValue: T): Boolean;
    function  Peek: T;                    // O(1)
    function  TryPeek(out aValue: T): Boolean;
  end;

  { TGBaseBinHeap implements maximizing priority queue with queue interface;
      functor TCmpRel (comparision relation) must provide
        class function Compare([const[ref]] L, R: T): SizeInt; }
  generic TGBaseBinHeap<T, TCmpRel> = class(specialize TGCustomBinHeap<T>)
  protected
    procedure BuildHeap; override;
    procedure SiftDown; override;
    procedure FloatUp(aIndex: SizeInt);  override;
    class function DoCompare(constref L, R: T): SizeInt; static;
  public
  type
    TComparator = TCompare;
    class function Comparator: TComparator; static; inline;
  end;

  { TGBinHeap assumes that type T implements TCmpRel}
  generic TGBinHeap<T> = class(specialize TGBaseBinHeap<T, T>);

  { TGObjectBinHeap implements maximizing priority queue with queue interface;
      TGObjectBinHeap.Dequeue(or TryDequeue) EXTRACTS object from queue,
      you need to free this object yourself }
  generic TGObjectBinHeap<T: class; TCmpRel> = class(specialize TGBaseBinHeap<T, TCmpRel>)
  private
    FOwnsObjects: Boolean;
  protected
    procedure DoClear; override;
  public
    constructor Create(aOwnsObjects: Boolean = True);
    constructor Create(aCapacity: SizeInt; aOwnsObjects: Boolean = True);
    constructor Create(constref A: array of T; aOwnsObjects: Boolean = True);
    constructor Create(e: IEnumerable; aOwnsObjects: Boolean = True);
    property  OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  { TGObjBinHeap assumes that class T implements TCmpRel}
  generic TGObjBinHeap<T: class> = class(specialize TGObjectBinHeap<T, T>);

  { TGComparableBinHeap }

  generic TGComparableBinHeap<T> = class(specialize TGCustomBinHeap<T>)
  protected
    class function DoCompare(constref L, R: T): SizeInt; static;
  public
  type
    TComparator = TCompare;
    class function Comparator: TComparator; static; inline;
  end;

  { TGComparableBinHeapMax: maximizing priority queue with queue interface,
    it assumes that type T has defined comparision operators}
  generic TGComparableBinHeapMax<T> = class(specialize TGComparableBinHeap<T>)
  protected
    procedure BuildHeap; override;
    procedure SiftDown; override;
    procedure FloatUp(aIndex: SizeInt); override;
  end;

  { TGComparableBinHeapMin: minimizing priority queue with queue interface,
    it assumes that type T has defined comparision operators}
  generic TGComparableBinHeapMin<T> = class(specialize TGComparableBinHeap<T>)
  protected
    procedure BuildHeap; override;
    procedure SiftDown; override;
    procedure FloatUp(aIndex: SizeInt); override;
  end;

  { TGRegularBinHeap: maximizing priority queue with queue interface and regular comparator }
  generic TGRegularBinHeap<T> = class(specialize TGCustomBinHeap<T>)
  private
    FCompare: TCompare;
  protected
    procedure BuildHeap; override;
    procedure SiftDown; override;
    procedure FloatUp(aIndex: SizeInt); override;
  public
  type
    TComparator = TCompare;
    constructor Create;
    constructor Create(c: TComparator);
    constructor Create(aCapacity: SizeInt; c: TComparator);
    constructor Create(constref A: array of T; c: TComparator);
    constructor Create(e: IEnumerable; c: TComparator);
    function Comparator: TComparator; inline;
  end;

  { TGDelegatedBinHeap: maximizing priority queue with queue interface and delegated comparator }
  generic TGDelegatedBinHeap<T> = class(specialize TGCustomBinHeap<T>)
  private
    FCompare: TOnCompare;
  protected
    procedure BuildHeap; override;
    procedure SiftDown; override;
    procedure FloatUp(aIndex: SizeInt); override;
  public
  type
    TComparator = TOnCompare;
    constructor Create;
    constructor Create(c: TComparator);
    constructor Create(aCapacity: SizeInt; c: TComparator);
    constructor Create(constref A: array of T; c: TComparator);
    constructor Create(e: IEnumerable; c: TComparator);
    function Comparator: TOnCompare; inline;
  end;

  { TGLiteBinHeap implements maximizing priority queue with queue interface;
      functor TCmpRel (comparision relation) must provide
        class function Compare([const[ref]] L, R: T): SizeInt; }
  generic TGLiteBinHeap<T, TCmpRel> = record
  public
  type
    TBuffer     = specialize TGLiteDynBuffer<T>;
    TEnumerator = TBuffer.TEnumerator;
    TMutable    = TBuffer.TMutable;
    TReverse    = TBuffer.TReverse;
    TArray      = TBuffer.TArray;
  strict private
  type
    TFake = {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}array[0..Pred(SizeOf(T))] of Byte{$ELSE}T{$ENDIF};

  private
    FBuffer: TBuffer;
    function  GetCapacity: SizeInt; inline;
    procedure BuildHeap;
    procedure SiftDown;
    procedure FloatUp(aIndex: SizeInt);
    function  DequeueItem: T;
    class function DoCompare(constref L, R: T): SizeInt; static;
  public
  type
    TComparator = specialize TGCompare<T>;
    function  Comparator: TComparator; inline;
    function  GetEnumerator: TEnumerator; inline;
    function  Mutable: TMutable; inline;
    function  Reverse: TReverse; inline;
    function  ToArray: TArray; inline;
    procedure Clear; inline;
    procedure Modified; inline;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure EnsureCapacity(aValue: SizeInt); inline;
    procedure TrimToFit; inline;
    procedure Enqueue(constref aValue: T); inline;
  { EXTRACTS element from the head of queue }
    function  Dequeue: T; inline;
    function  TryDequeue(out aValue: T): Boolean; inline;
    function  Peek: T; inline;
    function  TryPeek(out aValue: T): Boolean; inline;
    property  Count: SizeInt read FBuffer.FCount;
    property  Capacity: SizeInt read GetCapacity;
  end;

  { TGLiteThreadBinHeap }

  generic TGLiteThreadBinHeap<T, TCmpRel> = class
  public
  type
    TQueue = specialize TGLiteBinHeap<T, TCmpRel>;
    PQueue = ^TQueue;

  strict private
    FQueue: TQueue;
    FLock: TRTLCriticalSection;
    procedure DoLock; inline;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Enqueue(constref aValue: T);
    function  TryDequeue(out aValue: T): Boolean;
    function  TryPeek(out aValue: T): Boolean;
    function  Lock: PQueue;
    procedure Unlock; inline;
  end;

  generic TGLiteComparableBinHeapMin<T> = record
  public
  type
    TBuffer     = specialize TGLiteDynBuffer<T>;
    TEnumerator = TBuffer.TEnumerator;
    TMutable    = TBuffer.TMutable;
    TReverse    = TBuffer.TReverse;
    TArray      = TBuffer.TArray;
  strict private
  type
    TFake = {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}array[0..Pred(SizeOf(T))] of Byte{$ELSE}T{$ENDIF};

  private
    FBuffer: TBuffer;
    function  GetCapacity: SizeInt; inline;
    procedure BuildHeap;
    procedure SiftDown;
    procedure FloatUp(aIndex: SizeInt);
    function  DequeueItem: T;
    class function DoCompare(constref L, R: T): SizeInt; static;
  public
  type
    TComparator = specialize TGCompare<T>;
    function  Comparator: TComparator; inline;
    function  GetEnumerator: TEnumerator; inline;
    function  Mutable: TMutable; inline;
    function  Reverse: TReverse; inline;
    function  ToArray: TArray; inline;
    procedure Clear; inline;
    procedure Modified; inline;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure EnsureCapacity(aValue: SizeInt); inline;
    procedure TrimToFit; inline;
    procedure Enqueue(constref aValue: T); inline;
  { EXTRACTS element from the head of queue }
    function  Dequeue: T; inline;
    function  TryDequeue(out aValue: T): Boolean; inline;
    function  Peek: T; inline;
    function  TryPeek(out aValue: T): Boolean; inline;
    property  Count: SizeInt read FBuffer.FCount;
    property  Capacity: SizeInt read GetCapacity;
  end;

  THandle = SysUtils.THandle;

  { TGCustomBinHeapPQ: abstract ancestor class to implement priority queue on top of binary heap(array based) }
  generic TGCustomBinHeapPQ<T> = class abstract(specialize TGCustomContainer<T>, specialize IGQueue<T>,
    specialize IGPriorityQueue<T>)
    private
      FStackCount: SizeInt;
  public
  type
    TCustomBinHeapPQ = specialize TGCustomBinHeapPQ<T>;
    IPriorityQueue   = specialize IGPriorityQueue<T>;
  protected
  type
    TNode = record
      Data: T;
      HeapIndex: SizeInt;
    end;

    TIntArray = array of SizeInt;
    TNodeList = array of TNode;

    TEnumerator = class(TContainerEnumerator)
    private
      FHeap: TIntArray;
      FNodes: TNodeList;
      FCurrIndex,
      FLast: SizeInt;
    protected
      function  GetCurrent: T; override;
    public
      constructor Create(aQueue: TCustomBinHeapPQ);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TReverseEnum = class(TContainerEnumerable)
    protected
      FHeap: TIntArray;
      FNodes: TNodeList;
      FCurrIndex,
      FLast: SizeInt;
      function  GetCurrent: T; override;
    public
      constructor Create(aQueue: TGCustomBinHeapPQ);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

  const
    // MAX_CAPACITY must be <= Succ(SizeInt.MaxValue div 2)
    MAX_CAPACITY: SizeInt          = SizeInt(MAX_CONTAINER_SIZE div SizeOf(TItem));
    MAX_NODELIST_CAPACITY: SizeInt = SizeInt(MAX_CONTAINER_SIZE div SizeOf(TNode));
    NULL = -1;

  var
    FNodeList: TNodeList;
    FHeap: TIntArray;
    FCount,
    FNodeListCount,
    FStackTop: SizeInt;
    procedure CheckEmpty; inline;
    function  GetCount: SizeInt; override;
    function  GetCapacity: SizeInt; override;
    function  NodeListCapacity: SizeInt; inline;
    function  HeapCapacity: SizeInt; inline;
    procedure InitialAlloc(aCapacity: SizeInt);
    procedure HeapExpand(aValue: SizeInt);
    procedure NodeListExpand(aValue: SizeInt);
    function  StackPop: SizeInt; inline;
    procedure StackPush(aValue: SizeInt); inline;
    function  StackEmpty: Boolean; inline;
    procedure ItemAdding;
    function  AddNode(constref aValue: T; aIndex: SizeInt): SizeInt;
    function  NodeListRemove(aIndex: SizeInt): T; inline;
    procedure CopyItems(aBuffer: PItem); override;
    function  DoEnqueue(constref aValue: T): SizeInt;
    function  DoDequeue: T;
    function  GetValue(aNodeIndex: SizeInt): T;
    function  DoRemove(aNodeIndex: SizeInt): T;
    function  EnqueueArray(constref a: array of T): SizeInt;
    function  EnqueueContainer(c: TCustomContainer): SizeInt;
    function  EnqueueEnum(e: IEnumerable): SizeInt;
    procedure BuildHeap; virtual; abstract;
    procedure FixHeap(aIndex: SizeInt); inline;
    procedure FloatUp(aIndex: SizeInt); virtual; abstract;
    procedure SiftDown(aIndex: SizeInt); virtual; abstract;
    procedure DoUpdate(aNodeIndex: SizeInt; constref aValue: T); virtual; abstract;
    function  MergeBinHeap(bh: TGCustomBinHeapPQ): SizeInt;
    function  DoGetEnumerator: TCustomEnumerator; override;
    procedure DoClear; override;
    procedure DoTrimToFit; override;
    procedure DoEnsureCapacity(aValue: SizeInt); override;
    property  ElemCount: SizeInt read FCount;
    property  NodeListCount: SizeInt read FNodeListCount;
    property  StackTop: SizeInt read FStackTop;
    property  StackCount: SizeInt read FStackCount;
  public
    destructor Destroy; override;
    function  Reverse: IEnumerable; override;
    procedure Enqueue(constref aValue: T); inline;        // O(logN)
    function  EnqueueAll(constref a: array of T): SizeInt;
    function  EnqueueAll(e: IEnumerable): SizeInt;
    function  Dequeue: T;                                 // O(logN)
    function  TryDequeue(out aValue: T): Boolean;
    function  Peek: T;                                    // O(1)
    function  TryPeek(out aValue: T): Boolean;
    function  Insert(constref aValue: T): THandle; inline;
    function  PeekHead: THandle; inline;                      // O(1)
    function  TryPeekHead(out aHandle: THandle): Boolean; inline;
    function  Value(aHandle: THandle): T; inline;
    procedure Update(aHandle: THandle; constref aValue: T); inline;// O(logN)
    function  Remove(aHandle: THandle): T; inline;        // O(logN)
  { note: after Merge handles from aQueue will become invalid }
    function  Merge(aQueue: IPriorityQueue): SizeInt; //O(aQueue.Count)
  end;

  { TGBaseBinHeapPQ implements maximizing priority queue (array based);
      functor TCmpRel (comparision relation) must provide
        class function Compare([const[ref]] L, R: T): SizeInt; }
  generic TGBaseBinHeapPQ<T, TCmpRel> = class(specialize TGCustomBinHeapPQ<T>)
  protected
    procedure BuildHeap; override;
    procedure FloatUp(aIndex: SizeInt); override;
    procedure SiftDown(aIndex: SizeInt); override;
    procedure DoUpdate(aIndex: SizeInt; constref aValue: T); override;
    class function DoCompare(constref L, R: T): SizeInt; static;
  public
  type
    TComparator = TCompare;
    class function Comparator: TComparator; static; inline;
    constructor Create;
    constructor Create(aCapacity: SizeInt);
    constructor Create(constref A: array of T);
    constructor Create(e: IEnumerable);
  end;

  { TGBinHeapPQ assumes that type T implements TCmpRel}
  generic TGBinHeapPQ<T> = class(specialize TGBaseBinHeapPQ<T, T>);

  generic TGComparableBinHeapPQ<T> = class(specialize TGCustomBinHeapPQ<T>)
  protected
    class function DoCompare(constref L, R: T): SizeInt; static;
  public
  type
    TComparator = TCompare;
    class function Comparator: TComparator; static; inline;
  end;

  { TGComparableBinHeapPQMax: maximizing priority queue (array based),
    it assumes that type T has defined comparision operators }
  generic TGComparableBinHeapPQMax<T> = class(specialize TGComparableBinHeapPQ<T>)
  protected
    procedure BuildHeap; override;
    procedure FloatUp(aIndex: SizeInt); override;
    procedure SiftDown(aIndex: SizeInt); override;
    procedure DoUpdate(aIndex: SizeInt; constref aValue: T); override;
  public
    constructor Create;
    constructor Create(aCapacity: SizeInt);
    constructor Create(constref A: array of T);
    constructor Create(e: IEnumerable);
  end;

  { TGComparableBinHeapPQMin: minimizing priority queue(array based),
    it assumes that type T has defined comparision operators }
  generic TGComparableBinHeapPQMin<T> = class(specialize TGComparableBinHeapPQ<T>)
  protected
    procedure BuildHeap; override;
    procedure FloatUp(aIndex: SizeInt); override;
    procedure SiftDown(aIndex: SizeInt); override;
    procedure DoUpdate(aIndex: SizeInt; constref aValue: T); override;
  public
    constructor Create;
    constructor Create(aCapacity: SizeInt);
    constructor Create(constref A: array of T);
    constructor Create(e: IEnumerable);
  end;

  { TGRegularBinHeapPQ: maximizing priority queue with regular comparator(array based) }
  generic TGRegularBinHeapPQ<T> = class(specialize TGCustomBinHeapPQ<T>)
  protected
    FCompare: TCompare;
    procedure BuildHeap; override;
    procedure FloatUp(aIndex: SizeInt); override;
    procedure SiftDown(aIndex: SizeInt); override;
    procedure DoUpdate(aIndex: SizeInt; constref aValue: T); override;
  public
  type
    TComparator = TCompare;
    constructor Create;
    constructor Create(c: TComparator);
    constructor Create(aCapacity: SizeInt; c: TComparator);
    constructor Create(constref A: array of T; c: TComparator);
    constructor Create(e: IEnumerable; c: TComparator);
    function Comparator: TComparator; inline;
  end;

  { TGDelegatedBinHeapPQ: maximizing priority queue with delegated comparator(array based) }
  generic TGDelegatedBinHeapPQ<T> = class(specialize TGCustomBinHeapPQ<T>)
  protected
    FCompare: TOnCompare;
    procedure BuildHeap; override;
    procedure FloatUp(aIndex: SizeInt); override;
    procedure SiftDown(aIndex: SizeInt); override;
    procedure DoUpdate(aIndex: SizeInt; constref aValue: T); override;
  public
  type
    TComparator = TOnCompare;
    constructor Create;
    constructor Create(c: TComparator);
    constructor Create(aCapacity: SizeInt; c: TComparator);
    constructor Create(constref A: array of T; c: TComparator);
    constructor Create(e: IEnumerable; c: TComparator);
    function Comparator: TComparator; inline;
  end;

{.$DEFINE ENABLE_TWOPASSMERGE_RECURSION}  //if uncomment define, will compile recursive TwoPassMerge
{$DEFINE PAIRHEAP_ENABLE_PAGEDNODEMANAGER}//if uncomment define, will use TGJoinablePageNodeManager

  { TGCustomPairingHeap: abstract ancestor class to implement priority queue on top of pairing heap }
  generic TGCustomPairingHeap<T> = class abstract(specialize TGCustomContainer<T>, specialize IGQueue<T>,
    specialize IGPriorityQueue<T>)
  public
  type
    TCustomPairingHeap = specialize TGCustomPairingHeap<T>;
    IPriorityQueue     = specialize IGPriorityQueue<T>;
  protected
  type

    PNode = ^TNode;

    TNode = record
      Child,
      Sibling,
      Prev: PNode;
      Data: T;
      procedure AddChild(aNode: PNode);
      procedure ClearLinks; inline;
      function  Successor: PNode;
      function  Predecessor: PNode;
      //for node manager
      property  NextLink: PNode read Child write Child;
    end;

    TEnumerator = class(TContainerEnumerator)
    private
      FCurrNode,
      FLeftmost: PNode;
      FInCycle: Boolean;
    protected
      function  GetCurrent: T; override;
    public
      constructor Create(aHeap: TCustomPairingHeap);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TReverseEnum = class(TContainerEnumerable)
    protected
      FCurrNode,
      FRightmost: PNode;
      FInCycle: Boolean;
      function  GetCurrent: T; override;
    public
      constructor Create(aHeap: TCustomPairingHeap);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

{$IFDEF PAIRHEAP_ENABLE_PAGEDNODEMANAGER}
    TNodeManager = specialize TGJoinablePageNodeManager<TNode>;
{$ELSE PAIRHEAP_ENABLE_PAGEDNODEMANAGER}
    TNodeManager = specialize TGJoinableNodeManager<TNode>;
{$ENDIF PAIRHEAP_ENABLE_PAGEDNODEMANAGER}

  var
    FRoot: PNode;
    FNodeManager: TNodeManager;
    FCount: SizeInt;
    procedure CheckEmpty; inline;
    function  GetCount: SizeInt; override;
    function  GetCapacity: SizeInt; override;
    procedure CopyItems(aBuffer: PItem); override;
    function  DoGetEnumerator: TCustomEnumerator; override;
    procedure DoClear; override;
    procedure DoTrimToFit; override;
    procedure DoEnsureCapacity(aValue: SizeInt); override;
    function  NewNode(constref aValue: T): PNode;
    procedure DisposeNode(aNode: PNode); inline;
    procedure RemoveNodeWithChilds(aNode: PNode);
    procedure ClearTree;
    function  FindLeftmost: PNode;
    function  FindRightmost: PNode;
    function  EnqueueArray(constref a: array of T): SizeInt;
    function  EnqueueContainer(c: TCustomContainer): SizeInt;
    function  EnqueueEnum(e: IEnumerable): SizeInt;
    function  DoEnqueue(constref aValue: T): PNode; virtual; abstract;
    procedure DoUpdate(aNode: PNode; constref aValue: T); virtual; abstract;
    function  DoRemove(aNode: PNode): T; virtual; abstract;
    function  DoDequeue: T; virtual; abstract;
    function  DoMergeHeap(ph: TCustomPairingHeap): SizeInt; virtual; abstract;
    property  ElemCount: SizeInt read FCount;
    class procedure CutNode(aNode: PNode); static;
  public
    destructor Destroy; override;
    function  Reverse: IEnumerable; override;
    procedure Enqueue(constref aValue: T); inline;          // O(1)
    function  EnqueueAll(constref a: array of T): SizeInt;
    function  EnqueueAll(e: IEnumerable): SizeInt;
    function  Dequeue: T;                                   // amortized O(logN)
    function  TryDequeue(out aValue: T): Boolean;
    function  Peek: T;                                      // O(1)
    function  TryPeek(out aValue: T): Boolean;
    function  Insert(constref aValue: T): THandle; inline;  // O(1)
    function  PeekHead: THandle; inline;                    // O(1)
    function  TryPeekHead(out aHandle: THandle): Boolean; inline;
    function  Value(aHandle: THandle): T; inline;
    procedure Update(aHandle: THandle; constref aValue: T); inline; //O(1) IncreaseKey
    function  Remove(aHandle: THandle): T;                  // amortized O(logN)
  { note: after Merge all handles from aQueue will remain valid if aQueue has same ClassType }
    function  Merge(aQueue: IPriorityQueue): SizeInt;       // O(1) if aQueue has same ClassType
  end;

  { TGBasePairingHeap implements maximizing priority queue;
      functor TCmpRel (comparision relation) must provide
        class function Compare([const[ref]] L, R: T): SizeInt; }
  generic TGBasePairingHeap<T, TCmpRel> = class(specialize TGCustomPairingHeap<T>)
  protected
    procedure RootMerge(aNode: PNode); inline;
    procedure DoExtract(aNode: PNode);
    function  DoEnqueue(constref aValue: T): PNode; override;
    procedure DoUpdate(aNode: PNode; constref aValue: T); override;
    function  DoRemove(aNode: PNode): T; override;
    function  DoDequeue: T; override;
    function  DoMergeHeap(ph: TCustomPairingHeap): SizeInt; override;
    class function DoMerge(L, R: PNode): PNode; static; inline;
    class function TwoPassMerge(aNode: PNode): PNode; static;
    class function DoCompare(constref L, R: T): SizeInt; static;
  public
  type
    TComparator = TCompare;
    class function Comparator: TComparator; static; inline;
    constructor Create;
    constructor Create(aCapacity: SizeInt);
    constructor Create(constref A: array of T);
    constructor Create(e: IEnumerable);
  end;

  { TGPairingHeap assumes that type T implements TCmpRel}
  generic TGPairingHeap<T> = class(specialize TGBasePairingHeap<T, T>);

  generic TGComparablePairHeap<T> = class(specialize TGCustomPairingHeap<T>)
  protected
    class function DoCompare(constref L, R: T): SizeInt; static;
  public
  type
    TComparator = TCompare;
    class function Comparator: TComparator; static; inline;
  end;

  { TGComparablePairHeapMax: maximizing priority queue, it assumes
    that type T has defined comparision operators }
  generic TGComparablePairHeapMax<T> = class(specialize TGComparablePairHeap<T>)
  protected
    procedure RootMerge(aNode: PNode); inline;
    procedure DoExtract(aNode: PNode);
    function  DoEnqueue(constref aValue: T): PNode; override;
    procedure DoUpdate(aNode: PNode; constref aValue: T); override;
    function  DoRemove(aNode: PNode): T; override;
    function  DoDequeue: T; override;
    function  DoMergeHeap(ph: TCustomPairingHeap): SizeInt; override;
    class function DoMerge(L, R: PNode): PNode; static; inline;
    class function TwoPassMerge(aNode: PNode): PNode; static;
  public
    constructor Create;
    constructor Create(aCapacity: SizeInt);
    constructor Create(constref A: array of T);
    constructor Create(e: IEnumerable);
  end;

  { TGComparablePairHeapMin: minimizing priority queue, it assumes
    that type T has defined comparision operators }
  generic TGComparablePairHeapMin<T> = class(specialize TGComparablePairHeap<T>)
  protected
    procedure RootMerge(aNode: PNode); inline;
    procedure DoExtract(aNode: PNode);
    function  DoEnqueue(constref aValue: T): PNode; override;
    procedure DoUpdate(aNode: PNode; constref aValue: T); override;
    function  DoRemove(aNode: PNode): T; override;
    function  DoDequeue: T; override;
    function  DoMergeHeap(ph: TCustomPairingHeap): SizeInt; override;
    class function DoMerge(L, R: PNode): PNode; static; inline;
    class function TwoPassMerge(aNode: PNode): PNode; static;
  public
    constructor Create;
    constructor Create(aCapacity: SizeInt);
    constructor Create(constref A: array of T);
    constructor Create(e: IEnumerable);
  end;

  { TGRegularPairHeap: maximizing priority queue with regular comparator }
  generic TGRegularPairHeap<T> = class(specialize TGCustomPairingHeap<T>)
  protected
    FCompare: TCompare;
    function  DoMerge(L, R: PNode): PNode; inline;
    procedure RootMerge(aNode: PNode); inline;
    function  TwoPassMerge(aNode: PNode): PNode;
    procedure DoExtract(aNode: PNode);
    function  DoEnqueue(constref aValue: T): PNode; override;
    procedure DoUpdate(aNode: PNode; constref aValue: T); override;
    function  DoRemove(aNode: PNode): T; override;
    function  DoDequeue: T; override;
    function  DoMergeHeap(ph: TCustomPairingHeap): SizeInt; override;
  public
  type
    TComparator = TCompare;
    constructor Create;
    constructor Create(c: TComparator);
    constructor Create(aCapacity: SizeInt; c: TComparator);
    constructor Create(constref A: array of T; c: TComparator);
    constructor Create(e: IEnumerable; c: TComparator);
    function Comparator: TComparator; inline;
  end;

  { TGDelegatedPairHeap: maximizing priority queue with delegated comparator }
  generic TGDelegatedPairHeap<T> = class(specialize TGCustomPairingHeap<T>)
  protected
    FCompare: TOnCompare;
    function  DoMerge(L, R: PNode): PNode; inline;
    procedure RootMerge(aNode: PNode); inline;
    function  TwoPassMerge(aNode: PNode): PNode;
    procedure DoExtract(aNode: PNode);
    function  DoEnqueue(constref aValue: T): PNode; override;
    procedure DoUpdate(aNode: PNode; constref aValue: T); override;
    function  DoRemove(aNode: PNode): T; override;
    function  DoDequeue: T; override;
    function  DoMergeHeap(ph: TCustomPairingHeap): SizeInt; override;
  public
  type
    TComparator = TOnCompare;
    constructor Create;
    constructor Create(c: TComparator);
    constructor Create(aCapacity: SizeInt; c: TComparator);
    constructor Create(constref A: array of T; c: TComparator);
    constructor Create(e: IEnumerable; c: TComparator);
    function Comparator: TComparator; inline;
  end;

  { TGLitePairingHeap implements maximizing priority queue;
      functor TCmpRel (comparision relation) must provide
        class function Compare([const[ref]] L, R: T): SizeInt; }
  generic TGLitePairingHeap<T, TCmpRel> = record
  private
  type
    PGLitePairingHeap = ^TGLitePairingHeap;

    PNode = ^TNode;
    TNode = record
      Child,
      Sibling,
      Prev: PNode;
      Data: T;
      procedure AddChild(aNode: PNode); inline;
      procedure ClearLinks; inline;
      function  Successor: PNode;
      function  Predecessor: PNode;
      //for node manager
      property  NextLink: PNode read Child write Child;
    end;

{$IFDEF PAIRHEAP_ENABLE_PAGEDNODEMANAGER}
    TNodeManager = specialize TGJoinablePageNodeManager<TNode>;
{$ELSE PAIRHEAP_ENABLE_PAGEDNODEMANAGER}
    TNodeManager = specialize TGJoinableNodeManager<TNode>;
{$ENDIF PAIRHEAP_ENABLE_PAGEDNODEMANAGER}

  public
  type
    TArray = array of T;
    PItem  = ^T;

    TEnumerator = record
    private
      FCurrNode,
      FLeftmost: PNode;
      FInCycle: Boolean;
      function  GetCurrent: T; inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: T read GetCurrent;
    end;

    TReverseEnumerator = record
    private
      FCurrNode,
      FRightmost: PNode;
      FInCycle: Boolean;
      function  GetCurrent: T; inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
    end;

    TReverse = record
    private
      FHeap: PGLitePairingHeap;
    public
      function GetEnumerator: TReverseEnumerator; inline;
    end;

  private
    FRoot: PNode;
    FNodeManager: TNodeManager;
    FCount: SizeInt;
    function  GetCapacity: SizeInt; inline;
    function  GetReverseEnumerator: TReverseEnumerator; inline;
    function  FindLeftmost: PNode;
    function  FindRightmost: PNode;
    procedure RemoveNodeWithChilds(aNode: PNode);
    procedure ClearTree;
    function  NewNode(constref aValue: T): PNode;
    procedure DisposeNode(aNode: PNode); inline;
    procedure RootMerge(aNode: PNode); inline;
    function  DequeueItem: T;
    procedure UpdateNode(aNode: PNode; constref aValue: T);
    procedure ExtractNode(aNode: PNode);
    function  RemoveNode(aNode: PNode): T;
    procedure CheckEmpty; inline;
    procedure AccessEmptyError; inline;
    class operator  Initialize(var h: TGLitePairingHeap);
    class operator  Finalize(var h: TGLitePairingHeap);
    class operator  Copy(constref aSrc: TGLitePairingHeap; var aDst: TGLitePairingHeap);
    class function  NodeMerge(L, R: PNode): PNode; static; inline; //inline ???
    class function  TwoPassMerge(aNode: PNode): PNode; static;
    class procedure CutNode(aNode: PNode); static;
    class function  DoCompare(constref L, R: T): SizeInt; static;
  public
  type
    TComparator = specialize TGCompare<T>;
    function  Comparator: TComparator;
    function  GetEnumerator: TEnumerator; inline;
    function  Reverse: TReverse; inline;
    function  ToArray: TArray;
    procedure Clear; inline;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure EnsureCapacity(aValue: SizeInt); inline;
    procedure TrimToFit; inline;
    procedure Enqueue(constref aValue: T); inline;          // O(1)
    function  Dequeue: T;                                   // amortized O(logN)
    function  TryDequeue(out aValue: T): Boolean;
    function  Peek: T;                                      // O(1)
    function  TryPeek(out aValue: T): Boolean;
    function  Insert(constref aValue: T): THandle; inline;  // O(1)
    function  PeekHead: THandle; inline;                    // O(1)
    function  TryPeekHead(out aValue: THandle): Boolean; inline;
    function  Value(aHandle: THandle): T; inline;
    procedure Update(aHandle: THandle; constref aValue: T); inline; //O(1)              IncreaseKey
                                                                    //amortized O(logN) DecreaseKey
    function  Remove(aHandle: THandle): T;                  // amortized O(logN)
  { note: after Merge all handles from aHeap will remain valid }
    function  Merge(aHeap: TGLitePairingHeap): SizeInt;     // O(1)
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
  end;

  { TGLitePairingHeap implements maximizing priority queue;
      functor TCmpRel (comparision relation) must provide
        class function Compare([const[ref]] L, R: T): SizeInt; }
  generic TGLiteComparablePairHeapMin<T> = record
  private
  type
    PGLitePairingHeap = ^TGLiteComparablePairHeapMin;

    PNode = ^TNode;
    TNode = record
      Child,
      Sibling,
      Prev: PNode;
      Data: T;
      procedure AddChild(aNode: PNode); inline;
      procedure ClearLinks; inline;
      function  Successor: PNode;
      function  Predecessor: PNode;
      //for node manager
      property  NextLink: PNode read Child write Child;
    end;

{$IFDEF PAIRHEAP_ENABLE_PAGEDNODEMANAGER}
    TNodeManager = specialize TGJoinablePageNodeManager<TNode>;
{$ELSE PAIRHEAP_ENABLE_PAGEDNODEMANAGER}
    TNodeManager = specialize TGJoinableNodeManager<TNode>;
{$ENDIF PAIRHEAP_ENABLE_PAGEDNODEMANAGER}

  public
  type
    TArray = array of T;
    PItem  = ^T;

    TEnumerator = record
    private
      FCurrNode,
      FLeftmost: PNode;
      FInCycle: Boolean;
      function  GetCurrent: T; inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: T read GetCurrent;
    end;

    TReverseEnumerator = record
    private
      FCurrNode,
      FRightmost: PNode;
      FInCycle: Boolean;
      function  GetCurrent: T; inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
    end;

    TReverse = record
    private
      FHeap: PGLitePairingHeap;
    public
      function GetEnumerator: TReverseEnumerator; inline;
    end;

  private
    FRoot: PNode;
    FNodeManager: TNodeManager;
    FCount: SizeInt;
    function  GetCapacity: SizeInt; inline;
    function  GetReverseEnumerator: TReverseEnumerator; inline;
    function  FindLeftmost: PNode;
    function  FindRightmost: PNode;
    procedure RemoveNodeWithChilds(aNode: PNode);
    procedure ClearTree;
    function  NewNode(constref aValue: T): PNode;
    procedure DisposeNode(aNode: PNode); inline;
    procedure RootMerge(aNode: PNode); inline;
    function  DequeueItem: T;
    procedure UpdateNode(aNode: PNode; constref aValue: T);
    procedure ExtractNode(aNode: PNode);
    function  RemoveNode(aNode: PNode): T;
    procedure CheckEmpty; inline;
    procedure AccessEmptyError; inline;
    class operator  Initialize(var h: TGLiteComparablePairHeapMin);
    class operator  Finalize(var h: TGLiteComparablePairHeapMin);
    class operator  Copy(constref aSrc: TGLiteComparablePairHeapMin; var aDst: TGLiteComparablePairHeapMin);
    class function  NodeMerge(L, R: PNode): PNode; static; inline; //inline ???
    class function  TwoPassMerge(aNode: PNode): PNode; static;
    class procedure CutNode(aNode: PNode); static;
    class function  DoCompare(constref L, R: T): SizeInt; static;
  public
  type
    TComparator = specialize TGCompare<T>;
    function  Comparator: TComparator;
    function  GetEnumerator: TEnumerator; inline;
    function  Reverse: TReverse; inline;
    function  ToArray: TArray;
    procedure Clear; inline;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure EnsureCapacity(aValue: SizeInt); inline;
    procedure TrimToFit; inline;
    procedure Enqueue(constref aValue: T); inline;          // O(1)
    function  Dequeue: T;                                   // amortized O(logN)
    function  TryDequeue(out aValue: T): Boolean;
    function  Peek: T;                                      // O(1)
    function  TryPeek(out aValue: T): Boolean;
    function  Insert(constref aValue: T): THandle; inline;  // O(1)
    function  PeekHead: THandle; inline;                    // O(1)
    function  TryPeekHead(out aValue: THandle): Boolean; inline;
    function  Value(aHandle: THandle): T; inline;
    procedure Update(aHandle: THandle; constref aValue: T); inline; //O(1)              IncreaseKey
                                                                    //amortized O(logN) DecreaseKey
    function  Remove(aHandle: THandle): T;                  // amortized O(logN)
  { note: after Merge all handles from aHeap will remain valid }
    function  Merge(aHeap: TGLiteComparablePairHeapMin): SizeInt;// O(1)
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
  end;

implementation
{$B-}{$COPERATORS ON}

{ TGCustomBinHeap }

procedure TGCustomBinHeap.FixHeap(aFrom: SizeInt);
begin
  for aFrom := aFrom to Pred(ElemCount) do
    FloatUp(aFrom);
end;

function TGCustomBinHeap.DequeueItem: T;
begin
  Result := FItems[0];
  Dec(FCount);
  if ElemCount > 0 then
    begin
      FItems[0] := FItems[ElemCount];
      FItems[ElemCount] := Default(T);
      SiftDown;
    end
  else
    FItems[0] := Default(T);
end;

procedure TGCustomBinHeap.FixAfterEnqueue(aOldCount: SizeInt);
begin
  if aOldCount > 0 then
    FixHeap(aOldCount)
  else
    BuildHeap;
end;

procedure TGCustomBinHeap.AfterConstruction;
begin
  inherited;
  if ElemCount > 1 then
    BuildHeap;
end;

procedure TGCustomBinHeap.Enqueue(constref aValue: T);
var
  InsertPos: SizeInt;
begin
  CheckInIteration;
  InsertPos := ElemCount;
  ItemAdding;
  FItems[InsertPos] := aValue;
  Inc(FCount);
  if InsertPos > 0 then
    FloatUp(InsertPos);
end;

function TGCustomBinHeap.EnqueueAll(constref a: array of T): SizeInt;
var
  OldCount: SizeInt;
begin
  CheckInIteration;
  OldCount := ElemCount;
  Result := AppendArray(a);
  if Result > 0 then
    FixAfterEnqueue(OldCount);
end;

function TGCustomBinHeap.EnqueueAll(e: IEnumerable): SizeInt;
var
  OldCount: SizeInt;
begin
  CheckInIteration;
  OldCount := ElemCount;
  Result := AppendEnumerable(e);
  if Result > 0 then
    FixAfterEnqueue(OldCount);
end;

function TGCustomBinHeap.Dequeue: T;
begin
  CheckInIteration;
  CheckEmpty;
  Result := DequeueItem;
end;

function TGCustomBinHeap.TryDequeue(out aValue: T): Boolean;
begin
  Result := not InIteration and (ElemCount > 0);
  if Result then
    aValue := DequeueItem;
end;

function TGCustomBinHeap.Peek: T;
begin
  CheckEmpty;
  Result := FItems[0];
end;

function TGCustomBinHeap.TryPeek(out aValue: T): Boolean;
begin
  Result := ElemCount > 0;
  if Result then
    aValue := FItems[0];
end;

{ TGBaseBinHeap }

procedure TGBaseBinHeap.BuildHeap;
var
  I, CurrIdx, NextIdx, HighIdx: SizeInt;
  v: TFake;
begin
  if ElemCount > 1 then
    begin
      HighIdx := Pred(ElemCount);
      for I := Pred(ElemCount shr 1) downto 0 do
        begin
          CurrIdx := I;
          NextIdx := Succ(I shl 1);
          v := TFake(FItems[CurrIdx]);
          while NextIdx <= HighIdx do
            begin
              if(Succ(NextIdx) <= HighIdx) and (TCmpRel.Compare(FItems[NextIdx], FItems[Succ(NextIdx)]) < 0)then
                Inc(NextIdx);
              if TCmpRel.Compare(T(v), FItems[NextIdx]) >= 0 then
                break;
              TFake(FItems[CurrIdx]) := TFake(FItems[NextIdx]);
              CurrIdx := NextIdx;
              NextIdx := Succ(NextIdx shl 1);
            end;
          TFake(FItems[CurrIdx]) := v;
        end;
    end;
end;

procedure TGBaseBinHeap.SiftDown;
var
  CurrIdx, NextIdx, HighIdx: SizeInt;
  v: TFake;
begin
  HighIdx := Pred(ElemCount);
  if HighIdx > 0 then
    begin
      CurrIdx := 0;
      NextIdx := 1;
      v := TFake(FItems[0]);
      while NextIdx <= HighIdx do
        begin
          if(Succ(NextIdx) <= HighIdx) and (TCmpRel.Compare(FItems[NextIdx], FItems[Succ(NextIdx)]) < 0) then
            Inc(NextIdx);
          TFake(FItems[CurrIdx]) := TFake(FItems[NextIdx]);
          CurrIdx := NextIdx;
          NextIdx := Succ(NextIdx shl 1);
        end;
      NextIdx := Pred(CurrIdx) shr 1;
      while (CurrIdx > 0) and (TCmpRel.Compare(T(v), FItems[NextIdx]) > 0) do
        begin
          TFake(FItems[CurrIdx]) := TFake(FItems[NextIdx]);
          CurrIdx := NextIdx;
          NextIdx := Pred(NextIdx) shr 1;
        end;
      TFake(FItems[CurrIdx]) := v;
    end;
end;

procedure TGBaseBinHeap.FloatUp(aIndex: SizeInt);
var
  ParentIdx: SizeInt;
  v: TFake;
begin
  ParentIdx := Pred(aIndex) shr 1;
  v := TFake(FItems[aIndex]);
  while(aIndex > 0) and (TCmpRel.Compare(T(v), FItems[ParentIdx]) > 0) do
    begin
      TFake(FItems[aIndex]) := TFake(FItems[ParentIdx]);
      aIndex := ParentIdx;
      ParentIdx := Pred(ParentIdx) shr 1;
    end;
  TFake(FItems[aIndex]) := v;
end;

class function TGBaseBinHeap.DoCompare(constref L, R: T): SizeInt;
begin
  Result := TCmpRel.Compare(L, R);
end;

class function TGBaseBinHeap.Comparator: TComparator;
begin
  Result := @DoCompare;
end;

{ TGObjectBinHeap }

procedure TGObjectBinHeap.DoClear;
var
  I: SizeInt;
begin
  if OwnsObjects then
    for I := 0 to Pred(ElemCount) do
      FItems[I].Free;
  inherited;
end;

constructor TGObjectBinHeap.Create(aOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := aOwnsObjects;
end;

constructor TGObjectBinHeap.Create(aCapacity: SizeInt; aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGObjectBinHeap.Create(constref A: array of T; aOwnsObjects: Boolean = True);
begin
  inherited Create(A);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGObjectBinHeap.Create(e: IEnumerable; aOwnsObjects: Boolean = True);
begin
  inherited Create(e);
  FOwnsObjects := aOwnsObjects;
end;

{ TGComparableBinHeapMax }

procedure TGComparableBinHeapMax.BuildHeap;
var
  I, CurrIdx, NextIdx, HighIdx: SizeInt;
  v: TFake;
begin
  if ElemCount > 1 then
    begin
      HighIdx := Pred(ElemCount);
      for I := Pred(ElemCount shr 1) downto 0 do
        begin
          CurrIdx := I;
          NextIdx := Succ(I shl 1);
          v := TFake(FItems[CurrIdx]);
          while NextIdx <= HighIdx do
            begin
              if(Succ(NextIdx) <= HighIdx) and (FItems[NextIdx] < FItems[Succ(NextIdx)])then
                Inc(NextIdx);
              if T(v) >= FItems[NextIdx] then
                break;
              TFake(FItems[CurrIdx]) := TFake(FItems[NextIdx]);
              CurrIdx := NextIdx;
              NextIdx := Succ(NextIdx shl 1);
            end;
          TFake(FItems[CurrIdx]) := v;
        end;
    end;
end;

procedure TGComparableBinHeapMax.SiftDown;
var
  CurrIdx, NextIdx, HighIdx: SizeInt;
  v: TFake;
begin
  HighIdx := Pred(ElemCount);
  if HighIdx > 0 then
    begin
      CurrIdx := 0;
      NextIdx := 1;
      v := TFake(FItems[0]);
      while NextIdx <= HighIdx do
        begin
          if (Succ(NextIdx) <= HighIdx) and (FItems[NextIdx] < FItems[Succ(NextIdx)]) then
            Inc(NextIdx);
          TFake(FItems[CurrIdx]) := TFake(FItems[NextIdx]);
          CurrIdx := NextIdx;
          NextIdx := Succ(NextIdx shl 1);
        end;
      NextIdx := Pred(CurrIdx) shr 1;
      while (CurrIdx > 0) and (T(v) > FItems[NextIdx]) do
        begin
          TFake(FItems[CurrIdx]) := TFake(FItems[NextIdx]);
          CurrIdx := NextIdx;
          NextIdx := Pred(NextIdx) shr 1;
        end;
      TFake(FItems[CurrIdx]) := v;
    end;
end;

procedure TGComparableBinHeapMax.FloatUp(aIndex: SizeInt);
var
  ParentIdx: SizeInt;
  v: TFake;
begin
  ParentIdx := Pred(aIndex) shr 1;
  v := TFake(FItems[aIndex]);
  while (aIndex > 0) and (T(v) > FItems[ParentIdx]) do
    begin
      TFake(FItems[aIndex]) := TFake(FItems[ParentIdx]);
      aIndex := ParentIdx;
      ParentIdx := Pred(ParentIdx) shr 1;
    end;
  TFake(FItems[aIndex]) := v;
end;

{ TGComparableBinHeap }

class function TGComparableBinHeap.DoCompare(constref L, R: T): SizeInt;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
end;

class function TGComparableBinHeap.Comparator: TComparator;
begin
  Result := @DoCompare;
end;

{ TGComparableBinHeapMin }

procedure TGComparableBinHeapMin.BuildHeap;
var
  I, CurrIdx, NextIdx, HighIdx: SizeInt;
  v: TFake;
begin
  if ElemCount > 1 then
    begin
      HighIdx := Pred(ElemCount);
      for I := Pred(ElemCount shr 1) downto 0 do
        begin
          CurrIdx := I;
          NextIdx := Succ(I shl 1);
          v := TFake(FItems[CurrIdx]);
          while NextIdx <= HighIdx do
            begin
              if(Succ(NextIdx) <= HighIdx) and (FItems[NextIdx] > FItems[Succ(NextIdx)])then
                Inc(NextIdx);
              if T(v) <= FItems[NextIdx] then
                break;
              TFake(FItems[CurrIdx]) := TFake(FItems[NextIdx]);
              CurrIdx := NextIdx;
              NextIdx := Succ(NextIdx shl 1);
            end;
          TFake(FItems[CurrIdx]) := v;
        end;
    end;
end;

procedure TGComparableBinHeapMin.SiftDown;
var
  CurrIdx, NextIdx, HighIdx: SizeInt;
  v: TFake;
begin
  HighIdx := Pred(ElemCount);
  if HighIdx > 0 then
    begin
      CurrIdx := 0;
      NextIdx := 1;
      v := TFake(FItems[0]);
      while NextIdx <= HighIdx do
        begin
          if (Succ(NextIdx) <= HighIdx) and (FItems[NextIdx] > FItems[Succ(NextIdx)]) then
            Inc(NextIdx);
          TFake(FItems[CurrIdx]) := TFake(FItems[NextIdx]);
          CurrIdx := NextIdx;
          NextIdx := Succ(NextIdx shl 1);
        end;
      NextIdx := Pred(CurrIdx) shr 1;
      while (CurrIdx > 0) and (T(v) < FItems[NextIdx]) do
        begin
          TFake(FItems[CurrIdx]) := TFake(FItems[NextIdx]);
          CurrIdx := NextIdx;
          NextIdx := Pred(NextIdx) shr 1;
        end;
      TFake(FItems[CurrIdx]) := v;
    end;
end;

procedure TGComparableBinHeapMin.FloatUp(aIndex: SizeInt);
var
  ParentIdx: SizeInt;
  v: TFake;
begin
  ParentIdx := Pred(aIndex) shr 1;
  v := TFake(FItems[aIndex]);
  while (aIndex > 0) and (T(v) < FItems[ParentIdx]) do
    begin
      TFake(FItems[aIndex]) := TFake(FItems[ParentIdx]);
      aIndex := ParentIdx;
      ParentIdx := Pred(ParentIdx) shr 1;
    end;
  TFake(FItems[aIndex]) := v;
end;

{ TGRegularBinHeap }

procedure TGRegularBinHeap.BuildHeap;
var
  I, CurrIdx, NextIdx, HighIdx: SizeInt;
  v: TFake;
begin
  if ElemCount > 1 then
    begin
      HighIdx := Pred(ElemCount);
      for I := Pred(ElemCount shr 1) downto 0 do
        begin
          CurrIdx := I;
          NextIdx := Succ(I shl 1);
          v := TFake(FItems[CurrIdx]);
          while NextIdx <= HighIdx do
            begin
              if (Succ(NextIdx) <= HighIdx) and (FCompare(FItems[NextIdx], FItems[Succ(NextIdx)]) < 0) then
                Inc(NextIdx);
              if FCompare(T(v), FItems[NextIdx]) >= 0 then
                break;
              TFake(FItems[CurrIdx]) := TFake(FItems[NextIdx]);
              CurrIdx := NextIdx;
              NextIdx := Succ(NextIdx shl 1);
            end;
          TFake(FItems[CurrIdx]) := v;
        end;
    end;
end;

procedure TGRegularBinHeap.SiftDown;
var
  CurrIdx, NextIdx, HighIdx: SizeInt;
  v: TFake;
begin
  HighIdx := Pred(ElemCount);
  if HighIdx > 0 then
    begin
      CurrIdx := 0;
      NextIdx := 1;
      v := TFake(FItems[0]);
      while NextIdx <= HighIdx do
        begin
          if(Succ(NextIdx) <= HighIdx) and (FCompare(FItems[NextIdx], FItems[Succ(NextIdx)]) < 0) then
            Inc(NextIdx);
          TFake(FItems[CurrIdx]) := TFake(FItems[NextIdx]);
          CurrIdx := NextIdx;
          NextIdx := Succ(NextIdx shl 1);
        end;
      NextIdx := Pred(CurrIdx) shr 1;
      while (CurrIdx > 0) and (FCompare(T(v), FItems[NextIdx]) > 0) do
        begin
          TFake(FItems[CurrIdx]) := TFake(FItems[NextIdx]);
          CurrIdx := NextIdx;
          NextIdx := Pred(NextIdx) shr 1;
        end;
      TFake(FItems[CurrIdx]) := v;
    end;
end;

procedure TGRegularBinHeap.FloatUp(aIndex: SizeInt);
var
  ParentIdx: SizeInt;
  v: TFake;
begin
  ParentIdx := Pred(aIndex) shr 1;
  v := TFake(FItems[aIndex]);
  while(aIndex > 0) and (FCompare(T(v), FItems[ParentIdx]) > 0) do
    begin
      TFake(FItems[aIndex]) := TFake(FItems[ParentIdx]);
      aIndex := ParentIdx;
      ParentIdx := Pred(ParentIdx) shr 1;
    end;
  TFake( FItems[aIndex]) := v;
end;

constructor TGRegularBinHeap.Create;
begin
  Create(TDefaults.Compare);
end;

constructor TGRegularBinHeap.Create(c: TComparator);
begin
  inherited Create;
  FCompare := c;
end;

constructor TGRegularBinHeap.Create(aCapacity: SizeInt; c: TComparator);
begin
  inherited Create(aCapacity);
  FCompare := c;
end;

constructor TGRegularBinHeap.Create(constref A: array of T; c: TComparator);
begin
  inherited Create(A);
  FCompare := c;
end;

constructor TGRegularBinHeap.Create(e: IEnumerable; c: TComparator);
begin
  inherited Create(e);
  FCompare := c;
end;

function TGRegularBinHeap.Comparator: TComparator;
begin
  Result := FCompare;
end;

{ TGDelegatedBinHeap }

procedure TGDelegatedBinHeap.BuildHeap;
var
  I, CurrIdx, NextIdx, HighIdx: SizeInt;
  v: TFake;
begin
  if ElemCount > 1 then
    begin
      HighIdx := Pred(ElemCount);
      for I := Pred(ElemCount shr 1) downto 0 do
        begin
          CurrIdx := I;
          NextIdx := Succ(I shl 1);
          v := TFake(FItems[CurrIdx]);
          while NextIdx <= HighIdx do
            begin
              if (Succ(NextIdx) <= HighIdx) and (FCompare(FItems[NextIdx], FItems[Succ(NextIdx)]) < 0) then
                Inc(NextIdx);
              if FCompare(T(v), FItems[NextIdx]) >= 0 then
                break;
              TFake(FItems[CurrIdx]) := TFake(FItems[NextIdx]);
              CurrIdx := NextIdx;
              NextIdx := Succ(NextIdx shl 1);
            end;
          TFake(FItems[CurrIdx]) := v;
        end;
    end;
end;

procedure TGDelegatedBinHeap.SiftDown;
var
  CurrIdx, NextIdx, HighIdx: SizeInt;
  v: TFake;
begin
  HighIdx := Pred(ElemCount);
  if HighIdx > 0 then
    begin
      CurrIdx := 0;
      NextIdx := 1;
      v := TFake(FItems[0]);
      while NextIdx <= HighIdx do
        begin
          if(Succ(NextIdx) <= HighIdx) and (FCompare(FItems[NextIdx], FItems[Succ(NextIdx)]) < 0) then
            Inc(NextIdx);
          TFake(FItems[CurrIdx]) := TFake(FItems[NextIdx]);
          CurrIdx := NextIdx;
          NextIdx := Succ(NextIdx shl 1);
        end;
      NextIdx := Pred(CurrIdx) shr 1;
      while (CurrIdx > 0) and (FCompare(T(v), FItems[NextIdx]) > 0) do
        begin
          TFake(FItems[CurrIdx]) := TFake(FItems[NextIdx]);
          CurrIdx := NextIdx;
          NextIdx := Pred(NextIdx) shr 1;
        end;
      TFake(FItems[CurrIdx]) := v;
    end;
end;

procedure TGDelegatedBinHeap.FloatUp(aIndex: SizeInt);
var
  ParentIdx: SizeInt;
  v: TFake;
begin
  ParentIdx := Pred(aIndex) shr 1;
  v := TFake(FItems[aIndex]);
  while(aIndex > 0) and (FCompare(T(v), FItems[ParentIdx]) > 0) do
    begin
      TFake(FItems[aIndex]) := TFake(FItems[ParentIdx]);
      aIndex := ParentIdx;
      ParentIdx := Pred(ParentIdx) shr 1;
    end;
  TFake( FItems[aIndex]) := v;
end;

constructor TGDelegatedBinHeap.Create;
begin
  Create(TDefaults.OnCompare);
end;

constructor TGDelegatedBinHeap.Create(c: TComparator);
begin
  inherited Create;
  FCompare := c;
end;

constructor TGDelegatedBinHeap.Create(aCapacity: SizeInt; c: TComparator);
begin
  inherited Create(aCapacity);
  FCompare := c;
end;

constructor TGDelegatedBinHeap.Create(constref A: array of T; c: TComparator);
begin
  inherited Create(A);
  FCompare := c;
end;

constructor TGDelegatedBinHeap.Create(e: IEnumerable; c: TComparator);
begin
  inherited Create(e);
  FCompare := c;
end;

function TGDelegatedBinHeap.Comparator: TOnCompare;
begin
  Result := FCompare;
end;

{ TGLiteBinHeap }

function TGLiteBinHeap.GetCapacity: SizeInt;
begin
  Result := FBuffer.Capacity;
end;

procedure TGLiteBinHeap.BuildHeap;
var
  I, CurrIdx, NextIdx, HighIdx: SizeInt;
  v: TFake;
begin
  if Count > 1 then
    begin
      HighIdx := Pred(Count);
      for I := Pred(Count shr 1) downto 0 do
        begin
          CurrIdx := I;
          NextIdx := Succ(I shl 1);
          v := TFake(FBuffer.FItems[CurrIdx]);
          while NextIdx <= HighIdx do
            begin
              if(Succ(NextIdx) <= HighIdx) and
                (TCmpRel.Compare(FBuffer.FItems[NextIdx], FBuffer.FItems[Succ(NextIdx)]) < 0)then
                Inc(NextIdx);
              if TCmpRel.Compare(T(v), FBuffer.FItems[NextIdx]) >= 0 then
                break;
              TFake(FBuffer.FItems[CurrIdx]) := TFake(FBuffer.FItems[NextIdx]);
              CurrIdx := NextIdx;
              NextIdx := Succ(NextIdx shl 1);
            end;
          TFake(FBuffer.FItems[CurrIdx]) := v;
        end;
    end;
end;

procedure TGLiteBinHeap.SiftDown;
var
  CurrIdx, NextIdx, HighIdx: SizeInt;
  v: TFake;
begin
  HighIdx := Pred(Count);
  if HighIdx > 0 then
    begin
      CurrIdx := 0;
      NextIdx := 1;
      v := TFake(FBuffer.FItems[0]);
      while NextIdx <= HighIdx do
        begin
          if(Succ(NextIdx) <= HighIdx) and
            (TCmpRel.Compare(FBuffer.FItems[NextIdx], FBuffer.FItems[Succ(NextIdx)]) < 0) then
            Inc(NextIdx);
          TFake(FBuffer.FItems[CurrIdx]) := TFake(FBuffer.FItems[NextIdx]);
          CurrIdx := NextIdx;
          NextIdx := Succ(NextIdx shl 1);
        end;
      NextIdx := Pred(CurrIdx) shr 1;
      while (CurrIdx > 0) and (TCmpRel.Compare(T(v), FBuffer.FItems[NextIdx]) > 0) do
        begin
          TFake(FBuffer.FItems[CurrIdx]) := TFake(FBuffer.FItems[NextIdx]);
          CurrIdx := NextIdx;
          NextIdx := Pred(NextIdx) shr 1;
        end;
      TFake(FBuffer.FItems[CurrIdx]) := v;
    end;
end;

procedure TGLiteBinHeap.FloatUp(aIndex: SizeInt);
var
  ParentIdx: SizeInt;
  v: TFake;
begin
  if aIndex > 0 then
    begin
      ParentIdx := Pred(aIndex) shr 1;
      v := TFake(FBuffer.FItems[aIndex]);
      while(aIndex > 0) and (TCmpRel.Compare(T(v), FBuffer.FItems[ParentIdx]) > 0) do
        begin
          TFake(FBuffer.FItems[aIndex]) := TFake(FBuffer.FItems[ParentIdx]);
          aIndex := ParentIdx;
          ParentIdx := Pred(ParentIdx) shr 1;
        end;
      TFake(FBuffer.FItems[aIndex]) := v;
    end;
end;

function TGLiteBinHeap.DequeueItem: T;
begin
  Result :=  FBuffer.FItems[0];
  Dec(FBuffer.FCount);
  if Count > 0 then
    begin
      FBuffer.FItems[0] := FBuffer.FItems[Count];
      FBuffer.FItems[Count] := Default(T);
      SiftDown;
    end
  else
    FBuffer.FItems[0] := Default(T);
end;

class function TGLiteBinHeap.DoCompare(constref L, R: T): SizeInt;
begin
  Result := TCmpRel.Compare(L, R);
end;

function TGLiteBinHeap.Comparator: TComparator;
begin
  Result := @DoCompare;
end;

function TGLiteBinHeap.GetEnumerator: TEnumerator;
begin
  Result := FBuffer.GetEnumerator;
end;

function TGLiteBinHeap.Mutable: TMutable;
begin
  Result := FBuffer.Mutable;
end;

function TGLiteBinHeap.Reverse: TReverse;
begin
  Result := FBuffer.Reverse;
end;

function TGLiteBinHeap.ToArray: TArray;
begin
  Result := FBuffer.ToArray;
end;

procedure TGLiteBinHeap.Clear;
begin
  FBuffer.Clear;
end;

procedure TGLiteBinHeap.Modified;
begin
  BuildHeap;
end;

function TGLiteBinHeap.IsEmpty: Boolean;
begin
  Result := FBuffer.Count = 0;
end;

function TGLiteBinHeap.NonEmpty: Boolean;
begin
  Result := FBuffer.Count <> 0;
end;

procedure TGLiteBinHeap.EnsureCapacity(aValue: SizeInt);
begin
  FBuffer.EnsureCapacity(aValue);
end;

procedure TGLiteBinHeap.TrimToFit;
begin
  FBuffer.TrimToFit;
end;

procedure TGLiteBinHeap.Enqueue(constref aValue: T);
begin
  FloatUp(FBuffer.PushLast(aValue));
end;

function TGLiteBinHeap.Dequeue: T;
begin
  FBuffer.CheckEmpty;
  Result := DequeueItem;
end;

function TGLiteBinHeap.TryDequeue(out aValue: T): Boolean;
begin
  Result := NonEmpty;
  if Result then
    aValue := DequeueItem;
end;

function TGLiteBinHeap.Peek: T;
begin
  FBuffer.CheckEmpty;
  Result := FBuffer.FItems[0];
end;

function TGLiteBinHeap.TryPeek(out aValue: T): Boolean;
begin
  Result := NonEmpty;
  if Result then
    aValue := FBuffer.FItems[0];
end;

{ TGLiteThreadBinHeap }

procedure TGLiteThreadBinHeap.DoLock;
begin
  System.EnterCriticalSection(FLock);
end;

constructor TGLiteThreadBinHeap.Create;
begin
  System.InitCriticalSection(FLock);
end;

destructor TGLiteThreadBinHeap.Destroy;
begin
  DoLock;
  try
    Finalize(FQueue);
    inherited;
  finally
    UnLock;
    System.DoneCriticalSection(FLock);
  end;
end;

procedure TGLiteThreadBinHeap.Clear;
begin
  DoLock;
  try
    FQueue.Clear;
  finally
    UnLock;
  end;
end;

procedure TGLiteThreadBinHeap.Enqueue(constref aValue: T);
begin
  DoLock;
  try
    FQueue.Enqueue(aValue);
  finally
    UnLock;
  end;
end;

function TGLiteThreadBinHeap.TryDequeue(out aValue: T): Boolean;
begin
  DoLock;
  try
    Result := FQueue.TryDequeue(aValue);
  finally
    UnLock;
  end;
end;

function TGLiteThreadBinHeap.TryPeek(out aValue: T): Boolean;
begin
  DoLock;
  try
    Result := FQueue.TryPeek(aValue);
  finally
    UnLock;
  end;
end;

function TGLiteThreadBinHeap.Lock: PQueue;
begin
  Result := @FQueue;
  DoLock;
end;

procedure TGLiteThreadBinHeap.Unlock;
begin
  System.LeaveCriticalSection(FLock);
end;

{ TGLiteComparableBinHeapMin }

function TGLiteComparableBinHeapMin.GetCapacity: SizeInt;
begin
  Result := FBuffer.Capacity;
end;

procedure TGLiteComparableBinHeapMin.BuildHeap;
var
  I, CurrIdx, NextIdx, HighIdx: SizeInt;
  v: TFake;
begin
  if Count > 1 then
    begin
      HighIdx := Pred(Count);
      for I := Pred(Count shr 1) downto 0 do
        begin
          CurrIdx := I;
          NextIdx := Succ(I shl 1);
          v := TFake(FBuffer.FItems[CurrIdx]);
          while NextIdx <= HighIdx do
            begin
              if(Succ(NextIdx) <= HighIdx) and (FBuffer.FItems[NextIdx] > FBuffer.FItems[Succ(NextIdx)])then
                Inc(NextIdx);
              if T(v) <= FBuffer.FItems[NextIdx] then
                break;
              TFake(FBuffer.FItems[CurrIdx]) := TFake(FBuffer.FItems[NextIdx]);
              CurrIdx := NextIdx;
              NextIdx := Succ(NextIdx shl 1);
            end;
          TFake(FBuffer.FItems[CurrIdx]) := v;
        end;
    end;
end;

procedure TGLiteComparableBinHeapMin.SiftDown;
var
  CurrIdx, NextIdx, HighIdx: SizeInt;
  v: TFake;
begin
  HighIdx := Pred(Count);
  if HighIdx > 0 then
    begin
      CurrIdx := 0;
      NextIdx := 1;
      v := TFake(FBuffer.FItems[0]);
      while NextIdx <= HighIdx do
        begin
          if(Succ(NextIdx) <= HighIdx) and (FBuffer.FItems[NextIdx] > FBuffer.FItems[Succ(NextIdx)]) then
            Inc(NextIdx);
          TFake(FBuffer.FItems[CurrIdx]) := TFake(FBuffer.FItems[NextIdx]);
          CurrIdx := NextIdx;
          NextIdx := Succ(NextIdx shl 1);
        end;
      NextIdx := Pred(CurrIdx) shr 1;
      while (CurrIdx > 0) and (T(v) < FBuffer.FItems[NextIdx]) do
        begin
          TFake(FBuffer.FItems[CurrIdx]) := TFake(FBuffer.FItems[NextIdx]);
          CurrIdx := NextIdx;
          NextIdx := Pred(NextIdx) shr 1;
        end;
      TFake(FBuffer.FItems[CurrIdx]) := v;
    end;
end;

procedure TGLiteComparableBinHeapMin.FloatUp(aIndex: SizeInt);
var
  ParentIdx: SizeInt;
  v: TFake;
begin
  if aIndex > 0 then
    begin
      ParentIdx := Pred(aIndex) shr 1;
      v := TFake(FBuffer.FItems[aIndex]);
      while(aIndex > 0) and (T(v) < FBuffer.FItems[ParentIdx]) do
        begin
          TFake(FBuffer.FItems[aIndex]) := TFake(FBuffer.FItems[ParentIdx]);
          aIndex := ParentIdx;
          ParentIdx := Pred(ParentIdx) shr 1;
        end;
      TFake(FBuffer.FItems[aIndex]) := v;
    end;
end;

function TGLiteComparableBinHeapMin.DequeueItem: T;
begin
  Result :=  FBuffer.FItems[0];
  Dec(FBuffer.FCount);
  if Count > 0 then
    begin
      FBuffer.FItems[0] := FBuffer.FItems[Count];
      FBuffer.FItems[Count] := Default(T);
      SiftDown;
    end
  else
    FBuffer.FItems[0] := Default(T);
end;

class function TGLiteComparableBinHeapMin.DoCompare(constref L, R: T): SizeInt;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
end;

function TGLiteComparableBinHeapMin.Comparator: TComparator;
begin
  Result := @DoCompare;
end;

function TGLiteComparableBinHeapMin.GetEnumerator: TEnumerator;
begin
  Result := FBuffer.GetEnumerator;
end;

function TGLiteComparableBinHeapMin.Mutable: TMutable;
begin
  Result := FBuffer.Mutable;
end;

function TGLiteComparableBinHeapMin.Reverse: TReverse;
begin
  Result := FBuffer.Reverse;
end;

function TGLiteComparableBinHeapMin.ToArray: TArray;
begin
  Result := FBuffer.ToArray;
end;

procedure TGLiteComparableBinHeapMin.Clear;
begin
  FBuffer.Clear;
end;

procedure TGLiteComparableBinHeapMin.Modified;
begin
  BuildHeap;
end;

function TGLiteComparableBinHeapMin.IsEmpty: Boolean;
begin
  Result := FBuffer.Count = 0;
end;

function TGLiteComparableBinHeapMin.NonEmpty: Boolean;
begin
  Result := FBuffer.Count <> 0;
end;

procedure TGLiteComparableBinHeapMin.EnsureCapacity(aValue: SizeInt);
begin
  FBuffer.EnsureCapacity(aValue);
end;

procedure TGLiteComparableBinHeapMin.TrimToFit;
begin
  FBuffer.TrimToFit;
end;

procedure TGLiteComparableBinHeapMin.Enqueue(constref aValue: T);
begin
  FloatUp(FBuffer.PushLast(aValue));
end;

function TGLiteComparableBinHeapMin.Dequeue: T;
begin
  FBuffer.CheckEmpty;
  Result := DequeueItem;
end;

function TGLiteComparableBinHeapMin.TryDequeue(out aValue: T): Boolean;
begin
  Result := NonEmpty;
  if Result then
    aValue := DequeueItem;
end;

function TGLiteComparableBinHeapMin.Peek: T;
begin
  FBuffer.CheckEmpty;
  Result := FBuffer.FItems[0];
end;

function TGLiteComparableBinHeapMin.TryPeek(out aValue: T): Boolean;
begin
  Result := NonEmpty;
  if Result then
    aValue := FBuffer.FItems[0];
end;

{ TGCustomBinHeapPQ.TEnumerator }

function TGCustomBinHeapPQ.TEnumerator.GetCurrent: T;
begin
  Result := FNodes[FHeap[FCurrIndex]].Data;
end;

constructor TGCustomBinHeapPQ.TEnumerator.Create(aQueue: TCustomBinHeapPQ);
begin
  inherited Create(aQueue);
  FHeap := aQueue.FHeap;
  FNodes := aQueue.FNodeList;
  FLast := Pred(aQueue.ElemCount);
  FCurrIndex := -1;
end;

function TGCustomBinHeapPQ.TEnumerator.MoveNext: Boolean;
begin
  Result := FCurrIndex < FLast;
  FCurrIndex += Ord(Result);
end;

procedure TGCustomBinHeapPQ.TEnumerator.Reset;
begin
  FCurrIndex := -1;
end;

{ TGCustomBinHeapPQ.TReverseEnum }

function TGCustomBinHeapPQ.TReverseEnum.GetCurrent: T;
begin
  Result := FNodes[FHeap[FCurrIndex]].Data;
end;

constructor TGCustomBinHeapPQ.TReverseEnum.Create(aQueue: TGCustomBinHeapPQ);
begin
  inherited Create(aQueue);
  FHeap := aQueue.FHeap;
  FNodes := aQueue.FNodeList;
  FCurrIndex := aQueue.ElemCount;
  FLast := Pred(FCurrIndex);
end;

function TGCustomBinHeapPQ.TReverseEnum.MoveNext: Boolean;
begin
  Result := FCurrIndex > 0;
  FCurrIndex -= Ord(Result);
end;

procedure TGCustomBinHeapPQ.TReverseEnum.Reset;
begin
  FCurrIndex := Succ(FLast);
end;

{ TGCustomBinHeapPQ }

procedure TGCustomBinHeapPQ.CheckEmpty;
begin
  if ElemCount = 0 then
    AccessEmptyError;
end;

function TGCustomBinHeapPQ.GetCount: SizeInt;
begin
  Result := FCount;
end;

function TGCustomBinHeapPQ.GetCapacity: SizeInt;
begin
  Result := HeapCapacity;
end;

function TGCustomBinHeapPQ.NodeListCapacity: SizeInt;
begin
  Result := System.Length(FNodeList);
end;

function TGCustomBinHeapPQ.HeapCapacity: SizeInt;
begin
  Result := System.Length(FHeap);
end;

procedure TGCustomBinHeapPQ.InitialAlloc(aCapacity: SizeInt);
begin
  if aCapacity >= 0 then
    begin
      System.SetLength(FHeap, aCapacity);
      System.SetLength(FNodeList, aCapacity);
    end
  else
    begin
      System.SetLength(FHeap, DEFAULT_CONTAINER_CAPACITY);
      System.SetLength(FNodeList, DEFAULT_CONTAINER_CAPACITY);
    end;
  FStackTop := NULL;
end;

procedure TGCustomBinHeapPQ.HeapExpand(aValue: SizeInt);
begin
  //there aValue > array capacity
  if aValue <= DEFAULT_CONTAINER_CAPACITY then
    System.SetLength(FHeap, DEFAULT_CONTAINER_CAPACITY)
  else
    if aValue <= MAX_CAPACITY then
      begin
        aValue := Math.Min(MAX_CAPACITY, LGUtils.RoundUpTwoPower(aValue));
        System.SetLength(FHeap, aValue);
      end
    else
      CapacityExceedError(aValue);
end;

procedure TGCustomBinHeapPQ.NodeListExpand(aValue: SizeInt);
begin
  //there aValue > FNodeList capacity
  if aValue <= DEFAULT_CONTAINER_CAPACITY then
    System.SetLength(FNodeList, DEFAULT_CONTAINER_CAPACITY)
  else
    if aValue <= MAX_NODELIST_CAPACITY then
      begin
        aValue := Math.Min(MAX_NODELIST_CAPACITY, LGUtils.RoundUpTwoPower(aValue));
        System.SetLength(FNodeList, aValue);
      end
    else
      CapacityExceedError(aValue);
end;

function TGCustomBinHeapPQ.StackPop: SizeInt;
begin
  Result := StackTop;
  if Result <> NULL then
    begin
      FStackTop := FNodeList[Result].HeapIndex;
      FNodeList[Result].HeapIndex := NULL;
    end;
end;

procedure TGCustomBinHeapPQ.StackPush(aValue: SizeInt);
begin
  FNodeList[aValue].HeapIndex := StackTop;
  FStackTop := aValue;
end;

function TGCustomBinHeapPQ.StackEmpty: Boolean;
begin
  Result := StackTop = NULL;
end;

procedure TGCustomBinHeapPQ.ItemAdding;
begin
  if ElemCount = HeapCapacity then
    HeapExpand(Succ(ElemCount));
  if StackEmpty and (NodeListCount = NodeListCapacity) then
    NodeListExpand(Succ(NodeListCount));
end;

function TGCustomBinHeapPQ.AddNode(constref aValue: T; aIndex: SizeInt): SizeInt;
begin
  if not StackEmpty then
    Result := StackPop
  else
    begin
      Result := NodeListCount;
      Inc(FNodeListCount);
    end;
  FNodeList[Result].Data := aValue;
  FNodeList[Result].HeapIndex := aIndex;
  Inc(FCount);
end;

function TGCustomBinHeapPQ.NodeListRemove(aIndex: SizeInt): T;
begin
  StackPush(aIndex);
  Result := FNodeList[aIndex].Data;
  FNodeList[aIndex].Data := Default(T);
end;

procedure TGCustomBinHeapPQ.CopyItems(aBuffer: PItem);
var
  I: SizeInt;
begin
  for I := 0 to Pred(ElemCount) do
    aBuffer[I] := FNodeList[FHeap[I]].Data;
end;

function TGCustomBinHeapPQ.DoEnqueue(constref aValue: T): SizeInt;
var
  InsertPos: SizeInt;
begin
  ItemAdding;
  InsertPos := ElemCount;
  Result := AddNode(aValue, InsertPos);
  FHeap[InsertPos] := Result;
  if InsertPos > 0 then
    FloatUp(InsertPos);
end;

function TGCustomBinHeapPQ.GetValue(aNodeIndex: SizeInt): T;
begin
  if (aNodeIndex >= 0) and (aNodeIndex < NodeListCount) then
    Result := FNodeList[aNodeIndex].Data
  else
    IndexOutOfBoundError(aNodeIndex);
end;

function TGCustomBinHeapPQ.DoDequeue: T;
begin
  Result := NodeListRemove(FHeap[0]);
  Dec(FCount);
  if Count > 0 then
    begin
      FHeap[0] := FHeap[ElemCount];
      SiftDown(0);
    end;
end;

function TGCustomBinHeapPQ.DoRemove(aNodeIndex: SizeInt): T;
var
  I: SizeInt;
begin
  if (aNodeIndex >= 0) and (aNodeIndex < NodeListCount) then
    begin
      I := FNodeList[aNodeIndex].HeapIndex;
      Result := NodeListRemove(aNodeIndex);
      Dec(FCount);
      FHeap[I] := FHeap[ElemCount];
      FNodeList[FHeap[I]].HeapIndex := I;
      SiftDown(I);
    end
  else
    IndexOutOfBoundError(aNodeIndex);
end;

function TGCustomBinHeapPQ.EnqueueArray(constref a: array of T): SizeInt;
var
  OldCount, I: SizeInt;
  v: T;
begin
  Result := System.Length(a);
  if Result > 0 then
    begin
      OldCount := ElemCount;
      DoEnsureCapacity(OldCount + Result);
      I := OldCount;
      for v in a do
        begin
          FHeap[I] := AddNode(v, I);
          Inc(I);
        end;
      if OldCount > 0 then
        FixHeap(OldCount)
      else
        BuildHeap;
    end;
end;

function TGCustomBinHeapPQ.EnqueueContainer(c: TCustomContainer): SizeInt;
var
  OldCount, I: SizeInt;
  v: T;
begin
  if c <> Self then
    begin
      Result := c.Count;
      if Result > 0 then
        begin
          OldCount := ElemCount;
          DoEnsureCapacity(OldCount + Result);
          I := OldCount;
          for v in c do
            begin
              FHeap[I] := AddNode(v, I);
              Inc(I);
            end;
          if OldCount > 0 then
            FixHeap(OldCount)
          else
            BuildHeap;
        end;
    end
  else
    Result := EnqueueArray(c.ToArray);
end;

function TGCustomBinHeapPQ.EnqueueEnum(e: IEnumerable): SizeInt;
var
  OldCount, I: SizeInt;
  v: T;
begin
  OldCount := ElemCount;
  Result := 0;
  I := OldCount;
  for v in e do
    begin
      ItemAdding;
      FHeap[I] := AddNode(v, I);
      Inc(I);
    end;
  Result := I - OldCount;
  if Result > 0 then
    begin
      if OldCount > 0 then
        FixHeap(OldCount)
      else
        BuildHeap;
    end;
end;

procedure TGCustomBinHeapPQ.FixHeap(aIndex: SizeInt);
begin
  for aIndex := aIndex to Pred(ElemCount) do
    FloatUp(aIndex);
end;

function TGCustomBinHeapPQ.MergeBinHeap(bh: TGCustomBinHeapPQ): SizeInt;
var
  OldCount, I, J: SizeInt;
begin
  Result := bh.ElemCount;
  if Result > 0 then
    begin
      OldCount := ElemCount;
      EnsureCapacity(ElemCount + Result);
      J := OldCount;
      for I := 0 to Pred(Result) do
        begin
          FHeap[J] := AddNode(bh.FNodeList[bh.FHeap[I]].Data, J);
          Inc(J);
        end;
      bh.Clear;
      if OldCount > 0 then
        FixHeap(OldCount)
      else
        BuildHeap;
    end;
end;

function TGCustomBinHeapPQ.DoGetEnumerator: TCustomEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TGCustomBinHeapPQ.DoClear;
begin
  FNodeList := nil;
  FHeap := nil;
  FCount := 0;
  FNodeListCount := 0;
  FStackTop := NULL;
end;

procedure TGCustomBinHeapPQ.DoTrimToFit;
begin
  System.SetLength(FHeap, ElemCount);
  if ElemCount = 0 then
    begin
      FStackTop := NULL;
      FNodeListCount := 0;
    end;
  if StackCount = 0 then
    System.SetLength(FNodeList, ElemCount)
  else
    System.SetLength(FNodeList, NodeListCount);
end;

procedure TGCustomBinHeapPQ.DoEnsureCapacity(aValue: SizeInt);
begin
  if aValue > HeapCapacity then
    HeapExpand(aValue);
  if aValue > NodeListCapacity then
    NodeListExpand(aValue);
end;

destructor TGCustomBinHeapPQ.Destroy;
begin
  DoClear;
  inherited;
end;

function TGCustomBinHeapPQ.Reverse: IEnumerable;
begin
  Result := TReverseEnum.Create(Self);
end;

procedure TGCustomBinHeapPQ.Enqueue(constref aValue: T);
begin
  CheckInIteration;
  DoEnqueue(aValue);
end;

function TGCustomBinHeapPQ.EnqueueAll(constref a: array of T): SizeInt;
begin
  CheckInIteration;
  Result := EnqueueArray(a);
end;

function TGCustomBinHeapPQ.EnqueueAll(e: IEnumerable): SizeInt;
var
  o: TObject;
begin
  CheckInIteration;
  o := e._GetRef;
  if o is TCustomContainer then
    Result := EnqueueContainer(TCustomContainer(o))
  else
    Result := EnqueueEnum(e);
end;

function TGCustomBinHeapPQ.Dequeue: T;
begin
  CheckInIteration;
  CheckEmpty;
  Result := DoDequeue;
end;

function TGCustomBinHeapPQ.TryDequeue(out aValue: T): Boolean;
begin
  Result := not InIteration and (ElemCount > 0);
  if Result then
    aValue := DoDequeue;
end;

function TGCustomBinHeapPQ.Peek: T;
begin
  CheckEmpty;
  Result := FNodeList[FHeap[0]].Data;
end;

function TGCustomBinHeapPQ.TryPeek(out aValue: T): Boolean;
begin
  Result := ElemCount > 0;
  if Result then
    aValue := FNodeList[FHeap[0]].Data;
end;

function TGCustomBinHeapPQ.Insert(constref aValue: T): THandle;
begin
  CheckInIteration;
  Result := THandle(DoEnqueue(aValue));
end;

function TGCustomBinHeapPQ.PeekHead: THandle;
begin
  CheckEmpty;
  Result := THandle(FHeap[0]);
end;

function TGCustomBinHeapPQ.TryPeekHead(out aHandle: THandle): Boolean;
begin
  Result := ElemCount > 0;
  if Result then
    aHandle := THandle(FHeap[0]);
end;

function TGCustomBinHeapPQ.Value(aHandle: THandle): T;
begin
  Result := GetValue(SizeInt(aHandle));
end;

procedure TGCustomBinHeapPQ.Update(aHandle: THandle; constref aValue: T);
begin
  CheckInIteration;
  DoUpdate(SizeInt(aHandle), aValue);
end;

function TGCustomBinHeapPQ.Remove(aHandle: THandle): T;
begin
  CheckInIteration;
  Result := DoRemove(SizeInt(aHandle));
end;

function TGCustomBinHeapPQ.Merge(aQueue: IPriorityQueue): SizeInt;
var
  o: TObject;
begin
  o := aQueue._GetRef;
  if o <> Self then
    begin
      CheckInIteration;
      TCustomContainer(o).CheckInIteration;
      if o is TCustomBinHeapPQ then
        Result := MergeBinHeap(TCustomBinHeapPQ(o))
      else
        begin
          Result := EnqueueAll(aQueue);
          aQueue.Clear;
        end;
    end
  else // else nothing to do
    Result := 0;
end;

{ TGBaseBinHeapPQ }

procedure TGBaseBinHeapPQ.BuildHeap;
var
  I, CurrIdx, NextIdx, HighIdx, CurrHandle: SizeInt;
begin
  if ElemCount > 1 then
    begin
      HighIdx := Pred(ElemCount);
      for I := Pred(ElemCount shr 1) downto 0 do
        begin
          CurrIdx := I;
          NextIdx := Succ(I shl 1);
          CurrHandle := FHeap[CurrIdx];
          while NextIdx <= HighIdx do
            begin
              if(Succ(NextIdx) <= HighIdx) and
                (TCmpRel.Compare(FNodeList[FHeap[NextIdx]].Data, FNodeList[FHeap[Succ(NextIdx)]].Data) < 0)then
                  Inc(NextIdx);
              if TCmpRel.Compare(FNodeList[CurrHandle].Data, FNodeList[FHeap[NextIdx]].Data) >= 0 then
                break;
              FHeap[CurrIdx] := FHeap[NextIdx];
              FNodeList[FHeap[CurrIdx]].HeapIndex := CurrIdx;
              CurrIdx := NextIdx;
              NextIdx := Succ(NextIdx shl 1);
            end;
          FHeap[CurrIdx] := CurrHandle;
          FNodeList[CurrHandle].HeapIndex := CurrIdx;
        end;
    end;
end;

procedure TGBaseBinHeapPQ.FloatUp(aIndex: SizeInt);
var
  ParentIdx, CurrHandle: SizeInt;
begin
  ParentIdx := Pred(aIndex) shr 1;
  CurrHandle := FHeap[aIndex];
  while(aIndex > 0) and (TCmpRel.Compare(FNodeList[CurrHandle].Data, FNodeList[FHeap[ParentIdx]].Data) > 0) do
    begin
      FHeap[aIndex] := FHeap[ParentIdx];
      FNodeList[FHeap[aIndex]].HeapIndex := aIndex;
      aIndex := ParentIdx;
      ParentIdx := Pred(ParentIdx) shr 1;
    end;
  FHeap[aIndex] := CurrHandle;
  FNodeList[CurrHandle].HeapIndex := aIndex;
end;

procedure TGBaseBinHeapPQ.SiftDown(aIndex: SizeInt);
var
  NextIdx, HighIdx, CurrHandle: SizeInt;
begin
  HighIdx := Pred(ElemCount);
  if HighIdx > 0 then
    begin
      NextIdx := Succ(aIndex shl 1);
      CurrHandle := FHeap[aIndex];
      while NextIdx <= HighIdx do
        begin
          if(Succ(NextIdx) <= HighIdx) and
            (TCmpRel.Compare(FNodeList[FHeap[NextIdx]].Data, FNodeList[FHeap[Succ(NextIdx)]].Data) < 0) then
              Inc(NextIdx);
          FHeap[aIndex] := FHeap[NextIdx];
          FNodeList[FHeap[aIndex]].HeapIndex := aIndex;
          aIndex := NextIdx;
          NextIdx := Succ(NextIdx shl 1);
        end;
      NextIdx := Pred(aIndex) shr 1;
      while(aIndex > 0)and(TCmpRel.Compare(FNodeList[CurrHandle].Data, FNodeList[FHeap[NextIdx]].Data) > 0) do
        begin
          FHeap[aIndex] := FHeap[NextIdx];
          FNodeList[FHeap[aIndex]].HeapIndex := aIndex;
          aIndex := NextIdx;
          NextIdx := Pred(NextIdx) shr 1;
        end;
      FHeap[aIndex] := CurrHandle;
      FNodeList[CurrHandle].HeapIndex := aIndex;
    end;
end;

procedure TGBaseBinHeapPQ.DoUpdate(aIndex: SizeInt; constref aValue: T);
var
  c: SizeInt;
begin
  if (aIndex >= 0) and (aIndex < NodeListCount) then
    begin
      c := TCmpRel.Compare(aValue, FNodeList[aIndex].Data);
      FNodeList[aIndex].Data := aValue;
      if c > 0 then
        FloatUp(FNodeList[aIndex].HeapIndex)
      else
        if c < 0 then
          SiftDown(FNodeList[aIndex].HeapIndex);
    end
  else
    IndexOutOfBoundError(aIndex);
end;

class function TGBaseBinHeapPQ.DoCompare(constref L, R: T): SizeInt;
begin
  Result := TCmpRel.Compare(L, R);
end;

class function TGBaseBinHeapPQ.Comparator: TComparator;
begin
  Result := @DoCompare;
end;

constructor TGBaseBinHeapPQ.Create;
begin
  InitialAlloc(DEFAULT_CONTAINER_CAPACITY);
end;

constructor TGBaseBinHeapPQ.Create(aCapacity: SizeInt);
begin
  InitialAlloc(aCapacity);
end;

constructor TGBaseBinHeapPQ.Create(constref A: array of T);
begin
  Create(System.Length(A));
  EnqueueAll(A);
end;

constructor TGBaseBinHeapPQ.Create(e: IEnumerable);
begin
  Create;
  EnqueueAll(e);
end;

{ TGComparableBinHeapPQ }

class function TGComparableBinHeapPQ.DoCompare(constref L, R: T): SizeInt;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
end;

class function TGComparableBinHeapPQ.Comparator: TComparator;
begin
  Result := @DoCompare;
end;

{ TGComparableBinHeapPQMax }

procedure TGComparableBinHeapPQMax.BuildHeap;
var
  I, CurrIdx, NextIdx, HighIdx, CurrHandle: SizeInt;
begin
  if ElemCount > 1 then
    begin
      HighIdx := Pred(ElemCount);
      for I := Pred(ElemCount shr 1) downto 0 do
        begin
          CurrIdx := I;
          NextIdx := Succ(I shl 1);
          CurrHandle := FHeap[CurrIdx];
          while NextIdx <= HighIdx do
            begin
              if(Succ(NextIdx) <= HighIdx) and
                (FNodeList[FHeap[NextIdx]].Data < FNodeList[FHeap[Succ(NextIdx)]].Data) then
                  Inc(NextIdx);
              if FNodeList[CurrHandle].Data >= FNodeList[FHeap[NextIdx]].Data then
                break;
              FHeap[CurrIdx] := FHeap[NextIdx];
              FNodeList[FHeap[CurrIdx]].HeapIndex := CurrIdx;
              CurrIdx := NextIdx;
              NextIdx := Succ(NextIdx shl 1);
            end;
          FHeap[CurrIdx] := CurrHandle;
          FNodeList[CurrHandle].HeapIndex := CurrIdx;
        end;
    end;
end;

procedure TGComparableBinHeapPQMax.FloatUp(aIndex: SizeInt);
var
  ParentIdx, CurrHandle: SizeInt;
begin
  ParentIdx := Pred(aIndex) shr 1;
  CurrHandle := FHeap[aIndex];
  while (aIndex > 0) and (FNodeList[CurrHandle].Data > FNodeList[FHeap[ParentIdx]].Data) do
    begin
      FHeap[aIndex] := FHeap[ParentIdx];
      FNodeList[FHeap[aIndex]].HeapIndex := aIndex;
      aIndex := ParentIdx;
      ParentIdx := Pred(ParentIdx) shr 1;
    end;
  FHeap[aIndex] := CurrHandle;
  FNodeList[CurrHandle].HeapIndex := aIndex;
end;

procedure TGComparableBinHeapPQMax.SiftDown(aIndex: SizeInt);
var
  NextIdx, HighIdx, CurrHandle: SizeInt;
begin
  HighIdx := Pred(ElemCount);
  if HighIdx > 0 then
    begin
      NextIdx := Succ(aIndex shl 1);
      CurrHandle := FHeap[aIndex];
      while NextIdx <= HighIdx do
        begin
          if(Succ(NextIdx) <= HighIdx) and
            (FNodeList[FHeap[NextIdx]].Data < FNodeList[FHeap[Succ(NextIdx)]].Data) then
              Inc(NextIdx);
          FHeap[aIndex] := FHeap[NextIdx];
          FNodeList[FHeap[aIndex]].HeapIndex := aIndex;
          aIndex := NextIdx;
          NextIdx := Succ(NextIdx shl 1);
        end;
      NextIdx := Pred(aIndex) shr 1;
      while (aIndex > 0) and (FNodeList[CurrHandle].Data > FNodeList[FHeap[NextIdx]].Data) do
        begin
          FHeap[aIndex] := FHeap[NextIdx];
          FNodeList[FHeap[aIndex]].HeapIndex := aIndex;
          aIndex := NextIdx;
          NextIdx := Pred(NextIdx) shr 1;
        end;
      FHeap[aIndex] := CurrHandle;
      FNodeList[CurrHandle].HeapIndex := aIndex;
    end;
end;

procedure TGComparableBinHeapPQMax.DoUpdate(aIndex: SizeInt; constref aValue: T);
begin
  if (aIndex >= 0) and (aIndex < NodeListCount) then
    begin
      if aValue > FNodeList[aIndex].Data then
        begin
          FNodeList[aIndex].Data := aValue;
          FloatUp(FNodeList[aIndex].HeapIndex);
        end
      else
        if aValue < FNodeList[aIndex].Data then
          begin
            FNodeList[aIndex].Data := aValue;
            SiftDown(FNodeList[aIndex].HeapIndex);
          end;
    end
  else
    IndexOutOfBoundError(aIndex);
end;

constructor TGComparableBinHeapPQMax.Create;
begin
  InitialAlloc(DEFAULT_CONTAINER_CAPACITY);
end;

constructor TGComparableBinHeapPQMax.Create(aCapacity: SizeInt);
begin
  InitialAlloc(aCapacity);
end;

constructor TGComparableBinHeapPQMax.Create(constref A: array of T);
begin
  Create(System.Length(A));
  EnqueueAll(A);
end;

constructor TGComparableBinHeapPQMax.Create(e: IEnumerable);
begin
  Create;
  EnqueueAll(e);
end;

{ TGComparableBinHeapPQMin }

procedure TGComparableBinHeapPQMin.BuildHeap;
var
  I, CurrIdx, NextIdx, HighIdx, CurrHandle: SizeInt;
begin
  if ElemCount > 1 then
    begin
      HighIdx := Pred(ElemCount);
      for I := Pred(ElemCount shr 1) downto 0 do
        begin
          CurrIdx := I;
          NextIdx := Succ(I shl 1);
          CurrHandle := FHeap[CurrIdx];
          while NextIdx <= HighIdx do
            begin
              if(Succ(NextIdx) <= HighIdx) and
                (FNodeList[FHeap[NextIdx]].Data > FNodeList[FHeap[Succ(NextIdx)]].Data) then
                  Inc(NextIdx);
              if FNodeList[CurrHandle].Data <= FNodeList[FHeap[NextIdx]].Data then
                break;
              FHeap[CurrIdx] := FHeap[NextIdx];
              FNodeList[FHeap[CurrIdx]].HeapIndex := CurrIdx;
              CurrIdx := NextIdx;
              NextIdx := Succ(NextIdx shl 1);
            end;
          FHeap[CurrIdx] := CurrHandle;
          FNodeList[CurrHandle].HeapIndex := CurrIdx;
        end;
    end;
end;

procedure TGComparableBinHeapPQMin.FloatUp(aIndex: SizeInt);
var
  ParentIdx, CurrHandle: SizeInt;
begin
  ParentIdx := Pred(aIndex) shr 1;
  CurrHandle := FHeap[aIndex];
  while (aIndex > 0) and (FNodeList[CurrHandle].Data < FNodeList[FHeap[ParentIdx]].Data) do
    begin
      FHeap[aIndex] := FHeap[ParentIdx];
      FNodeList[FHeap[aIndex]].HeapIndex := aIndex;
      aIndex := ParentIdx;
      ParentIdx := Pred(ParentIdx) shr 1;
    end;
  FHeap[aIndex] := CurrHandle;
  FNodeList[CurrHandle].HeapIndex := aIndex;
end;

procedure TGComparableBinHeapPQMin.SiftDown(aIndex: SizeInt);
var
  NextIdx, HighIdx, CurrHandle: SizeInt;
begin
  HighIdx := Pred(ElemCount);
  if HighIdx > 0 then
    begin
      NextIdx := Succ(aIndex shl 1);
      CurrHandle := FHeap[aIndex];
      while NextIdx <= HighIdx do
        begin
          if(Succ(NextIdx) <= HighIdx) and
            (FNodeList[FHeap[NextIdx]].Data > FNodeList[FHeap[Succ(NextIdx)]].Data) then
              Inc(NextIdx);
          FHeap[aIndex] := FHeap[NextIdx];
          FNodeList[FHeap[aIndex]].HeapIndex := aIndex;
          aIndex := NextIdx;
          NextIdx := Succ(NextIdx shl 1);
        end;
      NextIdx := Pred(aIndex) shr 1;
      while (aIndex > 0) and (FNodeList[CurrHandle].Data < FNodeList[FHeap[NextIdx]].Data) do
        begin
          FHeap[aIndex] := FHeap[NextIdx];
          FNodeList[FHeap[aIndex]].HeapIndex := aIndex;
          aIndex := NextIdx;
          NextIdx := Pred(NextIdx) shr 1;
        end;
      FHeap[aIndex] := CurrHandle;
      FNodeList[CurrHandle].HeapIndex := aIndex;
    end;
end;

procedure TGComparableBinHeapPQMin.DoUpdate(aIndex: SizeInt; constref aValue: T);
begin
  if (aIndex >= 0) and (aIndex < NodeListCount) then
    begin
      if aValue < FNodeList[aIndex].Data then
        begin
          FNodeList[aIndex].Data := aValue;
          FloatUp(FNodeList[aIndex].HeapIndex);
        end
      else
        if aValue > FNodeList[aIndex].Data then
          begin
            FNodeList[aIndex].Data := aValue;
            SiftDown(FNodeList[aIndex].HeapIndex);
          end;
    end
  else
    IndexOutOfBoundError(aIndex);
end;

constructor TGComparableBinHeapPQMin.Create;
begin
  InitialAlloc(DEFAULT_CONTAINER_CAPACITY);
end;

constructor TGComparableBinHeapPQMin.Create(aCapacity: SizeInt);
begin
  InitialAlloc(aCapacity);
end;

constructor TGComparableBinHeapPQMin.Create(constref A: array of T);
begin
  Create(System.Length(A));
  EnqueueAll(A);
end;

constructor TGComparableBinHeapPQMin.Create(e: IEnumerable);
begin
  Create;
  EnqueueAll(e);
end;

{ TGRegularBinHeapPQ }

procedure TGRegularBinHeapPQ.BuildHeap;
var
  I, CurrIdx, NextIdx, HighIdx, CurrHandle: SizeInt;
begin
  if ElemCount > 1 then
    begin
      HighIdx := Pred(ElemCount);
      for I := Pred(ElemCount shr 1) downto 0 do
        begin
          CurrIdx := I;
          NextIdx := Succ(I shl 1);
          CurrHandle := FHeap[CurrIdx];
          while NextIdx <= HighIdx do
            begin
              if(Succ(NextIdx) <= HighIdx) and
                (FCompare(FNodeList[FHeap[NextIdx]].Data, FNodeList[FHeap[Succ(NextIdx)]].Data) < 0)then
                  Inc(NextIdx);
              if FCompare(FNodeList[CurrHandle].Data, FNodeList[FHeap[NextIdx]].Data) >= 0 then
                break;
              FHeap[CurrIdx] := FHeap[NextIdx];
              FNodeList[FHeap[CurrIdx]].HeapIndex := CurrIdx;
              CurrIdx := NextIdx;
              NextIdx := Succ(NextIdx shl 1);
            end;
          FHeap[CurrIdx] := CurrHandle;
          FNodeList[CurrHandle].HeapIndex := CurrIdx;
        end;
    end;
end;

procedure TGRegularBinHeapPQ.FloatUp(aIndex: SizeInt);
var
  ParentIdx, CurrHandle: SizeInt;
begin
  ParentIdx := Pred(aIndex) shr 1;
  CurrHandle := FHeap[aIndex];
  while(aIndex > 0) and (FCompare(FNodeList[CurrHandle].Data, FNodeList[FHeap[ParentIdx]].Data) > 0) do
    begin
      FHeap[aIndex] := FHeap[ParentIdx];
      FNodeList[FHeap[aIndex]].HeapIndex := aIndex;
      aIndex := ParentIdx;
      ParentIdx := Pred(ParentIdx) shr 1;
    end;
  FHeap[aIndex] := CurrHandle;
  FNodeList[CurrHandle].HeapIndex := aIndex;
end;

procedure TGRegularBinHeapPQ.SiftDown(aIndex: SizeInt);
var
  NextIdx, HighIdx, CurrHandle: SizeInt;
begin
  HighIdx := Pred(ElemCount);
  if HighIdx > 0 then
    begin
      NextIdx := Succ(aIndex shl 1);
      CurrHandle := FHeap[aIndex];
      while NextIdx <= HighIdx do
        begin
          if(Succ(NextIdx) <= HighIdx) and
            (FCompare(FNodeList[FHeap[NextIdx]].Data, FNodeList[FHeap[Succ(NextIdx)]].Data) < 0) then
              Inc(NextIdx);
          FHeap[aIndex] := FHeap[NextIdx];
          FNodeList[FHeap[aIndex]].HeapIndex := aIndex;
          aIndex := NextIdx;
          NextIdx := Succ(NextIdx shl 1);
        end;
      NextIdx := Pred(aIndex) shr 1;
      while (aIndex > 0) and (FCompare(FNodeList[CurrHandle].Data, FNodeList[FHeap[NextIdx]].Data) > 0) do
        begin
          FHeap[aIndex] := FHeap[NextIdx];
          FNodeList[FHeap[aIndex]].HeapIndex := aIndex;
          aIndex := NextIdx;
          NextIdx := Pred(NextIdx) shr 1;
        end;
      FHeap[aIndex] := CurrHandle;
      FNodeList[CurrHandle].HeapIndex := aIndex;
    end;
end;

procedure TGRegularBinHeapPQ.DoUpdate(aIndex: SizeInt; constref aValue: T);
var
  c: SizeInt;
begin
  if (aIndex >= 0) and (aIndex < NodeListCount) then
    begin
      c := FCompare(aValue, FNodeList[aIndex].Data);
      FNodeList[aIndex].Data := aValue;
      if c > 0 then
        FloatUp(FNodeList[aIndex].HeapIndex)
      else
        if c < 0 then
          SiftDown(FNodeList[aIndex].HeapIndex);
    end
  else
    IndexOutOfBoundError(aIndex);
end;

constructor TGRegularBinHeapPQ.Create;
begin
  Create(TDefaults.Compare);
end;

constructor TGRegularBinHeapPQ.Create(c: TComparator);
begin
  FCompare := c;
  InitialAlloc(DEFAULT_CONTAINER_CAPACITY);
end;

constructor TGRegularBinHeapPQ.Create(aCapacity: SizeInt; c: TComparator);
begin
  FCompare := c;
  InitialAlloc(aCapacity);
end;

constructor TGRegularBinHeapPQ.Create(constref A: array of T; c: TComparator);
begin
  Create(System.Length(A), c);
  EnqueueAll(A);
end;

constructor TGRegularBinHeapPQ.Create(e: IEnumerable; c: TComparator);
begin
  Create(c);
  EnqueueAll(e);
end;

function TGRegularBinHeapPQ.Comparator: TComparator;
begin
  Result := FCompare;
end;

{ TGDelegatedBinHeapPQ }

procedure TGDelegatedBinHeapPQ.BuildHeap;
var
  I, CurrIdx, NextIdx, HighIdx, CurrHandle: SizeInt;
begin
  if ElemCount > 1 then
    begin
      HighIdx := Pred(ElemCount);
      for I := Pred(ElemCount shr 1) downto 0 do
        begin
          CurrIdx := I;
          NextIdx := Succ(I shl 1);
          CurrHandle := FHeap[CurrIdx];
          while NextIdx <= HighIdx do
            begin
              if(Succ(NextIdx) <= HighIdx) and
                (FCompare(FNodeList[FHeap[NextIdx]].Data, FNodeList[FHeap[Succ(NextIdx)]].Data) < 0)then
                  Inc(NextIdx);
              if FCompare(FNodeList[CurrHandle].Data, FNodeList[FHeap[NextIdx]].Data) >= 0 then
                break;
              FHeap[CurrIdx] := FHeap[NextIdx];
              FNodeList[FHeap[CurrIdx]].HeapIndex := CurrIdx;
              CurrIdx := NextIdx;
              NextIdx := Succ(NextIdx shl 1);
            end;
          FHeap[CurrIdx] := CurrHandle;
          FNodeList[CurrHandle].HeapIndex := CurrIdx;
        end;
    end;
end;

procedure TGDelegatedBinHeapPQ.FloatUp(aIndex: SizeInt);
var
  ParentIdx, CurrHandle: SizeInt;
begin
  ParentIdx := Pred(aIndex) shr 1;
  CurrHandle := FHeap[aIndex];
  while(aIndex > 0) and (FCompare(FNodeList[CurrHandle].Data, FNodeList[FHeap[ParentIdx]].Data) > 0) do
    begin
      FHeap[aIndex] := FHeap[ParentIdx];
      FNodeList[FHeap[aIndex]].HeapIndex := aIndex;
      aIndex := ParentIdx;
      ParentIdx := Pred(ParentIdx) shr 1;
    end;
  FHeap[aIndex] := CurrHandle;
  FNodeList[CurrHandle].HeapIndex := aIndex;
end;

procedure TGDelegatedBinHeapPQ.SiftDown(aIndex: SizeInt);
var
  NextIdx, HighIdx, CurrHandle: SizeInt;
begin
  HighIdx := Pred(ElemCount);
  if HighIdx > 0 then
    begin
      NextIdx := Succ(aIndex shl 1);
      CurrHandle := FHeap[aIndex];
      while NextIdx <= HighIdx do
        begin
          if(Succ(NextIdx) <= HighIdx) and
            (FCompare(FNodeList[FHeap[NextIdx]].Data, FNodeList[FHeap[Succ(NextIdx)]].Data) < 0) then
              Inc(NextIdx);
          FHeap[aIndex] := FHeap[NextIdx];
          FNodeList[FHeap[aIndex]].HeapIndex := aIndex;
          aIndex := NextIdx;
          NextIdx := Succ(NextIdx shl 1);
        end;
      NextIdx := Pred(aIndex) shr 1;
      while (aIndex > 0) and (FCompare(FNodeList[CurrHandle].Data, FNodeList[FHeap[NextIdx]].Data) > 0) do
        begin
          FHeap[aIndex] := FHeap[NextIdx];
          FNodeList[FHeap[aIndex]].HeapIndex := aIndex;
          aIndex := NextIdx;
          NextIdx := Pred(NextIdx) shr 1;
        end;
      FHeap[aIndex] := CurrHandle;
      FNodeList[CurrHandle].HeapIndex := aIndex;
    end;
end;

procedure TGDelegatedBinHeapPQ.DoUpdate(aIndex: SizeInt; constref aValue: T);
var
  c: SizeInt;
begin
  if (aIndex >= 0) and (aIndex < NodeListCount) then
    begin
      c := FCompare(aValue, FNodeList[aIndex].Data);
      FNodeList[aIndex].Data := aValue;
      if c > 0 then
        FloatUp(FNodeList[aIndex].HeapIndex)
      else
        if c < 0 then
          SiftDown(FNodeList[aIndex].HeapIndex);
    end
  else
    IndexOutOfBoundError(aIndex);
end;

constructor TGDelegatedBinHeapPQ.Create;
begin
  Create(TDefaults.OnCompare);
end;

constructor TGDelegatedBinHeapPQ.Create(c: TComparator);
begin
  FCompare := c;
  InitialAlloc(DEFAULT_CONTAINER_CAPACITY);
end;

constructor TGDelegatedBinHeapPQ.Create(aCapacity: SizeInt; c: TComparator);
begin
  FCompare := c;
  InitialAlloc(aCapacity);
end;

constructor TGDelegatedBinHeapPQ.Create(constref A: array of T; c: TComparator);
begin
  Create(System.Length(A), c);
  EnqueueAll(A);
end;

constructor TGDelegatedBinHeapPQ.Create(e: IEnumerable; c: TComparator);
begin
  Create(c);
  EnqueueAll(e);
end;

function TGDelegatedBinHeapPQ.Comparator: TComparator;
begin
  Result := FCompare;
end;

{ TGCustomPairingHeap.TNode }

procedure TGCustomPairingHeap.TNode.AddChild(aNode: PNode);
begin
  if aNode <> nil then
    begin
      aNode^.Prev := @Self;
      Sibling :=  aNode^.Sibling;
      if Sibling <> nil then
        Sibling^.Prev := @Self;
      aNode^.Sibling := Child;
      if aNode^.Sibling <> nil then
        aNode^.Sibling^.Prev := aNode;
      Child := aNode;
    end;
end;

procedure TGCustomPairingHeap.TNode.ClearLinks;
begin
  Sibling := nil;
  Child := nil;
  Prev := nil;
end;

function TGCustomPairingHeap.TNode.Successor: PNode;
begin
  Result := Sibling;
  if Result <> nil then
    while Result^.Child <> nil do
      Result := Result^.Child
  else
    begin
      Result := @Self;
      while (Result^.Prev <> nil) and (Result^.Prev^.Sibling = Result) do
        Result := Result^.Prev;
      Result := Result^.Prev;
    end;
end;

function TGCustomPairingHeap.TNode.Predecessor: PNode;
begin
  Result := Child;
  if Result <> nil then
    while Result^.Sibling <> nil do
      Result := Result^.Sibling
  else
    begin
      Result := @Self;
      while (Result^.Prev <> nil) and (Result^.Prev^.Child = Result) do
        Result := Result^.Prev;
      Result := Result^.Prev;
    end;
end;

{ TGCustomPairingHeap.TEnumerator }

function TGCustomPairingHeap.TEnumerator.GetCurrent: T;
begin
  Result := FCurrNode^.Data;
end;

constructor TGCustomPairingHeap.TEnumerator.Create(aHeap: TCustomPairingHeap);
begin
  inherited Create(aHeap);
  FLeftmost := aHeap.FindLeftmost;
end;

function TGCustomPairingHeap.TEnumerator.MoveNext: Boolean;
var
  NextNode: PNode = nil;
begin
  if FCurrNode <> nil then
    NextNode := FCurrNode^.Successor
  else
    if not FInCycle then
      begin
        NextNode := FLeftmost;
        FInCycle := True;
      end;
  Result := NextNode <> nil;
  if Result then
    FCurrNode := NextNode;
end;

procedure TGCustomPairingHeap.TEnumerator.Reset;
begin
  FCurrNode := nil;
  FInCycle := False;
end;

{ TGCustomPairingHeap.TReverseEnum }

function TGCustomPairingHeap.TReverseEnum.GetCurrent: T;
begin
  Result := FCurrNode^.Data;
end;

constructor TGCustomPairingHeap.TReverseEnum.Create(aHeap: TCustomPairingHeap);
begin
  inherited Create(aHeap);
  FRightmost := aHeap.FindRightmost;
end;

function TGCustomPairingHeap.TReverseEnum.MoveNext: Boolean;
var
  NextNode: PNode = nil;
begin
  if FCurrNode <> nil then
    NextNode := FCurrNode^.Predecessor
  else
    if not FInCycle then
      begin
        NextNode := FRightmost;
        FInCycle := True;
      end;
  Result := NextNode <> nil;
  if Result then
    FCurrNode := NextNode;
end;

procedure TGCustomPairingHeap.TReverseEnum.Reset;
begin
  FCurrNode := nil;
  FInCycle := False;
end;

{ TGCustomPairingHeap }

procedure TGCustomPairingHeap.CheckEmpty;
begin
  if ElemCount = 0 then
    AccessEmptyError;
end;

function TGCustomPairingHeap.GetCount: SizeInt;
begin
  Result := FCount;
end;

function TGCustomPairingHeap.GetCapacity: SizeInt;
begin
  Result := ElemCount + FNodeManager.FreeCount;
end;

procedure TGCustomPairingHeap.CopyItems(aBuffer: PItem);
var
  Node: PNode;
  I: SizeInt = 0;
begin
  Node := FindLeftmost;
  while Node <> nil do
    begin
      aBuffer[I] := Node^.Data;
      Inc(I);
      Node := Node^.Successor;
    end;
end;

function TGCustomPairingHeap.DoGetEnumerator: TCustomEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TGCustomPairingHeap.DoClear;
begin
  ClearTree;
  FNodeManager.Clear;
end;

procedure TGCustomPairingHeap.DoTrimToFit;
begin
  if ElemCount > 0 then
    FNodeManager.ClearFreeList
  else
    FNodeManager.Clear;
end;

procedure TGCustomPairingHeap.DoEnsureCapacity(aValue: SizeInt);
begin
  FNodeManager.EnsureFreeCount(aValue - Capacity);
end;

function TGCustomPairingHeap.NewNode(constref aValue: T): PNode;
begin
  Result := FNodeManager.NewNode;
  Result^.Data := aValue;
  Inc(FCount);
end;

procedure TGCustomPairingHeap.DisposeNode(aNode: PNode);
begin
  if aNode <> nil then
    begin
      aNode^ := Default(TNode);
      FNodeManager.FreeNode(aNode);
      Dec(FCount);
    end;
end;

procedure TGCustomPairingHeap.RemoveNodeWithChilds(aNode: PNode);
var
  OldPrev, OldNode, CurrNode: PNode;
  FromSibling: Boolean = False;
begin
  if aNode <> nil then
    begin
      CurrNode := aNode;
      repeat  //postorder traverse
        while FromSibling do
          begin
            OldPrev := CurrNode^.Prev;
            OldNode := CurrNode;
            /////////////////////////////////
            CurrNode^.Data := Default(T);
            FNodeManager.DisposeNode(CurrNode);
            Dec(FCount);
            ////////////////////////////////
            if CurrNode = aNode then
              exit;
            CurrNode := OldPrev;
            FromSibling := CurrNode^.Sibling = OldNode;
            if not FromSibling and (CurrNode^.Sibling <> nil) then
              CurrNode := CurrNode^.Sibling
            else
              FromSibling := True;
          end;
        while CurrNode^.Child <> nil do
          CurrNode := CurrNode^.Child;
        if CurrNode^.Sibling <> nil then
          CurrNode := CurrNode^.Sibling
        else
          FromSibling := True;
      until False;
    end;
end;

procedure TGCustomPairingHeap.ClearTree;
begin
  RemoveNodeWithChilds(FRoot);
  FRoot := nil;
end;

function TGCustomPairingHeap.FindLeftmost: PNode;
begin
  Result := FRoot;
  if Result <> nil then
    while Result^.Child <> nil do
      Result := Result^.Child;
end;

function TGCustomPairingHeap.FindRightmost: PNode;
begin
  Result := FRoot;
  if Result <> nil then
    while Result^.Sibling <> nil do
      Result := Result^.Sibling;
end;

function TGCustomPairingHeap.EnqueueArray(constref a: array of T): SizeInt;
var
  v: T;
begin
  Result := System.Length(a);
  if Result > 0 then
    begin
      DoEnsureCapacity(ElemCount + Result);
      for v in a do
        DoEnqueue(v);
    end;
end;

function TGCustomPairingHeap.EnqueueContainer(c: TCustomContainer): SizeInt;
var
  v: T;
begin
  if c <> Self then
    begin
      Result := c.Count;
      DoEnsureCapacity(ElemCount + Result);
      for v in c do
        DoEnqueue(v);
    end
  else
    Result := EnqueueArray(c.ToArray);
end;

function TGCustomPairingHeap.EnqueueEnum(e: IEnumerable): SizeInt;
var
  v: T;
begin
  Result := 0;
  for v in e do
    begin
      DoEnqueue(v);
      Inc(Result);
    end;
end;

class procedure TGCustomPairingHeap.CutNode(aNode: PNode);
begin
  if aNode^.Sibling <> nil then
    aNode^.Sibling^.Prev := aNode^.Prev;
  if aNode^.Prev^.Child = aNode then
    aNode^.Prev^.Child := aNode^.Sibling
  else
    aNode^.Prev^.Sibling := aNode^.Sibling;
  aNode^.Sibling := nil;
end;

destructor TGCustomPairingHeap.Destroy;
begin
  DoClear;
  inherited;
end;

function TGCustomPairingHeap.Reverse: IEnumerable;
begin
  Result := TReverseEnum.Create(Self);
end;

procedure TGCustomPairingHeap.Enqueue(constref aValue: T);
begin
  CheckInIteration;
  DoEnqueue(aValue);
end;

function TGCustomPairingHeap.EnqueueAll(constref a: array of T): SizeInt;
begin
  CheckInIteration;
  Result := EnqueueArray(a);
end;

function TGCustomPairingHeap.EnqueueAll(e: IEnumerable): SizeInt;
var
  o: TObject;
begin
  CheckInIteration;
  o := e._GetRef;
  if o is TCustomContainer then
    Result := EnqueueContainer(TCustomContainer(o))
  else
    Result := EnqueueEnum(e);
end;

function TGCustomPairingHeap.Dequeue: T;
begin
  CheckInIteration;
  CheckEmpty;
  Result := DoDequeue;
end;

function TGCustomPairingHeap.TryDequeue(out aValue: T): Boolean;
begin
  Result := not InIteration and (ElemCount > 0);
  if Result then
    aValue := DoDequeue;
end;

function TGCustomPairingHeap.Peek: T;
begin
  CheckEmpty;
  Result := FRoot^.Data;
end;

function TGCustomPairingHeap.TryPeek(out aValue: T): Boolean;
begin
  Result := ElemCount > 0;
  if Result then
    aValue := FRoot^.Data;
end;

function TGCustomPairingHeap.Insert(constref aValue: T): THandle;
begin
  CheckInIteration;
  Result := {%H-}THandle(DoEnqueue(aValue));
end;

function TGCustomPairingHeap.PeekHead: THandle;
begin
  CheckEmpty;
  Result := {%H-}THandle(FRoot);
end;

function TGCustomPairingHeap.TryPeekHead(out aHandle: THandle): Boolean;
begin
  Result := ElemCount > 0;
  if Result then
    aHandle := {%H-}THandle(FRoot);
end;

function TGCustomPairingHeap.Value(aHandle: THandle): T;
begin
  Result := {%H-}PNode(aHandle)^.Data;
end;

procedure TGCustomPairingHeap.Update(aHandle: THandle; constref aValue: T);
begin
  CheckInIteration;
  CheckEmpty;
  DoUpdate({%H-}PNode(aHandle), aValue);
end;

function TGCustomPairingHeap.Remove(aHandle: THandle): T;
begin
  CheckInIteration;
  CheckEmpty;
  Result := DoRemove({%H-}PNode(aHandle));
end;

function TGCustomPairingHeap.Merge(aQueue: IPriorityQueue): SizeInt;
var
  o: TObject;
begin
  o := aQueue._GetRef;
  if o <> Self then
    begin
      CheckInIteration;
      TCustomContainer(o).CheckInIteration;
      if o is TCustomPairingHeap then
        Result := DoMergeHeap(TCustomPairingHeap(o))
      else
        begin
          Result := EnqueueAll(aQueue);
          aQueue.Clear;
        end;
    end
  else
    Result := 0;
end;

{ TGBasePairingHeap }

procedure TGBasePairingHeap.RootMerge(aNode: PNode);
begin
  FRoot := DoMerge(FRoot, aNode);
  if FRoot <> nil then
    FRoot^.Prev := nil;
end;

procedure TGBasePairingHeap.DoExtract(aNode: PNode);
begin
  if aNode <> FRoot then
    begin
      CutNode(aNode);
      RootMerge(TwoPassMerge(aNode^.Child));
    end
  else
    begin
      FRoot := TwoPassMerge(FRoot^.Child);
      if FRoot <> nil then
        FRoot^.Prev := nil;
    end;
  aNode^.ClearLinks;
end;

function TGBasePairingHeap.DoEnqueue(constref aValue: T): PNode;
begin
  Result := NewNode(aValue);
  RootMerge(Result);
end;

procedure TGBasePairingHeap.DoUpdate(aNode: PNode; constref aValue: T);
var
  c: SizeInt;
begin
  c := TCmpRel.Compare(aValue, aNode^.Data);
  if c > 0 then
    begin
      aNode^.Data := aValue;
      if aNode <> FRoot then
        begin
          CutNode(aNode);
          RootMerge(aNode);
        end;
    end
  else
    if c < 0 then
      begin
        aNode^.Data := aValue;
        DoExtract(aNode);
        RootMerge(aNode);
      end;
end;

function TGBasePairingHeap.DoRemove(aNode: PNode): T;
begin
  DoExtract(aNode);
  Result := aNode^.Data;
  DisposeNode(aNode);
end;

function TGBasePairingHeap.DoDequeue: T;
var
  OldRoot: PNode;
begin
  OldRoot := FRoot;
  Result := FRoot^.Data;
  FRoot := TwoPassMerge(FRoot^.Child);
  if FRoot <> nil then
    FRoot^.Prev := nil;
  DisposeNode(OldRoot);
end;

function TGBasePairingHeap.DoMergeHeap(ph: TCustomPairingHeap): SizeInt;
begin
  Result := ph.Count;
  if Result > 0 then
    begin
      if ph is TGBasePairingHeap then
        begin
          FNodeManager.Join(ph.FNodeManager);
          RootMerge(ph.FRoot);
          FCount += Result;
          ph.FCount := 0;
          ph.FRoot := nil;
        end
      else
        begin
          EnqueueAll(ph);
          ph.Clear;
        end;
    end;
end;

class function TGBasePairingHeap.DoMerge(L, R: PNode): PNode;
begin
  if L <> nil then
    begin
      if R <> nil then
        begin
          if TCmpRel.Compare(L^.Data, R^.Data) >= 0 then
            begin
              L^.AddChild(R);
              Result := L;
            end
          else
            begin
              R^.AddChild(L);
              Result := R;
            end;
        end
      else
        Result := L;
    end
  else
    Result := R;
end;

{$IFDEF ENABLE_TWOPASSMERGE_RECURSION}
class function TGBasePairingHeap.TwoPassMerge(aNode: PNode): PNode; // recursive
var
  Sibling, NextSibling: PNode;
begin
  if (aNode <> nil) and (aNode^.Sibling <> nil) then
    begin
      Sibling := aNode^.Sibling;
      NextSibling := Sibling^.Sibling;
      aNode^.Sibling := nil;
      Sibling^.Sibling := nil;
      Result := DoMerge(DoMerge(aNode, Sibling), TwoPassMerge(NextSibling));
    end
  else
    Result := aNode;
end;
{$ELSE ENABLE_TWOPASSMERGE_RECURSION}
class function TGBasePairingHeap.TwoPassMerge(aNode: PNode): PNode;
var
  CurrNode, NextNode: PNode;
begin
  Result := nil;
  while (aNode <> nil) and (aNode^.Sibling <> nil) do
    begin
      CurrNode := aNode;
      NextNode := aNode^.Sibling;
      aNode := NextNode^.Sibling;
      CurrNode^.Sibling := nil;
      NextNode^.Sibling := nil;
      Result := DoMerge(Result, DoMerge(CurrNode, NextNode));
    end;
  Result := DoMerge(Result, aNode);
end;
{$ENDIF ENABLE_TWOPASSMERGE_RECURSION}

class function TGBasePairingHeap.DoCompare(constref L, R: T): SizeInt;
begin
  Result := TCmpRel.Compare(L, R);
end;

class function TGBasePairingHeap.Comparator: TComparator;
begin
  Result := @DoCompare;
end;

constructor TGBasePairingHeap.Create;
begin
  FNodeManager.EnsureFreeCount(DEFAULT_CONTAINER_CAPACITY);
end;

constructor TGBasePairingHeap.Create(aCapacity: SizeInt);
begin
  if aCapacity > 0 then
    FNodeManager.EnsureFreeCount(aCapacity);
end;

constructor TGBasePairingHeap.Create(constref A: array of T);
begin
  Create;
  EnqueueAll(A);
end;

constructor TGBasePairingHeap.Create(e: IEnumerable);
begin
  Create;
  EnqueueAll(e);
end;

{ TGComparablePairHeap }

class function TGComparablePairHeap.DoCompare(constref L, R: T): SizeInt;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
end;

class function TGComparablePairHeap.Comparator: TComparator;
begin
  Result := @DoCompare;
end;

{ TGComparablePairHeapMax }

procedure TGComparablePairHeapMax.RootMerge(aNode: PNode);
begin
  FRoot := DoMerge(FRoot, aNode);
  if FRoot <> nil then
    FRoot^.Prev := nil;
end;

procedure TGComparablePairHeapMax.DoExtract(aNode: PNode);
begin
  if aNode <> FRoot then
    begin
      CutNode(aNode);
      RootMerge(TwoPassMerge(aNode^.Child));
    end
  else
    begin
      FRoot := TwoPassMerge(FRoot^.Child);
      if FRoot <> nil then
        FRoot^.Prev := nil;
    end;
  aNode^.ClearLinks;
end;

function TGComparablePairHeapMax.DoEnqueue(constref aValue: T): PNode;
begin
  Result := NewNode(aValue);
  RootMerge(Result);
end;

procedure TGComparablePairHeapMax.DoUpdate(aNode: PNode; constref aValue: T);
begin
  if aValue > aNode^.Data then
    begin
      aNode^.Data := aValue;
      if aNode <> FRoot then
        begin
          CutNode(aNode);
          RootMerge(aNode);
        end;
    end
  else
    if aValue < aNode^.Data then
      begin
        aNode^.Data := aValue;
        DoExtract(aNode);
        RootMerge(aNode);
      end;
end;

function TGComparablePairHeapMax.DoRemove(aNode: PNode): T;
begin
  DoExtract(aNode);
  Result := aNode^.Data;
  DisposeNode(aNode);
end;

function TGComparablePairHeapMax.DoDequeue: T;
var
  OldRoot: PNode;
begin
  OldRoot := FRoot;
  Result := FRoot^.Data;
  FRoot := TwoPassMerge(FRoot^.Child);
  if FRoot <> nil then
    FRoot^.Prev := nil;
  DisposeNode(OldRoot);
end;

function TGComparablePairHeapMax.DoMergeHeap(ph: TCustomPairingHeap): SizeInt;
begin
  Result := ph.Count;
  if Result > 0 then
    begin
      if ph is TGComparablePairHeapMax then
        begin
          FNodeManager.Join(ph.FNodeManager);
          RootMerge(ph.FRoot);
          FCount += Result;
          ph.FCount := 0;
          ph.FRoot := nil;
        end
      else
        begin
          EnqueueAll(ph);
          ph.Clear;
        end;
    end;
end;

class function TGComparablePairHeapMax.DoMerge(L, R: PNode): PNode;
begin
  if L <> nil then
    begin
      if R <> nil then
        begin
          if L^.Data >= R^.Data then
            begin
              L^.AddChild(R);
              Result := L;
            end
          else
            begin
              R^.AddChild(L);
              Result := R;
            end;
        end
      else
        Result := L;
    end
  else
    Result := R;
end;

{$IFDEF ENABLE_TWOPASSMERGE_RECURSION}
class function TGComparablePairHeapMax.TwoPassMerge(aNode: PNode): PNode; // recursive
var
  Sibling, NextSibling: PNode;
begin
  if (aNode <> nil) and (aNode^.Sibling <> nil) then
    begin
      Sibling := aNode^.Sibling;
      NextSibling := Sibling^.Sibling;
      aNode^.Sibling := nil;
      Sibling^.Sibling := nil;
      Result := DoMerge(DoMerge(aNode, Sibling), TwoPassMerge(NextSibling));
    end
  else
    Result := aNode;
end;
{$ELSE ENABLE_TWOPASSMERGE_RECURSION}
class function TGComparablePairHeapMax.TwoPassMerge(aNode: PNode): PNode;
var
  CurrNode, NextNode: PNode;
begin
  Result := nil;
  while (aNode <> nil) and (aNode^.Sibling <> nil) do
    begin
      CurrNode := aNode;
      NextNode := aNode^.Sibling;
      aNode := NextNode^.Sibling;
      CurrNode^.Sibling := nil;
      NextNode^.Sibling := nil;
      Result := DoMerge(Result, DoMerge(CurrNode, NextNode));
    end;
  Result := DoMerge(Result, aNode);
end;
{$ENDIF ENABLE_TWOPASSMERGE_RECURSION}

constructor TGComparablePairHeapMax.Create;
begin
  FNodeManager.EnsureFreeCount(DEFAULT_CONTAINER_CAPACITY);
end;

constructor TGComparablePairHeapMax.Create(aCapacity: SizeInt);
begin
  if aCapacity > 0 then
    FNodeManager.EnsureFreeCount(aCapacity);
end;

constructor TGComparablePairHeapMax.Create(constref A: array of T);
begin
  Create;
  EnqueueAll(A);
end;

constructor TGComparablePairHeapMax.Create(e: IEnumerable);
begin
  Create;
  EnqueueAll(e);
end;

{ TGComparablePairHeapMin }

procedure TGComparablePairHeapMin.RootMerge(aNode: PNode);
begin
  FRoot := DoMerge(FRoot, aNode);
  if FRoot <> nil then
    FRoot^.Prev := nil;
end;

procedure TGComparablePairHeapMin.DoExtract(aNode: PNode);
begin
  if aNode <> FRoot then
    begin
      CutNode(aNode);
      RootMerge(TwoPassMerge(aNode^.Child));
    end
  else
    begin
      FRoot := TwoPassMerge(FRoot^.Child);
      if FRoot <> nil then
        FRoot^.Prev := nil;
    end;
  aNode^.ClearLinks;
end;

function TGComparablePairHeapMin.DoEnqueue(constref aValue: T): PNode;
begin
  Result := NewNode(aValue);
  RootMerge(Result);
end;

procedure TGComparablePairHeapMin.DoUpdate(aNode: PNode; constref aValue: T);
begin
  if aValue < aNode^.Data then
    begin
      aNode^.Data := aValue;
      if aNode <> FRoot then
        begin
          CutNode(aNode);
          RootMerge(aNode);
        end;
    end
  else
    if aValue > aNode^.Data then
      begin
        aNode^.Data := aValue;
        DoExtract(aNode);
        RootMerge(aNode);
      end;
end;

function TGComparablePairHeapMin.DoRemove(aNode: PNode): T;
begin
  DoExtract(aNode);
  Result := aNode^.Data;
  DisposeNode(aNode);
end;

function TGComparablePairHeapMin.DoDequeue: T;
var
  OldRoot: PNode;
begin
  OldRoot := FRoot;
  Result := FRoot^.Data;
  FRoot := TwoPassMerge(FRoot^.Child);
  if FRoot <> nil then
    FRoot^.Prev := nil;
  DisposeNode(OldRoot);
end;

function TGComparablePairHeapMin.DoMergeHeap(ph: TCustomPairingHeap): SizeInt;
begin
  Result := ph.Count;
  if Result > 0 then
    begin
      if ph is TGComparablePairHeapMin then
        begin
          FNodeManager.Join(ph.FNodeManager);
          RootMerge(ph.FRoot);
          FCount += Result;
          ph.FCount := 0;
          ph.FRoot := nil;
        end
      else
        begin
          EnqueueAll(ph);
          ph.Clear;
        end;
    end;
end;

class function TGComparablePairHeapMin.DoMerge(L, R: PNode): PNode;
begin
  if L <> nil then
    begin
      if R <> nil then
        begin
          if L^.Data <= R^.Data then
            begin
              L^.AddChild(R);
              Result := L;
            end
          else
            begin
              R^.AddChild(L);
              Result := R;
            end;
        end
      else
        Result := L;
    end
  else
    Result := R;
end;

{$IFDEF ENABLE_TWOPASSMERGE_RECURSION}
class function TGComparablePairHeapMin.TwoPassMerge(aNode: PNode): PNode; // recursive
var
  Sibling, NextSibling: PNode;
begin
  if (aNode <> nil) and (aNode^.Sibling <> nil) then
    begin
      Sibling := aNode^.Sibling;
      NextSibling := Sibling^.Sibling;
      aNode^.Sibling := nil;
      Sibling^.Sibling := nil;
      Result := DoMerge(DoMerge(aNode, Sibling), TwoPassMerge(NextSibling));
    end
  else
    Result := aNode;
end;
{$ELSE ENABLE_TWOPASSMERGE_RECURSION}
class function TGComparablePairHeapMin.TwoPassMerge(aNode: PNode): PNode; // non recursive
var
  CurrNode, NextNode: PNode;
begin
  Result := nil;
  while (aNode <> nil) and (aNode^.Sibling <> nil) do
    begin
      CurrNode := aNode;
      NextNode := aNode^.Sibling;
      aNode := NextNode^.Sibling;
      CurrNode^.Sibling := nil;
      NextNode^.Sibling := nil;
      Result := DoMerge(Result, DoMerge(CurrNode, NextNode));
    end;
  Result := DoMerge(Result, aNode);
end;
{$ENDIF ENABLE_TWOPASSMERGE_RECURSION}

constructor TGComparablePairHeapMin.Create;
begin
  FNodeManager.EnsureFreeCount(DEFAULT_CONTAINER_CAPACITY);
end;

constructor TGComparablePairHeapMin.Create(aCapacity: SizeInt);
begin
  if aCapacity > 0 then
    FNodeManager.EnsureFreeCount(aCapacity);
end;

constructor TGComparablePairHeapMin.Create(constref A: array of T);
begin
  Create;
  EnqueueAll(A);
end;

constructor TGComparablePairHeapMin.Create(e: IEnumerable);
begin
  Create;
  EnqueueAll(e);
end;

{ TGRegularPairHeap }

function TGRegularPairHeap.DoMerge(L, R: PNode): PNode;
begin
  if L <> nil then
    begin
      if R <> nil then
        begin
          if FCompare(L^.Data, R^.Data) >= 0 then
            begin
              L^.AddChild(R);
              Result := L;
            end
          else
            begin
              R^.AddChild(L);
              Result := R;
            end;
        end
      else
        Result := L;
    end
  else
    Result := R;
end;

procedure TGRegularPairHeap.RootMerge(aNode: PNode);
begin
  FRoot := DoMerge(FRoot, aNode);
  if FRoot <> nil then
    FRoot^.Prev := nil;
end;

{$IFDEF ENABLE_TWOPASSMERGE_RECURSION}
function TGRegularPairHeap.TwoPassMerge(aNode: PNode): PNode; // recursive
var
  Sibling, NextSibling: PNode;
begin
  if (aNode <> nil) and (aNode^.Sibling <> nil) then
    begin
      Sibling := aNode^.Sibling;
      NextSibling := Sibling^.Sibling;
      aNode^.Sibling := nil;
      Sibling^.Sibling := nil;
      Result := DoMerge(DoMerge(aNode, Sibling), TwoPassMerge(NextSibling));
    end
  else
    Result := aNode;
end;
{$ELSE ENABLE_TWOPASSMERGE_RECURSION}
function TGRegularPairHeap.TwoPassMerge(aNode: PNode): PNode; // non recursive
var
  CurrNode, NextNode: PNode;
begin
  Result := nil;
  while (aNode <> nil) and (aNode^.Sibling <> nil) do
    begin
      CurrNode := aNode;
      NextNode := aNode^.Sibling;
      aNode := NextNode^.Sibling;
      CurrNode^.Sibling := nil;
      NextNode^.Sibling := nil;
      Result := DoMerge(Result, DoMerge(CurrNode, NextNode));
    end;
  if aNode <> nil then
    Result := DoMerge(Result, aNode);
end;
{$ENDIF ENABLE_TWOPASSMERGE_RECURSION}

procedure TGRegularPairHeap.DoExtract(aNode: PNode);
begin
  if aNode <> FRoot then
    begin
      CutNode(aNode);
      RootMerge(TwoPassMerge(aNode^.Child));
    end
  else
    begin
      FRoot := TwoPassMerge(FRoot^.Child);
      if FRoot <> nil then
        FRoot^.Prev := nil;
    end;
  aNode^.ClearLinks;
end;

function TGRegularPairHeap.DoEnqueue(constref aValue: T): PNode;
begin
  Result := NewNode(aValue);
  RootMerge(Result);
end;

procedure TGRegularPairHeap.DoUpdate(aNode: PNode; constref aValue: T);
var
  c: SizeInt;
begin
  c := FCompare(aValue, aNode^.Data);
  if c > 0 then
    begin
      aNode^.Data := aValue;
      if aNode <> FRoot then
        begin
          CutNode(aNode);
          RootMerge(aNode);
        end;
    end
  else
    if c < 0 then
      begin
        aNode^.Data := aValue;
        DoExtract(aNode);
        RootMerge(aNode);
      end;
end;

function TGRegularPairHeap.DoRemove(aNode: PNode): T;
begin
  DoExtract(aNode);
  Result := aNode^.Data;
  DisposeNode(aNode);
end;

function TGRegularPairHeap.DoDequeue: T;
var
  OldRoot: PNode;
begin
  OldRoot := FRoot;
  Result := FRoot^.Data;
  FRoot := TwoPassMerge(FRoot^.Child);
  if FRoot <> nil then
    FRoot^.Prev := nil;
  DisposeNode(OldRoot);
end;

function TGRegularPairHeap.DoMergeHeap(ph: TCustomPairingHeap): SizeInt;
begin
  Result := ph.Count;
  if Result > 0 then
    begin
      if ph is TGRegularPairHeap then
        begin
          FNodeManager.Join(ph.FNodeManager);
          RootMerge(ph.FRoot);
          FCount += Result;
          ph.FCount := 0;
          ph.FRoot := nil;
        end
      else
        begin
          EnqueueAll(ph);
          ph.Clear;
        end;
    end;
end;

constructor TGRegularPairHeap.Create;
begin
  Create(TDefaults.Compare);
end;

constructor TGRegularPairHeap.Create(c: TComparator);
begin
  FCompare := c;
  FNodeManager.EnsureFreeCount(DEFAULT_CONTAINER_CAPACITY);
end;

constructor TGRegularPairHeap.Create(aCapacity: SizeInt; c: TComparator);
begin
  FCompare := c;
  if aCapacity > 0 then
    FNodeManager.EnsureFreeCount(aCapacity);
end;

constructor TGRegularPairHeap.Create(constref A: array of T; c: TComparator);
begin
  Create(System.Length(A), c);
  EnqueueAll(A);
end;

constructor TGRegularPairHeap.Create(e: IEnumerable; c: TComparator);
begin
  Create(c);
  EnqueueAll(e);
end;

function TGRegularPairHeap.Comparator: TComparator;
begin
  Result := FCompare;
end;

{ TGDelegatedPairHeap }

function TGDelegatedPairHeap.DoMerge(L, R: PNode): PNode;
begin
  if L <> nil then
    begin
      if R <> nil then
        begin
          if FCompare(L^.Data, R^.Data) >= 0 then
            begin
              L^.AddChild(R);
              Result := L;
            end
          else
            begin
              R^.AddChild(L);
              Result := R;
            end;
        end
      else
        Result := L;
    end
  else
    Result := R;
end;

procedure TGDelegatedPairHeap.RootMerge(aNode: PNode);
begin
  FRoot := DoMerge(FRoot, aNode);
  if FRoot <> nil then
    FRoot^.Prev := nil;
end;

{$IFDEF ENABLE_TWOPASSMERGE_RECURSION}
function TGDelegatedPairHeap.TwoPassMerge(aNode: PNode): PNode; // recursive
var
  Sibling, NextSibling: PNode;
begin
  if (aNode <> nil) and (aNode^.Sibling <> nil) then
    begin
      Sibling := aNode^.Sibling;
      NextSibling := Sibling^.Sibling;
      aNode^.Sibling := nil;
      Sibling^.Sibling := nil;
      Result := DoMerge(DoMerge(aNode, Sibling), TwoPassMerge(NextSibling));
    end
  else
    Result := aNode;
end;
{$ELSE ENABLE_TWOPASSMERGE_RECURSION}
function TGDelegatedPairHeap.TwoPassMerge(aNode: PNode): PNode; // non recursive
var
  CurrNode, NextNode: PNode;
begin
  Result := nil;
  while (aNode <> nil) and (aNode^.Sibling <> nil) do
    begin
      CurrNode := aNode;
      NextNode := aNode^.Sibling;
      aNode := NextNode^.Sibling;
      CurrNode^.Sibling := nil;
      NextNode^.Sibling := nil;
      Result := DoMerge(Result, DoMerge(CurrNode, NextNode));
    end;
  if aNode <> nil then
    Result := DoMerge(Result, aNode);
end;
{$ENDIF ENABLE_TWOPASSMERGE_RECURSION}

procedure TGDelegatedPairHeap.DoExtract(aNode: PNode);
begin
  if aNode <> FRoot then
    begin
      CutNode(aNode);
      RootMerge(TwoPassMerge(aNode^.Child));
    end
  else
    begin
      FRoot := TwoPassMerge(FRoot^.Child);
      if FRoot <> nil then
        FRoot^.Prev := nil;
    end;
  aNode^.ClearLinks;
end;

function TGDelegatedPairHeap.DoEnqueue(constref aValue: T): PNode;
begin
  Result := NewNode(aValue);
  RootMerge(Result);
end;

procedure TGDelegatedPairHeap.DoUpdate(aNode: PNode; constref aValue: T);
var
  c: SizeInt;
begin
  c := FCompare(aValue, aNode^.Data);
  if c > 0 then
    begin
      aNode^.Data := aValue;
      if aNode <> FRoot then
        begin
          CutNode(aNode);
          RootMerge(aNode);
        end;
    end
  else
    if c < 0 then
      begin
        aNode^.Data := aValue;
        DoExtract(aNode);
        RootMerge(aNode);
      end;
end;

function TGDelegatedPairHeap.DoRemove(aNode: PNode): T;
begin
  DoExtract(aNode);
  Result := aNode^.Data;
  DisposeNode(aNode);
end;

function TGDelegatedPairHeap.DoDequeue: T;
var
  OldRoot: PNode;
begin
  OldRoot := FRoot;
  Result := FRoot^.Data;
  FRoot := TwoPassMerge(FRoot^.Child);
  if FRoot <> nil then
    FRoot^.Prev := nil;
  DisposeNode(OldRoot);
end;

function TGDelegatedPairHeap.DoMergeHeap(ph: TCustomPairingHeap): SizeInt;
begin
  Result := ph.Count;
  if Result > 0 then
    begin
      if ph is TGDelegatedPairHeap then
        begin
          FNodeManager.Join(ph.FNodeManager);
          RootMerge(ph.FRoot);
          FCount += Result;
          ph.FCount := 0;
          ph.FRoot := nil;
        end
      else
        begin
          EnqueueAll(ph);
          ph.Clear;
        end;
    end;
end;

constructor TGDelegatedPairHeap.Create;
begin
  Create(TDefaults.OnCompare);
end;

constructor TGDelegatedPairHeap.Create(c: TComparator);
begin
  FCompare := c;
  FNodeManager.EnsureFreeCount(DEFAULT_CONTAINER_CAPACITY);
end;

constructor TGDelegatedPairHeap.Create(aCapacity: SizeInt; c: TComparator);
begin
  FCompare := c;
  if aCapacity > 0 then
    FNodeManager.EnsureFreeCount(aCapacity);
end;

constructor TGDelegatedPairHeap.Create(constref A: array of T; c: TComparator);
begin
  Create(c);
  EnqueueAll(A);
end;

constructor TGDelegatedPairHeap.Create(e: IEnumerable; c: TComparator);
begin
  Create(c);
  EnqueueAll(e);
end;

function TGDelegatedPairHeap.Comparator: TComparator;
begin
  Result := FCompare;
end;

{ TGLitePairingHeap.TNode }

procedure TGLitePairingHeap.TNode.AddChild(aNode: PNode);
begin
  if aNode <> nil then
    begin
      aNode^.Prev := @Self;
      Sibling :=  aNode^.Sibling;
      if Sibling <> nil then
        Sibling^.Prev := @Self;
      aNode^.Sibling := Child;
      if aNode^.Sibling <> nil then
        aNode^.Sibling^.Prev := aNode;
      Child := aNode;
    end;
end;

procedure TGLitePairingHeap.TNode.ClearLinks;
begin
  Sibling := nil;
  Child := nil;
  Prev := nil;
end;

function TGLitePairingHeap.TNode.Successor: PNode;
begin
  Result := Sibling;
  if Result <> nil then
    while Result^.Child <> nil do
      Result := Result^.Child
  else
    begin
      Result := @Self;
      while (Result^.Prev <> nil) and (Result^.Prev^.Sibling = Result) do
        Result := Result^.Prev;
      Result := Result^.Prev;
    end;
end;

function TGLitePairingHeap.TNode.Predecessor: PNode;
begin
  Result := Child;
  if Result <> nil then
    while Result^.Sibling <> nil do
      Result := Result^.Sibling
  else
    begin
      Result := @Self;
      while (Result^.Prev <> nil) and (Result^.Prev^.Child = Result) do
        Result := Result^.Prev;
      Result := Result^.Prev;
    end;
end;

{ TGLitePairingHeap.TEnumerator }

function TGLitePairingHeap.TEnumerator.GetCurrent: T;
begin
  Result := FCurrNode^.Data;
end;

function TGLitePairingHeap.TEnumerator.MoveNext: Boolean;
var
  NextNode: PNode = nil;
begin
  if FCurrNode <> nil then
    NextNode := FCurrNode^.Successor
  else
    if not FInCycle then
      begin
        NextNode := FLeftmost;
        FInCycle := True;
      end;
  Result := NextNode <> nil;
  if Result then
    FCurrNode := NextNode;
end;

procedure TGLitePairingHeap.TEnumerator.Reset;
begin
  FCurrNode := nil;
  FInCycle := False;
end;

{ TGLitePairingHeap.TReverseEnumerator }

function TGLitePairingHeap.TReverseEnumerator.GetCurrent: T;
begin
  Result := FCurrNode^.Data;
end;

function TGLitePairingHeap.TReverseEnumerator.MoveNext: Boolean;
var
  NextNode: PNode = nil;
begin
  if FCurrNode <> nil then
    NextNode := FCurrNode^.Predecessor
  else
    if not FInCycle then
      begin
        NextNode := FRightmost;
        FInCycle := True;
      end;
  Result := NextNode <> nil;
  if Result then
    FCurrNode := NextNode;
end;

procedure TGLitePairingHeap.TReverseEnumerator.Reset;
begin
  FCurrNode := nil;
  FInCycle := False;
end;

{ TGLitePairingHeap.TReverse }

function TGLitePairingHeap.TReverse.GetEnumerator: TReverseEnumerator;
begin
  Result := FHeap^.GetReverseEnumerator;
end;

{ TGLitePairingHeap }

function TGLitePairingHeap.GetCapacity: SizeInt;
begin
  Result := Count + FNodeManager.FreeCount;
end;

function TGLitePairingHeap.GetReverseEnumerator: TReverseEnumerator;
begin
  Result.FCurrNode := nil;
  Result.FRightmost := FindRightmost;
  Result.FInCycle := False;
end;

function TGLitePairingHeap.FindLeftmost: PNode;
begin
  Result := FRoot;
  if Result <> nil then
    while Result^.Child <> nil do
      Result := Result^.Child;
end;

function TGLitePairingHeap.FindRightmost: PNode;
begin
  Result := FRoot;
  if Result <> nil then
    while Result^.Sibling <> nil do
      Result := Result^.Sibling;
end;

procedure TGLitePairingHeap.RemoveNodeWithChilds(aNode: PNode);
var
  OldPrev, OldNode, CurrNode: PNode;
  FromSibling: Boolean = False;
begin
  if aNode <> nil then
    begin
      CurrNode := aNode;
      repeat  //postorder traverse
        while FromSibling do
          begin
            OldPrev := CurrNode^.Prev;
            OldNode := CurrNode;
            /////////////////////////////////
            CurrNode^.Data := Default(T);
            FNodeManager.DisposeNode(CurrNode);
            Dec(FCount);
            ////////////////////////////////
            if CurrNode = aNode then
              exit;
            CurrNode := OldPrev;
            FromSibling := CurrNode^.Sibling = OldNode;
            if not FromSibling and (CurrNode^.Sibling <> nil) then
              CurrNode := CurrNode^.Sibling
            else
              FromSibling := True;
          end;
        while CurrNode^.Child <> nil do
          CurrNode := CurrNode^.Child;
        if CurrNode^.Sibling <> nil then
          CurrNode := CurrNode^.Sibling
        else
          FromSibling := True;
      until False;
    end;
end;

procedure TGLitePairingHeap.ClearTree;
begin
  RemoveNodeWithChilds(FRoot);
  FRoot := nil;
end;

function TGLitePairingHeap.NewNode(constref aValue: T): PNode;
begin
  Result := FNodeManager.NewNode;
  Result^.Data := aValue;
  Inc(FCount);
end;

procedure TGLitePairingHeap.DisposeNode(aNode: PNode);
begin
  if aNode <> nil then
    begin
      aNode^ := Default(TNode);
      FNodeManager.FreeNode(aNode);
      Dec(FCount);
    end;
end;

procedure TGLitePairingHeap.RootMerge(aNode: PNode);
begin
  FRoot := NodeMerge(FRoot, aNode);
  if FRoot <> nil then
    FRoot^.Prev := nil;
end;

function TGLitePairingHeap.DequeueItem: T;
var
  OldRoot: PNode;
begin
  OldRoot := FRoot;
  Result := FRoot^.Data;
  FRoot := TwoPassMerge(FRoot^.Child);
  if FRoot <> nil then
    FRoot^.Prev := nil;
  DisposeNode(OldRoot);
end;

procedure TGLitePairingHeap.UpdateNode(aNode: PNode; constref aValue: T);
var
  c: SizeInt;
begin
  c := TCmpRel.Compare(aValue, aNode^.Data);
  if c <> 0 then
    begin
      aNode^.Data := aValue;
      if c > 0 then
        begin
          if aNode <> FRoot then
            begin
              CutNode(aNode);
              RootMerge(aNode);
            end;
        end
      else
        begin
          ExtractNode(aNode);
          RootMerge(aNode);
        end;
    end;
end;

procedure TGLitePairingHeap.ExtractNode(aNode: PNode);
begin
  if aNode <> FRoot then
    begin
      CutNode(aNode);
      RootMerge(TwoPassMerge(aNode^.Child));
    end
  else
    begin
      FRoot := TwoPassMerge(FRoot^.Child);
      if FRoot <> nil then
        FRoot^.Prev := nil;
    end;
  aNode^.ClearLinks;
end;

function TGLitePairingHeap.RemoveNode(aNode: PNode): T;
begin
  ExtractNode(aNode);
  Result := aNode^.Data;
  DisposeNode(aNode);
end;

procedure TGLitePairingHeap.CheckEmpty;
begin
  if Count = 0 then
    AccessEmptyError;
end;

procedure TGLitePairingHeap.AccessEmptyError;
begin
  raise ELGAccessEmpty.Create(SECantAccessEmpty);
end;

class operator TGLitePairingHeap.Initialize(var h: TGLitePairingHeap);
begin
  h := Default(TGLitePairingHeap);
end;

class operator TGLitePairingHeap.Finalize(var h: TGLitePairingHeap);
begin
  h.Clear;
end;

class operator TGLitePairingHeap.Copy(constref aSrc: TGLitePairingHeap; var aDst: TGLitePairingHeap);
var
  v: T;
begin
  System.FillChar(aDst, SizeOf(aDst), 0);
  aDst.EnsureCapacity(aSrc.Count);
  for v in aSrc do
    aDst.Enqueue(v);
end;

class function TGLitePairingHeap.NodeMerge(L, R: PNode): PNode;
begin
  if L <> nil then
    if R <> nil then
      if TCmpRel.Compare(L^.Data, R^.Data) >= 0 then
        begin
          L^.AddChild(R);
          Result := L;
        end
      else
        begin
          R^.AddChild(L);
          Result := R;
        end
    else
      Result := L
  else
    Result := R;
end;

{$IFDEF ENABLE_TWOPASSMERGE_RECURSION}
class function TGLitePairingHeap.TwoPassMerge(aNode: PNode): PNode; // recursive
var
  Sibling, NextSibling: PNode;
begin
  if (aNode <> nil) and (aNode^.Sibling <> nil) then
    begin
      Sibling := aNode^.Sibling;
      NextSibling := Sibling^.Sibling;
      aNode^.Sibling := nil;
      Sibling^.Sibling := nil;
      Result := NodeMerge(NodeMerge(aNode, Sibling), TwoPassMerge(NextSibling));
    end
  else
    Result := aNode;
end;
{$ELSE ENABLE_TWOPASSMERGE_RECURSION}
class function TGLitePairingHeap.TwoPassMerge(aNode: PNode): PNode;
var
  CurrNode, NextNode: PNode;
begin
  Result := nil;
  while (aNode <> nil) and (aNode^.Sibling <> nil) do
    begin
      CurrNode := aNode;
      NextNode := aNode^.Sibling;
      aNode := NextNode^.Sibling;
      CurrNode^.Sibling := nil;
      NextNode^.Sibling := nil;
      Result := NodeMerge(Result, NodeMerge(CurrNode, NextNode));
    end;
  Result := NodeMerge(Result, aNode);
end;
{$ENDIF ENABLE_TWOPASSMERGE_RECURSION}

class procedure TGLitePairingHeap.CutNode(aNode: PNode);
begin
  if aNode^.Sibling <> nil then
    aNode^.Sibling^.Prev := aNode^.Prev;
  if aNode^.Prev^.Child = aNode then
    aNode^.Prev^.Child := aNode^.Sibling
  else
    aNode^.Prev^.Sibling := aNode^.Sibling;
  aNode^.Sibling := nil;
end;

class function TGLitePairingHeap.DoCompare(constref L, R: T): SizeInt;
begin
  Result := TCmpRel.Compare(L, R);
end;

function TGLitePairingHeap.Comparator: TComparator;
begin
  Result := @DoCompare;
end;

function TGLitePairingHeap.GetEnumerator: TEnumerator;
begin
  Result.FCurrNode := nil;
  Result.FLeftmost := FindLeftmost;
  Result.FInCycle := False;
end;

function TGLitePairingHeap.Reverse: TReverse;
begin
  Result.FHeap := @Self;
end;

function TGLitePairingHeap.ToArray: TArray;
var
  I: SizeInt = 0;
  v: T;
begin
  System.SetLength(Result, Count);
  for v in Self do
    begin
      Result[I] := v;
      Inc(I);
    end;
end;

procedure TGLitePairingHeap.Clear;
begin
  ClearTree;
  FNodeManager.Clear;
end;

function TGLitePairingHeap.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TGLitePairingHeap.NonEmpty: Boolean;
begin
  Result := Count <> 0;
end;

procedure TGLitePairingHeap.EnsureCapacity(aValue: SizeInt);
begin
  FNodeManager.EnsureFreeCount(aValue - Capacity);
end;

procedure TGLitePairingHeap.TrimToFit;
begin
  if Count > 0 then
    FNodeManager.ClearFreeList
  else
    FNodeManager.Clear;
end;

procedure TGLitePairingHeap.Enqueue(constref aValue: T);
begin
  RootMerge(NewNode(aValue));
end;

function TGLitePairingHeap.Dequeue: T;
begin
  CheckEmpty;
  Result := DequeueItem;
end;

function TGLitePairingHeap.TryDequeue(out aValue: T): Boolean;
begin
  Result := NonEmpty;
  if Result then
    aValue := DequeueItem;
end;

function TGLitePairingHeap.Peek: T;
begin
  CheckEmpty;
  Result := FRoot^.Data;
end;

function TGLitePairingHeap.TryPeek(out aValue: T): Boolean;
begin
  Result := NonEmpty;
  if Result then
    aValue := FRoot^.Data;
end;

function TGLitePairingHeap.Insert(constref aValue: T): THandle;
var
  p: PNode;
begin
  p := NewNode(aValue);
  RootMerge(p);
  Result := {%H-}THandle(p);
end;

function TGLitePairingHeap.PeekHead: THandle;
begin
  CheckEmpty;
  Result := {%H-}THandle(FRoot);
end;

function TGLitePairingHeap.TryPeekHead(out aValue: THandle): Boolean;
begin
  Result := NonEmpty;
  if Result then
    aValue := {%H-}THandle(FRoot);
end;

function TGLitePairingHeap.Value(aHandle: THandle): T;
begin
  Result := {%H-}PNode(aHandle)^.Data;
end;

procedure TGLitePairingHeap.Update(aHandle: THandle; constref aValue: T);
begin
  CheckEmpty;
  UpdateNode({%H-}PNode(aHandle), aValue);
end;

function TGLitePairingHeap.Remove(aHandle: THandle): T;
begin
  CheckEmpty;
  Result := RemoveNode({%H-}PNode(aHandle));
end;

function TGLitePairingHeap.Merge(aHeap: TGLitePairingHeap): SizeInt;
begin
  Result := aHeap.Count;
  if Result > 0 then
    begin
      FNodeManager.Join(aHeap.FNodeManager);
      RootMerge(aHeap.FRoot);
      FCount += Result;
      aHeap.FCount := 0;
      aHeap.FRoot := nil;
    end;
end;

{ TGLiteComparablePairHeapMin.TNode }

procedure TGLiteComparablePairHeapMin.TNode.AddChild(aNode: PNode);
begin
  if aNode <> nil then
    begin
      aNode^.Prev := @Self;
      Sibling :=  aNode^.Sibling;
      if Sibling <> nil then
        Sibling^.Prev := @Self;
      aNode^.Sibling := Child;
      if aNode^.Sibling <> nil then
        aNode^.Sibling^.Prev := aNode;
      Child := aNode;
    end;
end;

procedure TGLiteComparablePairHeapMin.TNode.ClearLinks;
begin
  Sibling := nil;
  Child := nil;
  Prev := nil;
end;

function TGLiteComparablePairHeapMin.TNode.Successor: PNode;
begin
  Result := Sibling;
  if Result <> nil then
    while Result^.Child <> nil do
      Result := Result^.Child
  else
    begin
      Result := @Self;
      while (Result^.Prev <> nil) and (Result^.Prev^.Sibling = Result) do
        Result := Result^.Prev;
      Result := Result^.Prev;
    end;
end;

function TGLiteComparablePairHeapMin.TNode.Predecessor: PNode;
begin
  Result := Child;
  if Result <> nil then
    while Result^.Sibling <> nil do
      Result := Result^.Sibling
  else
    begin
      Result := @Self;
      while (Result^.Prev <> nil) and (Result^.Prev^.Child = Result) do
        Result := Result^.Prev;
      Result := Result^.Prev;
    end;
end;

{ TGLiteComparablePairHeapMin.TEnumerator }

function TGLiteComparablePairHeapMin.TEnumerator.GetCurrent: T;
begin
  Result := FCurrNode^.Data;
end;

function TGLiteComparablePairHeapMin.TEnumerator.MoveNext: Boolean;
var
  NextNode: PNode = nil;
begin
  if FCurrNode <> nil then
    NextNode := FCurrNode^.Successor
  else
    if not FInCycle then
      begin
        NextNode := FLeftmost;
        FInCycle := True;
      end;
  Result := NextNode <> nil;
  if Result then
    FCurrNode := NextNode;
end;

procedure TGLiteComparablePairHeapMin.TEnumerator.Reset;
begin
  FCurrNode := nil;
  FInCycle := False;
end;

{ TGLiteComparablePairHeapMin.TReverseEnumerator }

function TGLiteComparablePairHeapMin.TReverseEnumerator.GetCurrent: T;
begin
  Result := FCurrNode^.Data;
end;

function TGLiteComparablePairHeapMin.TReverseEnumerator.MoveNext: Boolean;
var
  NextNode: PNode = nil;
begin
  if FCurrNode <> nil then
    NextNode := FCurrNode^.Predecessor
  else
    if not FInCycle then
      begin
        NextNode := FRightmost;
        FInCycle := True;
      end;
  Result := NextNode <> nil;
  if Result then
    FCurrNode := NextNode;
end;

procedure TGLiteComparablePairHeapMin.TReverseEnumerator.Reset;
begin
  FCurrNode := nil;
  FInCycle := False;
end;

{ TGLiteComparablePairHeapMin.TReverse }

function TGLiteComparablePairHeapMin.TReverse.GetEnumerator: TReverseEnumerator;
begin
  Result := FHeap^.GetReverseEnumerator;
end;

{ TGLiteComparablePairHeapMin }

function TGLiteComparablePairHeapMin.GetCapacity: SizeInt;
begin
  Result := Count + FNodeManager.FreeCount;
end;

function TGLiteComparablePairHeapMin.GetReverseEnumerator: TReverseEnumerator;
begin
  Result.FCurrNode := nil;
  Result.FRightmost := FindRightmost;
  Result.FInCycle := False;
end;

function TGLiteComparablePairHeapMin.FindLeftmost: PNode;
begin
  Result := FRoot;
  if Result <> nil then
    while Result^.Child <> nil do
      Result := Result^.Child;
end;

function TGLiteComparablePairHeapMin.FindRightmost: PNode;
begin
  Result := FRoot;
  if Result <> nil then
    while Result^.Sibling <> nil do
      Result := Result^.Sibling;
end;

procedure TGLiteComparablePairHeapMin.RemoveNodeWithChilds(aNode: PNode);
var
  OldPrev, OldNode, CurrNode: PNode;
  FromSibling: Boolean = False;
begin
  if aNode <> nil then
    begin
      CurrNode := aNode;
      repeat  //postorder traverse
        while FromSibling do
          begin
            OldPrev := CurrNode^.Prev;
            OldNode := CurrNode;
            /////////////////////////////////
            CurrNode^.Data := Default(T);
            FNodeManager.DisposeNode(CurrNode);
            Dec(FCount);
            ////////////////////////////////
            if CurrNode = aNode then
              exit;
            CurrNode := OldPrev;
            FromSibling := CurrNode^.Sibling = OldNode;
            if not FromSibling and (CurrNode^.Sibling <> nil) then
              CurrNode := CurrNode^.Sibling
            else
              FromSibling := True;
          end;
        while CurrNode^.Child <> nil do
          CurrNode := CurrNode^.Child;
        if CurrNode^.Sibling <> nil then
          CurrNode := CurrNode^.Sibling
        else
          FromSibling := True;
      until False;
    end;
end;

procedure TGLiteComparablePairHeapMin.ClearTree;
begin
  RemoveNodeWithChilds(FRoot);
  FRoot := nil;
end;

function TGLiteComparablePairHeapMin.NewNode(constref aValue: T): PNode;
begin
  Result := FNodeManager.NewNode;
  Result^.Data := aValue;
  Inc(FCount);
end;

procedure TGLiteComparablePairHeapMin.DisposeNode(aNode: PNode);
begin
  if aNode <> nil then
    begin
      aNode^ := Default(TNode);
      FNodeManager.FreeNode(aNode);
      Dec(FCount);
    end;
end;

procedure TGLiteComparablePairHeapMin.RootMerge(aNode: PNode);
begin
  FRoot := NodeMerge(FRoot, aNode);
  if FRoot <> nil then
    FRoot^.Prev := nil;
end;

function TGLiteComparablePairHeapMin.DequeueItem: T;
var
  OldRoot: PNode;
begin
  OldRoot := FRoot;
  Result := FRoot^.Data;
  FRoot := TwoPassMerge(FRoot^.Child);
  if FRoot <> nil then
    FRoot^.Prev := nil;
  DisposeNode(OldRoot);
end;

procedure TGLiteComparablePairHeapMin.UpdateNode(aNode: PNode; constref aValue: T);
begin
  if aValue < aNode^.Data then
    begin
      aNode^.Data := aValue;
      if aNode <> FRoot then
        begin
          CutNode(aNode);
          RootMerge(aNode);
        end;
    end
  else
    if aValue > aNode^.Data then
      begin
        aNode^.Data := aValue;
        ExtractNode(aNode);
        RootMerge(aNode);
      end;
end;

procedure TGLiteComparablePairHeapMin.ExtractNode(aNode: PNode);
begin
  if aNode <> FRoot then
    begin
      CutNode(aNode);
      RootMerge(TwoPassMerge(aNode^.Child));
    end
  else
    begin
      FRoot := TwoPassMerge(FRoot^.Child);
      if FRoot <> nil then
        FRoot^.Prev := nil;
    end;
  aNode^.ClearLinks;
end;

function TGLiteComparablePairHeapMin.RemoveNode(aNode: PNode): T;
begin
  ExtractNode(aNode);
  Result := aNode^.Data;
  DisposeNode(aNode);
end;

procedure TGLiteComparablePairHeapMin.CheckEmpty;
begin
  if Count = 0 then
    AccessEmptyError;
end;

procedure TGLiteComparablePairHeapMin.AccessEmptyError;
begin
  raise ELGAccessEmpty.Create(SECantAccessEmpty);
end;

class operator TGLiteComparablePairHeapMin.Initialize(var h: TGLiteComparablePairHeapMin);
begin
  h := Default(TGLiteComparablePairHeapMin);
end;

class operator TGLiteComparablePairHeapMin.Finalize(var h: TGLiteComparablePairHeapMin);
begin
  h.Clear;
end;

class operator TGLiteComparablePairHeapMin.Copy(constref aSrc: TGLiteComparablePairHeapMin;
  var aDst: TGLiteComparablePairHeapMin);
var
  v: T;
begin
  System.FillChar(aDst, SizeOf(aDst), 0);
  aDst.EnsureCapacity(aSrc.Count);
  for v in aSrc do
    aDst.Enqueue(v);
end;

class function TGLiteComparablePairHeapMin.NodeMerge(L, R: PNode): PNode;
begin
  if L <> nil then
    if R <> nil then
      if L^.Data <= R^.Data then
        begin
          L^.AddChild(R);
          Result := L;
        end
      else
        begin
          R^.AddChild(L);
          Result := R;
        end
    else
      Result := L
  else
    Result := R;
end;

{$IFDEF ENABLE_TWOPASSMERGE_RECURSION}
class function TGLiteComparablePairHeapMin.TwoPassMerge(aNode: PNode): PNode; // recursive
var
  Sibling, NextSibling: PNode;
begin
  if (aNode <> nil) and (aNode^.Sibling <> nil) then
    begin
      Sibling := aNode^.Sibling;
      NextSibling := Sibling^.Sibling;
      aNode^.Sibling := nil;
      Sibling^.Sibling := nil;
      Result := NodeMerge(NodeMerge(aNode, Sibling), TwoPassMerge(NextSibling));
    end
  else
    Result := aNode;
end;
{$ELSE ENABLE_TWOPASSMERGE_RECURSION}
class function TGLiteComparablePairHeapMin.TwoPassMerge(aNode: PNode): PNode;
var
  CurrNode, NextNode: PNode;
begin
  Result := nil;
  while (aNode <> nil) and (aNode^.Sibling <> nil) do
    begin
      CurrNode := aNode;
      NextNode := aNode^.Sibling;
      aNode := NextNode^.Sibling;
      CurrNode^.Sibling := nil;
      NextNode^.Sibling := nil;
      Result := NodeMerge(Result, NodeMerge(CurrNode, NextNode));
    end;
  Result := NodeMerge(Result, aNode);
end;
{$ENDIF ENABLE_TWOPASSMERGE_RECURSION}

class procedure TGLiteComparablePairHeapMin.CutNode(aNode: PNode);
begin
  if aNode^.Sibling <> nil then
    aNode^.Sibling^.Prev := aNode^.Prev;
  if aNode^.Prev^.Child = aNode then
    aNode^.Prev^.Child := aNode^.Sibling
  else
    aNode^.Prev^.Sibling := aNode^.Sibling;
  aNode^.Sibling := nil;
end;

class function TGLiteComparablePairHeapMin.DoCompare(constref L, R: T): SizeInt;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
end;

function TGLiteComparablePairHeapMin.Comparator: TComparator;
begin
  Result := @DoCompare;
end;

function TGLiteComparablePairHeapMin.GetEnumerator: TEnumerator;
begin
  Result.FCurrNode := nil;
  Result.FLeftmost := FindLeftmost;
  Result.FInCycle := False;
end;

function TGLiteComparablePairHeapMin.Reverse: TReverse;
begin
  Result.FHeap := @Self;
end;

function TGLiteComparablePairHeapMin.ToArray: TArray;
var
  I: SizeInt = 0;
  v: T;
begin
  System.SetLength(Result, Count);
  for v in Self do
    begin
      Result[I] := v;
      Inc(I);
    end;
end;

procedure TGLiteComparablePairHeapMin.Clear;
begin
  ClearTree;
  FNodeManager.Clear;
end;

function TGLiteComparablePairHeapMin.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TGLiteComparablePairHeapMin.NonEmpty: Boolean;
begin
  Result := Count <> 0;
end;

procedure TGLiteComparablePairHeapMin.EnsureCapacity(aValue: SizeInt);
begin
  FNodeManager.EnsureFreeCount(aValue - Capacity);
end;

procedure TGLiteComparablePairHeapMin.TrimToFit;
begin
  if Count > 0 then
    FNodeManager.ClearFreeList
  else
    FNodeManager.Clear;
end;

procedure TGLiteComparablePairHeapMin.Enqueue(constref aValue: T);
begin
  RootMerge(NewNode(aValue));
end;

function TGLiteComparablePairHeapMin.Dequeue: T;
begin
  CheckEmpty;
  Result := DequeueItem;
end;

function TGLiteComparablePairHeapMin.TryDequeue(out aValue: T): Boolean;
begin
  Result := NonEmpty;
  if Result then
    aValue := DequeueItem;
end;

function TGLiteComparablePairHeapMin.Peek: T;
begin
  CheckEmpty;
  Result := FRoot^.Data;
end;

function TGLiteComparablePairHeapMin.TryPeek(out aValue: T): Boolean;
begin
  Result := NonEmpty;
  if Result then
    aValue := FRoot^.Data;
end;

function TGLiteComparablePairHeapMin.Insert(constref aValue: T): THandle;
var
  p: PNode;
begin
  p := NewNode(aValue);
  RootMerge(p);
  Result := {%H-}THandle(p);
end;

function TGLiteComparablePairHeapMin.PeekHead: THandle;
begin
  CheckEmpty;
  Result := {%H-}THandle(FRoot);
end;

function TGLiteComparablePairHeapMin.TryPeekHead(out aValue: THandle): Boolean;
begin
  Result := NonEmpty;
  if Result then
    aValue := {%H-}THandle(FRoot);
end;

function TGLiteComparablePairHeapMin.Value(aHandle: THandle): T;
begin
  Result := {%H-}PNode(aHandle)^.Data;
end;

procedure TGLiteComparablePairHeapMin.Update(aHandle: THandle; constref aValue: T);
begin
  CheckEmpty;
  UpdateNode({%H-}PNode(aHandle), aValue);
end;

function TGLiteComparablePairHeapMin.Remove(aHandle: THandle): T;
begin
  CheckEmpty;
  Result := RemoveNode({%H-}PNode(aHandle));
end;

function TGLiteComparablePairHeapMin.Merge(aHeap: TGLiteComparablePairHeapMin): SizeInt;
begin
  Result := aHeap.Count;
  if Result > 0 then
    begin
      FNodeManager.Join(aHeap.FNodeManager);
      RootMerge(aHeap.FRoot);
      FCount += Result;
      aHeap.FCount := 0;
      aHeap.FRoot := nil;
    end;
end;

end.

