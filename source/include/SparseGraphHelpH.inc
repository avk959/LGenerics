{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
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

type
  TIntArrayHelper = type helper for TIntArray
  private
    function  GetLenght: SizeInt; inline;
    procedure SetLength(aValue: SizeInt); inline;
  public
    class function Construct(aLength: SizeInt; aInitValue: SizeInt = 0): TIntArray; static; inline;
    function Copy: TIntArray; inline;
    property Length: SizeInt read GetLenght write SetLength;
  end;

  TLGSmallIntHelper = type helper(TGSmallIntHelper) for SmallInt
    const INF_VALUE: SmallInt    = 32767;
    const NEGINF_VALUE: SmallInt = -32768;
  end;

  TLGLongIntHelper = type helper(TGLongIntHelper) for LongInt
    const INF_VALUE: LongInt    = 2147483647;
    const NEGINF_VALUE: LongInt = -2147483648;
  end;

  TLGInt64Helper = type helper(TGInt64Helper) for Int64
    const INF_VALUE: Int64    = 9223372036854775807;
    const NEGINF_VALUE: Int64 = -9223372036854775808;
  end;

  TLGSingleHelper = type helper(TGSingleHelper) for Single
    const INF_VALUE: Single    = 340282346638528859811704183484516925440.0;
    const NEGINF_VALUE: Single = -340282346638528859811704183484516925440.0;
  end;

  TLGDoubleHelper = type helper(TGDoubleHelper) for Double
    const INF_VALUE: Double    = 1.7976931348623157081e+308;
    const NEGINF_VALUE: Double = -1.7976931348623157081e+308;
  end;

{$ifdef FPC_HAS_TYPE_EXTENDED}
  TLGExtendedHelper = type helper(TGExtendedHelper) for Extended
    const INF_VALUE: Extended    = 1.18973149535723176505e+4932;
    const NEGINF_VALUE: Extended = -1.18973149535723176505e+4932;
  end;
{$ENDIF}

  TLGCompHelper = type helper(TCompHelper) for Comp
    const INF_VALUE: Comp    = 9223372036854775807;
    const NEGINF_VALUE: Comp = -9223372036854775808;
  end;

  TLGCurrencyHelper = type helper(TGCurrencyHelper) for Currency
    const INF_VALUE: Currency    = 922337203685477.5807;
    const NEGINF_VALUE: Currency = -922337203685477.5808;
  end;

  generic TGSimpleWeight<T> = record
    Weight: T;
    constructor Create(aValue: T);
  end;

  TTspMatrixState = (tmsProper, tmsTrivial, tmsNonSquare, tmsNegElement);

  { TGTspHelper: some algorithms for TSP }
  generic TGTspHelper<T> = class
  public
  type
    PItem      = ^T;
    TArray     = array of T;
    TTspMatrix = array of array of T;
  protected
  type
    TOnTourReady = procedure(const m: TTspMatrix; var aTour: TArray; var aCost: T);

    { TBbTsp: branch and bound TSP algorithm;
      Little, Murty, Sweeney, and Karel "An Algorithm for Traveling Salesman Problem";
      Syslo, Deo, Kowalik "Discrete Optimization Algorithms: With Pascal Programs";
      advanced matrix reduction:
      Костюк Ю.Л. "Эффективная реализация алгоритма решения задачи коммивояжёра методом ветвей и границ" }
    TBbTsp = object
    protected
    type
      TMinData = record
        Value: T;
        ZeroFlag: Boolean;
        procedure Clear; inline;
      end;

      PMinData    = ^TMinData;
      PInt        = PInteger;
      TMinArray   = array of TMinData;
      TArray      = array of T;
      TBoolMatrix = array of TBoolVector;

    const
      ADV_CUTOFF = 4;

    var
      FMatrix: TArray;
      FZeros: TBoolMatrix;
      FForwardTour,
      FBackTour,
      FBestTour: array of Integer;
      FRowMin,
      FColMin: TMinArray;
      FMatrixSize,
      FTimeOut: Integer;
      FUpBound: T;
      FStartTime: TDateTime;
      FCancelled: Boolean;
      procedure Init(const m: TTspMatrix; const aTour: TIntArray; aTimeOut: Integer);
      function  TimeOut: Boolean; inline;
      function  Reduce(aSize: Integer; aCost: T; aRows, aCols: PInt; aRowRed, aColRed: PItem): T;
      function  ReduceA(aSize: Integer; aCost: T; aRows, aCols: PInt; aRowRed, aColRed: PItem): T;
      function  SelectNext(aSize: Integer; aRows, aCols: PInt; out aRowIdx, aColIdx: Integer): T;
      procedure Search(aSize: Integer; aCost: T; aRows, aCols: PInt);
      procedure CopyBest(var aTour: TIntArray; out aCost: T);
    public
      function  Execute(const m: TTspMatrix; aTimeOut: Integer; var aTour: TIntArray; out aCost: T): Boolean;
    end;

    { TApproxBbTsp }
    TApproxBbTsp = object(TBbTsp)
    protected
      Factor: Double;
      function  Reduce(aSize: Integer; aCost: T; aRows, aCols: PInt; aRowRed, aColRed: PItem): T;
      function  ReduceA(aSize: Integer; aCost: T; aRows, aCols: PInt; aRowRed, aColRed: PItem): T;
      procedure Search(aSize: Integer; aCost: T; aRows, aCols: PInt);
    public
      function  Execute(const m: TTspMatrix; aEps: Double; aTimeOut: Integer; var aTour: TIntArray;
                out aCost: T): Boolean;
    end;

  { TLs3Opt: 3-opt local search algorithm for the traveling salesman problem;
    Syslo, Deo, Kowalik "Discrete Optimization Algorithms: With Pascal Programs"; }
    TLs3Opt = record
    strict private
    type
      TSwap  = record
        X1, X2, Y1, Y2, Z1, Z2: SizeInt;
        Gain: T;
        IsAsymm: Boolean;
      end;

    var
      Matrix: TTspMatrix;
      CurrTour: TIntArray;
      procedure PickSwapKind(var aSwap: TSwap);
      procedure Reverse(aFirst, aLast: SizeInt);
      procedure Execute(var aCost: T);
    public
      procedure OptPath(const m: TTspMatrix; var aTour: TIntArray; var aCost: T);
      procedure OptTree(const m: TTspMatrix; var aTour: TIntArray; var aCost: T);
    end;

    class function  vMin(L, R: T): T; static; inline;
  { returns True if matrix m is symmetric;
    raises exception if m is not proper matrix }
    class function  CheckMatrixProper(const m: TTspMatrix): Boolean; static;
  { cyclic shifts aTour so that element aSrc becomes the first;
    does not checks if aSrc exists in aTour }
    class procedure NormalizeTour(aSrc: SizeInt; var aTour: TIntArray); static;
  { 2-opt local search; does not checks not matrix nor path }
    class procedure Ls2Opt(const m: TTspMatrix; var aTour: TIntArray; var aCost: T); static;
  { 3-opt local search; does not checks not matrix nor path }
    class procedure Ls3OptPath(const m: TTspMatrix; var aTour: TIntArray; var aCost: T); static;
  { 3-opt local search; does not checks not matrix nor path }
    class procedure Ls3OptTree(const m: TTspMatrix; var aTour: TIntArray; var aCost: T); static;
  { best of farthest insertion starting from every vertex; does not checks matrix;
    Syslo, Deo, Kowalik "Discrete Optimization Algorithms: With Pascal Programs"  }
    class function GreedyFInsTsp(const m: TTspMatrix; aOnReady: TOnTourReady; out aCost: T): TIntArray; static;
  { best of nearest neighbour, starting from every vertex; does not checks matrix }
    class function GreedyNearNeighb(const m: TTspMatrix; aOnReady: TOnTourReady; out aCost: T): TIntArray; static;
  public
    class function GetMatrixState(const m: TTspMatrix; out aIsSymm: Boolean): TTspMatrixState; static;
  { returns total cost of TS tour specified by aTour;
    warning: does not checks not matrix not tour }
    class function GetTotalCost(const m: TTspMatrix; const aTour: TIntArray): T; static;
  { best of farthest insertion starting from every vertex;
    will raise EGraphError if m is not proper TSP matrix }
    class function FindGreedyFast(const m: TTspMatrix; out aCost: T): TIntArray; static;
  { best of nearest neighbour starting from every vertex;
    will raise EGraphError if m is not proper TSP matrix }
    class function FindGreedyFastNn(const m: TTspMatrix; out aCost: T): TIntArray; static;
  { best of nearest neighbour + 2-opt local search starting from every vertex +
    3-opt local search at the end; applicable only for symmetric matrices;
    returns best of nearest neighbour starting from every vertex, if matrix is asymmetric;
    will raise EGraphError if m is not proper TSP matrix }
    class function FindGreedy2Opt(const m: TTspMatrix; out aCost: T): TIntArray; static;
  { best of farthest insertion starting from every vertex + 3-opt local search at the end;
    applicable only for symmetric matrices;
    returns best of nearest neighbour starting from every vertex, if matrix is asymmetric;
    will raise EGraphError if m is not proper TSP matrix }
    class function FindGreedy3Opt(const m: TTspMatrix; out aCost: T): TIntArray; static;
  { best of farthest insertion + 3-opt local search, starting from every vertex;
    applicable only for symmetric matrices;
    returns best of nearest neighbour starting from every vertex, if matrix is asymmetric;
    will raise EGraphError if m is not proper TSP matrix }
    class function FindSlowGreedy3Opt(const m: TTspMatrix; out aCost: T): TIntArray; static;
  { exact branch and bound algorithm for TSP;
    aTimeOut specifies the timeout in seconds; at the end of the timeout,
    will be returned False and the best recent solution;
    will raise EGraphError if m is not proper TSP matrix }
    class function FindExact(const m: TTspMatrix; out aTour: TIntArray; out aCost: T;
                   aTimeOut: Integer = WAIT_INFINITE): Boolean; static;
  { suboptimal branch and bound algorithm for TSP;
    aTimeOut specifies the timeout in seconds; at the end of the timeout,
    will be returned False and the best recent solution, otherwise
    returns solution of a given guaranteed accuracy, specified with param Accuracy;
    will raise EGraphError if m is not proper TSP matrix }
    class function FindApprox(const m: TTspMatrix; Accuracy: Double; out aTour: TIntArray; out aCost: T;
                   aTimeOut: Integer = WAIT_INFINITE): Boolean; static;
  end;

  generic TGPoint2D<T> = record
    X, Y: T;
    constructor Create(aX, aY: T);
    class function Equal(constref L, R: TGPoint2D): Boolean; static; inline;
    class function HashCode(constref aPoint: TGPoint2D): SizeInt; static; inline;
    function Distance(constref aPoint: TGPoint2D): ValReal; inline;
  end;

  generic TGPoint3D<T> = record
    X, Y, Z: T;
    class function Equal(constref L, R: TGPoint3D): Boolean; static; inline;
    class function HashCode(constref aPoint: TGPoint3D): SizeInt; static; inline;
    constructor Create(aX, aY, aZ: T);
    function Distance(constref aPoint: TGPoint3D): ValReal; inline;
  end;

  generic TGEuclidTspHelper<TGPoint> = class
  public
  type
    TPointArray = array of TGPoint;
  private
  type
    THelper  = specialize TGTspHelper<ValReal>;
    THashSet = specialize TGLiteHashSetLP<TGPoint, TGPoint>;

  public

  end;

  TDisjointSetUnion = record
  strict private
    FList: array of SizeInt;
    function  GetSize: SizeInt; inline;
    procedure SetSize(aValue: SizeInt);
  public
    procedure Clear; inline;
    procedure Reset;
  { values related to the same subset will have the same Tag }
    function  Tag(aValue: SizeInt): SizeInt;
    function  InSameSet(L, R: SizeInt): Boolean; inline;
    function  InDiffSets(L, R: SizeInt): Boolean; inline;
  { returns True and merges L and R, if L and R related to the different subsets, False otherwise }
    function  Join(L, R: SizeInt): Boolean;
    property  Size: SizeInt read GetSize write SetSize;
  end;

  TIntValue = record
    Value: SizeInt;
    constructor Create(aValue: SizeInt);
    property Key: SizeInt read Value write Value;
  end;

  TIntHashSet = record
  strict private
  type
    TTable = specialize TGLiteIntHashTable<SizeInt, TIntValue>;
    PEntry = TTable.PEntry;

  public
  type
    TEnumerator = record
    private
      FEnum: TTable.TEnumerator;
      function  GetCurrent: SizeInt; inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: SizeInt read GetCurrent;
    end;

  strict private
    FTable: TTable;
    function  GetCount: SizeInt; inline;
  public
    function  GetEnumerator: TEnumerator; inline;
    function  ToArray: TIntArray;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure MakeEmpty; inline;
    procedure Clear; inline;
    procedure EnsureCapacity(aValue: SizeInt); inline;
    function  Contains(aValue: SizeInt): Boolean; inline;
    function  Add(aValue: SizeInt): Boolean;
    function  AddAll(constref a: array of SizeInt): SizeInt;
    function  AddAll(constref s: TIntHashSet): SizeInt;
    function  Remove(aValue: SizeInt): Boolean; inline;
    property  Count: SizeInt read GetCount;
  end;

  TOrdIntPair = packed record
  strict private
    FLess,
    FGreater: SizeInt;
  public
    class function HashCode(constref aValue: TOrdIntPair): SizeInt; static; inline;
    class function Equal(constref L, R: TOrdIntPair): Boolean; static; inline;
    constructor Create(L, R: SizeInt);
    function Key: TOrdIntPair; inline;
    property Left: SizeInt read FLess;
    property Right: SizeInt read FGreater;
  end;

  POrdIntPair = ^TOrdIntPair;

  TIntPairSet = record
  strict private
  type
    TTable = specialize TGLiteHashTableLP<TOrdIntPair, TOrdIntPair, TOrdIntPair>;
  var
    FTable: TTable;
    function GetCount: SizeInt; inline;
  public
    procedure Clear; inline;
    function  Contains(L, R: SizeInt): Boolean; inline;
    function  Add(L, R: SizeInt): Boolean;
    function  Remove(L, R: SizeInt): Boolean; inline;
    property  Count: SizeInt read GetCount;
  end;

  TIntNode = record
    Index,
    Data: SizeInt;
    class operator = (constref L, R: TIntNode): Boolean; inline;
    class operator <>(constref L, R: TIntNode): Boolean; inline;
    class operator > (constref L, R: TIntNode): Boolean; inline;
    class operator < (constref L, R: TIntNode): Boolean; inline;
    class operator >=(constref L, R: TIntNode): Boolean; inline;
    class operator <=(constref L, R: TIntNode): Boolean; inline;
    constructor Create(aIndex, aData: SizeInt);
  end;

  generic TGBinHeapMin<T> = record // for internal use only
  strict private
  type
    THeap = array of T;

  var
    FHeap: THeap;
    FHandle2Index: TIntArray;
    FIndex2Handle: TIntArray;
    FCount: SizeInt;
    function  GetCapacity: SizeInt; inline;
    procedure FloatUp(aIndex: SizeInt);
    procedure SiftDown(aIndex: SizeInt);
  public
  type
    PItem = ^T;
    constructor Create(aSize: SizeInt);
    procedure MakeEmpty; inline;
    function  TryDequeue(out aValue: T): Boolean;
    procedure Enqueue(aHandle: SizeInt; constref aValue: T);
    procedure Update(aHandle: SizeInt; constref aNewValue: T);
    function  Peek(aHandle: SizeInt): T; inline;
    function  HeadPtr(aHandle: SizeInt): PItem; inline;
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
  end;

  generic TGPairHeapMin<T> = record // for internal use only
  strict private
  type
    PNode = ^TNode;
    TNode = record
      Prev,
      Child,
      Sibling: PNode;
      Data: T;
      function AddChild(aNode: PNode): PNode; inline;
    end;

    TNodeList = array of TNode;

  var
    FNodeList: TNodeList;
    FRoot: PNode;
    FCount: SizeInt;
    function  GetCapacity: SizeInt; inline;
    function  NewNode(constref aValue: T; aHandle: SizeInt): PNode;
    function  DequeueItem: T;
    procedure RootMerge(aNode: PNode); inline;
    procedure ExtractNode(aNode: PNode);
    class function  NodeMerge(L, R: PNode): PNode; static;
    class function  TwoPassMerge(aNode: PNode): PNode; static;
    class procedure CutNode(aNode: PNode); static; inline;
  public
  type
    PItem = ^T;
    constructor Create(aSize: SizeInt);
    procedure MakeEmpty; inline;
    function  TryDequeue(out aValue: T): Boolean; inline;
    function  Dequeue: T; inline;
    procedure Enqueue(aHandle: SizeInt; constref aValue: T); inline;
    procedure Update(aHandle: SizeInt; constref aNewValue: T);
    procedure Remove(aHandle: SizeInt); inline;
    function  Peek(aHandle: SizeInt): T; inline;
    function  HeadPtr(aHandle: SizeInt): PItem; inline;
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
  end;

  generic TGPairHeapMax<T> = record // for internal use only
  strict private
  type
    PNode = ^TNode;
    TNode = record
      Prev,
      Child,
      Sibling: PNode;
      Data: T;
      function AddChild(aNode: PNode): PNode; inline;
    end;

    TNodeList = array of TNode;

  var
    FNodeList: TNodeList;
    FRoot: PNode;
    FCount: SizeInt;
    function  GetCapacity: SizeInt; inline;
    function  NewNode(constref aValue: T; aHandle: SizeInt): PNode; inline;
    function  DequeueItem: T;
    procedure RootMerge(aNode: PNode); inline;
    class function  NodeMerge(L, R: PNode): PNode; static;
    class function  TwoPassMerge(aNode: PNode): PNode; static;
    class procedure CutNode(aNode: PNode); static; inline;
  public
  type
    PItem = ^T;
    constructor Create(aSize: SizeInt);
    procedure MakeEmpty; inline;
    function  TryDequeue(out aValue: T): Boolean; inline;
    function  Dequeue: T; inline;
    procedure Enqueue(aHandle: SizeInt; constref aValue: T); inline;
    procedure Update(aHandle: SizeInt; constref aNewValue: T);
    function  Peek(aHandle: SizeInt): T; inline;
    function  HeadPtr(aHandle: SizeInt): PItem; inline;
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
  end;

  TINodePqMin = specialize TGPairHeapMin<TIntNode>;
  TINodePqMax = specialize TGPairHeapMax<TIntNode>;

  { TGJoinableHashList: for internal use only; TEntry must provide
      property Key: SizeInt and numeric field Weight }
  generic TGJoinableHashList<TEntry> = record
  strict private
  type
    TTable = specialize TGLiteIntHashTable<SizeInt, TEntry>;
  public
  type
    TEnumerator = TTable.TEnumerator;
    PEntry      = ^TEntry;

  strict private
    FTable: TTable;
    function  GetCount: SizeInt; inline;
  public
    function  GetEnumerator: TEnumerator; inline;
    procedure EnsureCapacity(aValue: SizeInt); inline;
    procedure Add(constref aValue: TEntry);
    procedure AddAll(constref aList: TGJoinableHashList);
    procedure Remove(aValue: SizeInt); inline;
    property  Count: SizeInt read GetCount;
  end;

  TSimpleStack = record
  strict private
    Items: TIntArray;
    Top: SizeInt;
    function  GetCapacity: SizeInt; inline;
    function  GetCount: SizeInt; inline;
  public
    constructor Create(aSize: SizeInt);
    function  ToArray: TIntArray; inline;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure MakeEmpty; inline;
    procedure Push(aValue: SizeInt); inline;
    function  Pop: SizeInt; inline;
    function  TryPop(out aValue: SizeInt): Boolean; inline;
    function  Peek: SizeInt; inline;
    function  TryPeek(out aValue: SizeInt): Boolean; inline;
    property  Count: SizeInt read GetCount;
    property  Capacity: SizeInt read GetCapacity;
  end;
  PSimpleStack = ^TSimpleStack;

  TCostItem = record
    Index: SizeInt;
    Cost: TCost;
    class operator = (constref L, R: TCostItem): Boolean; inline;
    class operator <>(constref L, R: TCostItem): Boolean; inline;
    class operator > (constref L, R: TCostItem): Boolean; inline;
    class operator < (constref L, R: TCostItem): Boolean; inline;
    class operator >=(constref L, R: TCostItem): Boolean; inline;
    class operator <=(constref L, R: TCostItem): Boolean; inline;
    constructor Create(aIndex: SizeInt; aCost: TCost);
    property Key: SizeInt read Index;
  end;

  { TGWeightHelper }
  generic TGWeightHelper<TVertex, TWeight, TEdgeData, TEqRel> = class sealed
  public
  type
    TWeightArray  = array of TWeight;
    PWeight       = ^TWeight;

  strict private
    class function  CreateAndFill(aValue: TWeight; aSize: SizeInt): TWeightArray; static;
    class procedure Fill(var a: TWeightArray; aValue: TWeight); static;
    class function  wMax(L, R: TWeight): TWeight; static; inline;
    class function  wMin(L, R: TWeight): TWeight; static; inline;
    class function  ExtractCycle(aRoot, aLen: SizeInt; constref aTree: TIntArray): TIntArray; static;

  public
  type
    TWeightEdge = record
      Source,
      Destination: SizeInt;
      Weight:  TWeight;
      class operator = (constref L, R: TWeightEdge): Boolean; inline;
      class operator <>(constref L, R: TWeightEdge): Boolean; inline;
      class operator > (constref L, R: TWeightEdge): Boolean; inline;
      class operator < (constref L, R: TWeightEdge): Boolean; inline;
      class operator >=(constref L, R: TWeightEdge): Boolean; inline;
      class operator <=(constref L, R: TWeightEdge): Boolean; inline;
      constructor Create(s, d: SizeInt; w: TWeight);
      function Edge: TIntEdge; inline;
    end;

    TWeightItem = record
      Index: SizeInt;
      Weight: TWeight;
      class operator = (constref L, R: TWeightItem): Boolean; inline;
      class operator <>(constref L, R: TWeightItem): Boolean; inline;
      class operator > (constref L, R: TWeightItem): Boolean; inline;
      class operator < (constref L, R: TWeightItem): Boolean; inline;
      class operator >=(constref L, R: TWeightItem): Boolean; inline;
      class operator <=(constref L, R: TWeightItem): Boolean; inline;
      constructor Create(aIndex: SizeInt; w: TWeight);
      property Key: SizeInt read Index;
    end;

    TRankItem = record
      Index: SizeInt;
      Rank,
      Weight: TWeight;
      class operator = (constref L, R: TRankItem): Boolean; inline;
      class operator <>(constref L, R: TRankItem): Boolean; inline;
      class operator > (constref L, R: TRankItem): Boolean; inline;
      class operator < (constref L, R: TRankItem): Boolean; inline;
      class operator >=(constref L, R: TRankItem): Boolean; inline;
      class operator <=(constref L, R: TRankItem): Boolean; inline;
      constructor Create(aIndex: SizeInt; aRank, aWeight: TWeight);
    end;

    TApspCell = record
      Weight: TWeight;
      Predecessor: SizeInt;
      constructor Create(aWeight: TWeight; aSource: SizeInt);
    end;

    TGraph        = specialize TGSparseGraph<TVertex, TEdgeData, TEqRel>;
    TEstimate     = function(constref aSrc, aDst: TVertex): TWeight;
    TPairHeap     = specialize TGPairHeapMin<TWeightItem>;
    TBinHeap      = specialize TGBinHeapMin<TWeightItem>;
    TAStarHeap    = specialize TGBinHeapMin<TRankItem>;
    TEdgeArray    = array of TWeightEdge;
    TWeightMatrix = array of array of TWeight;
    TApspMatrix   = array of array of TApspCell;

    { THungarian: hungarian weighted bipartite matching algorithm }
    THungarian = record
    strict private
      FGraph: TGraph;
      FMates,
      FParents,
      FQueue: TIntArray;
      FPhi: TWeightArray;
      FWhites,
      FVisited: TBoolVector;
      FMatchCount: SizeInt;
      procedure Match(aNode, aMate: SizeInt); inline;
      procedure Init(aGraph: TGraph; const w, g: TIntArray);
      procedure InitMax(aGraph: TGraph; const w, g: TIntArray);
      function  FindAugmentPathMin(aRoot: SizeInt; var aDelta: TWeight): SizeInt;
      function  FindAugmentPathMax(aRoot: SizeInt; var aDelta: TWeight): SizeInt;
      procedure AlternatePath(aRoot: SizeInt);
      function  TryAugmentMin(var aDelta: TWeight): SizeInt;
      function  TryAugmentMax(var aDelta: TWeight): SizeInt;
      procedure CorrectPhi(aDelta: TWeight); inline;
      procedure ExecuteMin;
      procedure ExecuteMax;
      function  CreateEdges: TEdgeArray;
    public
      function  MinWeightMatching(aGraph: TGraph; const w, g: TIntArray): TEdgeArray;
      function  MaxWeightMatching(aGraph: TGraph; const w, g: TIntArray): TEdgeArray;
    end;

    { TBfmt: Bellman-Ford-Moore SSSP algorithm with Tarjan subtree disassembly;
      copies graph into internal representation }
    TBfmt = record
    strict private
    type
      PNode  = ^TNode;
      PArc   = ^TArc;

      TArc = record
        Target: PNode;       // pointer to target node
        Weight: TWeight;
        constructor Create(aTarget: PNode; aWeight: TWeight);
      end;

      TNode = record
      private
        FirstArc: PArc;      // pointer to first incident arc
        Weight: TWeight;
        TreePrev,
        TreeNext,
        Parent: PNode;
        Level: SizeInt;
      end;

    public
      Nodes: array of TNode;
    strict private
      FArcs: array of TArc;
      FQueue: array of PNode;
      FInQueue,
      FActive: TGraph.TBitVector;
      FGraph: TGraph;
      FNodeCount: SizeInt;
      procedure CopyGraph(aDirected: Boolean);
      procedure SsspInit(aSrc: SizeInt);
    public
      constructor Create(aGraph: TGraph; aDirected: Boolean);
      function  IndexOf(aNode: PNode): SizeInt; inline;
      procedure Sssp(aSrc: SizeInt);
    end;

  { returns True if aMatch is maximal matching }
    class function  IsMaxMatching(g: TGraph; const aMatch: TEdgeArray): Boolean; static;
  { returns True if aMatch is perfect matching }
    class function  IsPerfectMatching(g: TGraph; const aMatch: TEdgeArray): Boolean; static;
  { Dijkstra's algorithm: single-source shortest paths problem for non-negative weights  }
    class function  DijkstraSssp(g: TGraph; aSrc: SizeInt): TWeightArray; static;
    class function  DijkstraSssp(g: TGraph; aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray; static;
  { Dijkstra's pathfinding algorithm }
    class function  DijkstraPath(g: TGraph; aSrc, aDst: SizeInt): TWeight; static;
    class function  DijkstraPath(g: TGraph; aSrc, aDst: SizeInt; out aWeight: TWeight): TIntArray; static;
  { A* pathfinding algorithm }
    class function  AStar(g: TGraph; aSrc, aDst: SizeInt; out aWeight: TWeight; aEst: TEstimate): TIntArray; static;
  { modification of Bellman-Ford-Moore algorithm(aka SPFA), faster on random graps;
    en.wikipedia.org/wiki/Shortest_Path_Faster_Algorithm }
    class function  SpfaBase(g: TGraph; aSrc: SizeInt; out aTree: TIntArray; out aWeights: TWeightArray): SizeInt;
                    static;
    class function  Spfa2Base(g: TGraph; aSrc: SizeInt; out aTree: TIntArray; out aWeights: TWeightArray): SizeInt;
                    static;
  { inplace Bellman-Ford-Moore algorithm with Tarjan subtree disassembly,
    faster negative cycle detection }
    class function  BfmtBase(g: TGraph; aSrc: SizeInt; out aParents: TIntArray; out aWeights: TWeightArray): SizeInt;
                    static;
    class function  BfmtReweight(g: TGraph; out aWeights: TWeightArray): SizeInt; static;
  { negative cycle detection }
    class function  NegCycleDetect(g: TGraph; aSrc: SizeInt): TIntArray; static;
  { BFMT single-source shortest paths problem }
    class function  BfmtSssp(g: TGraph; aSrc: SizeInt; out aWeights: TWeightArray): Boolean; static;
    class function  BfmtSssp(g: TGraph; aSrc: SizeInt; out aPaths: TIntArray; out aWeights: TWeightArray): Boolean;
                    static;
  { BFMT pathfinding }
    class function  BfmtPath(g: TGraph; aSrc, aDst: SizeInt; out aPath: TIntArray; out aWeight: TWeight): Boolean;
                    static;
  { returns True if no negative cycle found, otherwise returns False and aPaths will contain
    single element with the index of an element on negative cycle }
    class function  FloydApsp(aGraph: TGraph; out aPaths: TApspMatrix): Boolean; static;
    class function  JohnsonApsp(aGraph: TGraph; out aPaths: TApspMatrix): Boolean; static;
    class function  BfmtApsp(aGraph: TGraph; aDirect: Boolean; out aPaths: TApspMatrix): Boolean; static;
  { creates array of length aLen and fills with InfWeight }
    class function  CreateWeightArray(aLen: SizeInt): TWeightArray; static; inline;
  { creates array of length aLen and fills array with NegInfWeight }
    class function  CreateWeightArrayNI(aLen: SizeInt): TWeightArray; static; inline;
  { creates array of length aLen and fills array with ZeroWeight }
    class function  CreateWeightArrayZ(aLen: SizeInt): TWeightArray; static; inline;
  { resizes array to length aLen and fills array with aValue }
    class procedure ResizeAndFill(var a: TWeightArray; aLen: SizeInt; aValue: TWeight); static;
    class function  CreateWeightsMatrix(aGraph: TGraph): TWeightMatrix; static;
  { creates square matrix, fills main diagonal with (I, ZeroWeight) and
    fills other element with corresponding weights }
    class function  CreateAPSPMatrix(aGraph: TGraph): TApspMatrix; static;
  { warning: does not checks input }
    class function  ExtractMinPath(aSrc, aDst: SizeInt; const aMatrix: TApspMatrix): TIntArray; static;
  { returns bipartite matching of maximum cardinality and minimum weight;
    warning: does not checks if aGraph is bipartite }
    class function  MinWeightMatchingB(aGraph: TGraph; const w, g: TIntArray): TEdgeArray; static;
  { returns bipartite matching of maximum cardinality and maximum weight;
    warning: does not checks if aGraph is bipartite }
    class function  MaxWeightMatchingB(aGraph: TGraph; const w, g: TIntArray): TEdgeArray; static;
  end;

  { TGAbstractDotWriter: abstract writer into Graphviz dot format }
  generic TGAbstractDotWriter<TVertex, TEdgeData, TEqRel> = class abstract
  public
  type
    TWriteDirection = (wdTopToBottom, wdLeftToWrite);
    TGraph          = specialize TGSparseGraph<TVertex, TEdgeData, TEqRel>;
    TOnStartWrite   = function(aGraph: TGraph): utf8string of object;
    TOnWriteVertex  = function(aGraph: TGraph; aIndex: SizeInt): utf8string of object;
    TOnWriteEdge    = function(aGraph: TGraph; constref aEdge: TGraph.TEdge): utf8string of object;

  protected
  const
    DIRECTS: array[TWriteDirection] of utf8string = ('rankdir=TB;', 'rankdir=LR;');
  var
    FGraphMark,
    FEdgeMark: utf8string;
    FDirection: TWriteDirection;
    FOnStartWrite: TOnStartWrite;
    FOnWriteVertex: TOnWriteVertex;
    FOnWriteEdge: TOnWriteEdge;
    function Graph2Dot(aGraph: TGraph): utf8string; virtual; abstract;
    function DefaultWriteEdge({%H-}aGraph: TGraph; constref aEdge: TGraph.TEdge): utf8string; virtual;
  public
    procedure SaveToStream(aGraph: TGraph; aStream: TStream);
    procedure SaveToFile(aGraph: TGraph; const aFileName: string);
    property Direction: TWriteDirection read FDirection write FDirection;
    property OnStartWrite: TOnStartWrite read FOnStartWrite write FOnStartWrite;
    property OnWriteVertex: TOnWriteVertex read FOnWriteVertex write FOnWriteVertex;
    property OnWriteEdge: TOnWriteEdge read FOnWriteEdge write FOnWriteEdge;
  end;

