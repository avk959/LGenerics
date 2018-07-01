{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generic simple undirected graphs implementation.                        *
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
unit LGSimpleGraph;

{$mode objfpc}{$H+}
{$INLINE ON}{$WARN 6058 off : }
{$WARN 5024 off : Parameter "$1" not used}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils,
  LGUtils,
  {%H-}LGHelpers,
  LGArrayHelpers,
  LGVector,
  LGraphUtils,
  LGStrConst;

type
  { TGSimpleGraph implements simple sparse undirected graph based on adjacency lists;

      functor TEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGSimpleGraph<TVertex, TEdgeData, TEqRel> = class(specialize TGCustomGraph<TVertex, TEdgeData, TEqRel>)
  protected
  type
    TBoolMatrix      = array of TBoolVector;
    TIntDegreeHelper = specialize TGDelegatedArrayHelper<SizeInt>;

    TSortByNebDegrees = object
    private
      FGraph: TGSimpleGraph;
      FNebDegrees: TIntArray;
      function Cmp(constref L, R: SizeInt): SizeInt;
    public
      procedure Sort(var a: TIntArray; constref aDegrees: TIntArray; g: TGSimpleGraph; o: TSortOrder);
    end;

    TCliqueHelper = record
    private
      FMatrix: TBoolMatrix;
      FAccum: TBoolVector;
      FResult: TIntArray;
      FCurrSize: SizeInt;
      FVertices: TIntArray;
      FOnFind: TOnFindSet;
      procedure GetColors(constref aCand: TBoolVector; var aColOrd, aColors: TIntArray);
      procedure Extend(var aCand: TBoolVector);
      procedure Extend(var aCand, aTested: TBoolVector);
    public
      function  MaxClique(aGraph: TGSimpleGraph): TIntArray;
      procedure ListCliques(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
    end;

    TListIsHelper = record
    private
      FMatrix: TBoolMatrix;
      FAccum: TBoolVector;
      FOnFindSet: TOnFindSet;
      procedure Extend(var aCand, aTested: TBoolVector);
    public
      procedure ListIS(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
    end;

    TMaxIsHelper = record
    private
      FMatrix: TBoolMatrix;
      FAccum: TBoolVector;
      FResult: TIntArray;
      FCurrSize: SizeInt;
      procedure Extend(var aCand: TBoolVector);
    public
      function  MaxIS(aGraph: TGSimpleGraph): TIntArray;
    end;

    TBits256 = record
    public
    const
      BITNESS      = 256;
      BIT_PER_LIMB = BitsizeOf(SizeUInt);
      LIMB_COUNT = BITNESS div BIT_PER_LIMB;

    type
      PBits256 = ^TBits256;

      TEnumerator = record
      private
        FValue: PBits256;
        FBitIndex,
        FLimbIndex: SizeInt;
        FCurrLimb: SizeUInt;
        FInCycle: Boolean;
        function GetCurrent: SizeInt; inline;
        function FindFirst: Boolean;
      public
        function MoveNext: Boolean;
        property Current: SizeInt read GetCurrent;
      end;

      TReverseEnumerator = record
      private
        FValue: PBits256;
        FBitIndex,
        FLimbIndex: SizeInt;
        FCurrLimb: SizeUInt;
        FInCycle: Boolean;
        function GetCurrent: SizeInt; inline;
        function FindFirst: Boolean;
      public
        function MoveNext: Boolean;
        property Current: SizeInt read GetCurrent;
      end;

      TReverseOrder = record
      private
        FValue: PBits256;
      public
        function GetEnumerator: TReverseEnumerator; inline;
      end;

    private
    type
      TBits = array[0..Pred(LIMB_COUNT)] of SizeUInt;

    var
      FBits: TBits;
      function  GetBit(aIndex: SizeInt): Boolean; inline;
      procedure SetBit(aIndex: SizeInt; aValue: Boolean); inline;
      class function  BsfValue(aValue: SizeUInt): SizeInt; static; inline;
      class function  BsrValue(aValue: SizeUInt): SizeInt; static; inline;
      class procedure ClearBit(aIndex: SizeInt; var aValue: SizeUInt); static; inline;
    public
      function  GetEnumerator: TEnumerator; inline;
      function  ReverseOrder: TReverseOrder; inline;
      procedure InitRange(aRange: SizeInt);
      procedure InitZero; inline;
    { returns an array containing the indices of the set bits }
      function  ToArray: TIntArray;
      function  IsEmpty: Boolean;
      function  NonEmpty: Boolean; inline;
      function  Bsf: SizeInt; inline;
      function  Bsr: SizeInt; inline;
      function  Intersecting(constref aVector: TBits256): Boolean;
      function  ContainsAll(constref aVector: TBits256): Boolean;
      procedure Subtract(constref aVector: TBits256); inline;
      procedure Intersect(constref aVector: TBits256); inline;
    { returns count of set bits }
      function  PopCount: SizeInt; inline;
      property  Bits[aIndex: SizeInt]: Boolean read GetBit write SetBit; default;
    end;

    TBits256Matrix = array of TBits256;

    TCliqueHelper256 = record
    private
      FMatrix: TBits256Matrix;
      FAccum,
      FResult: TBits256;
      FCurrSize: SizeInt;
      FVertices: TIntArray;
      FOnFindSet: TOnFindSet;
      procedure GetColors(constref aCand: TBits256; var aColOrd, aColors: TIntArray);
      procedure Extend(var aCand: TBits256);
      procedure Extend(var aCand, aTested: TBits256);
    public
      function  MaxClique(aGraph: TGSimpleGraph): TIntArray;
      procedure ListCliques(aGraph: TGSimpleGraph; aOnFindSet: TOnFindSet);
    end;

    TStaticListIsHelper = record
    private
      FMatrix: TBits256Matrix;
      FAccum: TBits256;
      FOnFindSet: TOnFindSet;
      procedure Extend(var aCand, aTested: TBits256);
    public
      procedure ListIS(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
    end;

    TStaticMaxIsHelper = record
    private
      FMatrix: TBits256Matrix;
      FAccum: TBits256;
      FResult: TIntArray;
      FCurrSize: SizeInt;
      FDegrees: TIntArray;
      procedure Extend(var aCand: TBits256);
    public
      function  MaxIS(aGraph: TGSimpleGraph): TIntArray;
    end;

    TDistinctEdgeEnumerator = record
    private
      FList: PNode;
      FEnum: TAdjList.TEnumerator;
      FCurrIndex,
      FLastIndex: SizeInt;
      FEnumDone: Boolean;
      function  GetCurrent: TEdge;
    public
      function  MoveNext: Boolean;
      procedure Reset;
      property  Current: TEdge read GetCurrent;
    end;

    TDistinctEdges = record
      private
        FGraph: TGSimpleGraph;
      public
        function GetEnumerator: TDistinctEdgeEnumerator;
    end;

  var
    FCompCount: SizeInt;
    FConnected,
    FConnectedValid: Boolean;
    procedure ResetTags;
    function  SeparateTag(aIndex: SizeInt): SizeInt;
    function  SeparateMerged(L, R: SizeInt): Boolean;
    procedure ValidateConnected;
    function  GetConnected: Boolean; inline;
    procedure DoRemoveVertex(aIndex: SizeInt);
    function  DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
    function  CreateSkeleton: TSkeleton;
    function  CreateBoolMatrix: TBoolMatrix;
    function  CreateBits256Matrix: TBits256Matrix;
    function  GetSeparateGraph(aIndex: SizeInt): TGSimpleGraph;
    function  GetSeparateCount: SizeInt;
    function  CountPop(aTag: SizeInt): SizeInt;
    function  GetEccentricity(aIndex: SizeInt): SizeInt;
    function  MakeConnected(aOnAddEdge: TOnAddEdge): SizeInt;
    function  CycleExists(aRoot: SizeInt; out aCycle: TIntArray): Boolean;
    function  GetMaxClique: TIntArray;
    function  GetMaxCliqueStatic: TIntArray;
    procedure ListCliques(aOnFindSet: TOnFindSet);
    procedure ListCliquesStatic(aOnFindSet: TOnFindSet);
    function  GetMaxIS: TIntArray;
    function  GetMaxISStatic: TIntArray;
    procedure ListIS(aOnFindSet: TOnFindSet);
    procedure ListISStatic(aOnFindSet: TOnFindSet);
    procedure SearchForCutPoints(aRoot: SizeInt; var aPoints: TIntVector);
    function  CutPointExists(aRoot: SizeInt): Boolean;
    procedure SearchForBiconnect(aRoot: SizeInt; var aEdges: TIntEdgeVector);
    procedure SearchForBicomponent(aRoot: SizeInt; var aComp: TEdgeArrayVector);
    function  BridgeExists: Boolean;
    procedure SearchForBridges(var aBridges: TIntEdgeVector);
    procedure SearchForFundamentalsCycles(out aCycles: TIntArrayVector);
    procedure SearchForFundamentalsCyclesLen(out aCycleLens: TIntVector);
    procedure FindFundamentalCyclesLen(out aCycleLens: TIntVector);
    function  CreateSortedByDegree(aOrder: TSortOrder = soAsc): TIntArray;
    procedure FillSortedMatrix(out aMatrix: TBoolMatrix; out aVertices: TIntArray; o: TSortOrder = soAsc);
    procedure FillSortedMatrix256(out aMatrix: TBits256Matrix; out aVertices: TIntArray; o: TSortOrder = soAsc);
    function  CreateDegreeArray: TIntArray;
    function  CmpIntArrayLen(constref L, R: TIntArray): SizeInt;
    function  CmpVertexDegree(constref L, R: SizeInt): SizeInt;
    property  InnerConnected: Boolean read FConnected;
  public
    class function MayBeEqual(L, R: TGSimpleGraph): Boolean;
    constructor Create;
    constructor Create(aCapacity: SizeInt);
    procedure Clear; override;
  { returns True and vertex index, if it is added, False if such a vertex already exists }
    function  AddVertex(constref aVertex: TVertex; out aIndex: SizeInt): Boolean;
    function  AddVertex(constref aVertex: TVertex): Boolean; inline;
  { note: removing destroys validity of connected }
    procedure RemoveVertex(constref aVertex: TVertex); inline;
    procedure RemoveVertexI(aIndex: SizeInt);
  { returns True if the edge is added, False, if such an edge already exists }
    function  AddEdge(constref aSrc, aDst: TVertex; aData: TEdgeData): Boolean;
    function  AddEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  AddEdgeI(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  AddEdgeI(aSrc, aDst: SizeInt): Boolean; inline;
  { returns False if there is no such edge; note: removing destroys validity of connected }
    function  RemoveEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  RemoveEdgeI(aSrc, aDst: SizeInt): Boolean;
  { warning: currently binary format is not portable }
    procedure SaveToStream(aStream: TStream; aWriteVertex: TOnWriteVertex; aWriteData: TOnWriteData);
    procedure LoadFromStream(aStream: TStream; aReadVertex: TOnReadVertex; aReadData: TOnReadData);
    procedure SaveToFile(const aFileName: string; aWriteVertex: TOnWriteVertex; aWriteData: TOnWriteData);
    procedure LoadFromFile(const aFileName: string; aReadVertex: TOnReadVertex; aReadData: TOnReadData);
    function  Degree(constref aVertex: TVertex): SizeInt; inline;
    function  DegreeI(aIndex: SizeInt): SizeInt;
    function  Isolated(constref aVertex: TVertex): Boolean; inline;
    function  IsolatedI(aIndex: SizeInt): Boolean; inline;
    function  DistinctEdges: TDistinctEdges; inline;
  { returns the eccentricity of the aVertex within its connected component }
    function  Eccentricity(constref aVertex: TVertex): SizeInt; inline;
    function  EccentricityI(aIndex: SizeInt): SizeInt;
  { returns local clustering coefficient of the aVertex: how close its neighbours are to being a clique }
    function  LocalClustering(constref aVertex: TVertex): ValReal; inline;
    function  LocalClusteringI(aIndex: SizeInt): ValReal;
  { if the graph is not empty, then make graph connected, adding, if necessary, new edges
    from the vertex with the index 0; returns count of added edges }
    function  EnsureConnected(aOnAddEdge: TOnAddEdge): SizeInt;
  { checks whether the aDst reachable from the aSrc; each vertex reachable from itself  }
    function  PathExists(constref aSrc, aDst: TVertex): Boolean; inline;
    function  PathExistsI(aSrc, aDst: SizeInt): Boolean;
  { returns index of the connected component that contains aVertex }
    function  SeparateIndexOf(constref aVertex: TVertex): SizeInt; inline;
    function  SeparateIndexI(aIndex: SizeInt): SizeInt;
  { returns number of Items(population) in the InnerConnected component that contains aVertex }
    function  SeparatePop(constref aVertex: TVertex): SizeInt; inline;
    function  SeparatePopI(aIndex: SizeInt): SizeInt;
    function  IsTree: Boolean;
    function  CyclomaticNumber: SizeInt;
  { checks whether the graph is a regular graph (that is, the degree of all its vertices equal);
    an empty graph is considered regular }
    function  IsRegular: Boolean;
  { checks whether exists any cycle in theh same connected component as aRoot;
    if True then aCycle will contain indices of the vertices of the cycle }
    function  ContainsCycle(constref aRoot: TVertex; out aCycle: TIntArray): Boolean; inline;
    function  ContainsCycleI(aRoot: SizeInt; out aCycle: TIntArray): Boolean;
    function  ContainsEulerianCircuit: Boolean;
  { looking for some Eulerian cycle in the first connected component along the path from the first vertex }
    function  FindEulerianCircuit(out aCircuit: TIntVector): Boolean;
  { finds a certain system of fundamental cycles of the graph;
    note: pretty costly time/memory operation }
    function  FindFundamentalCycles(out aCycles: TIntArrayVector): Boolean;
  { lists all found maximal independent sets }
    procedure ListIndependentSets(aOnFindSet: TOnFindSet);
  { returns indices of the vertices of the some found maximum independent set;
    worst case time cost O(3^n/3)}
    function  MaxIndependentSet: TIntArray;
  { lists all found maximal cliques }
    procedure ListAllCliques(aOnFindSet: TOnFindSet);
  { returns indices of the vertices of the some found maximum clique; worst case time cost O(3^n/3) }
    function  MaxClique: TIntArray;
  { checks whether exists any articulation point that belong to the same connected component as aRoot }
    function  ContainsCutPoint(constref aRoot: TVertex): Boolean; inline;
    function  ContainsCutPointI(aRoot: SizeInt = 0): Boolean;
  { returns the articulation points that belong to the same connection component as aRoot, if any,
    otherwise the empty vector }
    function  FindCutPoints(constref aRoot: TVertex): TIntArray; inline;
    function  FindCutPointsI(aRoot: SizeInt = 0): TIntArray;
  { removes the articulation points that belong to the same connected component as aRoot,
    adding, if necessary, new edges; returns count of added edges }
    function  RemoveCutPoints(constref aRoot: TVertex; aOnAddEdge: TOnAddEdge): SizeInt; inline;
    function  RemoveCutPointsI(aRoot: SizeInt; aOnAddEdge: TOnAddEdge): SizeInt;
  { checks whether exists any bridge in graph }
    function  ContainsBridge: Boolean;
  { returns all bridges in the result vector, if any, otherwise the empty vector }
    function  FindBridges: TIntEdgeArray;
  { returns count of biconnected components in the same connected component as aRoot;
    the corresponding elements of the aComponents will contain  the edges of this bicomponent }
    function  FindBicomponents(constref aRoot: TVertex; out aComponents: TEdgeArrayVector): SizeInt;
    function  FindBicomponentsI(aRoot: SizeInt; out aComponents: TEdgeArrayVector): SizeInt;
  { checks whether the graph is biconnected; graph with single vertex is considered biconnected }
    function  IsBiconnected: Boolean; inline;
  { makes graph biconnected, adding, if necessary, new edges; returns count of added edges }
    function  EnsureBiconnected(aOnAddEdge: TOnAddEdge): SizeInt;
  { returns the spanning tree, which is constructed starting from aRoot;
    each element contains the index of its parent (or -1 if it is root or not connected),
    i.e. provides a pair of source - destination(Result[index] - source, index - destination) }
    function  DfsSpanningTree(constref aRoot: TVertex): TIntArray; inline;
    function  DfsSpanningTreeI(aRoot: SizeInt = 0): TIntArray;
    function  BfsSpanningTree(constref aRoot: TVertex): TIntArray; inline;
    function  BfsSpanningTreeI(aRoot: SizeInt = 0): TIntArray;
  { returns graph of connected component that contains aVertex }
    function  SeparateGraph(constref aVertex: TVertex): TGSimpleGraph; inline;
    function  SeparateGraphI(aIndex: SizeInt): TGSimpleGraph;
  { returns a graph consisting of vertices whose indices are contained in the array aList }
    function  SubgraphFromVertexList(constref aList: TIntArray): TGSimpleGraph;
  { returns a graph constructed from the pairs provided by the aPairs,
    i.e. each element treates as pair of source - destination(value -> source, index -> destination ) }
    function  SubgraphFromPairs(constref aPairs: TIntArray): TGSimpleGraph;
  { returns a graph constructed from the edges provided by the aEdges }
    function  SubgraphFromEdges(constref aEdges: TIntEdgeArray): TGSimpleGraph;
  { returns copy of the source graph }
    function  Clone: TGSimpleGraph;
  { returns adjacency matrix of the complement graph;
    warning: maximal matrix size limited, see MaxBitMatrixSize }
    function  Complement: TAdjacencyMatrix;

  { checks whether the cached info about connected is up-to-date }
    property  ConnectedValid: Boolean read FConnectedValid;
  { checks whether the graph is connected; an empty graph is considered disconnected }
    property  Connected: Boolean read GetConnected;
  { count of connected components }
    property  SeparateCount: SizeInt read GetSeparateCount;
  end;

  { TGChart: simple outline;
      functor TEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGChart<TVertex, TEqRel> = class(specialize TGSimpleGraph<TVertex, TEmptyRec, TEqRel>)
  protected
    procedure WriteData(aStream: TStream; constref aValue: TEmptyRec);
    procedure ReadData(aStream: TStream; out aValue: TEmptyRec);
  public
    function  AddEdge(constref aSrc, aDst: TVertex): Boolean;
    function  AddEdgeI(aSrc, aDst: SizeInt): Boolean;
    function  EnsureConnected: SizeInt;
    function  RemoveCutPoints(constref aRoot: TVertex): SizeInt;
    function  RemoveCutPointsI(aRoot: SizeInt): SizeInt;
    function  EnsureBiconnected: SizeInt;
    procedure SaveToStream(aStream: TStream; aOnWriteVertex: TOnWriteVertex);
    procedure LoadFromStream(aStream: TStream; aOnReadVertex: TOnReadVertex);
    procedure SaveToFile(const aFileName: string; aOnWriteVertex: TOnWriteVertex);
    procedure LoadFromFile(const aFileName: string; aOnReadVertex: TOnReadVertex);
    function  SeparateGraph(constref aVertex: TVertex): TGChart;
    function  SeparateGraphI(aIndex: SizeInt): TGChart;
    function  SubgraphFromVertexList(constref aList: TIntArray): TGChart;
    function  SubgraphFromPairs(constref aPairs: TIntArray): TGChart;
    function  SubgraphFromEdges(constref aEdges: TIntEdgeArray): TGChart;
    function  Clone: TGChart;
  end;

  { TIntChart }

  TIntChart = class(specialize TGChart<SizeInt, SizeInt>)
  protected
    procedure WriteVertex(aStream: TStream; constref aValue: SizeInt);
    procedure ReadVertex(aStream: TStream; out aValue: SizeInt);
  public
    procedure SaveToStream(aStream: TStream);
    procedure LoadFromStream(aStream: TStream);
    procedure SaveToFile(const aFileName: string);
    procedure LoadFromFile(const aFileName: string);
    function  SeparateGraph(aVertex: SizeInt): TIntChart;
    function  SeparateGraphI(aIndex: SizeInt): TIntChart;
    function  SubgraphFromVertexList(constref aList: TIntArray): TIntChart;
    function  SubgraphFromPairs(constref aValue: TIntArray): TIntChart;
    function  SubgraphFromEdges(constref aEdges: TIntEdgeArray): TIntChart;
    function  Clone: TIntChart;
  end;

  { TStrChart
    warning: SaveToStream limitation for max string length = High(SmallInt) }
  TStrChart = class(specialize TGChart<string, string>)
  protected
    procedure WriteVertex(aStream: TStream; constref aValue: string);
    procedure ReadVertex(aStream: TStream; out aValue: string);
  public
    procedure SaveToStream(aStream: TStream);
    procedure LoadFromStream(aStream: TStream);
    procedure SaveToFile(const aFileName: string);
    procedure LoadFromFile(const aFileName: string);
    function  SeparateGraph(const aVertex: string): TStrChart;
    function  SeparateGraphI(aIndex: SizeInt): TStrChart;
    function  SubgraphFromVertexList(constref aList: TIntArray): TStrChart;
    function  SubgraphFromPairs(constref aPairs: TIntArray): TStrChart;
    function  SubgraphFromEdges(constref aEdges: TIntEdgeArray): TStrChart;
    function  Clone: TStrChart;
  end;

  { TGWeightedGraph implements simple sparse undirected weighed graph based on adjacency lists;

      functor TEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean;

      TEdgeData must provide field/property/function Weight: TWeight;

      TWeight must have defined comparision operators and properties MinValue, MaxValue,
      which used as infinity weight values;
      Default(TWeight) used as zero weight value }
  generic TGWeightedGraph<TVertex, TWeight, TEdgeData, TEqRel> = class(
    specialize TGSimpleGraph<TVertex, TEdgeData, TEqRel>)
  private
  type
    THelper = specialize TGWeightedHelper<TVertex, TWeight, TEdgeData, TEqRel>;

  public
  type
    TWeightArray = THelper.TWeightArray;
    TEstimate    = THelper.TEstimate;

  protected
  type
    TWeightEdge  = THelper.TWeightEdge;
    TPairingHeap = THelper.TPairingHeap;
    TWeightItem  = THelper.TWeightItem;
    TEdgeArray   = array of TWeightEdge;
    TEdgeHelper  = specialize TGComparableArrayHelper<TWeightEdge>;

    function  CreateEdgeArray: TEdgeArray;
  public
    class function InfiniteWeight: TWeight; static; inline;
    class function NegInfiniteWeight: TWeight; static; inline;
    class function ZeroWeight: TWeight; static; inline;
  { returns True if exists edge with negative weight }
    function  ContainsNegWeighedEdge: Boolean;
  { finds all paths of minimal weight from a given vertex to the remaining vertices in the same
    connected component(SSSP), the weights of all edges must be nonnegative;
    the result contains in the corresponding component the weight of the path to the vertex or
    InfiniteWeight if the vertex is unreachable; used Dijkstra's algorithm  }
    function  MinPathsMap(constref aSrc: TVertex): TWeightArray; inline;
    function  MinPathsMapI(aSrc: SizeInt): TWeightArray;
  { same as above and in aPathTree returns paths }
    function  MinPathsMap(constref aSrc: TVertex; out aPathTree: TIntArray): TWeightArray; inline;
    function  MinPathsMapI(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
  { finds the path of minimal weight from a aSrc to aDst if it exists(pathfinding);
    the weights of all edges must be nonnegative;
    returns weight of the path or InfiniteWeight if the vertex is unreachable; used Dijkstra's algorithm  }
    function  MinPathWeight(constref aSrc, aDst: TVertex): TWeight; inline;
    function  MinPathWeightI(aSrc, aDst: SizeInt): TWeight;
  { returns the vertex path of minimal weight from a aSrc to aDst, if exists, and its weight in aWeight }
    function  MinPath(constref aSrc, aDst: TVertex; out aWeight: TWeight): TIntArray; inline;
    function  MinPathI(aSrc, aDst: SizeInt; out aWeight: TWeight): TIntArray;
  { finds the path of minimal weight from a aSrc to aDst if it exists;
    the weights of all edges must be nonnegative; used A* algorithm if aHeur <> nil }
    function  MinPathAStar(constref aSrc, aDst: TVertex; out aWeight: TWeight; aHeur: TEstimate): TIntArray; inline;
    function  MinPathAStarI(aSrc, aDst: SizeInt; out aWeight: TWeight; aHeur: TEstimate): TIntArray;
  { finds a spanning tree(or spanning forest if not connected) of minimal weight, Kruskal's algorithm used }
    function  MinSpanningTreeKrus(out aTotalWeight: TWeight): TIntArray;
  { finds a spanning tree(or spanning forest if not connected) of minimal weight, Prim's algorithm used }
    function  MinSpanningTreePrim(out aTotalWeight: TWeight): TIntArray;
    function  SeparateGraph(constref aVertex: TVertex): TGWeightedGraph;
    function  SeparateGraphI(aIndex: SizeInt): TGWeightedGraph;
    function  SubgraphFromVertexList(constref aList: TIntArray): TGWeightedGraph;
    function  SubgraphFromPairs(constref aPairs: TIntArray): TGWeightedGraph;
    function  SubgraphFromEdges(constref aEdges: TIntEdgeArray): TGWeightedGraph;
    function  Clone: TGWeightedGraph;
  end;

  TRealPointEdge = record
    Weight: ValReal;
    constructor Create(const aWeight: ValReal);
  end;

  { TPointsChart }

  TPointsChart = class(specialize TGWeightedGraph<TPoint, ValReal, TRealPointEdge, TPoint>)
  protected
    procedure OnAddEdge(constref aSrc, aDst: TPoint; aData: Pointer);
    procedure WritePoint(aStream: TStream; constref aValue: TPoint);
    procedure WriteWeight(aStream: TStream; constref aValue: TRealPointEdge);
    procedure ReadPoint(aStream: TStream; out aValue: TPoint);
    procedure ReadWeight(aStream: TStream; out aValue: TRealPointEdge);
  public
    class function Distance(constref aSrc, aDst: TPoint): ValReal; static;
    function  AddEdge(constref aSrc, aDst: TPoint): Boolean;
    function  AddEdgeI(aSrc, aDst: SizeInt): Boolean;
    function  EnsureConnected: SizeInt;
    function  RemoveCutPoints(constref aRoot: TPoint): SizeInt;
    function  RemoveCutPointsI(aRoot: SizeInt): SizeInt;
    function  EnsureBiconnected: SizeInt;
    procedure SaveToStream(aStream: TStream);
    procedure LoadFromStream(aStream: TStream);
    procedure SaveToFile(const aFileName: string);
    procedure LoadFromFile(const aFileName: string);
    function  SeparateGraph(aVertex: TPoint): TPointsChart;
    function  SeparateGraphI(aIndex: SizeInt): TPointsChart;
    function  SubgraphFromVertexList(constref aList: TIntArray): TPointsChart;
    function  SubgraphFromPairs(constref aPairs: TIntArray): TPointsChart;
    function  SubgraphFromEdges(constref aEdges: TIntEdgeArray): TPointsChart;
    function  Clone: TPointsChart;
    function  MinPathAStar(constref aSrc, aDst: TPoint; out aWeight: ValReal; aHeur: TEstimate = nil): TIntArray; inline;
    function  MinPathAStarI(aSrc, aDst: SizeInt; out aWeight: ValReal; aHeur: TEstimate = nil): TIntArray;
  end;

implementation
{$B-}{$COPERATORS ON}

uses
  bufstream;

{ TGSimpleGraph.TSortByNebDegrees }

function TGSimpleGraph.TSortByNebDegrees.Cmp(constref L, R: SizeInt): SizeInt;
begin
  if FGraph.DegreeI(L) < FGraph.DegreeI(R) then
    exit(-1)
  else
     if FGraph.DegreeI(L) > FGraph.DegreeI(R) then
       exit(1);
  if FNebDegrees[L] < FNebDegrees[R] then
    exit(-1)
  else
    if FNebDegrees[L] > FNebDegrees[R] then
      exit(1);
  if L < R then
    exit(-1);
  Result := 1;
end;

procedure TGSimpleGraph.TSortByNebDegrees.Sort(var a: TIntArray; constref aDegrees: TIntArray; g: TGSimpleGraph;
  o: TSortOrder);
begin
  FGraph := g;
  FNebDegrees := aDegrees;
  TIntDegreeHelper.Sort(a, @Cmp, o);
end;

{ TGSimpleGraph.TCliqueHelper }

procedure TGSimpleGraph.TCliqueHelper.GetColors(constref aCand: TBoolVector; var aColOrd, aColors: TIntArray);
var
  P, Q: TBoolVector;
  I, J, ColorClass: SizeInt;
begin
  P := aCand;
  ColorClass := 0;
  I := 0;
  while P.NonEmpty do
    begin
      Inc(ColorClass);
      Q := P;
      while Q.NonEmpty do
        begin
          J := Q.Bsf;
          P[J] := False;
          Q[J] := False;
          Q.Subtract(FMatrix[J]);
          aColOrd[I] := J;
          aColors[I] := ColorClass;
          Inc(I);
        end;
    end;
end;

procedure TGSimpleGraph.TCliqueHelper.Extend(var aCand: TBoolVector);
var
  NewCand: TBoolVector;
  ColOrd, Colors: TIntArray;
  I, J, Size: SizeInt;
begin
  Size := aCand.PopCount;
  If Size > 0 then
    begin
      System.SetLength(ColOrd, Size);
      System.SetLength(Colors, Size);
      GetColors(aCand, ColOrd, Colors);
      for I := Pred(Size) downto 0 do
        begin
          if Colors[I] + FAccum.PopCount <= FCurrSize then
            exit;
          J := ColOrd[I];
          aCand[J] := False;
          FAccum[FVertices[J]] := True;
          NewCand := aCand;
          NewCand.Intersect(FMatrix[J]);
          if NewCand.IsEmpty then  // found clique
            begin
              Size := FAccum.PopCount;
              if Size > FCurrSize then
                begin
                  FCurrSize := Size;
                  FResult := FAccum.ToArray;
                end;
            end
          else
            Extend(NewCand);
          FAccum[FVertices[J]] := False;
        end;
    end;
end;

procedure TGSimpleGraph.TCliqueHelper.Extend(var aCand, aTested: TBoolVector);
var
  NewCand, NewTested: TBoolVector;
  ColOrd, Colors: TIntArray;
  I, J, Size: SizeInt;
begin
  Size := aCand.PopCount;
  If Size > 0 then
    begin
      System.SetLength(ColOrd, Size);
      System.SetLength(Colors, Size);
      GetColors(aCand, ColOrd, Colors);
      for I := Pred(Size) downto 0 do
        begin
          for J in aTested do
            if FMatrix[J].ContainsAll(aCand) then
              exit;
          J := ColOrd[I];
          aCand[J] := False;
          FAccum[FVertices[J]] := True;
          NewCand := aCand;
          NewTested := aTested;
          NewCand.Intersect(FMatrix[J]);
          NewTested.Intersect(FMatrix[J]);
          if NewCand.IsEmpty and NewTested.IsEmpty then  // found clique
            FOnFind(FAccum.ToArray)
          else
            Extend(NewCand, NewTested);
          FAccum[FVertices[J]] := False;
          aTested[J] := True;
        end;
    end;
end;

function TGSimpleGraph.TCliqueHelper.MaxClique(aGraph: TGSimpleGraph): TIntArray;
var
  Cand: TBoolVector;
begin
  aGraph.FillSortedMatrix(FMatrix, FVertices, soDesc);
  Cand.InitRange(aGraph.VertexCount);
  FAccum.Size := aGraph.VertexCount;
  FCurrSize := 0;
  Extend(Cand);
  Result := FResult;
end;

procedure TGSimpleGraph.TCliqueHelper.ListCliques(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
var
  Cand, Tested: TBoolVector;
begin
  aGraph.FillSortedMatrix(FMatrix, FVertices);
  Cand.InitRange(aGraph.VertexCount);
  {%H-}Tested.Size := aGraph.VertexCount;
  FAccum.Size := aGraph.VertexCount;
  FOnFind := aOnFind;
  Extend(Cand, Tested);
end;

{ TGSimpleGraph.TListIsHelper }

procedure TGSimpleGraph.TListIsHelper.Extend(var aCand, aTested: TBoolVector);
var
  NewCand,
  NewTested: TBoolVector;
  I: SizeInt;
begin
  while aCand.NonEmpty do  //Bron-Kerbosch algorithm
    begin
      for I in aTested do
        if not FMatrix[I].Intersecting(aCand) then
          exit;
      I := aCand.Bsf;
      FAccum[I] := True;
      NewCand := aCand;
      NewCand[I] := False;
      NewTested := aTested;
      NewCand.Subtract(FMatrix[I]);
      NewTested.Subtract(FMatrix[I]);
      if NewCand.IsEmpty and NewTested.IsEmpty then // found IS
        FOnFindSet(FAccum.ToArray)
      else
        if NewCand.NonEmpty then
          Extend(NewCand, NewTested);
      FAccum[I] := False;
      aCand[I] := False;
      aTested[I] := True;
    end;
end;

procedure TGSimpleGraph.TListIsHelper.ListIS(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
var
  Cand, Tested: TBoolVector;
begin
  FMatrix := aGraph.CreateBoolMatrix;
  FOnFindSet := aOnFind;
  Cand.InitRange(aGraph.VertexCount);
  Tested.Size := aGraph.VertexCount;
  FAccum.Size := aGraph.VertexCount;
  Extend(Cand, Tested);
end;

{ TGSimpleGraph.TMaxIsHelper }

procedure TGSimpleGraph.TMaxIsHelper.Extend(var aCand: TBoolVector);
var
  NewCand: TBoolVector;
  I: SizeInt;
begin
  while aCand.NonEmpty do
    begin
      if aCand.PopCount + FAccum.PopCount <= FCurrSize then
        exit;
      I := aCand.Bsf;
      FAccum[I] := True;
      NewCand := aCand;
      NewCand[I] := False;
      NewCand.Subtract(FMatrix[I]);
      if NewCand.IsEmpty then // found IS
        begin
          if FAccum.PopCount > FCurrSize then
            begin
              FCurrSize := FAccum.PopCount;
              FResult := FAccum.ToArray;
            end;
        end
      else
        Extend(NewCand);
      FAccum[I] := False;
      aCand[I] := False;
    end;
end;

function TGSimpleGraph.TMaxIsHelper.MaxIS(aGraph: TGSimpleGraph): TIntArray;
var
  Cand: TBoolVector;
begin
  FMatrix := aGraph.CreateBoolMatrix;
  Cand.InitRange(aGraph.VertexCount);
  FAccum.Size := aGraph.VertexCount;
  FCurrSize := 0;
  Extend(Cand);
  Result := FResult;
end;

{ TGSimpleGraph.TBits256.TEnumerator }

function TGSimpleGraph.TBits256.TEnumerator.GetCurrent: SizeInt;
begin
  Result := FLimbIndex shl INT_SIZE_LOG + FBitIndex;
end;

function TGSimpleGraph.TBits256.TEnumerator.FindFirst: Boolean;
var
  I: SizeInt;
begin
  I := FValue^.Bsf;
  if I >= 0 then
    begin
      FLimbIndex := I shr INT_SIZE_LOG;
      FBitIndex := I and INT_SIZE_MASK;
      FCurrLimb := FValue^.FBits[FLimbIndex];
      TBits256.ClearBit(FBitIndex, FCurrLimb);
      Result := True;
    end
  else
    begin
      FLimbIndex := LIMB_COUNT;
      FBitIndex := BIT_PER_LIMB;
      Result := False;
    end;
end;

function TGSimpleGraph.TBits256.TEnumerator.MoveNext: Boolean;
begin
  if FInCycle then
    repeat
      FBitIndex := TBits256.BsfValue(FCurrLimb);
      Result := FBitIndex >= 0;
      if Result then
        TBits256.ClearBit(FBitIndex, FCurrLimb)
      else
        begin
          if FLimbIndex >= Pred(LIMB_COUNT) then
            exit(False);
          Inc(FLimbIndex);
          FCurrLimb := FValue^.FBits[FLimbIndex];
        end;
    until Result
  else
    begin
      Result := FindFirst;
      FInCycle := True;
    end;
end;

{ TGSimpleGraph.TBits256.TReverseEnumerator }

function TGSimpleGraph.TBits256.TReverseEnumerator.GetCurrent: SizeInt;
begin
  Result := FLimbIndex shl INT_SIZE_LOG + FBitIndex;
end;

function TGSimpleGraph.TBits256.TReverseEnumerator.FindFirst: Boolean;
var
  I: SizeInt;
begin
  I := FValue^.Bsr;
  if I >= 0 then
    begin
      FLimbIndex := I shr INT_SIZE_LOG;
      FBitIndex := I and INT_SIZE_MASK;
      FCurrLimb := FValue^.FBits[FLimbIndex];
      TBits256.ClearBit(FBitIndex, FCurrLimb);
      Result := True;
    end
  else
    begin
      FLimbIndex := -1;
      FBitIndex := BIT_PER_LIMB;
      Result := False;
    end;
end;

function TGSimpleGraph.TBits256.TReverseEnumerator.MoveNext: Boolean;
begin
  if FInCycle then
    repeat
      FBitIndex := TBits256.BsrValue(FCurrLimb);
      Result := FBitIndex >= 0;
      if Result then
        TBits256.ClearBit(FBitIndex, FCurrLimb)
      else
        begin
          if FLimbIndex <= 0 then
            exit(False);
          Dec(FLimbIndex);
          FCurrLimb := FValue^.FBits[FLimbIndex];
        end;
    until Result
  else
    begin
      Result := FindFirst;
      FInCycle := True;
    end;
end;

{ TGSimpleGraph.TBits256.TReverseOrder }

function TGSimpleGraph.TBits256.TReverseOrder.GetEnumerator: TReverseEnumerator;
begin
  Result.FValue := FValue;
  Result.FInCycle := False;
end;

{ TGSimpleGraph.TBits256 }

function TGSimpleGraph.TBits256.GetBit(aIndex: SizeInt): Boolean;
begin
  Result := (FBits[aIndex shr INT_SIZE_LOG] and (SizeUInt(1) shl (aIndex and INT_SIZE_MASK))) <> 0;
end;

procedure TGSimpleGraph.TBits256.SetBit(aIndex: SizeInt; aValue: Boolean);
begin
  if aValue then
    FBits[aIndex shr INT_SIZE_LOG] :=
      FBits[aIndex shr INT_SIZE_LOG] or (SizeUInt(1) shl (aIndex and INT_SIZE_MASK))
  else
    FBits[aIndex shr INT_SIZE_LOG] :=
      FBits[aIndex shr INT_SIZE_LOG] and not (SizeUInt(1) shl (aIndex and INT_SIZE_MASK));
end;

class function TGSimpleGraph.TBits256.BsfValue(aValue: SizeUInt): SizeInt;
begin
{$IF DEFINED(CPU64)}
  Result := ShortInt(BsfQWord(aValue));
{$ELSEIF DEFINED(CPU32)}
  Result := ShortInt(BsfDWord(aValue));
{$ELSE}
  Result := ShortInt(BsfWord(aValue));
{$ENDIF}
end;

class function TGSimpleGraph.TBits256.BsrValue(aValue: SizeUInt): SizeInt;
begin
{$IF DEFINED(CPU64)}
  Result := ShortInt(BsrQWord(aValue));
{$ELSEIF DEFINED(CPU32)}
  Result := ShortInt(BsrDWord(aValue));
{$ELSE}
  Result := ShortInt(BsrWord(aValue));
{$ENDIF}
end;

class procedure TGSimpleGraph.TBits256.ClearBit(aIndex: SizeInt; var aValue: SizeUInt);
begin
  aValue := aValue and not (SizeUInt(1) shl aIndex);
end;

function TGSimpleGraph.TBits256.GetEnumerator: TEnumerator;
begin
  Result.FValue := @Self;
  Result.FInCycle := False;
end;

function TGSimpleGraph.TBits256.ReverseOrder: TReverseOrder;
begin
  Result.FValue := @Self;
end;

procedure TGSimpleGraph.TBits256.InitRange(aRange: SizeInt);
var
  msb: SizeInt;
begin
  InitZero;
  if aRange > 0 then
    begin
      msb := aRange and INT_SIZE_MASK;
      aRange := aRange shr INT_SIZE_LOG  + Ord(msb <> 0);
      System.FillChar(FBits[0], aRange * SizeOf(SizeUInt), $ff);
      if msb <> 0 then
        FBits[Pred(aRange)] := FBits[Pred(aRange)] shr (BitsizeOf(SizeUint) - msb);
    end;
end;

procedure TGSimpleGraph.TBits256.InitZero;
begin
{$IF DEFINED(CPU64)}
  System.FillQWord(FBits[0], LIMB_COUNT, 0);
{$ELSEIF DEFINED(CPU32)}
  System.FillDWord(FBits[0], LIMB_COUNT, 0);
{$ELSE}
  System.FillWord(FBits[0], LIMB_COUNT, 0);
{$ENDIF}
end;

function TGSimpleGraph.TBits256.ToArray: TIntArray;
var
  I, Pos: SizeInt;
begin
  System.SetLength(Result, PopCount);
  Pos := 0;
  for I in Self do
    begin
      Result[Pos] := I;
      Inc(Pos);
    end;
end;

function TGSimpleGraph.TBits256.IsEmpty: Boolean;
var
  I: SizeUInt;
begin
  for I in FBits do
    if I <> 0 then
      exit(False);
  Result := True;
end;

function TGSimpleGraph.TBits256.NonEmpty: Boolean;
begin
  Result := not IsEmpty;
end;

function TGSimpleGraph.TBits256.Bsf: SizeInt;
var
  I: SizeInt;
begin
  for I := 0 to Pred(LIMB_COUNT) do
    if FBits[I] <> 0 then
      exit(I shl INT_SIZE_LOG + BsfValue(FBits[I]));
  Result := -1;
end;

function TGSimpleGraph.TBits256.Bsr: SizeInt;
var
  I: SizeInt;
begin
  for I := Pred(LIMB_COUNT) downto 0 do
    if FBits[I] <> 0 then
      exit(I shl INT_SIZE_LOG + BsrValue(FBits[I]));
  Result := -1;
end;

function TGSimpleGraph.TBits256.Intersecting(constref aVector: TBits256): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to Pred(LIMB_COUNT) do
    if FBits[I] and aVector.FBits[I] <> 0 then
      exit(True);
  Result := False;
end;

function TGSimpleGraph.TBits256.ContainsAll(constref aVector: TBits256): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to Pred(LIMB_COUNT) do
    if FBits[I] and aVector.FBits[I] <> aVector.FBits[I] then
      exit(False);
  Result := True;
end;

procedure TGSimpleGraph.TBits256.Subtract(constref aVector: TBits256);
{$IF DEFINED(CPU64)}
begin
  FBits[0] := FBits[0] and not aVector.FBits[0];
  FBits[1] := FBits[1] and not aVector.FBits[1];
  FBits[2] := FBits[2] and not aVector.FBits[2];
  FBits[3] := FBits[3] and not aVector.FBits[3];
{$ELSEIF DEFINED(CPU32)}
begin
  FBits[0] := FBits[0] and not aVector.FBits[0];
  FBits[1] := FBits[1] and not aVector.FBits[1];
  FBits[2] := FBits[2] and not aVector.FBits[2];
  FBits[3] := FBits[3] and not aVector.FBits[3];
  FBits[4] := FBits[4] and not aVector.FBits[4];
  FBits[5] := FBits[5] and not aVector.FBits[5];
  FBits[6] := FBits[6] and not aVector.FBits[6];
  FBits[7] := FBits[7] and not aVector.FBits[7];
{$ELSE }
var
  I: SizeInt;
begin
  for I := 0 to Pred(LIMB_COUNT) do
    FBits[I] := FBits[I] and not aVector.FBits[I];
{$ENDIF }
end;

procedure TGSimpleGraph.TBits256.Intersect(constref aVector: TBits256);
{$IF DEFINED(CPU64)}
begin
  FBits[0] := FBits[0] and aVector.FBits[0];
  FBits[1] := FBits[1] and aVector.FBits[1];
  FBits[2] := FBits[2] and aVector.FBits[2];
  FBits[3] := FBits[3] and aVector.FBits[3];
{$ELSEIF DEFINED(CPU32)}
begin
  FBits[0] := FBits[0] and aVector.FBits[0];
  FBits[1] := FBits[1] and aVector.FBits[1];
  FBits[2] := FBits[2] and aVector.FBits[2];
  FBits[3] := FBits[3] and aVector.FBits[3];
  FBits[4] := FBits[4] and aVector.FBits[4];
  FBits[5] := FBits[5] and aVector.FBits[5];
  FBits[6] := FBits[6] and aVector.FBits[6];
  FBits[7] := FBits[7] and aVector.FBits[7];
{$ELSE }
var
  I: SizeInt;
begin
  for I := 0 to Pred(LIMB_COUNT) do
    FBits[I] := FBits[I] and aVector.FBits[I];
{$ENDIF }
end;

function TGSimpleGraph.TBits256.PopCount: SizeInt;
{$IF DEFINED(CPU64)}
begin
  Result := SizeInt(PopCnt(FBits[0]));
  Result += SizeInt(PopCnt(FBits[1]));
  Result += SizeInt(PopCnt(FBits[2]));
  Result += SizeInt(PopCnt(FBits[3]));
{$ELSEIF DEFINED(CPU32)}
begin
  Result := SizeInt(PopCnt(FBits[0]));
  Result += SizeInt(PopCnt(FBits[1]));
  Result += SizeInt(PopCnt(FBits[2]));
  Result += SizeInt(PopCnt(FBits[3]));
  Result += SizeInt(PopCnt(FBits[4]));
  Result += SizeInt(PopCnt(FBits[5]));
  Result += SizeInt(PopCnt(FBits[6]));
  Result += SizeInt(PopCnt(FBits[7]));
{$ELSE }
var
  I: SizeUInt;
begin
  Result := 0;
  for I in FBits do
    Result += SizeInt(PopCnt(I));
{$ENDIF }
end;

{ TGSimpleGraph.TCliqueHelper256 }

procedure TGSimpleGraph.TCliqueHelper256.GetColors(constref aCand: TBits256; var aColOrd, aColors: TIntArray);
var
  P, Q: TBits256;
  I, J, ColorClass: SizeInt;
begin
  P := aCand;
  ColorClass := 0;
  I := 0;
  while P.NonEmpty do
    begin
      Inc(ColorClass);
      Q := P;
      while Q.NonEmpty do
        begin
          J := Q.Bsf;
          P[J] := False;
          Q[J] := False;
          Q.Subtract(FMatrix[J]);
          aColOrd[I] := J;
          aColors[I] := ColorClass;
          Inc(I);
        end;
    end;
end;

procedure TGSimpleGraph.TCliqueHelper256.Extend(var aCand: TBits256);
var
  NewCand: TBits256;
  ColOrd, Colors: TIntArray;
  I, J, Size: SizeInt;
begin
  Size := aCand.PopCount;
  If Size > 0 then
    begin
      System.SetLength(ColOrd, Size);
      System.SetLength(Colors, Size);
      GetColors(aCand, ColOrd, Colors);
      for I := Pred(Size) downto 0 do
        begin
          if Colors[I] + FAccum.PopCount <= FCurrSize then
            exit;
          J := ColOrd[I];
          aCand[J] := False;
          FAccum[FVertices[J]] := True;
          NewCand := aCand;
          NewCand.Intersect(FMatrix[J]);
          if NewCand.IsEmpty then  // found clique
            begin
              Size := FAccum.PopCount;
              if Size > FCurrSize then
                begin
                  FCurrSize := Size;
                  FResult := FAccum;
                end;
            end
          else
            Extend(NewCand);
          FAccum[FVertices[J]] := False;
        end;
    end;
end;

procedure TGSimpleGraph.TCliqueHelper256.Extend(var aCand, aTested: TBits256);
var
  NewCand, NewTested: TBits256;
  ColOrd, Colors: TIntArray;
  I, J, Size: SizeInt;
begin
  Size := aCand.PopCount;
  If Size > 0 then
    begin
      System.SetLength(ColOrd, Size);
      System.SetLength(Colors, Size);
      GetColors(aCand, ColOrd, Colors);
      for I := Pred(Size) downto 0 do
        begin
          for J in aTested do
            if FMatrix[J].ContainsAll(aCand) then
              exit;
          J := ColOrd[I];
          aCand[J] := False;
          FAccum[FVertices[J]] := True;
          NewCand := aCand;
          NewTested := aTested;
          NewCand.Intersect(FMatrix[J]);
          NewTested.Intersect(FMatrix[J]);
          if NewCand.IsEmpty and NewTested.IsEmpty then  // found clique
            FOnFindSet(FAccum.ToArray)
          else
            Extend(NewCand, NewTested);
          FAccum[FVertices[J]] := False;
          aTested[J] := True;
        end;
    end;
end;

function TGSimpleGraph.TCliqueHelper256.MaxClique(aGraph: TGSimpleGraph): TIntArray;
var
  Cand: TBits256;
begin
  aGraph.FillSortedMatrix256(FMatrix, FVertices, soDesc);
  Cand.InitRange(aGraph.VertexCount);
  FCurrSize := 0;
  FAccum.InitZero;
  Extend(Cand);
  Result := FResult.ToArray;
end;

procedure TGSimpleGraph.TCliqueHelper256.ListCliques(aGraph: TGSimpleGraph; aOnFindSet: TOnFindSet);
var
  Cand, Tested: TBits256;
begin
  aGraph.FillSortedMatrix256(FMatrix, FVertices);
  Cand.InitRange(aGraph.VertexCount);
  {%H-}Tested.InitZero;
  FAccum.InitZero;
  FOnFindSet := aOnFindSet;
  Extend(Cand, Tested);
end;

{ TGSimpleGraph.TStaticListIsHelper }

procedure TGSimpleGraph.TStaticListIsHelper.Extend(var aCand, aTested: TBits256);
var
  NewCand,
  NewTested: TBits256;
  I: SizeInt;
begin
  while aCand.NonEmpty do //Bron-Kerbosch algorithm
    begin
      for I := 0 to Pred(aTested.BITNESS) do
        if aTested[I] and FMatrix[I].ContainsAll(aCand) then
          exit;
      I := aCand.Bsf;
      FAccum[I] := True;
      NewCand := aCand;
      NewCand[I] := False;
      NewTested := aTested;
      NewCand.Subtract(FMatrix[I]);
      NewTested.Subtract(FMatrix[I]);
      if NewCand.IsEmpty and NewTested.IsEmpty then // found IS
        FOnFindSet(FAccum.ToArray)
      else
        if NewCand.NonEmpty then
          Extend(NewCand, NewTested);
      FAccum[I] := False;
      aCand[I] := False;
      aTested[I] := True;
    end;
end;

procedure TGSimpleGraph.TStaticListIsHelper.ListIS(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
var
  Cand, Tested: TBits256;
begin
  FMatrix := aGraph.CreateBits256Matrix;
  FOnFindSet := aOnFind;
  Cand.InitRange(aGraph.VertexCount);
  {%H-}Tested.InitZero;
  FAccum.InitZero;
  Extend(Cand, Tested);
end;

{ TGSimpleGraph.TStaticMaxIsHelper }

procedure TGSimpleGraph.TStaticMaxIsHelper.Extend(var aCand: TBits256);
var
  NewCand: TBits256;
  I: SizeInt;
begin
  for I in aCand do
    begin
      if aCand.PopCount + FAccum.PopCount <= FCurrSize then
        exit;
      aCand[I] := False;
      FAccum[I] := True;
      NewCand := aCand;
      NewCand[I] := False;
      NewCand.Subtract(FMatrix[I]);
      if NewCand.IsEmpty then // found IS
        begin
          if FAccum.PopCount > FCurrSize then
            begin
              FCurrSize := FAccum.PopCount;
              FResult := FAccum.ToArray;
            end;
        end
      else
         Extend(NewCand);
      FAccum[I] := False;
    end;
end;

function TGSimpleGraph.TStaticMaxIsHelper.MaxIS(aGraph: TGSimpleGraph): TIntArray;
var
  Cand: TBits256;
begin
  FMatrix := aGraph.CreateBits256Matrix;
  Cand.InitRange(aGraph.VertexCount);
  FAccum.InitZero;
  FCurrSize := 0;
  Extend(Cand);
  Result := FResult;
end;

{ TGSimpleGraph.TDistinctEdgeEnumerator }

function TGSimpleGraph.TDistinctEdgeEnumerator.GetCurrent: TEdge;
begin
  Result := TEdge.Create(FCurrIndex, FEnum.Current);
end;

function TGSimpleGraph.TDistinctEdgeEnumerator.MoveNext: Boolean;
begin
  repeat
    if FEnumDone then
      begin
        if FCurrIndex >= FLastIndex then
          exit(False);
        Inc(FCurrIndex);
        FEnum := FList[FCurrIndex].AdjList.GetEnumerator;
      end;
    Result := FEnum.MoveNext;
    FEnumDone := not Result;
    if Result then
      Result := FEnum.Current^.Destination > FCurrIndex;
  until Result;
end;

procedure TGSimpleGraph.TDistinctEdgeEnumerator.Reset;
begin
  FCurrIndex := -1;
  FEnumDone := True;
end;

{ TGSimpleGraph.TDistinctEdges }

function TGSimpleGraph.TDistinctEdges.GetEnumerator: TDistinctEdgeEnumerator;
begin
  Result.FList := Pointer(FGraph.FNodeList);
  Result.FLastIndex := Pred(FGraph.VertexCount);
  Result.FCurrIndex := -1;
  Result.FEnumDone := True;
end;

{ TGSimpleGraph }

procedure TGSimpleGraph.ResetTags;
var
  I: SizeInt;
begin
  for I := 0 to Pred(VertexCount) do
    FNodeList[I].Tag := I;
end;

function TGSimpleGraph.SeparateTag(aIndex: SizeInt): SizeInt;
begin
  if FNodeList[aIndex].Tag = aIndex then
    exit(aIndex);
  Result := SeparateTag(FNodeList[aIndex].Tag);
  FNodeList[aIndex].Tag := Result;
end;

function TGSimpleGraph.SeparateMerged(L, R: SizeInt): Boolean;
begin
  L := SeparateTag(L);
  R := SeparateTag(R);
  if L = R then
    exit(False);
  if NextRandomBoolean then
    FNodeList[L].Tag := R
  else
    FNodeList[R].Tag := L;
  Result := True;
end;

procedure TGSimpleGraph.ValidateConnected;
var
  Visited: TBitVector;
  Queue: TIntQueue;
  I, Curr, Next: SizeInt;
begin
  if ConnectedValid then
    exit;
  if IsEmpty then
    begin
      FCompCount := 0;
      FConnectedValid := True;
      exit;
    end;
  Visited.Size := VertexCount;
  FCompCount := VertexCount;
  ResetTags;
  for I := 0 to Pred(VertexCount) do
    if not Visited[I] then
      begin
        Curr := I;
        Visited[Curr] := True;
        repeat
          for Next in AdjVerticesI(Curr) do
            if not Visited[Next] then
              begin
                Visited[Next] := True;
                Queue.Enqueue(Next);
                if SeparateMerged(Curr, Next) then
                  Dec(FCompCount);
              end;
        until not Queue.TryDequeue(Curr);
      end;
  FConnected := FCompCount = 1;
  FConnectedValid := True;
end;

function TGSimpleGraph.GetConnected: Boolean;
begin
  Result := SeparateCount = 1;
end;

procedure TGSimpleGraph.DoRemoveVertex(aIndex: SizeInt);
var
  CurrEdges: TAdjList.TAdjItemArray;
  I, J: SizeInt;
begin
  FEdgeCount -= FNodeList[aIndex].AdjList.Count;
  Delete(aIndex);
  FConnectedValid := False;
  for I := 0 to Pred(VertexCount) do
    begin
      CurrEdges := FNodeList[I].AdjList.ToArray;
      FNodeList[I].AdjList.MakeEmpty;
      for J := 0 to System.High(CurrEdges) do
        begin
          if CurrEdges[J].Destination <> aIndex then
            begin
              if CurrEdges[J].Destination > aIndex then
                Dec(CurrEdges[J].Destination);
              FNodeList[I].AdjList.Add(CurrEdges[J]);
            end;
        end;
    end;
end;

function TGSimpleGraph.DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
begin
  if aSrc = aDst then
    exit(False);
  Result := FNodeList[aSrc].AdjList.Add(TAdjItem.Create(aDst, aData));
  if Result then
    begin
      if FNodeList[aDst].AdjList.Add(TAdjItem.Create(aSrc, aData)) then
        begin
          Inc(FEdgeCount);
          if ConnectedValid and SeparateMerged(aSrc, aDst) then
            begin
              Dec(FCompCount);
              FConnected := FCompCount = 1;
            end;
        end
      else
        raise ELGraphError.Create(SEGrapInconsist);
    end;
end;

function TGSimpleGraph.DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
begin
  if aSrc = aDst then
    exit(False);
  Result := FNodeList[aSrc].AdjList.Remove(aDst);
  if Result then
    begin
      FNodeList[aDst].AdjList.Remove(aSrc);
      Dec(FEdgeCount);
      FConnectedValid := False;
    end;
end;

function TGSimpleGraph.CreateSkeleton: TSkeleton;
var
  I: SizeInt;
begin
  Result := TSkeleton.Create(VertexCount);
  Result.FEdgeCount := EdgeCount;
  for I := 0 to Pred(VertexCount) do
    FNodeList[I].AdjList.CopyTo(Result[I]);
end;

function TGSimpleGraph.CreateBoolMatrix: TBoolMatrix;
var
  I: SizeInt;
  p: PAdjItem;
begin
  System.SetLength(Result, VertexCount);
  for I := 0 to Pred(VertexCount) do
    begin
      Result[I].Size := VertexCount;
      for p in AdjLists[I]^ do
        Result[I].Bits[p^.Key] := True;
    end;
end;

function TGSimpleGraph.CreateBits256Matrix: TBits256Matrix;
var
  I: SizeInt;
  p: PAdjItem;
begin
  System.SetLength(Result, VertexCount);
  for I := 0 to Pred(VertexCount) do
    begin
      Result[I].InitZero;
      for p in AdjLists[I]^ do
        Result[I].Bits[p^.Key] := True;
    end;
end;

function TGSimpleGraph.GetSeparateGraph(aIndex: SizeInt): TGSimpleGraph;
var
  cIdx, I: SizeInt;
  p: PAdjItem;
begin
  Result := TGSimpleGraph.Create;
  cIdx := SeparateTag(aIndex);
  for I := 0 to Pred(VertexCount) do
    if SeparateTag(I) = cIdx then
      for p in FNodeList[I].AdjList do
        Result.AddEdge(Items[I], Items[p^.Destination], p^.Data);
end;

function TGSimpleGraph.GetSeparateCount: SizeInt;
begin
  if not ConnectedValid then
    ValidateConnected;
  Result := FCompCount;
end;

function TGSimpleGraph.CountPop(aTag: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I := 0 to Pred(VertexCount) do
    Result += Ord(SeparateTag(I) = aTag);
end;

function TGSimpleGraph.GetEccentricity(aIndex: SizeInt): SizeInt;
var
  Queue: TIntQueue;
  Dist: TIntArray;
  Next, d: SizeInt;
begin
  Dist := CreateIntArray;
  Dist[aIndex] := 0;
  Result := 0;
  repeat
    for Next in AdjVerticesI(aIndex) do
      if Dist[Next] = -1 then
        begin
          Queue.Enqueue(Next);
          d := Succ(Dist[aIndex]);
          if Result < d then
            Result := d;
          Dist[Next] := d;
        end;
  until not Queue.TryDequeue(aIndex);
end;

function TGSimpleGraph.MakeConnected(aOnAddEdge: TOnAddEdge): SizeInt;
var
  I: SizeInt;
  d: TEdgeData;
begin
  Result := 0;
  d := DefaultEdgeData;
  for I := 1 to Pred(VertexCount) do
    if SeparateTag(0) <> SeparateTag(I) then
      begin
        if Assigned(aOnAddEdge) then
          aOnAddEdge(FNodeList[0].Vertex, FNodeList[I].Vertex, @d);
        AddEdgeI(0, I, d);
        Inc(Result);
      end;
end;

function TGSimpleGraph.CycleExists(aRoot: SizeInt; out aCycle: TIntArray): Boolean;
var
  Stack: TIntStack;
  Visited: TBitVector;
  AdjEnums: TAdjEnumArray;
  Parents: TIntArray;
  Next: SizeInt;
begin
  Visited.Size := VertexCount;
  AdjEnums := CreateAdjEnumArray;
  Parents := CreateIntArray;
  Visited[aRoot] := True;
  {%H-}Stack.Push(aRoot);
  while Stack.TryPeek(aRoot) do
    if AdjEnums[aRoot].MoveNext then
      begin
        Next := AdjEnums[aRoot].Current;
        if not Visited[Next] then
          begin
            Visited[Next] := True;
            Parents[Next] := aRoot;
            Stack.Push(Next);
          end
        else
          if Parents[aRoot] <> Next then
            begin
              aCycle := TreeToCycle(Parents, Next, aRoot);
              exit(True);
            end;
      end
    else
      Stack.Pop;
  Result := False;
end;

function TGSimpleGraph.GetMaxClique: TIntArray;
var
  Helper: TCliqueHelper;
begin
  Result := Helper.MaxClique(Self);
end;

function TGSimpleGraph.GetMaxCliqueStatic: TIntArray;
var
  Helper: TCliqueHelper256;
begin
  //Helper.FVertexOrder := CreateIntArrayRange;
  //TIntDegreeHelper.Sort(Helper.FVertexOrder, @CmpVertexDegree);
  Result := Helper.MaxClique(Self);
end;

procedure TGSimpleGraph.ListCliques(aOnFindSet: TOnFindSet);
var
  Helper: TCliqueHelper;
begin
  Helper.ListCliques(Self, aOnFindSet);
end;

procedure TGSimpleGraph.ListCliquesStatic(aOnFindSet: TOnFindSet);
var
  Helper: TCliqueHelper256;
begin
  Helper.ListCliques(Self, aOnFindSet);
end;

function TGSimpleGraph.GetMaxIS: TIntArray;
var
  Helper: TMaxIsHelper;
begin
  Result := Helper.MaxIS(Self);
end;

function TGSimpleGraph.GetMaxISStatic: TIntArray;
var
  Helper: TStaticMaxIsHelper;
begin
  Result := Helper.MaxIS(Self);
end;

procedure TGSimpleGraph.ListIS(aOnFindSet: TOnFindSet);
var
  Helper: TListIsHelper;
begin
  Helper.ListIS(Self, aOnFindSet);
end;

procedure TGSimpleGraph.ListISStatic(aOnFindSet: TOnFindSet);
var
  Helper: TStaticListIsHelper;
begin
  Helper.ListIS(Self, aOnFindSet);
end;

procedure TGSimpleGraph.SearchForCutPoints(aRoot: SizeInt; var aPoints: TIntVector);
var
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  LowPt, PreOrd, Parents: TIntArray;
  Counter, Curr, Next, ChildCount: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  LowPt := CreateIntArray;
  PreOrd := CreateIntArray;
  Parents := CreateIntArray;
  PreOrd[aRoot] := 0;
  LowPt[aRoot] := 0;
  {%H-}Stack.Push(aRoot);
  Counter := 1;
  ChildCount := 0;
  while Stack.TryPeek(Curr) do
    if AdjEnums[{%H-}Curr].MoveNext then
      begin
        Next := AdjEnums[Curr].Current;
        if Next <> Parents[Curr] then
          if PreOrd[Next] = -1 then
            begin
              Parents[Next] := Curr;
              PreOrd[Next] := Counter;
              LowPt[Next] := Counter;
              Inc(Counter);
              Inc(ChildCount, Ord(Curr = aRoot));
              Stack.Push(Next);
            end
          else
            if LowPt[Curr] > PreOrd[Next] then
              LowPt[Curr] := PreOrd[Next];
      end
    else
      begin
        Stack.Pop;
        Next := Curr;
        Curr := Parents[Curr];
        if LowPt[Curr] > LowPt[Next] then
          LowPt[Curr] := LowPt[Next];
        if (LowPt[Next] >= PreOrd[Curr]) and (Curr <> aRoot) then
          aPoints.Add(Curr);
      end;
  if ChildCount > 1 then
    aPoints.Add(aRoot);
end;

function TGSimpleGraph.CutPointExists(aRoot: SizeInt): Boolean;
var
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  LowPt, PreOrd, Parents: TIntArray;
  Counter, Curr, Next, ChildCount: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  LowPt := CreateIntArray;
  PreOrd := CreateIntArray;
  Parents := CreateIntArray;
  PreOrd[aRoot] := 0;
  LowPt[aRoot] := 0;
  {%H-}Stack.Push(aRoot);
  Counter := 1;
  ChildCount := 0;
  while Stack.TryPeek(Curr) do
    if AdjEnums[{%H-}Curr].MoveNext then
      begin
        Next := AdjEnums[Curr].Current;
        if Next <> Parents[Curr] then
          if PreOrd[Next] = -1 then
            begin
              Parents[Next] := Curr;
              PreOrd[Next] := Counter;
              LowPt[Next] := Counter;
              Inc(Counter);
              Inc(ChildCount, Ord(Curr = aRoot));
              Stack.Push(Next);
            end
          else
            if LowPt[Curr] > PreOrd[Next] then
              LowPt[Curr] := PreOrd[Next];
      end
    else
      begin
        Stack.Pop;
        Next := Curr;
        Curr := Parents[Curr];
        if LowPt[Curr] > LowPt[Next] then
          LowPt[Curr] := LowPt[Next];
        if (LowPt[Next] >= PreOrd[Curr]) and (Curr <> aRoot) then
          exit(True);
      end;
  Result := ChildCount > 1;
end;

procedure TGSimpleGraph.SearchForBiconnect(aRoot: SizeInt; var aEdges: TIntEdgeVector);
var
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  LowPt, PreOrd, Parents, Across: TIntArray;
  Counter, Curr, Next: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  LowPt := CreateIntArray;
  PreOrd := CreateIntArray;
  Parents := CreateIntArray;
  Across := CreateIntArray;
  PreOrd[aRoot] := 0;
  LowPt[aRoot] := 0;
  {%H-}Stack.Push(aRoot);
  Counter := 1;
  while Stack.TryPeek(Curr) do
    if AdjEnums[{%H-}Curr].MoveNext then
      begin
        Next := AdjEnums[Curr].Current;
        if Next <> Parents[Curr] then
          if PreOrd[Next] = -1 then
            begin
              if Across[Curr] = -1 then
                Across[Curr] := Next;
              Parents[Next] := Curr;
              PreOrd[Next] := Counter;
              LowPt[Next] := Counter;
              Inc(Counter);
              Stack.Push(Next);
            end
          else
            if LowPt[Curr] > PreOrd[Next] then
              LowPt[Curr] := PreOrd[Next];
      end
    else
      begin
        Stack.Pop;
        Next := Curr;
        Curr := Parents[Curr];
        if LowPt[Curr] > LowPt[Next] then
          LowPt[Curr] := LowPt[Next];
        if LowPt[Next] >= PreOrd[Curr] then
          begin
            if Next = Across[Curr] then
              begin
                if Curr <> aRoot then
                  aEdges.Add(TIntEdge.Create(Parents[Curr], Next));
              end
            else
              aEdges.Add(TIntEdge.Create(Across[Curr], Next));
          end;
      end;
end;

procedure TGSimpleGraph.SearchForBicomponent(aRoot: SizeInt; var aComp: TEdgeArrayVector);
var
  Stack: TIntStack;
  EdgeStack: TIntEdgeVector;
  AdjEnums: TAdjEnumArray;
  LowPt, PreOrd, Parents: TIntArray;
  Counter, Curr, Next, ChildCount, I: SizeInt;
  e: TIntEdge;
begin
  AdjEnums := CreateAdjEnumArray;
  LowPt := CreateIntArray;
  PreOrd := CreateIntArray;
  Parents := CreateIntArray;
  PreOrd[aRoot] := 0;
  LowPt[aRoot] := 0;
  {%H-}Stack.Push(aRoot);
  Counter := 1;
  ChildCount := 0;
  while Stack.TryPeek(Curr) do
    if AdjEnums[{%H-}Curr].MoveNext then
      begin
        Next := AdjEnums[Curr].Current;
        if Next <> Parents[Curr] then
          if PreOrd[Next] = -1 then
            begin
              Parents[Next] := Curr;
              PreOrd[Next] := Counter;
              LowPt[Next] := Counter;
              Inc(Counter);
              Inc(ChildCount, Ord(Curr = aRoot));
              Stack.Push(Next);
              EdgeStack.Add(TIntEdge.Create(Curr, Next));
            end
          else
            if LowPt[Curr] > PreOrd[Next] then
              begin
                LowPt[Curr] := PreOrd[Next];
                EdgeStack.Add(TIntEdge.Create(Curr, Next));
              end;
      end
    else
      begin
        Stack.Pop;
        Next := Curr;
        Curr := Parents[Curr];
        if LowPt[Curr] > LowPt[Next] then
          LowPt[Curr] := LowPt[Next];
        if (LowPt[Next] >= PreOrd[Curr]) and (Curr <> aRoot) then
          begin
            I := EdgeStack.Count;
            repeat
              Dec(I);
              e := EdgeStack[I];
            until (e.Source = Curr) or (e.Destination = Next);
            aComp.Add(EdgeStack.ExtractAll(I, EdgeStack.Count - I));
          end;
      end;
  if ChildCount > 1 then
    begin
      I := EdgeStack.Count;
      repeat
        Dec(I);
        e := EdgeStack[I];
      until e.Source = aRoot;
      aComp.Add(EdgeStack.ExtractAll(I, EdgeStack.Count - I));
    end;
  if EdgeStack.NonEmpty then
    aComp.Add(EdgeStack.ToArray);
end;

function TGSimpleGraph.BridgeExists: Boolean;
var
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  LowPt, PreOrd, Parents: TIntArray;
  Counter, Curr, Next, I: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  LowPt := CreateIntArray;
  PreOrd := CreateIntArray;
  Parents := CreateIntArray;
  Counter := 0;
  for I := 0 to Pred(VertexCount) do
    if PreOrd[I] = -1 then
      begin
        PreOrd[I] := Counter;
        LowPt[I] := Counter;
        Inc(Counter);
        {%H-}Stack.Push(I);
        while Stack.TryPeek(Curr) do
          if AdjEnums[{%H-}Curr].MoveNext then
            begin
              Next := AdjEnums[Curr].Current;
              if Next <> Parents[Curr] then
                if PreOrd[Next] = -1 then
                  begin
                    Parents[Next] := Curr;
                    PreOrd[Next] := Counter;
                    LowPt[Next] := Counter;
                    Inc(Counter);
                    Stack.Push(Next);
                  end
                else
                  if LowPt[Curr] > PreOrd[Next] then
                    LowPt[Curr] := PreOrd[Next];
            end
          else
            begin
              Stack.Pop;
              Next := Curr;
              Curr := Parents[Curr];
              if LowPt[Curr] > LowPt[Next] then
                LowPt[Curr] := LowPt[Next];
              if LowPt[Next] > PreOrd[Curr] then
                exit(True);
            end;
      end;
  Result := False;
end;

procedure TGSimpleGraph.SearchForBridges(var aBridges: TIntEdgeVector);
var
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  LowPt, PreOrd, Parents: TIntArray;
  Counter, Curr, Next, I: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  LowPt := CreateIntArray;
  PreOrd := CreateIntArray;
  Parents := CreateIntArray;
  Counter := 0;
  for I := 0 to Pred(VertexCount) do
    if PreOrd[I] = -1 then
      begin
        PreOrd[I] := Counter;
        LowPt[I] := Counter;
        Inc(Counter);
        {%H-}Stack.Push(I);
        while Stack.TryPeek(Curr) do
          if AdjEnums[{%H-}Curr].MoveNext then
            begin
              Next := AdjEnums[Curr].Current;
              if Next <> Parents[Curr] then
                if PreOrd[Next] = -1 then
                  begin
                    Parents[Next] := Curr;
                    PreOrd[Next] := Counter;
                    LowPt[Next] := Counter;
                    Inc(Counter);
                    Stack.Push(Next);
                  end
                else
                  if LowPt[Curr] > PreOrd[Next] then
                    LowPt[Curr] := PreOrd[Next];
            end
          else
            begin
              Stack.Pop;
              Next := Curr;
              Curr := Parents[Curr];
              if LowPt[Curr] > LowPt[Next] then
                LowPt[Curr] := LowPt[Next];
              if LowPt[Next] > PreOrd[Curr] then
                aBridges.Add(TIntEdge.Create(Curr, Next));
            end;
      end;
end;

procedure TGSimpleGraph.SearchForFundamentalsCycles(out aCycles: TIntArrayVector);
var
  Stack: TIntStack;
  Visited: TBitVector;
  AdjEnums: TAdjEnumArray;
  Parents: TIntArray;
  EdgeSet: TIntPairSet;
  I, Curr, Next: SizeInt;
begin
  Visited.Size := VertexCount;
  AdjEnums := CreateAdjEnumArray;
  Parents := CreateIntArray;
  for I := 0 to Pred(VertexCount) do
    if not Visited[I] then
      begin
        Visited[I] := True;
        {%H-}Stack.Push(I);
        while Stack.TryPeek(Curr) do
          if AdjEnums[{%H-}Curr].MoveNext then
            begin
              Next := AdjEnums[Curr].Current;
              if not Visited[Next] then
                begin
                  Visited[Next] := True;
                  Parents[Next] := Curr;
                  Stack.Push(Next);
                end
              else
                if (Parents[Curr] <> Next) and EdgeSet.Add(Curr, Next) then
                  aCycles.Add(TreeToCycle(Parents, Next, Curr));
            end
          else
            Stack.Pop;
      end;
end;

procedure TGSimpleGraph.SearchForFundamentalsCyclesLen(out aCycleLens: TIntVector);
var
  Stack: TIntStack;
  Visited: TBitVector;
  AdjEnums: TAdjEnumArray;
  Parents: TIntArray;
  EdgeSet: TIntPairSet;
  I, Curr, Next: SizeInt;
begin
  Visited.Size := VertexCount;
  AdjEnums := CreateAdjEnumArray;
  Parents := CreateIntArray;
  for I := 0 to Pred(VertexCount) do
    if not Visited[I] then
      begin
        Visited[I] := True;
        {%H-}Stack.Push(I);
        while Stack.TryPeek(Curr) do
          if AdjEnums[{%H-}Curr].MoveNext then
            begin
              Next := AdjEnums[Curr].Current;
              if not Visited[Next] then
                begin
                  Visited[Next] := True;
                  Parents[Next] := Curr;
                  Stack.Push(Next);
                end
              else
                if (Parents[Curr] <> Next) and EdgeSet.Add(Curr, Next) then
                  aCycleLens.Add(Tree2CycleLen(Parents, Next, Curr));
            end
          else
            Stack.Pop;
      end;
end;

procedure TGSimpleGraph.FindFundamentalCyclesLen(out aCycleLens: TIntVector);
begin
  if IsTree then
    exit;
  SearchForFundamentalsCyclesLen(aCycleLens);
  if aCycleLens.Count <> CyclomaticNumber then
    raise ELGraphError.Create(SEGrapInconsist);
  TIntVectorHelper.Sort(aCycleLens);
end;

function TGSimpleGraph.CreateSortedByDegree(aOrder: TSortOrder): TIntArray;
begin
  Result := CreateIntArrayRange;
  TIntDegreeHelper.Sort(Result, @CmpVertexDegree, aOrder);
end;

procedure TGSimpleGraph.FillSortedMatrix(out aMatrix: TBoolMatrix; out aVertices: TIntArray; o: TSortOrder);
var
  NebDegrees: TIntArray;
  I, J, Sum: SizeInt;
  List: TIntSet;
  Stack: TIntStack;
  p: PAdjList;
begin
  NebDegrees := CreateIntArray;
  for I := 0 to Pred(VertexCount) do
    NebDegrees[I] := AdjLists[I]^.Count;
  List.InitRange(VertexCount);
  while List.NonEmpty do
    begin
      I := List[0];
      for J in List do
        if NebDegrees[J] < NebDegrees[I] then
          I := J;
      Stack.Push(I);
      List.Remove(I);
      for J in List do
        if AdjLists[J]^.Contains(I) then
          Dec(NebDegrees[J]);
    end;
  aVertices := CreateIntArrayRange;
  System.SetLength(aMatrix, VertexCount);
  for I := 0 to Pred(VertexCount) do
    begin
      aMatrix[I].Size := VertexCount;
      p := AdjLists[aVertices[I]];
      for J := 0 to Pred(VertexCount) do
        if (I <> J) and p^.Contains(aVertices[J]) then
          aMatrix[I][J] := True;
    end;
end;

procedure TGSimpleGraph.FillSortedMatrix256(out aMatrix: TBits256Matrix; out aVertices: TIntArray; o: TSortOrder);
var
  NebDegrees: TIntArray;
  I, J, Sum: SizeInt;
  Helper: TSortByNebDegrees;
  p: PAdjList;
begin
  NebDegrees := CreateIntArray;
  for I := 0 to Pred(VertexCount) do
    begin
      Sum := 0;
      for J in AdjVerticesI(I) do
        Sum += AdjLists[J]^.Count;
      NebDegrees[I] := Sum;
    end;
  aVertices := CreateIntArrayRange;
  Helper.Sort(aVertices, NebDegrees, Self, o);
  System.SetLength(aMatrix, VertexCount);
  for I := 0 to Pred(VertexCount) do
    begin
      aMatrix[I].InitZero;
      p := AdjLists[aVertices[I]];
      for J := 0 to Pred(VertexCount) do
        if (I <> J) and p^.Contains(aVertices[J]) then
          aMatrix[I][J] := True;
    end;
end;

function TGSimpleGraph.CreateDegreeArray: TIntArray;
var
  I: SizeInt;
begin
  System.SetLength(Result, VertexCount);
  for I := 0 to Pred(VertexCount) do
    Result[I] := AdjLists[I]^.Count;
end;

function TGSimpleGraph.CmpIntArrayLen(constref L, R: TIntArray): SizeInt;
begin
  if System.Length(L) > System.Length(R) then
    Result := 1
  else
    if System.Length(L) < System.Length(R) then
      Result := -1
    else
      Result := 0;
end;

function TGSimpleGraph.CmpVertexDegree(constref L, R: SizeInt): SizeInt;
begin
  Result := SizeInt.Compare(AdjLists[L]^.Count, AdjLists[R]^.Count);
end;

class function TGSimpleGraph.MayBeEqual(L, R: TGSimpleGraph): Boolean;
var
  fcL, fcR: TIntVector;
  I: SizeInt;
begin
  if L = R then
    exit(True);
  if L.IsEmpty then
    exit(L.IsEmpty)
  else
    if R.IsEmpty then
      exit(False);
  if L.VertexCount <> R.VertexCount then
    exit(False);
  if L.EdgeCount <> R.EdgeCount then
    exit(False);
  L.FindFundamentalCyclesLen(fcL);
  R.FindFundamentalCyclesLen(fcR);
  if fcL.Count <> fcR.Count then
    exit(False);
  for I := 0 to Pred(fcL.Count) do
    if fcL[I] <> fcR[I] then
      exit(False);
  Result := True;
end;

constructor TGSimpleGraph.Create;
begin
  inherited;
  FConnectedValid := True;
end;

constructor TGSimpleGraph.Create(aCapacity: SizeInt);
begin
  inherited Create(aCapacity);
  FConnectedValid := True;
end;

procedure TGSimpleGraph.Clear;
begin
  inherited;
  FCompCount := 0;
  FConnected := False;
  FConnectedValid := True;
end;

function TGSimpleGraph.AddVertex(constref aVertex: TVertex; out aIndex: SizeInt): Boolean;
begin
  Result := not FindOrAdd(aVertex, aIndex);
  if not Result then
    exit;
  if ConnectedValid then
    begin
      FNodeList[aIndex].Tag := FCompCount;
      Inc(FCompCount);
      FConnected := FCompCount = 1;
    end
  else
    FNodeList[aIndex].Tag := FCompCount;
end;

function TGSimpleGraph.AddVertex(constref aVertex: TVertex): Boolean;
var
  Dummy: SizeInt;
begin
  Result := AddVertex(aVertex, Dummy);
end;

procedure TGSimpleGraph.RemoveVertex(constref aVertex: TVertex);
begin
  RemoveVertexI(IndexOf(aVertex));
end;

procedure TGSimpleGraph.RemoveVertexI(aIndex: SizeInt);
begin
  CheckIndexRange(aIndex);
  DoRemoveVertex(aIndex);
end;

function TGSimpleGraph.AddEdge(constref aSrc, aDst: TVertex; aData: TEdgeData): Boolean;
var
  SrcIdx, DstIdx: SizeInt;
begin
  AddVertex(aSrc, SrcIdx);
  AddVertex(aDst, DstIdx);
  Result := DoAddEdge(SrcIdx, DstIdx, aData);
end;

function TGSimpleGraph.AddEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := AddEdge(aSrc, aDst, DefaultEdgeData);
end;

function TGSimpleGraph.AddEdgeI(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  Result := DoAddEdge(aSrc, aDst, aData);
end;

function TGSimpleGraph.AddEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := AddEdgeI(aSrc, aDst, DefaultEdgeData);
end;

function TGSimpleGraph.RemoveEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := RemoveEdgeI(IndexOf(aSrc), IndexOf(aDst));
end;

function TGSimpleGraph.RemoveEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  Result := DoRemoveEdge(aSrc, aDst);
end;

procedure TGSimpleGraph.SaveToStream(aStream: TStream; aWriteVertex: TOnWriteVertex; aWriteData: TOnWriteData);
var
  Header: TStreamHeader;
  I: SizeInt;
  Edge: TEdge;
  wbs: TWriteBufStream;
begin
  if not (Assigned(aWriteVertex) and Assigned(aWriteData)) then
    raise ELGraphError.Create(SEWriteCallbackMissed);
  wbs := TWriteBufStream.Create(aStream);
  try
    //write header
    Header.Magic := GRAPH_MAGIC;
    Header.Version := GRAPH_HEADER_VERSION;
    Header.TitleSize := System.Length(Title);
    Header.VertexCount := VertexCount;
    Header.EdgeCount := EdgeCount;
    wbs.WriteBuffer(Header, SizeOf(Header));
    //write title
    if Header.TitleSize > 0 then
      wbs.WriteBuffer(FTitle[1], Header.TitleSize);
    //write Items, but does not save any info about connected
    //this should allow transfer data between directed/undirected graphs ???
    for I := 0 to Pred(Header.VertexCount) do
      aWriteVertex(wbs, FNodeList[I].Vertex);
    //write edges
    for Edge in DistinctEdges do
      begin
        wbs.WriteBuffer(Edge.Source, SizeOf(Edge.Source));
        wbs.WriteBuffer(Edge.Destination, SizeOf(Edge.Destination));
        aWriteData(wbs, Edge.Data);
      end;
  finally
    wbs.Free;
  end;
end;

procedure TGSimpleGraph.LoadFromStream(aStream: TStream; aReadVertex: TOnReadVertex; aReadData: TOnReadData);
var
  Header: TStreamHeader;
  I, vInd: SizeInt;
  Edge: TEdge;
  Vertex: TVertex;
  rbs: TReadBufStream;
begin
  if not (Assigned(aReadVertex) and Assigned(aReadData)) then
    raise ELGraphError.Create(SEReadCallbackMissed);
  rbs := TReadBufStream.Create(aStream);
  try
    //read header
    rbs.ReadBuffer(Header, SizeOf(Header));
    if Header.Magic <> GRAPH_MAGIC then
      raise ELGraphError.Create(SEUnknownGraphStreamFmt);
    if Header.Version > GRAPH_HEADER_VERSION then
      raise ELGraphError.Create(SEUnsuppGraphFmtVersion);
    Clear;
    EnsureCapacity(Header.VertexCount);
    //read title
    System.SetLength(FTitle, Header.TitleSize);
    if Header.TitleSize > 0 then
      rbs.ReadBuffer(FTitle[1], Header.TitleSize);
    //read Items
    for I := 0 to Pred(Header.VertexCount) do
      begin
        aReadVertex(rbs, Vertex);
        if not AddVertex(Vertex, vInd) then
          raise ELGraphError.Create(SEGraphStreamCorrupt);
        if vInd <> I then
          raise ELGraphError.Create(SEGraphStreamReadIntern);
      end;
    //read edges
    for I := 0 to Pred(Header.EdgeCount) do
      begin
        rbs.ReadBuffer(Edge.Source, SizeOf(Edge.Source));
        rbs.ReadBuffer(Edge.Destination, SizeOf(Edge.Destination));
        aReadData(rbs, Edge.Data);
        AddEdgeI(Edge.Source, Edge.Destination, Edge.Data);
      end;
  finally
    rbs.Free;
  end;
end;

procedure TGSimpleGraph.SaveToFile(const aFileName: string; aWriteVertex: TOnWriteVertex;
  aWriteData: TOnWriteData);
var
  fs: TStream;
begin
  fs := TFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(fs, aWriteVertex, aWriteData);
  finally
    fs.Free;
  end;
end;

procedure TGSimpleGraph.LoadFromFile(const aFileName: string; aReadVertex: TOnReadVertex; aReadData: TOnReadData);
var
  fs: TStream;
begin
  fs := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fs, aReadVertex, aReadData);
  finally
    fs.Free;
  end;
end;

function TGSimpleGraph.Degree(constref aVertex: TVertex): SizeInt;
begin
  Result := DegreeI(IndexOf(aVertex));
end;

function TGSimpleGraph.DegreeI(aIndex: SizeInt): SizeInt;
begin
  CheckIndexRange(aIndex);
  Result := FNodeList[aIndex].AdjList.Count;
end;

function TGSimpleGraph.Isolated(constref aVertex: TVertex): Boolean;
begin
  Result := Degree(aVertex) = 0;
end;

function TGSimpleGraph.IsolatedI(aIndex: SizeInt): Boolean;
begin
  Result := DegreeI(aIndex) = 0;
end;

function TGSimpleGraph.DistinctEdges: TDistinctEdges;
begin
  Result.FGraph := Self;
end;

function TGSimpleGraph.Eccentricity(constref aVertex: TVertex): SizeInt;
begin
  Result := EccentricityI(IndexOf(aVertex));
end;

function TGSimpleGraph.EccentricityI(aIndex: SizeInt): SizeInt;
begin
  CheckIndexRange(aIndex);
  Result := GetEccentricity(aIndex);
end;

function TGSimpleGraph.LocalClustering(constref aVertex: TVertex): ValReal;
begin
  Result := LocalClusteringI(IndexOf(aVertex));
end;

function TGSimpleGraph.LocalClusteringI(aIndex: SizeInt): ValReal;
var
  I, J, Counter, d: SizeInt;
begin
  CheckIndexRange(aIndex);
  d := DegreeI(aIndex);
  if d <= 1 then
    exit(0.0);
  Counter := 0;
  for I in AdjVerticesI(aIndex) do
    for J in AdjVerticesI(aIndex) do
      Counter += Ord((I <> J) and FNodeList[I].AdjList.Contains(J));
  Result := ValReal(Counter) / ValReal(d * Pred(d));
end;

function TGSimpleGraph.EnsureConnected(aOnAddEdge: TOnAddEdge): SizeInt;
begin
  Result := 0;
  if VertexCount < 2 then
    exit;
  if SeparateCount < 2 then
    exit;
  Result := MakeConnected(aOnAddEdge);
end;

function TGSimpleGraph.PathExists(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := PathExistsI(IndexOf(aSrc), IndexOf(aDst));
end;

function TGSimpleGraph.PathExistsI(aSrc, aDst: SizeInt): Boolean;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  if aSrc = aDst then
    exit(True);
  if SeparateCount > 1 then
    Result := SeparateTag(aSrc) = SeparateTag(aDst)
  else
    Result := True;
end;

function TGSimpleGraph.SeparateIndexOf(constref aVertex: TVertex): SizeInt;
begin
   Result := SeparateIndexI(IndexOf(aVertex));
end;

function TGSimpleGraph.SeparateIndexI(aIndex: SizeInt): SizeInt;
begin
  CheckIndexRange(aIndex);
  if not ConnectedValid then
    ValidateConnected;
  Result := SeparateTag(aIndex);
end;

function TGSimpleGraph.SeparatePop(constref aVertex: TVertex): SizeInt;
begin
  Result := SeparatePopI(IndexOf(aVertex));
end;

function TGSimpleGraph.SeparatePopI(aIndex: SizeInt): SizeInt;
begin
  CheckIndexRange(aIndex);
  if SeparateCount > 1 then
    Result := CountPop(SeparateTag(aIndex))
  else
    Result := VertexCount;
end;

function TGSimpleGraph.IsTree: Boolean;
begin
  Result := (EdgeCount = Pred(VertexCount)) and Connected;
end;

function TGSimpleGraph.CyclomaticNumber: SizeInt;
begin
  Result := EdgeCount - VertexCount + SeparateCount;
end;

function TGSimpleGraph.IsRegular: Boolean;
var
  I, d: SizeInt;
begin
  if NonEmpty then
    begin
      d := DegreeI(0);
      for I := 1 to Pred(VertexCount) do
        if DegreeI(I) <> d then
          exit(False);
    end;
  Result := True;
end;

function TGSimpleGraph.ContainsCycle(constref aRoot: TVertex; out aCycle: TIntArray): Boolean;
begin
  Result := ContainsCycleI(IndexOf(aRoot), aCycle);
end;

function TGSimpleGraph.ContainsCycleI(aRoot: SizeInt; out aCycle: TIntArray): Boolean;
begin
  CheckIndexRange(aRoot);
  if VertexCount < 3 then
    exit(False);
  if ConnectedValid and IsTree then
    exit(False);
  Result := CycleExists(aRoot, aCycle);
end;

function TGSimpleGraph.ContainsEulerianCircuit: Boolean;
var
  I, d, cd: SizeInt;
begin
  if VertexCount < 3 then
    exit(False);
  d := 0;
  for I := 0 to Pred(VertexCount) do
    begin
      cd := DegreeI(I);
      if Odd(cd) then
        exit(False);
      d += cd;
    end;
  Result := d > 0;
end;

function TGSimpleGraph.FindEulerianCircuit(out aCircuit: TIntVector): Boolean;
var
  g: TSkeleton;
  Stack: TIntStack;
  s, d: SizeInt;
begin
  if not ContainsEulerianCircuit then
    exit(False);
  g := CreateSkeleton;
  s := 0;
  while g.Degree[s] = 0 do
    Inc(s);
  aCircuit.Add(s);
  repeat
    while g[s]^.FindFirst(d) do
      begin
        {%H-}Stack.Push(s);
        g.RemoveEdge(s, d);
        s := d;
      end;
    if not Stack.TryPop(s) then
      break;
    aCircuit.Add(s);
  until False;
  Result := aCircuit.Count > 0;
  if Result then
    TIntVectorHelper.Reverse(aCircuit);
end;

function TGSimpleGraph.FindFundamentalCycles(out aCycles: TIntArrayVector): Boolean;
begin
  if IsTree then
    exit(False);
  SearchForFundamentalsCycles(aCycles);
  if aCycles.Count <> CyclomaticNumber then
    raise ELGraphError.Create(SEGrapInconsist);
  TIntArrayVectorHelper.Sort(aCycles, @CmpIntArrayLen);
  Result := True;
end;

procedure TGSimpleGraph.ListIndependentSets(aOnFindSet: TOnFindSet);
begin
  if IsEmpty then
    exit;
  if aOnFindSet = nil then
    raise ELGraphError.Create(SECallbackMissed);
  if VertexCount > 256 then
    ListIS(aOnFindSet)
  else
    ListISStatic(aOnFindSet)
end;

function TGSimpleGraph.MaxIndependentSet: TIntArray;
begin
  if IsEmpty then
    exit(nil);
  if VertexCount > 256 then
    Result := GetMaxIS
  else
    Result := GetMaxISStatic;
end;

procedure TGSimpleGraph.ListAllCliques(aOnFindSet: TOnFindSet);
begin
  if IsEmpty then
    exit;
  if aOnFindSet = nil then
    raise ELGraphError.Create(SECallbackMissed);
  if VertexCount > 256 then
    ListCliques(aOnFindSet)
  else
    ListCliquesStatic(aOnFindSet);
end;

function TGSimpleGraph.MaxClique: TIntArray;
begin
  if IsEmpty then
    exit(nil);
  if VertexCount > 256 then
    Result := GetMaxClique
  else
    Result := GetMaxCliqueStatic;
end;

function TGSimpleGraph.ContainsCutPoint(constref aRoot: TVertex): Boolean;
begin
  Result := ContainsCutPointI(IndexOf(aRoot));
end;

function TGSimpleGraph.ContainsCutPointI(aRoot: SizeInt): Boolean;
begin
  CheckIndexRange(aRoot);
  if VertexCount < 3 then
    exit(False);
  Result := CutPointExists(aRoot);
end;

function TGSimpleGraph.FindCutPoints(constref aRoot: TVertex): TIntArray;
begin
  Result := FindCutPointsI(IndexOf(aRoot));
end;

function TGSimpleGraph.FindCutPointsI(aRoot: SizeInt): TIntArray;
var
  v: TIntVector;
begin
  CheckIndexRange(aRoot);
  if VertexCount > 2 then
    begin
      SearchForCutPoints(aRoot, v{%H-});
      Result := v.ToArray;
    end
  else
    Result := nil;
end;

function TGSimpleGraph.RemoveCutPoints(constref aRoot: TVertex; aOnAddEdge: TOnAddEdge): SizeInt;
begin
  Result := RemoveCutPointsI(IndexOf(aRoot), aOnAddEdge);
end;

function TGSimpleGraph.RemoveCutPointsI(aRoot: SizeInt; aOnAddEdge: TOnAddEdge): SizeInt;
var
  NewEdges: TIntEdgeVector;
  e: TIntEdge;
  d: TEdgeData;
begin
  Result := 0;
  if VertexCount < 3 then
    exit;
  Result += EnsureConnected(aOnAddEdge);
  SearchForBiconnect(aRoot, NewEdges{%H-});
  d := DefaultEdgeData;
  for e in NewEdges do
    begin
      if Assigned(aOnAddEdge) then
        aOnAddEdge(FNodeList[e.Source].Vertex, FNodeList[e.Destination].Vertex, @d);
      Result += Ord(AddEdgeI(e.Source, e.Destination, d));
    end;
end;

function TGSimpleGraph.ContainsBridge: Boolean;
begin
  if VertexCount > 1 then
    Result := BridgeExists
  else
    Result := False;
end;

function TGSimpleGraph.FindBridges: TIntEdgeArray;
var
  v: TIntEdgeVector;
begin
  if VertexCount > 1 then
    SearchForBridges(v{%H-});
  Result := v.ToArray;
end;

function TGSimpleGraph.FindBicomponents(constref aRoot: TVertex; out aComponents: TEdgeArrayVector): SizeInt;
begin
  Result := FindBicomponentsI(IndexOf(aRoot), aComponents);
end;

function TGSimpleGraph.FindBicomponentsI(aRoot: SizeInt; out aComponents: TEdgeArrayVector): SizeInt;
begin
  Result := 0;
  CheckIndexRange(aRoot);
  if VertexCount > 2 then
    begin
      SearchForBicomponent(aRoot, aComponents{%H-});
      Result := aComponents.Count;
    end
  else
    if (VertexCount = 2) and ContainsEdgeI(0, 1) then
      begin
        aComponents.Add([TIntEdge.Create(0, 1)]);
        Result := 1;
      end;
end;

function TGSimpleGraph.IsBiconnected: Boolean;
begin
  if Connected then
    Result := not ContainsCutPointI
  else
    Result := False;
end;

function TGSimpleGraph.EnsureBiconnected(aOnAddEdge: TOnAddEdge): SizeInt;
var
  NewEdges: TIntEdgeVector;
  e: TIntEdge;
  d: TEdgeData;
begin
  Result := EnsureConnected(aOnAddEdge);
  if VertexCount < 3 then
    exit;
  SearchForBiconnect(0, NewEdges{%H-});
  d := DefaultEdgeData;
  for e in NewEdges do
    begin
      if Assigned(aOnAddEdge) then
        aOnAddEdge(Items[e.Source], Items[e.Destination], @d);
      Result += Ord(AddEdgeI(e.Source, e.Destination, d));
    end;
end;

function TGSimpleGraph.DfsSpanningTree(constref aRoot: TVertex): TIntArray;
begin
  Result := DfsSpanningTreeI(IndexOf(aRoot));
end;

function TGSimpleGraph.DfsSpanningTreeI(aRoot: SizeInt): TIntArray;
var
  Visited: TBitVector;
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  Next: SizeInt;
begin
  CheckIndexRange(aRoot);
  Visited.Size := VertexCount;
  Result := CreateIntArray;
  AdjEnums := CreateAdjEnumArray;
  Visited[aRoot] := True;
  {%H-}Stack.Push(aRoot);
  while Stack.TryPeek(aRoot) do
    if AdjEnums[aRoot].MoveNext then
      begin
        Next := AdjEnums[aRoot].Current;
        if not Visited[Next] then
          begin
            Visited[Next] := True;
            Result[Next] := aRoot;
            Stack.Push(Next);
          end;
      end
    else
      Stack.Pop;
end;

function TGSimpleGraph.BfsSpanningTree(constref aRoot: TVertex): TIntArray;
begin
  Result := BfsSpanningTreeI(IndexOf(aRoot));
end;

function TGSimpleGraph.BfsSpanningTreeI(aRoot: SizeInt): TIntArray;
var
  Visited: TBitVector;
  Queue: TIntQueue;
  Next: SizeInt;
begin
  CheckIndexRange(aRoot);
  Visited.Size := VertexCount;
  {%H-}Queue.EnsureCapacity(VertexCount);
  Result := CreateIntArray;
  repeat
    for Next in AdjVerticesI(aRoot) do
      if not Visited[Next] then
        begin
          Visited[Next] := True;
          Result[Next] := aRoot;
          Queue.Enqueue(Next);
        end;
  until not Queue.TryDequeue(aRoot);
end;

function TGSimpleGraph.SeparateGraph(constref aVertex: TVertex): TGSimpleGraph;
begin
  Result := SeparateGraphI(IndexOf(aVertex));
end;

function TGSimpleGraph.SeparateGraphI(aIndex: SizeInt): TGSimpleGraph;
begin
  if SeparateCount > 1 then
    Result := GetSeparateGraph(aIndex)
  else
    Result := Clone;
end;

function TGSimpleGraph.SubgraphFromVertexList(constref aList: TIntArray): TGSimpleGraph;
var
  vSet: TIntHashSet;
  I, J: SizeInt;
begin
  vSet.AddAll(aList);
  Result := TGSimpleGraph.Create(vSet.Count);
  for I in vSet do
    for J in AdjVerticesI(I) do
      if vSet.Contains(J) then
        Result.AddEdge(Items[I], Items[J], GetEdgeDataPtr(I, J)^);
end;

function TGSimpleGraph.SubgraphFromPairs(constref aPairs: TIntArray): TGSimpleGraph;
var
  I, Src: SizeInt;
begin
  Result := TGSimpleGraph.Create;
  for I := 0 to Pred(System.Length(aPairs)) do
    begin
      Src := aPairs[I];
      if Src <> -1 then
        Result.AddEdge(Items[Src], Items[I], GetEdgeDataPtr(Src, I)^);
    end;
end;

function TGSimpleGraph.SubgraphFromEdges(constref aEdges: TIntEdgeArray): TGSimpleGraph;
var
  e: TIntEdge;
begin
  Result := TGSimpleGraph.Create;
  for e in aEdges do
    Result.AddEdge(Items[e.Source], Items[e.Destination], GetEdgeDataPtr(e.Source, e.Destination)^);
end;

function TGSimpleGraph.Clone: TGSimpleGraph;
var
  I: SizeInt;
begin
  Result := TGSimpleGraph.Create;
  Result.FCount := VertexCount;
  Result.FEdgeCount := EdgeCount;
  Result.FCompCount := FCompCount;
  Result.FTitle := Title;
  Result.FConnected := Connected;
  Result.FConnectedValid := ConnectedValid;
  if NonEmpty then
    begin
      Result.FChainList := System.Copy(FChainList);
      System.SetLength(Result.FNodeList, System.Length(FNodeList));
      for I := 0 to Pred(VertexCount) do
        Result.FNodeList[I].Assign(FNodeList[I]);
    end;
end;

function TGSimpleGraph.Complement: TAdjacencyMatrix;
var
  m: TSquareBitMatrix;
  s, d: SizeInt;
begin
  if IsEmpty then
    exit(Default(TAdjacencyMatrix));
  m := TSquareBitMatrix.Create(VertexCount);
  for s := 0 to Pred(VertexCount) do
    for d := 0 to Pred(VertexCount) do
      if (s <> d) and not FNodeList[s].AdjList.Contains(d) then
        m[s, d] := True;
  Result := TAdjacencyMatrix.Create(m);
end;

{ TGChart }

procedure TGChart.WriteData(aStream: TStream; constref aValue: TEmptyRec);
begin
//do nothing
end;

procedure TGChart.ReadData(aStream: TStream; out aValue: TEmptyRec);
begin
  //do nothing
end;

function TGChart.AddEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := inherited AddEdge(aSrc, aDst);
end;

function TGChart.AddEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := inherited AddEdgeI(aSrc, aDst);
end;

function TGChart.EnsureConnected: SizeInt;
begin
  Result := inherited EnsureConnected(nil);
end;

function TGChart.RemoveCutPoints(constref aRoot: TVertex): SizeInt;
begin
  Result := inherited RemoveCutPoints(aRoot, nil);
end;

function TGChart.RemoveCutPointsI(aRoot: SizeInt): SizeInt;
begin
  Result := inherited RemoveCutPointsI(aRoot, nil);
end;

function TGChart.EnsureBiconnected: SizeInt;
begin
  Result := inherited EnsureBiconnected(nil);
end;

procedure TGChart.SaveToStream(aStream: TStream; aOnWriteVertex: TOnWriteVertex);
begin
  inherited SaveToStream(aStream, aOnWriteVertex, @WriteData);
end;

procedure TGChart.LoadFromStream(aStream: TStream; aOnReadVertex: TOnReadVertex);
begin
  inherited LoadFromStream(aStream, aOnReadVertex, @ReadData);
end;

procedure TGChart.SaveToFile(const aFileName: string; aOnWriteVertex: TOnWriteVertex);
begin
  inherited SaveToFile(aFileName, aOnWriteVertex, @WriteData);
end;

procedure TGChart.LoadFromFile(const aFileName: string; aOnReadVertex: TOnReadVertex);
begin
  inherited LoadFromFile(aFileName, aOnReadVertex, @ReadData);
end;

function TGChart.SeparateGraph(constref aVertex: TVertex): TGChart;
begin
  Result := inherited SeparateGraph(aVertex) as TGChart;
end;

function TGChart.SeparateGraphI(aIndex: SizeInt): TGChart;
begin
  Result := inherited SeparateGraphI(aIndex) as TGChart;
end;

function TGChart.SubgraphFromVertexList(constref aList: TIntArray): TGChart;
begin
  Result := inherited SubgraphFromVertexList(aList) as TGChart;
end;

function TGChart.SubgraphFromPairs(constref aPairs: TIntArray): TGChart;
begin
  Result := inherited SubgraphFromPairs(aPairs) as TGChart;
end;

function TGChart.SubgraphFromEdges(constref aEdges: TIntEdgeArray): TGChart;
begin
  Result := inherited SubgraphFromEdges(aEdges) as TGChart;
end;

function TGChart.Clone: TGChart;
begin
  Result := inherited Clone as TGChart;
end;

{ TIntChart }

procedure TIntChart.WriteVertex(aStream: TStream; constref aValue: SizeInt);
begin
  aStream.WriteBuffer(aValue, SizeOf(Integer));
end;

procedure TIntChart.ReadVertex(aStream: TStream; out aValue: SizeInt);
var
  v: Integer;
begin
  aStream.ReadBuffer(v{%H-}, SizeOf(v));
  aValue := v;
end;

procedure TIntChart.SaveToStream(aStream: TStream);
begin
  inherited SaveToStream(aStream, @WriteVertex);
end;

procedure TIntChart.LoadFromStream(aStream: TStream);
begin
  inherited LoadFromStream(aStream, @ReadVertex);
end;

procedure TIntChart.SaveToFile(const aFileName: string);
begin
  inherited SaveToFile(aFileName, @WriteVertex);
end;

procedure TIntChart.LoadFromFile(const aFileName: string);
begin
  inherited LoadFromFile(aFileName, @ReadVertex);
end;

function TIntChart.SeparateGraph(aVertex: SizeInt): TIntChart;
begin
  Result := inherited SeparateGraph(aVertex) as TIntChart;
end;

function TIntChart.SeparateGraphI(aIndex: SizeInt): TIntChart;
begin
  Result := inherited SeparateGraphI(aIndex) as TIntChart;
end;

function TIntChart.SubgraphFromVertexList(constref aList: TIntArray): TIntChart;
begin
  Result := inherited SubgraphFromVertexList(aList) as TIntChart;
end;

function TIntChart.SubgraphFromPairs(constref aValue: TIntArray): TIntChart;
begin
  Result := inherited SubgraphFromPairs(aValue) as TIntChart;
end;

function TIntChart.SubgraphFromEdges(constref aEdges: TIntEdgeArray): TIntChart;
begin
  Result := inherited SubgraphFromEdges(aEdges) as TIntChart;
end;

function TIntChart.Clone: TIntChart;
begin
  Result := inherited Clone as TIntChart;
end;

{ TStrChart }

procedure TStrChart.WriteVertex(aStream: TStream; constref aValue: string);
var
  Len: SizeInt;
begin
  Len := System.Length(aValue);
  if Len > High(SmallInt) then
    raise ELGraphError.CreateFmt(SEStrLenExceedFmt, [Len]);
  aStream.WriteBuffer(Len, 2);
  if Len > 0 then
    aStream.WriteBuffer(aValue[1], Len);
end;

procedure TStrChart.ReadVertex(aStream: TStream; out aValue: string);
var
  Len: SmallInt;
begin
  aStream.ReadBuffer(Len{%H-}, SizeOf(Len));
  System.SetLength(aValue, Len);
  if Len > 0 then
    aStream.ReadBuffer(aValue[1], Len);
end;

procedure TStrChart.SaveToStream(aStream: TStream);
begin
  inherited SaveToStream(aStream, @WriteVertex);
end;

procedure TStrChart.LoadFromStream(aStream: TStream);
begin
  inherited LoadFromStream(aStream, @ReadVertex);
end;

procedure TStrChart.SaveToFile(const aFileName: string);
begin
  inherited SaveToFile(aFileName, @WriteVertex);
end;

procedure TStrChart.LoadFromFile(const aFileName: string);
begin
  inherited LoadFromFile(aFileName, @ReadVertex);
end;

function TStrChart.SeparateGraph(const aVertex: string): TStrChart;
begin
  Result := inherited SeparateGraph(aVertex) as TStrChart;
end;

function TStrChart.SeparateGraphI(aIndex: SizeInt): TStrChart;
begin
  Result := inherited SeparateGraphI(aIndex) as TStrChart;
end;

function TStrChart.SubgraphFromVertexList(constref aList: TIntArray): TStrChart;
begin
  Result := inherited SubgraphFromVertexList(aList) as TStrChart;
end;

function TStrChart.SubgraphFromPairs(constref aPairs: TIntArray): TStrChart;
begin
  Result := inherited SubgraphFromPairs(aPairs) as TStrChart;
end;

function TStrChart.SubgraphFromEdges(constref aEdges: TIntEdgeArray): TStrChart;
begin
  Result := inherited SubgraphFromEdges(aEdges) as TStrChart;
end;

function TStrChart.Clone: TStrChart;
begin
  Result := inherited Clone as TStrChart;
end;

{ TGWeightedGraph }

function TGWeightedGraph.CreateEdgeArray: TEdgeArray;
var
  I, J: SizeInt;
  p: PAdjItem;
begin
  System.SetLength(Result, EdgeCount);
  J := 0;
  for I := 0 to Pred(VertexCount) do
    for p in AdjLists[I]^ do
      if p^.Destination > I then
        begin
          Result[J] := TWeightEdge.Create(I, p^.Destination, p^.Data.Weight);
          Inc(J);
        end;
end;

class function TGWeightedGraph.InfiniteWeight: TWeight;
begin
  Result := THelper.InfiniteWeight;
end;

class function TGWeightedGraph.NegInfiniteWeight: TWeight;
begin
  Result := THelper.NegInfiniteWeight;
end;

class function TGWeightedGraph.ZeroWeight: TWeight;
begin
  Result := THelper.ZeroWeight;
end;

function TGWeightedGraph.ContainsNegWeighedEdge: Boolean;
var
  e: TEdge;
begin
  for e in DistinctEdges do
    if e.Data.Weight < ZeroWeight then
      exit(True);
  Result := False;
end;

function TGWeightedGraph.MinPathsMap(constref aSrc: TVertex): TWeightArray;
begin
  Result := MinPathsMapI(IndexOf(aSrc));
end;

function TGWeightedGraph.MinPathsMapI(aSrc: SizeInt): TWeightArray;
begin
  CheckIndexRange(aSrc);
  Result := THelper.DijkstraSssp(Self, aSrc);
end;

function TGWeightedGraph.MinPathsMap(constref aSrc: TVertex; out aPathTree: TIntArray): TWeightArray;
begin
  Result := MinPathsMapI(IndexOf(aSrc), aPathTree);
end;

function TGWeightedGraph.MinPathsMapI(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
begin
  CheckIndexRange(aSrc);
  Result := THelper.DijkstraSssp(Self, aSrc, aPathTree);
end;

function TGWeightedGraph.MinPathWeight(constref aSrc, aDst: TVertex): TWeight;
begin
  Result := MinPathWeightI(IndexOf(aSrc), IndexOf(aDst));
end;

function TGWeightedGraph.MinPathWeightI(aSrc, aDst: SizeInt): TWeight;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  Result := THelper.DijkstraPath(Self, aSrc, aDst);
end;

function TGWeightedGraph.MinPath(constref aSrc, aDst: TVertex; out aWeight: TWeight): TIntArray;
begin
  Result := MinPathI(IndexOf(aSrc), IndexOf(aDst), aWeight);
end;

function TGWeightedGraph.MinPathI(aSrc, aDst: SizeInt; out aWeight: TWeight): TIntArray;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  Result := THelper.DijkstraPath(Self, aSrc, aDst, aWeight);
end;

function TGWeightedGraph.MinPathAStar(constref aSrc, aDst: TVertex; out aWeight: TWeight;
  aHeur: TEstimate): TIntArray;
begin
  Result := MinPathAStarI(IndexOf(aSrc), IndexOf(aSrc), aWeight, aHeur);
end;

function TGWeightedGraph.MinPathAStarI(aSrc, aDst: SizeInt; out aWeight: TWeight; aHeur: TEstimate): TIntArray;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  if aHeur <> nil then
    Result := THelper.AStar(Self, aSrc, aDst, aWeight, aHeur)
  else
    Result := THelper.DijkstraPath(Self, aSrc, aDst, aWeight);
end;

function TGWeightedGraph.MinSpanningTreeKrus(out aTotalWeight: TWeight): TIntArray;
var
  e: TWeightEdge;
  LocEdges: TEdgeArray;
  Dsu: TDisjointSetUnion;
begin
  LocEdges := CreateEdgeArray;
  TEdgeHelper.Sort(LocEdges);
  Result := CreateIntArray;
  Dsu.Size := VertexCount;
  aTotalWeight := ZeroWeight;
  for e in LocEdges do
    if Dsu.Merged(e.Source, e.Destination)  then
      begin
        Result[e.Destination] := e.Source;
        aTotalWeight += e.Weight;
      end;
end;

function TGWeightedGraph.MinSpanningTreePrim(out aTotalWeight: TWeight): TIntArray;
var
  Visited: TBitVector;
  Queue: TPairingHeap;
  I, Curr: SizeInt;
  Item: TWeightItem;
  p: PAdjItem;
begin
  Result := CreateIntArray;
  Queue := TPairingHeap.Create(VertexCount);
  Visited.Size := VertexCount;
  aTotalWeight := 0;
  for I := 0 to Pred(VertexCount) do
    if not Visited[I] then
      begin
        Queue.Enqueue(TWeightItem.Create(ZeroWeight, 0), I);
        while Queue.TryDequeue(Item) do
          begin
            Curr := {%H-}Item.Index;
            aTotalWeight += Item.Weight;
            Visited[Curr] := True;
            for p in AdjLists[Curr]^ do
              begin
                if Queue.NotUsed(p^.Key) then
                  begin
                    Queue.Enqueue(TWeightItem.Create(p^.Data.Weight, p^.Key), p^.Key);
                    Result[p^.Key] := Curr;
                  end
                else
                  if not Visited[p^.Key] and (p^.Data.Weight < Queue.Peek(p^.Key).Weight) then
                    begin
                      Queue.Update(p^.Key, TWeightItem.Create(p^.Data.Weight, p^.Key));
                      Result[p^.Key] := Curr;
                    end;
              end;
          end;
      end;
end;

function TGWeightedGraph.SeparateGraph(constref aVertex: TVertex): TGWeightedGraph;
begin
  Result := inherited SeparateGraph(aVertex) as TGWeightedGraph;
end;

function TGWeightedGraph.SeparateGraphI(aIndex: SizeInt): TGWeightedGraph;
begin
  Result := inherited SeparateGraphI(aIndex) as TGWeightedGraph;
end;

function TGWeightedGraph.SubgraphFromVertexList(constref aList: TIntArray): TGWeightedGraph;
begin
  Result := inherited SubgraphFromVertexList(aList) as TGWeightedGraph;
end;

function TGWeightedGraph.SubgraphFromPairs(constref aPairs: TIntArray): TGWeightedGraph;
begin
  Result := inherited SubgraphFromPairs(aPairs) as TGWeightedGraph;
end;

function TGWeightedGraph.SubgraphFromEdges(constref aEdges: TIntEdgeArray): TGWeightedGraph;
begin
  Result := inherited SubgraphFromEdges(aEdges) as TGWeightedGraph;
end;

function TGWeightedGraph.Clone: TGWeightedGraph;
begin
  Result := inherited Clone as TGWeightedGraph;
end;

{ TRealPointEdge }

constructor TRealPointEdge.Create(const aWeight: ValReal);
begin
  Weight := aWeight;
end;

{ TPointsChart }

procedure TPointsChart.OnAddEdge(constref aSrc, aDst: TPoint; aData: Pointer);
begin
  PEdgeData(aData)^.Weight := aSrc.Distance(aDst);
end;

procedure TPointsChart.WritePoint(aStream: TStream; constref aValue: TPoint);
begin
  aStream.WriteBuffer(aValue, SizeOf(aValue));
end;

procedure TPointsChart.WriteWeight(aStream: TStream; constref aValue: TRealPointEdge);
begin
  aStream.WriteBuffer(aValue, SizeOf(aValue));
end;

procedure TPointsChart.ReadPoint(aStream: TStream; out aValue: TPoint);
begin
  aStream.ReadBuffer(aValue{%H-}, SizeOf(aValue));
end;

procedure TPointsChart.ReadWeight(aStream: TStream; out aValue: TRealPointEdge);
begin
  aStream.ReadBuffer(aValue{%H-}, SizeOf(aValue));
end;

class function TPointsChart.Distance(constref aSrc, aDst: TPoint): ValReal;
begin
  Result := aSrc.Distance(aDst);
end;

function TPointsChart.AddEdge(constref aSrc, aDst: TPoint): Boolean;
begin
  Result := inherited AddEdge(aSrc, aDst, TRealPointEdge.Create(aSrc.Distance(aDst)));
end;

function TPointsChart.AddEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := inherited AddEdgeI(aSrc, aDst, TRealPointEdge.Create(Items[aSrc].Distance(Items[aDst])));
end;

function TPointsChart.EnsureConnected: SizeInt;
begin
  Result := inherited EnsureConnected(@OnAddEdge);
end;

function TPointsChart.RemoveCutPoints(constref aRoot: TPoint): SizeInt;
begin
  Result := inherited RemoveCutPoints(aRoot, @OnAddEdge);
end;

function TPointsChart.RemoveCutPointsI(aRoot: SizeInt): SizeInt;
begin
  Result := inherited RemoveCutPointsI(aRoot, @OnAddEdge);
end;

function TPointsChart.EnsureBiconnected: SizeInt;
begin
  Result := inherited EnsureBiconnected(@OnAddEdge);
end;

procedure TPointsChart.SaveToStream(aStream: TStream);
begin
  inherited SaveToStream(aStream, @WritePoint, @WriteWeight);
end;

procedure TPointsChart.LoadFromStream(aStream: TStream);
begin
  inherited LoadFromStream(aStream, @ReadPoint, @ReadWeight);
end;

procedure TPointsChart.SaveToFile(const aFileName: string);
begin
   inherited SaveToFile(aFileName, @WritePoint, @WriteWeight);
end;

procedure TPointsChart.LoadFromFile(const aFileName: string);
begin
  inherited LoadFromFile(aFileName, @ReadPoint, @ReadWeight);
end;

function TPointsChart.SeparateGraph(aVertex: TPoint): TPointsChart;
begin
  Result := inherited SeparateGraph(aVertex) as TPointsChart;
end;

function TPointsChart.SeparateGraphI(aIndex: SizeInt): TPointsChart;
begin
  Result := inherited SeparateGraphI(aIndex) as TPointsChart;
end;

function TPointsChart.SubgraphFromVertexList(constref aList: TIntArray): TPointsChart;
begin
  Result := inherited SubgraphFromVertexList(aList) as TPointsChart;
end;

function TPointsChart.SubgraphFromPairs(constref aPairs: TIntArray): TPointsChart;
begin
  Result := inherited SubgraphFromPairs(aPairs) as TPointsChart;
end;

function TPointsChart.SubgraphFromEdges(constref aEdges: TIntEdgeArray): TPointsChart;
begin
  Result := inherited SubgraphFromEdges(aEdges) as TPointsChart;
end;

function TPointsChart.Clone: TPointsChart;
begin
  Result := TPointsChart(inherited Clone);
end;

function TPointsChart.MinPathAStar(constref aSrc, aDst: TPoint; out aWeight: ValReal; aHeur: TEstimate): TIntArray;
begin
  Result := MinPathAStarI(IndexOf(aSrc), IndexOf(aDst), aWeight, aHeur);
end;

function TPointsChart.MinPathAStarI(aSrc, aDst: SizeInt; out aWeight: ValReal; aHeur: TEstimate): TIntArray;
begin
  if aHeur = nil then
    Result := inherited MinPathAStarI(aSrc, aDst, aWeight, @Distance)
  else
    Result := inherited MinPathAStarI(aSrc, aDst, aWeight, aHeur);
end;

end.

