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
  Classes, SysUtils, DateUtils,
  LGUtils,
  {%H-}LGHelpers,
  LGArrayHelpers,
  LGVector,
  LGraphUtils,
  LGStrHelpers,
  LGMiscUtils,
  LGStrConst;

type
  { TGSimpleGraph implements simple sparse undirected graph based on adjacency lists;

      functor TEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGSimpleGraph<TVertex, TEdgeData, TEqRel> = class(specialize TGCustomGraph<TVertex, TEdgeData, TEqRel>)
  protected
  type
    TSortByDegreeHelper = specialize TGDelegatedArrayHelper<SizeInt>;

    TBPCliqueIsHelper = record // BP -> bit-parallel
    private
      FMatrix: TBoolMatrix;
      FCurrSet: TBoolVector;
      FVertices,
      FResult: TIntArray;
      FCurrSize: SizeInt;
      FOnFind: TOnFindSet;
      FCanceled: Boolean;
      procedure Recolor(constref aCand: TBoolVector; var aColOrd, aColors: TIntArray);//aka BB_Color
      procedure Extend(var aCand: TBoolVector); // in Bron-Kerbosch terminlogy
      procedure Extend(var aSub, aCand: TBoolVector);
      procedure FillMatrix(aGraph: TGSimpleGraph; aComplement: Boolean);
      procedure SortMatrixByDegeneracy(aGraph: TGSimpleGraph; aComplement: Boolean);
      procedure SortMatrixByDegree(aGraph: TGSimpleGraph; aComplement: Boolean);
    public
    { some variant of BB-MaxClique -
        San Segundo, P, Rodriguez-Losada, D., Jimenez, A.:
          "An exact bit-parallel algorithm for the maximum clique problem",
        Patrick Prosser: "Exact Algorithms for Maximum Clique: a computational study." }
      function  MaxClique(aGraph: TGSimpleGraph): TIntArray;
    { executes MaxClique upon complement graph }
      function  MaxIS(aGraph: TGSimpleGraph): TIntArray;
    { something like Tomita's Cliques on bit strings, except pivot maximizing -
        Etsuji Tomitaa, Akira Tanakaa, Haruhisa Takahashi:
          "The worst-case time complexity for generating all maximal cliques and
           computational experiments. }
      procedure ListCliques(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
    { executes ListCliques upon complement graph }
      procedure ListIS(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
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
    { returns index of the least significant bit }
      function  Bsf: SizeInt; inline;
    { returns index of the most significant bit }
      function  Bsr: SizeInt; inline;
      function  Intersecting(constref aVector: TBits256): Boolean;
      function  Contains(constref aVector: TBits256): Boolean;
      procedure Join(constref aVector: TBits256);
      procedure Subtract(constref aVector: TBits256); inline;
      procedure Intersect(constref aVector: TBits256); inline;
    { returns count of set bits }
      function  PopCount: SizeInt; inline;
      property  Bits[aIndex: SizeInt]: Boolean read GetBit write SetBit; default;
    end;

    TBitMatrix256 = array of TBits256;

    TBPCliqueIsHelper256 = record // BP -> bit-parallel
    private
      FMatrix: TBitMatrix256;
      FCurrSet,
      FResult: TBits256;
      FVertices: TIntArray;
      FCurrSize: SizeInt;
      FOnFind: TOnFindSet;
      FCanceled: Boolean;
      procedure Recolor(constref aCand: TBits256; var aColOrd, aColors: TIntArray);
      procedure Extend(var aCand: TBits256);
      procedure Extend(var aSub, aCand: TBits256);
      procedure FillMatrix(aGraph: TGSimpleGraph; aComplement: Boolean);
      procedure SortMatrixByDegeneracy(aGraph: TGSimpleGraph; aComplement: Boolean);
      procedure SortMatrixByDegree(aGraph: TGSimpleGraph; aComplement: Boolean);
    public
      function  MaxClique(aGraph: TGSimpleGraph): TIntArray;
      function  MaxIS(aGraph: TGSimpleGraph): TIntArray;
      procedure ListCliques(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
      procedure ListIS(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
    end;

    TCliqueHelper = record
    private
      FMatrix: TSkeleton;
      FCurrSet: TIntSet;
      FVertices,
      FResult: TIntArray;
      FCurrSize: SizeInt;
      FOnFind: TOnFindSet;
      FCanceled: Boolean;
      procedure Recolor(constref aCand: TIntSet; var aColOrd, aColors: TIntArray);
      procedure Extend(var aCand: TIntSet);
      procedure Extend(var aSub, aCand: TIntSet);
      procedure FillMatrix(aGraph: TGSimpleGraph);
      procedure SortMatrixByDegeneracy(aGraph: TGSimpleGraph);
      procedure SortMatrixByDegree(aGraph: TGSimpleGraph);
    public
      function  MaxClique(aGraph: TGSimpleGraph): TIntArray;
      procedure ListCliques(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
    end;

    TBPDomSetHelper = record // BP -> bit-parallel
    private
      FMatrix: TBoolMatrix;
      FVertices,
      FResult: TIntArray;
      FStartTime: TDateTime;
      FTimeOut: Integer;
      FCancel: Boolean;
      function  TimeOut: Boolean; inline;
      procedure FillMatrix(aGraph: TGSimpleGraph);
      procedure Extend(constref aSub, aCand: TBoolVector);
    public
      function  MinDomSet(aGraph: TGSimpleGraph; aTimeOut: Integer; out aExact: Boolean): TIntArray;
    end;

    TBPDomSetHelper256 = record // BP -> bit-parallel
    private
      FMatrix: TBitMatrix256;
      FVertices,
      FResult: TIntArray;
      FStartTime: TDateTime;
      FTimeOut: Integer;
      FCancel: Boolean;
      function  TimeOut: Boolean; inline;
      procedure FillMatrix(aGraph: TGSimpleGraph);
      procedure Extend(constref aSub, aCand: TBits256);
    public
      function  MinDomSet(aGraph: TGSimpleGraph; aTimeOut: Integer; out aExact: Boolean): TIntArray;
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
  const
    LISTCLIQUES_BP_CUTOFF       = 60000;
    MAXCLIQUE_BP_CUTOFF         = 50000;
    MAXCLIQUE_BP_DENSITY_CUTOFF = 0.005;
    MINIS_BP_CUTOFF             = 50000;

  protected
    FCompCount: SizeInt;
    FConnected,
    FConnectedValid: Boolean;
    procedure ResetTags;
    function  SeparateTag(aIndex: SizeInt): SizeInt;
    function  SeparateMerged(L, R: SizeInt): Boolean;
    procedure ValidateConnected;
    function  GetConnected: Boolean; inline;
    function  GetDensity: Double; inline;
    procedure DoRemoveVertex(aIndex: SizeInt);
    function  DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
    function  CreateSkeleton: TSkeleton;
    function  GetSeparateGraph(aIndex: SizeInt): TGSimpleGraph;
    function  GetSeparateCount: SizeInt;
    function  CountPop(aTag: SizeInt): SizeInt;
    function  GetEccentricity(aIndex: SizeInt): SizeInt;
    function  MakeConnected(aOnAddEdge: TOnAddEdge): SizeInt;
    function  CycleExists(aRoot: SizeInt; out aCycle: TIntArray): Boolean;
    function  GetMaxCliqueBP: TIntArray;
    function  GetMaxCliqueBP256: TIntArray;
    function  GetMaxClique: TIntArray;
    procedure ListCliquesBP(aOnFind: TOnFindSet);
    procedure ListCliquesBP256(aOnFind: TOnFindSet);
    procedure ListCliques(aOnFind: TOnFindSet);
    function  GetMaxIsBP: TIntArray;
    function  GetMaxIsBP256: TIntArray;
    procedure ListIsBP(aOnFind: TOnFindSet);
    procedure ListIsBP256(aOnFind: TOnFindSet);
    function  GetMinIS: TIntArray;
    function  GetMinIsBP: TIntArray;
    function  GetMdsBP(aTimeOut: Integer; out aExact: Boolean): TIntArray;
    function  GetMdsBP256(aTimeOut: Integer; out aExact: Boolean): TIntArray;
    procedure SearchForCutPoints(aRoot: SizeInt; var aPoints: TIntVector);
    function  CutPointExists(aRoot: SizeInt): Boolean;
    procedure SearchForBiconnect(aRoot: SizeInt; var aEdges: TIntEdgeVector);
    procedure SearchForBicomponent(aRoot: SizeInt; var aComp: TEdgeArrayVector);
    function  BridgeExists: Boolean;
    procedure SearchForBridges(var aBridges: TIntEdgeVector);
    procedure SearchForFundamentalsCycles(out aCycles: TIntArrayVector);
    procedure SearchForFundamentalsCyclesLen(out aCycleLens: TIntVector);
    procedure FindFundamentalCyclesLen(out aCycleLens: TIntVector);
    function  CreateDegreeArray: TIntArray;
    function  CreateComplementDegreeArray: TIntArray;
    function  SortVerticesByDegeneracy: TIntArray;
    function  SortComplementByDegeneracy: TIntArray;
    function  SortVerticesByDegree(o: TSortOrder): TIntArray;
    function  CmpByDegree(constref L, R: SizeInt): SizeInt;
    function  CmpIntArrayLen(constref L, R: TIntArray): SizeInt;
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
  { saves graph in its own binary format }
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
    function  LocalClusteringI(aIndex: SizeInt): Double;
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
  { checks whether exists any cycle in the same connected component as aRoot;
    if True then aCycle will contain indices of the vertices of the cycle }
    function  ContainsCycle(constref aRoot: TVertex; out aCycle: TIntArray): Boolean; inline;
    function  ContainsCycleI(aRoot: SizeInt; out aCycle: TIntArray): Boolean;
    function  ContainsEulerianCircuit: Boolean;
  { looking for some Eulerian cycle in the first connected component along the path from the first vertex }
    function  FindEulerianCircuit(out aCircuit: TIntVector): Boolean;
  { finds a certain system of fundamental cycles of the graph;
    note: pretty costly time/memory operation }
    function  FindFundamentalCycles(out aCycles: TIntArrayVector): Boolean;
  { lists all maximal independent sets of vertices }
    procedure ListIndependentSets(aOnFindSet: TOnFindSet);
  { returns indices of the vertices of the some found maximum independent set of vertices;
    worst case time cost O(3^n/3) }
    function  MaxIndependentSet: TIntArray;
    function  ApproxMaxIndependentSet: TIntArray;
    function  ApproxMinIndependentSet: TIntArray;
  { returns indices of the vertices of the some found minimum dominating set;
    worst case time cost O(2^n);
    aTimeOut specifies the timeout in seconds;
    at the end of the timeout, the best solution found by this time will be returned,
    and aExactSolution will be set to False }
    function  MinDominatingSet(out aExactSolution: Boolean; aTimeOut: Integer = WAIT_INFINITE): TIntArray;
  { lists all maximal cliques }
    procedure ListMaxCliques(aOnFindClique: TOnFindSet);
  { returns indices of the vertices of the some found maximum clique; worst case time cost O(3^n/3) }
    function  MaxClique: TIntArray;
    function  ApproxMaxClique: TIntArray;
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
    the corresponding elements of the aComponents will contain  the edges of those bicomponents }
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
    property  Density: Double read GetDensity;
  end;

  { TGChart: simple outline;
      functor TEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGChart<TVertex, TEqRel> = class(specialize TGSimpleGraph<TVertex, TEmptyRec, TEqRel>)
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

  TIntChart = class(specialize TGChart<Integer, Integer>)
  protected
    procedure WriteVertex(aStream: TStream; constref aValue: Integer);
    procedure ReadVertex(aStream: TStream; out aValue: Integer);
  public
    procedure SaveToStream(aStream: TStream);
    procedure LoadFromStream(aStream: TStream);
    procedure SaveToFile(const aFileName: string);
    procedure LoadFromFile(const aFileName: string);
    procedure LoadFromDIMACSAsciiFile(const aFileName: string);
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

  TPointsChart = class(specialize TGWeightedGraph<TPoint, ValReal, TRealPointEdge, TPoint>)
  protected
    procedure OnAddEdge(constref aSrc, aDst: TPoint; aData: Pointer);
    procedure WritePoint(aStream: TStream; constref aValue: TPoint);
    procedure ReadPoint(aStream: TStream; out aValue: TPoint);
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

{ TGSimpleGraph.TBPCliqueIsHelper }

procedure TGSimpleGraph.TBPCliqueIsHelper.Recolor(constref aCand: TBoolVector; var aColOrd, aColors: TIntArray);
var
  P, Q: TBoolVector;
  I, J, ColorClass, PCount: SizeInt;
begin
  P := aCand;
  ColorClass := 0;
  I := 0;
  PCount := P.PopCount;
  while PCount > 0 do
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
          Dec(PCount);
        end;
    end;
end;

procedure TGSimpleGraph.TBPCliqueIsHelper.Extend(var aCand: TBoolVector);
var
  NewCand: TBoolVector;
  ColOrd, Colors: TIntArray;
  I, J, ItemCount: SizeInt;
begin
  if aCand.IsEmpty then
    begin
      ItemCount := FCurrSet.PopCount;
      if ItemCount > FCurrSize then
        begin
          FCurrSize := ItemCount;
          FResult := FCurrSet.ToArray;
        end;
      exit;
    end;
  ItemCount := aCand.PopCount;
  if ItemCount > 0 then
    begin
      System.SetLength(ColOrd, ItemCount);
      System.SetLength(Colors, ItemCount);
      Recolor(aCand, ColOrd, Colors);
      for I := Pred(ItemCount) downto 0 do
        begin
          if Colors[I] + FCurrSet.PopCount <= FCurrSize then
            exit;
          J := ColOrd[I];
          aCand[J] := False;
          FCurrSet[FVertices[J]] := True;
          NewCand := aCand;
          NewCand.Intersect(FMatrix[J]);
          Extend(NewCand);
          FCurrSet[FVertices[J]] := False;
        end;
    end;
end;

procedure TGSimpleGraph.TBPCliqueIsHelper.Extend(var aSub, aCand: TBoolVector);
var
  NewSub, NewCand, Diff: TBoolVector;
  I: SizeInt;
begin
  if FCanceled then
    exit;
  if aSub.NonEmpty then
    begin
      if aCand.NonEmpty then
        begin
          Diff := aCand;
          Diff.Subtract(FMatrix[aSub.Bsf]);
          for I in Diff do
            begin
              aCand[I] := False;
              NewCand := aCand;
              NewSub := aSub;
              FCurrSet[FVertices[I]] := True;
              NewCand.Intersect(FMatrix[I]);
              NewSub.Intersect(FMatrix[I]);
              Extend(NewSub, NewCand);
              FCurrSet[FVertices[I]] := False;
            end;
        end;
    end
  else
    FOnFind(FCurrSet.ToArray, FCanceled);
end;

procedure TGSimpleGraph.TBPCliqueIsHelper.FillMatrix(aGraph: TGSimpleGraph; aComplement: Boolean);
var
  I, J: SizeInt;
  p: PAdjList;
begin
  System.SetLength(FMatrix, aGraph.VertexCount);
  for I := 0 to Pred(aGraph.VertexCount) do
    begin
      FMatrix[I].Size := aGraph.VertexCount;
      p := aGraph.AdjLists[FVertices[I]];
      for J := 0 to Pred(aGraph.VertexCount) do
        if (I <> J) and (p^.Contains(FVertices[J]) xor aComplement) then
          FMatrix[I][J] := True;
    end;
end;

procedure TGSimpleGraph.TBPCliqueIsHelper.SortMatrixByDegeneracy(aGraph: TGSimpleGraph; aComplement: Boolean);
begin
  if aComplement then
    FVertices := aGraph.SortComplementByDegeneracy
  else
    FVertices := aGraph.SortVerticesByDegeneracy;
  FillMatrix(aGraph, aComplement);
end;

procedure TGSimpleGraph.TBPCliqueIsHelper.SortMatrixByDegree(aGraph: TGSimpleGraph; aComplement: Boolean);
begin
  if aComplement then
    FVertices := aGraph.SortVerticesByDegree(soAsc)
  else
    FVertices := aGraph.SortVerticesByDegree(soDesc);
  FillMatrix(aGraph, aComplement);
end;

function TGSimpleGraph.TBPCliqueIsHelper.MaxClique(aGraph: TGSimpleGraph): TIntArray;
var
  Cand: TBoolVector;
begin
  SortMatrixByDegeneracy(aGraph, False);
  Cand.InitRange(aGraph.VertexCount);
  FCurrSize := 0;
  FCurrSet.Size := aGraph.VertexCount;
  Extend(Cand);
  Result := FResult;
end;

function TGSimpleGraph.TBPCliqueIsHelper.MaxIS(aGraph: TGSimpleGraph): TIntArray;
var
  Cand: TBoolVector;
begin
  SortMatrixByDegeneracy(aGraph, True);
  Cand.InitRange(aGraph.VertexCount);
  FCurrSize := 0;
  FCurrSet.Size := aGraph.VertexCount;
  Extend(Cand);
  Result := FResult;
end;

procedure TGSimpleGraph.TBPCliqueIsHelper.ListCliques(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
var
  Sub, Cand: TBoolVector;
begin
  SortMatrixByDegree(aGraph, False);
  Sub.InitRange(aGraph.VertexCount);
  Cand.InitRange(aGraph.VertexCount);
  FCurrSet.Size := aGraph.VertexCount;
  FOnFind := aOnFind;
  FCanceled := False;
  Extend(Sub, Cand);
end;

procedure TGSimpleGraph.TBPCliqueIsHelper.ListIS(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
var
  Sub, Cand: TBoolVector;
begin
  SortMatrixByDegree(aGraph, True);
  Sub.InitRange(aGraph.VertexCount);
  Cand.InitRange(aGraph.VertexCount);
  FCurrSet.Size := aGraph.VertexCount;
  FOnFind := aOnFind;
  FCanceled := False;
  Extend(Sub, Cand);
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

function TGSimpleGraph.TBits256.Contains(constref aVector: TBits256): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to Pred(LIMB_COUNT) do
    if FBits[I] and aVector.FBits[I] <> aVector.FBits[I] then
      exit(False);
  Result := True;
end;

procedure TGSimpleGraph.TBits256.Join(constref aVector: TBits256);
{$IF DEFINED(CPU64)}
begin
  FBits[0] := FBits[0] or aVector.FBits[0];
  FBits[1] := FBits[1] or aVector.FBits[1];
  FBits[2] := FBits[2] or aVector.FBits[2];
  FBits[3] := FBits[3] or aVector.FBits[3];
{$ELSEIF DEFINED(CPU32)}
begin
  FBits[0] := FBits[0] or aVector.FBits[0];
  FBits[1] := FBits[1] or aVector.FBits[1];
  FBits[2] := FBits[2] or aVector.FBits[2];
  FBits[3] := FBits[3] or aVector.FBits[3];
  FBits[4] := FBits[4] or aVector.FBits[4];
  FBits[5] := FBits[5] or aVector.FBits[5];
  FBits[6] := FBits[6] or aVector.FBits[6];
  FBits[7] := FBits[7] or aVector.FBits[7];
{$ELSE }
var
  I: SizeInt;
begin
  for I := 0 to Pred(LIMB_COUNT) do
    FBits[I] := FBits[I] or aVector.FBits[I];
{$ENDIF }
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
  Result := SizeInt(PopCnt(FBits[0])) + SizeInt(PopCnt(FBits[1])) +
            SizeInt(PopCnt(FBits[2])) + SizeInt(PopCnt(FBits[3]));
{$ELSEIF DEFINED(CPU32)}
begin
  Result := SizeInt(PopCnt(FBits[0])) + SizeInt(PopCnt(FBits[1])) +
            SizeInt(PopCnt(FBits[2])) + SizeInt(PopCnt(FBits[3])) +
            SizeInt(PopCnt(FBits[4])) + SizeInt(PopCnt(FBits[5])) +
            SizeInt(PopCnt(FBits[6])) + SizeInt(PopCnt(FBits[7]));
{$ELSE }
var
  I: SizeUInt;
begin
  Result := 0;
  for I in FBits do
    Result += SizeInt(PopCnt(I));
{$ENDIF }
end;

{ TGSimpleGraph.TBPCliqueIsHelper256 }

procedure TGSimpleGraph.TBPCliqueIsHelper256.Recolor(constref aCand: TBits256; var aColOrd, aColors: TIntArray);
var
  P, Q: TBits256;
  I, J, ColorClass, PCount: SizeInt;
begin
  P := aCand;
  ColorClass := 0;
  I := 0;
  PCount := P.PopCount;
  while PCount > 0 do
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
          Dec(PCount);
        end;
    end;
end;

procedure TGSimpleGraph.TBPCliqueIsHelper256.Extend(var aCand: TBits256);
var
  NewCand: TBits256;
  ColOrd, Colors: TIntArray;
  I, J, ItemCount: SizeInt;
begin
  if aCand.NonEmpty then
    begin
      ItemCount := aCand.PopCount;
      System.SetLength(ColOrd, ItemCount);
      System.SetLength(Colors, ItemCount);
      Recolor(aCand, ColOrd, Colors);
      for I := Pred(ItemCount) downto 0 do
        begin
          if Colors[I] + FCurrSet.PopCount <= FCurrSize then
            exit;
          J := ColOrd[I];
          aCand[J] := False;
          FCurrSet[FVertices[J]] := True;
          NewCand := aCand;
          NewCand.Intersect(FMatrix[J]);
          Extend(NewCand);
          FCurrSet[FVertices[J]] := False;
        end;
    end
  else
    begin
      ItemCount := FCurrSet.PopCount;
      if ItemCount > FCurrSize then
        begin
          FCurrSize := ItemCount;
          FResult := FCurrSet;
        end;
    end;
end;

procedure TGSimpleGraph.TBPCliqueIsHelper256.Extend(var aSub, aCand: TBits256);
var
  NewSub, NewCand, Diff: TBits256;
  I: SizeInt;
begin
  if FCanceled then
    exit;
  if aSub.NonEmpty then
    begin
      if aCand.NonEmpty then
        begin
          Diff := aCand;
          Diff.Subtract(FMatrix[aSub.Bsf]);
          for I in Diff do
            begin
              aCand[I] := False;
              NewCand := aCand;
              NewSub := aSub;
              FCurrSet[FVertices[I]] := True;
              NewCand.Intersect(FMatrix[I]);
              NewSub.Intersect(FMatrix[I]);
              Extend(NewSub, NewCand);
              FCurrSet[FVertices[I]] := False;
            end;
        end;
    end
  else
    FOnFind(FCurrSet.ToArray, FCanceled);
end;

procedure TGSimpleGraph.TBPCliqueIsHelper256.FillMatrix(aGraph: TGSimpleGraph; aComplement: Boolean);
var
  I, J: SizeInt;
  pA: PAdjList;
begin
  System.SetLength(FMatrix, aGraph.VertexCount);
  for I := 0 to Pred(aGraph.VertexCount) do
    begin
      pA := aGraph.AdjLists[FVertices[I]];
      FMatrix[I].InitZero;
      for J := 0 to Pred(aGraph.VertexCount) do
        if (I <> J) and (pA^.Contains(FVertices[J]) xor aComplement) then
          FMatrix[I][J] := True;
    end;
end;

procedure TGSimpleGraph.TBPCliqueIsHelper256.SortMatrixByDegeneracy(aGraph: TGSimpleGraph; aComplement: Boolean);
begin
  if aComplement then
    FVertices := aGraph.SortComplementByDegeneracy
  else
    FVertices := aGraph.SortVerticesByDegeneracy;
  FillMatrix(aGraph, aComplement);
end;

procedure TGSimpleGraph.TBPCliqueIsHelper256.SortMatrixByDegree(aGraph: TGSimpleGraph; aComplement: Boolean);
begin
  if aComplement then
    FVertices := aGraph.SortVerticesByDegree(soAsc)
  else
    FVertices := aGraph.SortVerticesByDegree(soDesc);
  FillMatrix(aGraph, aComplement);
end;

function TGSimpleGraph.TBPCliqueIsHelper256.MaxClique(aGraph: TGSimpleGraph): TIntArray;
var
  Cand: TBits256;
begin
  SortMatrixByDegeneracy(aGraph, False);
  Cand.InitRange(aGraph.VertexCount);
  FCurrSize := 0;
  FCurrSet.InitZero;
  Extend(Cand);
  Result := FResult.ToArray;
end;

function TGSimpleGraph.TBPCliqueIsHelper256.MaxIS(aGraph: TGSimpleGraph): TIntArray;
var
  Cand: TBits256;
begin
  SortMatrixByDegeneracy(aGraph, True);
  Cand.InitRange(aGraph.VertexCount);
  FCurrSize := 0;
  FCurrSet.InitZero;
  Extend(Cand);
  Result := FResult.ToArray;
end;

procedure TGSimpleGraph.TBPCliqueIsHelper256.ListCliques(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
var
  Sub, Cand: TBits256;
begin
  SortMatrixByDegree(aGraph, False);
  Sub.InitRange(aGraph.VertexCount);
  Cand.InitRange(aGraph.VertexCount);
  FCurrSet.InitZero;
  FOnFind := aOnFind;
  FCanceled := False;
  Extend(Sub, Cand);
end;

procedure TGSimpleGraph.TBPCliqueIsHelper256.ListIS(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
var
  Sub, Cand: TBits256;
begin
  SortMatrixByDegree(aGraph, True);
  Sub.InitRange(aGraph.VertexCount);
  Cand.InitRange(aGraph.VertexCount);
  FCurrSet.InitZero;
  FOnFind := aOnFind;
  FCanceled := False;
  Extend(Sub, Cand);
end;

procedure TGSimpleGraph.TCliqueHelper.Recolor(constref aCand: TIntSet; var aColOrd, aColors: TIntArray);
var
  P, Q: TIntSet;
  I, J, ColorClass: SizeInt;
begin
  P.Assign(aCand);
  P.Reverse;
  ColorClass := 0;
  I := 0;
  while P.NonEmpty do
    begin
      Inc(ColorClass);
      Q.Assign(P);
      while Q.NonEmpty do
        begin
          J := Q.Pop;
          P.Delete(J);
          Q.Subtract(FMatrix[J]^);
          aColOrd[I] := J;
          aColors[I] := ColorClass;
          Inc(I);
        end;
    end;
end;

procedure TGSimpleGraph.TCliqueHelper.Extend(var aCand: TIntSet);
var
  NewCand: TIntSet;
  ColOrd, Colors: TIntArray;
  I, J: SizeInt;
begin
  if aCand.NonEmpty then
    begin
      System.SetLength(ColOrd, aCand.Count);
      System.SetLength(Colors, aCand.Count);
      Recolor(aCand, ColOrd, Colors);
      for I := Pred(aCand.Count) downto 0 do
        begin
          if Colors[I] + FCurrSet.Count <= FCurrSize then
            exit;
          J := ColOrd[I];
          aCand.Delete(J);
          FCurrSet.Push(FVertices[J]);
          NewCand.Assign(aCand);
          NewCand.Intersect(FMatrix[J]^);
          Extend(NewCand);
          FCurrSet.Pop;
        end;
    end
  else
    if FCurrSet.Count > FCurrSize then
      begin
        FCurrSize := FCurrSet.Count;
        FResult := FCurrSet.ToArray;
      end;
end;

procedure TGSimpleGraph.TCliqueHelper.Extend(var aSub, aCand: TIntSet);
var
  NewSub, NewCand, Diff: TIntSet;
  I: SizeInt;
begin
  if FCanceled then
    exit;
  if aSub.NonEmpty then
    begin
      if aCand.NonEmpty then
        begin
          Diff.Assign(aCand);
          Diff.Subtract(FMatrix[aSub[0]]^);
          for I in Diff do
            begin
              aCand.Delete(I);
              NewCand.Assign(aCand);
              NewSub.Assign(aSub);
              FCurrSet.Push(FVertices[I]);
              NewCand.Intersect(FMatrix[I]^);
              NewSub.Intersect(FMatrix[I]^);
              Extend(NewSub, NewCand);
              FCurrSet.Pop;
            end;
        end;
    end
  else
    FOnFind(FCurrSet.ToArray, FCanceled);
end;

procedure TGSimpleGraph.TCliqueHelper.FillMatrix(aGraph: TGSimpleGraph);
var
  I, J: SizeInt;
  pA: PAdjList;
begin
  FMatrix := TSkeleton.Create(aGraph.VertexCount);
  for I := 0 to Pred(aGraph.VertexCount) do
    begin
      pA := aGraph.AdjLists[FVertices[I]];
      for J := 0 to Pred(aGraph.VertexCount) do
        if (I <> J) and pA^.Contains(FVertices[J]) then
          FMatrix[I]^.Push(J);
    end;
end;

procedure TGSimpleGraph.TCliqueHelper.SortMatrixByDegeneracy(aGraph: TGSimpleGraph);
begin
  FVertices := aGraph.SortVerticesByDegeneracy;
  FillMatrix(aGraph);
end;

procedure TGSimpleGraph.TCliqueHelper.SortMatrixByDegree(aGraph: TGSimpleGraph);
begin
  FVertices := aGraph.SortVerticesByDegree(soDesc);
  FillMatrix(aGraph);
end;

function TGSimpleGraph.TCliqueHelper.MaxClique(aGraph: TGSimpleGraph): TIntArray;
var
  Cand: TIntSet;
begin
  SortMatrixByDegeneracy(aGraph);
  Cand.InitRange(aGraph.VertexCount);
  FCurrSize := 0;
  Extend(Cand);
  Result := FResult;
end;

procedure TGSimpleGraph.TCliqueHelper.ListCliques(aGraph: TGSimpleGraph; aOnFind: TOnFindSet);
var
  Sub, Cand: TIntSet;
begin
  SortMatrixByDegree(aGraph);
  Sub.InitRange(aGraph.VertexCount);
  Cand.InitRange(aGraph.VertexCount);
  FOnFind := aOnFind;
  FCanceled := False;
  Extend(Sub, Cand);
end;

{ TGSimpleGraph.TBPDomSetHelper }

function TGSimpleGraph.TBPDomSetHelper.TimeOut: Boolean;
begin
  Result := SecondsBetween(Now, FStartTime) > FTimeOut;
end;

procedure TGSimpleGraph.TBPDomSetHelper.FillMatrix(aGraph: TGSimpleGraph);
var
  I, J: SizeInt;
  pA: PAdjList;
begin
  FVertices := aGraph.SortVerticesByDegree(soDesc);
  System.SetLength(FMatrix, System.Length(FVertices));
  for I := 0 to System.High(FVertices) do
    begin
      FMatrix[I].Size := System.Length(FVertices);
      pA := aGraph.AdjLists[FVertices[I]];
      for J := 0 to System.High(FVertices) do
        if (I <> J) and pA^.Contains(FVertices[J]) then
          FMatrix[I][J] := True;
    end;
end;

procedure TGSimpleGraph.TBPDomSetHelper.Extend(constref aSub, aCand: TBoolVector);
var
  NewSub, NewCand, Neib: TBoolVector;
  I, J: SizeInt;
begin
  if aSub.NonEmpty then
    begin
      if aCand.PopCount >= System.High(FResult) then
        exit;
      if TimeOut then
        begin
          FCancel := True;
          exit;
        end;
      NewSub := aSub;
      I := NewSub.Bsf;
      NewCand := aCand;
      NewCand[FVertices[I]] := True;
      NewSub[I] := False;
      NewSub.Subtract(FMatrix[I]);
      Extend(NewSub, NewCand);
      if aCand.PopCount >= System.High(FResult) then
        exit;
      NewCand[FVertices[I]] := False;
      Neib := aSub;
      Neib.Intersect(FMatrix[I]);
      while Neib.NonEmpty do
        begin
          J := Neib.Bsf;
          Neib[J] := False;
          NewCand[FVertices[J]] := True;
          NewSub := aSub;
          NewSub[J] := False;
          NewSub.Subtract(FMatrix[J]);
          Extend(NewSub, NewCand);
          if NewCand.PopCount >= System.High(FResult) then
            exit;
          NewCand[FVertices[I]] := True;
          NewSub.Subtract(FMatrix[I]);
          Extend(NewSub, NewCand);
          if NewCand.PopCount >= System.Length(FResult) then
            exit;
          NewCand[FVertices[I]] := False;
          NewCand[FVertices[J]] := False;
        end;
    end
  else
    if aCand.PopCount < System.Length(FResult) then
      FResult := aCand.ToArray;
end;

function TGSimpleGraph.TBPDomSetHelper.MinDomSet(aGraph: TGSimpleGraph; aTimeOut: Integer;
  out aExact: Boolean): TIntArray;
var
  Sub, Cand: TBoolVector;
  I: SizeInt;
begin
  FStartTime := Now;
  if aTimeOut = WAIT_INFINITE then
    aTimeOut := System.High(Integer)
  else
    FTimeOut := aTimeOut;
  FCancel := False;
  FillMatrix(aGraph);
  FResult := aGraph.ApproxMinIndependentSet;
  Cand.Size := aGraph.VertexCount;
  Sub.InitRange(aGraph.VertexCount);
  for I := 0 to Pred(aGraph.VertexCount) do
    if aGraph.DegreeI(FVertices[I]) = 0 then
      Sub[I] := False;
  Extend(Sub, Cand);
  aExact := not FCancel;
  Result := FResult;
end;

{ TGSimpleGraph.TBPDomSetHelper256 }

function TGSimpleGraph.TBPDomSetHelper256.TimeOut: Boolean;
begin
  Result := SecondsBetween(Now, FStartTime) > FTimeOut;
end;

procedure TGSimpleGraph.TBPDomSetHelper256.FillMatrix(aGraph: TGSimpleGraph);
var
  I, J: SizeInt;
  pA: PAdjList;
begin
  FVertices := aGraph.SortVerticesByDegree(soDesc);
  System.SetLength(FMatrix, System.Length(FVertices));
  for I := 0 to System.High(FVertices) do
    begin
      FMatrix[I].InitZero;
      pA := aGraph.AdjLists[FVertices[I]];
      for J := 0 to System.High(FVertices) do
        if (I <> J) and pA^.Contains(FVertices[J]) then
          FMatrix[I][J] := True;
    end;
end;

procedure TGSimpleGraph.TBPDomSetHelper256.Extend(constref aSub, aCand: TBits256);
var
  NewSub, NewCand, Neib: TBits256;
  I, J: SizeInt;
begin
  if aSub.NonEmpty then
    begin
      if aCand.PopCount >= System.High(FResult) then
        exit;
      if TimeOut then
        begin
          FCancel := True;
          exit;
        end;
      NewSub := aSub;
      I := aSub.Bsf;
      NewCand := aCand;
      NewCand[FVertices[I]] := True;
      NewSub[I] := False;
      NewSub.Subtract(FMatrix[I]);
      Extend(NewSub, NewCand);
      if aCand.PopCount >= System.High(FResult) then
        exit;
      NewCand[FVertices[I]] := False;
      Neib := aSub;
      Neib.Intersect(FMatrix[I]);
      while Neib.NonEmpty do
        begin
          J := Neib.Bsf;
          Neib[J] := False;
          NewCand[FVertices[J]] := True;
          NewSub := aSub;
          NewSub[J] := False;
          NewSub.Subtract(FMatrix[J]);
          Extend(NewSub, NewCand);
          if NewCand.PopCount >= System.High(FResult) then
            exit;
          NewCand[FVertices[I]] := True;
          NewSub.Subtract(FMatrix[I]);
          Extend(NewSub, NewCand);
          if NewCand.PopCount >= System.Length(FResult) then
            exit;
          NewCand[FVertices[I]] := False;
          NewCand[FVertices[J]] := False;
        end;
    end
  else
    if aCand.PopCount < System.Length(FResult) then
      FResult := aCand.ToArray;
end;

function TGSimpleGraph.TBPDomSetHelper256.MinDomSet(aGraph: TGSimpleGraph; aTimeOut: Integer;
  out aExact: Boolean): TIntArray;
var
  Sub, Cand: TBits256;
  I: SizeInt;
begin
  FStartTime := Now;
  if aTimeOut = WAIT_INFINITE then
    aTimeOut := System.High(Integer)
  else
    FTimeOut := aTimeOut;
  FCancel := False;
  FillMatrix(aGraph);
  FResult := aGraph.ApproxMinIndependentSet;
  {%H-}Cand.InitZero;
  Sub.InitRange(aGraph.VertexCount);
  for I := 0 to Pred(aGraph.VertexCount) do
    if aGraph.DegreeI(FVertices[I]) = 0 then
      Sub[I] := False;
  Extend(Sub, Cand);
  aExact := not FCancel;
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

function TGSimpleGraph.GetDensity: Double;
begin
  if NonEmpty then
    Result := (Double(EdgeCount) * 2)/(Double(VertexCount) * Double(Pred(VertexCount)))
  else
    Result := 0.0;
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
    FNodeList[I].AdjList.CopyTo(Result[I]^);
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
  until not Queue{%H-}.TryDequeue(aIndex);
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

function TGSimpleGraph.GetMaxCliqueBP: TIntArray;
var
  Helper: TBPCliqueIsHelper;
begin
  Result := Helper.MaxClique(Self);
end;

function TGSimpleGraph.GetMaxCliqueBP256: TIntArray;
var
  Helper: TBPCliqueIsHelper256;
begin
  Result := Helper.MaxClique(Self);
end;

function TGSimpleGraph.GetMaxClique: TIntArray;
var
  Helper: TCliqueHelper;
begin
  Result := Helper.MaxClique(Self);
end;

procedure TGSimpleGraph.ListCliquesBP(aOnFind: TOnFindSet);
var
  Helper: TBPCliqueIsHelper;
begin
  Helper.ListCliques(Self, aOnFind);
end;

procedure TGSimpleGraph.ListCliquesBP256(aOnFind: TOnFindSet);
var
  Helper: TBPCliqueIsHelper256;
begin
  Helper.ListCliques(Self, aOnFind);
end;

procedure TGSimpleGraph.ListCliques(aOnFind: TOnFindSet);
var
  Helper: TCliqueHelper;
begin
  Helper.ListCliques(Self, aOnFind);
end;

function TGSimpleGraph.GetMaxIsBP: TIntArray;
var
  Helper: TBPCliqueIsHelper;
begin
  Result := Helper.MaxIS(Self);
end;

function TGSimpleGraph.GetMaxIsBP256: TIntArray;
var
  Helper: TBPCliqueIsHelper256;
begin
  Result := Helper.MaxIS(Self);
end;

procedure TGSimpleGraph.ListIsBP(aOnFind: TOnFindSet);
var
  Helper: TBPCliqueIsHelper;
begin
  Helper.ListIS(Self, aOnFind);
end;

procedure TGSimpleGraph.ListIsBP256(aOnFind: TOnFindSet);
var
  Helper: TBPCliqueIsHelper256;
begin
  Helper.ListIS(Self, aOnFind);
end;

function TGSimpleGraph.GetMinIS: TIntArray;
var
  Cand, Stack: TIntSet;
  I, J, v, w, Card: SizeInt;
begin
  Cand.InitRange(VertexCount);
  while Cand.NonEmpty do
    begin
      J := 0;
      Card := 0;
      for I in Cand do
        begin
          w := 1;
          for v in AdjVerticesI(I) do
            if Cand.Contains(v) then
              Inc(w);
          if w > Card then
            begin
              Card := w;
              J := I;
            end;
        end;
      Cand.Delete(J);
      for I in AdjVerticesI(J) do
        Cand.Delete(I);
      {%H-}Stack.Push(J);
    end;
  Result := Stack.ToArray;
end;

function TGSimpleGraph.GetMinIsBP: TIntArray;
var
  Matrix: TBoolMatrix;
  Cand: TBoolVector;
  Stack: TIntSet;
  I, J, w, Card: SizeInt;
begin
  Matrix := CreateBoolMatrix;
  Cand.InitRange(VertexCount);
  while Cand.NonEmpty do
    begin
      J := 0;
      Card := 0;
      for I in Cand do
        begin
          w := Succ(Cand.IntersectionCount(Matrix[I]));
          if w > Card then
            begin
              Card := w;
              J := I;
            end;
        end;
      Cand[J] := False;
      Cand.Subtract(Matrix[J]);
      {%H-}Stack.Push(J);
    end;
  Result := Stack.ToArray;
end;

function TGSimpleGraph.GetMdsBP(aTimeOut: Integer; out aExact: Boolean): TIntArray;
var
  Helper: TBPDomSetHelper;
begin
  Result := Helper.MinDomSet(Self, aTimeOut, aExact);
end;

function TGSimpleGraph.GetMdsBP256(aTimeOut: Integer; out aExact: Boolean): TIntArray;
var
  Helper: TBPDomSetHelper256;
begin
  Result := Helper.MinDomSet(Self, aTimeOut, aExact);
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

function TGSimpleGraph.CreateDegreeArray: TIntArray;
var
  I: SizeInt;
begin
  Result := CreateIntArray;
  for I := 0 to Pred(VertexCount) do
    Result[I] := AdjLists[I]^.Count;
end;

function TGSimpleGraph.CreateComplementDegreeArray: TIntArray;
var
  I: SizeInt;
begin
  Result := CreateIntArray;
  for I := 0 to Pred(VertexCount) do
    Result[I] := VertexCount - AdjLists[I]^.Count;
end;

function TGSimpleGraph.SortVerticesByDegeneracy: TIntArray;
var
  I, J: SizeInt;
  List, Stack: TIntSet;
begin
  Result := CreateDegreeArray;
  List.InitRange(VertexCount);
  while List.NonEmpty do
    begin
      I := List[0];
      for J in List do
        if Result[J] < Result[I] then
          I := J;
      {%H-}Stack.Push(I);
      List.Remove(I);
      for J in List do
        if AdjLists[J]^.Contains(I) then
          Dec(Result[J]);
    end;
  Result := Stack.ToArray;
  TIntHelper.Reverse(Result);
end;

function TGSimpleGraph.SortComplementByDegeneracy: TIntArray;
var
  I, J: SizeInt;
  List, Stack: TIntSet;
begin
  Result := CreateComplementDegreeArray;
  List.InitRange(VertexCount);
  while List.NonEmpty do
    begin
      I := List[0];
      for J in List do
        if Result[J] < Result[I] then
          I := J;
      {%H-}Stack.Push(I);
      List.Remove(I);
      for J in List do
        if not AdjLists[J]^.Contains(I) then
          Dec(Result[J]);
    end;
  Result := Stack.ToArray;
  TIntHelper.Reverse(Result);
end;

function TGSimpleGraph.SortVerticesByDegree(o: TSortOrder): TIntArray;
begin
  Result := CreateIntArrayRange;
  TSortByDegreeHelper.Sort(Result, @CmpByDegree, o);
end;

function TGSimpleGraph.CmpByDegree(constref L, R: SizeInt): SizeInt;
begin
  Result := SizeInt.Compare(AdjLists[L]^.Count, AdjLists[R]^.Count);
  if Result <> 0 then
    exit;
  if L < R then
    exit(-1);
  Result := 1;
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
      FNodeList[aIndex].Tag := aIndex;
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
  s, d: Integer;
  Edge: TEdge;
  gTitle, Descr: utf8string;
  wbs: TWriteBufStream;
begin
  if not Assigned(aWriteVertex) then
    raise ELGraphError.Create(SEWriteCallbackMissed);
{$IFDEF CPU64}
  if VertexCount > System.High(Integer) then
    raise ELGraphError.CreateFmt(SEStreamSizeExceedFmt, [VertexCount]);
{$ENDIF CPU64}
  wbs := TWriteBufStream.Create(aStream);
  try
    //write header
    Header.Magic := GRAPH_MAGIC;
    Header.Version := GRAPH_HEADER_VERSION;
    gTitle := Title;
    Header.TitleLen := System.Length(gTitle);
    Descr := Description.Text;
    Header.DescriptionLen := System.Length(Descr);
    Header.VertexCount := VertexCount;
    Header.EdgeCount := EdgeCount;
    wbs.WriteBuffer(Header, SizeOf(Header));
    //write title
    if Header.TitleLen > 0 then
      wbs.WriteBuffer(Pointer(gTitle)^, Header.TitleLen);
    //write description
    if Header.DescriptionLen > 0 then
      wbs.WriteBuffer(Pointer(Descr)^, Header.DescriptionLen);
    //write Items, but does not save any info about connected
    //this should allow transfer data between directed/undirected graphs ???
    for s := 0 to Pred(Header.VertexCount) do
      aWriteVertex(wbs, FNodeList[s].Vertex);
    //write edges
    for Edge in DistinctEdges do
      begin
        s := Edge.Source;
        d := Edge.Destination;
        wbs.WriteBuffer(NtoLE(s), SizeOf(s));
        wbs.WriteBuffer(NtoLE(d), SizeOf(d));
        if Assigned(aWriteData) then
          aWriteData(wbs, Edge.Data);
      end;
  finally
    wbs.Free;
  end;
end;

procedure TGSimpleGraph.LoadFromStream(aStream: TStream; aReadVertex: TOnReadVertex; aReadData: TOnReadData);
var
  Header: TStreamHeader;
  s, d: Integer;
  I, Ind: SizeInt;
  Data: TEdgeData;
  Vertex: TVertex;
  gTitle, Descr: utf8string;
  rbs: TReadBufStream;
begin
  if not Assigned(aReadVertex) then
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
    if Header.TitleLen > 0 then
      begin
        System.SetLength(gTitle, Header.TitleLen);
        rbs.ReadBuffer(Pointer(gTitle)^, Header.TitleLen);
        FTitle := gTitle;
      end;
    //read description
    if Header.DescriptionLen > 0 then
      begin
        System.SetLength(Descr, Header.DescriptionLen);
        rbs.ReadBuffer(Pointer(Descr)^, Header.DescriptionLen);
        Description.Text := Descr;
      end;
    //read Items
    for I := 0 to Pred(Header.VertexCount) do
      begin
        aReadVertex(rbs, Vertex);
        if not AddVertex(Vertex, Ind) then
          raise ELGraphError.Create(SEGraphStreamCorrupt);
        if Ind <> I then
          raise ELGraphError.Create(SEGraphStreamReadIntern);
      end;
    //read edges
    Data := DefaultEdgeData;
    for I := 0 to Pred(Header.EdgeCount) do
      begin
        rbs.ReadBuffer(s, SizeOf(s));
        rbs.ReadBuffer(d, SizeOf(d));
        if Assigned(aReadData) then
          aReadData(rbs, Data);
        AddEdgeI(LEToN(s), LEToN(d), Data);
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

function TGSimpleGraph.LocalClusteringI(aIndex: SizeInt): Double;
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
  Result := Double(Counter) / (Double(d) * Double(Pred(d)));
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
    ListIsBP(aOnFindSet)
  else
    ListIsBP256(aOnFindSet)
end;

function TGSimpleGraph.MaxIndependentSet: TIntArray;
begin
  if IsEmpty then
    exit(nil);
  if VertexCount > 256 then
    Result := GetMaxIsBP
  else
    Result := GetMaxIsBP256;
end;

function TGSimpleGraph.ApproxMaxIndependentSet: TIntArray;
var
  Cand, Stack: TIntSet;
  I, J, v, w, Card: SizeInt;
begin
  if IsEmpty then
    exit(nil);
  Cand.InitRange(VertexCount);
  while Cand.NonEmpty do
    begin
      J := 0;
      Card := VertexCount;
      for I in Cand do
        begin
          w := 1;
          for v in AdjVerticesI(I) do
            if Cand.Contains(v) then
              Inc(w);
          if w < Card then
            begin
              Card := w;
              J := I;
            end;
        end;
      Cand.Delete(J);
      for I in AdjVerticesI(J) do
        Cand.Delete(I);
      {%H-}Stack.Push(J);
    end;
  Result := Stack.ToArray;
end;

function TGSimpleGraph.ApproxMinIndependentSet: TIntArray;
begin
  if IsEmpty then
    exit(nil);
  if VertexCount > MINIS_BP_CUTOFF then
    Result := GetMinIS
  else
    Result := GetMinIsBP;
end;

function TGSimpleGraph.MinDominatingSet(out aExactSolution: Boolean; aTimeOut: Integer): TIntArray;
begin
  if IsEmpty then
    exit(nil);
  if VertexCount > 256 then
    Result := GetMdsBP(aTimeOut, aExactSolution)
  else
    Result := GetMdsBP256(aTimeOut, aExactSolution);
end;

procedure TGSimpleGraph.ListMaxCliques(aOnFindClique: TOnFindSet);
begin
  if IsEmpty or (EdgeCount = 0) then
    exit;
  if aOnFindClique = nil then
    raise ELGraphError.Create(SECallbackMissed);
  if (VertexCount > LISTCLIQUES_BP_CUTOFF) or (Density <= MAXCLIQUE_BP_DENSITY_CUTOFF) then
    ListCliques(aOnFindClique)
  else
    if VertexCount > 256 then
      ListCliquesBP(aOnFindClique)
    else
      ListCliquesBP256(aOnFindClique);
end;

function TGSimpleGraph.MaxClique: TIntArray;
begin
  if IsEmpty or (EdgeCount = 0) then
    exit(nil);
  if (VertexCount >= MAXCLIQUE_BP_CUTOFF) or (Density <= MAXCLIQUE_BP_DENSITY_CUTOFF) then
    Result := GetMaxClique
  else
    if VertexCount > 256 then
      Result := GetMaxCliqueBP
    else
      Result := GetMaxCliqueBP256;
end;

function TGSimpleGraph.ApproxMaxClique: TIntArray;
var
  Cand, Stack, Q: TIntSet;
  I, J: SizeInt;
begin
  if IsEmpty or (EdgeCount = 0) then
    exit(nil);
  Cand.AssignArray(SortVerticesByDegeneracy);
  Cand.Reverse;
  while Cand.NonEmpty do
    begin
      I := Cand.Pop;
      {%H-}Stack.Push(I);
      {%H-}Q.MakeEmpty;
      for J in Cand do
        if AdjLists[I]^.Contains(J) then
          Q.Push(J);
      Cand.Assign(Q);
    end;
  Result := Stack.ToArray;
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
  inherited SaveToStream(aStream, aOnWriteVertex, nil);
end;

procedure TGChart.LoadFromStream(aStream: TStream; aOnReadVertex: TOnReadVertex);
begin
  inherited LoadFromStream(aStream, aOnReadVertex, nil);
end;

procedure TGChart.SaveToFile(const aFileName: string; aOnWriteVertex: TOnWriteVertex);
begin
  inherited SaveToFile(aFileName, aOnWriteVertex, nil);
end;

procedure TGChart.LoadFromFile(const aFileName: string; aOnReadVertex: TOnReadVertex);
begin
  inherited LoadFromFile(aFileName, aOnReadVertex, nil);
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

procedure TIntChart.WriteVertex(aStream: TStream; constref aValue: Integer);
begin
  aStream.WriteBuffer(NtoLE(aValue), SizeOf(aValue));
end;

procedure TIntChart.ReadVertex(aStream: TStream; out aValue: Integer);
begin
  aStream.ReadBuffer(aValue{%H-}, SizeOf(aValue));
  aValue := LEtoN(aValue);
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

procedure TIntChart.LoadFromDIMACSAsciiFile(const aFileName: string);
type
  TReaderRef = specialize TGAutoRef<TTextFileReader>;
var
  ReaderRef: TReaderRef;
  Reader: TTextFileReader;
  Line, ParseLine, Elem: string;
  I: SizeInt;
  CurrEdge: array[0..1] of SizeInt;
  Symb: AnsiChar;
begin
  Reader := ReaderRef;
  if not Reader.Open(aFileName) then
    raise ELGraphError.CreateFmt(SEUnableOpenFileFmt, [aFileName]);
  Clear;
  for Line in Reader do
    begin
      ParseLine := Trim(Line);
      Symb := LowerCase(ParseLine)[1];
      case Symb of
        'c':
          begin
            Description.Add(System.Copy(ParseLine, 3, System.Length(ParseLine)));
            continue;
          end;
        'e':
          begin
            I := 0;
            for Elem in Line.SplitSB([' ']) do
              begin
                if LowerCase(Elem) = Symb then
                  continue;
                if I > 1 then
                  begin
                    Clear;
                    raise ELGraphError.Create(SEUnexpectElem);
                  end;
                CurrEdge[I] := StrToInt(Elem);
                Inc(I);
              end;
            AddEdge(CurrEdge[0], CurrEdge[1]);
          end;
      end;
    end;
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
var
  p: TPoint;
begin
  p.X := NtoLE(aValue.X);
  p.Y := NtoLE(aValue.Y);
  aStream.WriteBuffer(p, SizeOf(p));
end;

procedure TPointsChart.ReadPoint(aStream: TStream; out aValue: TPoint);
begin
  aStream.ReadBuffer(aValue{%H-}, SizeOf(aValue));
  aValue.X := LEtoN(aValue.X);
  aValue.Y := LEtoN(aValue.Y);
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
  inherited SaveToStream(aStream, @WritePoint, nil);
end;

procedure TPointsChart.LoadFromStream(aStream: TStream);
var
  I, J: SizeInt;
begin
  inherited LoadFromStream(aStream, @ReadPoint, nil);
  for I := 0 to Pred(VertexCount) do
    for J in AdjVerticesI(I) do
      SetEdgeDataI(I, J, TRealPointEdge.Create(Distance(Items[I], Items[J])));
end;

procedure TPointsChart.SaveToFile(const aFileName: string);
begin
   inherited SaveToFile(aFileName, @WritePoint, nil);
end;

procedure TPointsChart.LoadFromFile(const aFileName: string);
begin
  inherited LoadFromFile(aFileName, @ReadPoint, nil);
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

