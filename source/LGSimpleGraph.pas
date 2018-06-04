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
{$INLINE ON}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils, math,
  LGUtils,
  {%H-}LGHelpers,
  LGArrayHelpers,
  LGStack,
  LGQueue,
  LGVector,
  LGPriorityQueue,
  LGraphUtils,
  LGStrConst;

type
  { TGSimpleUGraph: simple sparse undirected graph based on adjacency lists;
      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGSimpleUGraph<TVertex, TEdgeData, TVertexEqRel> = class(specialize TGCustomGraph
    <TVertex, TEdgeData, TVertexEqRel>)
  protected
  type
    TDistinctEdgeEnumerator = record
    private
      FVisited: TBitVector;
      FList: TVertexList.TNodeList;
      FEnum: TVertexItem.TEnumerator;
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
        FGraph: TGSimpleUGraph;
      public
        function GetEnumerator: TDistinctEdgeEnumerator;
    end;

    TCutPointHelper = record
    private
      Graph: TGSimpleUGraph;
      Visited: TBitVector;
      Lowest, InOrder: TIntArray;
      Counter: SizeInt;
      Points: PIntVector;
      procedure Init(aGraph: TGSimpleUGraph; aVector: PIntVector);
      procedure DfsR(Curr: SizeInt; Prev: SizeInt = -1);
      function  DfsRFind(Curr: SizeInt; Prev: SizeInt = -1): Boolean;
    public
      procedure Search(aGraph: TGSimpleUGraph; aVector: PIntVector; aFrom: SizeInt);
      function  ContainsAny(aGraph: TGSimpleUGraph; aFrom: SizeInt): Boolean;
    end;

    TBridgeHelper = record
      Graph: TGSimpleUGraph;
      Visited: TBitVector;
      Lowest, InOrder: TIntArray;
      Counter: SizeInt;
      Bridges: PIntEdgeVector;
      procedure Init(aGraph: TGSimpleUGraph; aVector: PIntEdgeVector);
      procedure DfsR(Curr: SizeInt; Prev: SizeInt = -1);
      function  DfsRFind(Curr: SizeInt; Prev: SizeInt = -1): Boolean;
    public
      procedure Search(aGraph: TGSimpleUGraph; aVector: PIntEdgeVector);
      function  ContainsAny(aGraph: TGSimpleUGraph): Boolean;
    end;
  var
    FCompCount: SizeInt;
    FConnected,
    FConnectedValid: Boolean;
    procedure DoRemoveVertex(aIndex: SizeInt);
    function  DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
    function  GetSeparateGraph(aIndex: SizeInt): TGSimpleUGraph;
    function  CheckPathExistsI(aSrc, aDst: SizeInt): Boolean;
    procedure CheckSeparateCount;
    function  FindSeparateCount: SizeInt;
    function  GetSeparateCount: SizeInt;
    function  CountPop(aCompIndex: SizeInt): SizeInt;
    function  MakeConnected(aOnAddEdge: TOnAddEdge): SizeInt;
    function  CycleExists(out aCycle: TIntVector): Boolean;
    function  FindCycle(aIndex: SizeInt; out aCycle: TIntVector): Boolean;
    property  Connected: Boolean read FConnected;
    property  ConnectedValid: Boolean read FConnectedValid;
  public
    procedure Clear; override;
  { returns True and vertex index, if it was added, False otherwise }
    function  AddVertex(constref aVertex: TVertex; out aIndex: SizeInt): Boolean;
    function  AddVertex(constref aVertex: TVertex): Boolean; inline;
    procedure RemoveVertex(constref aVertex: TVertex); inline;
    procedure RemoveVertexI(aVtxIndex: SizeInt);
    function  AddEdge(constref aSrc, aDst: TVertex; aData: TEdgeData): Boolean;
    function  AddEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  AddEdgeI(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  AddEdgeI(aSrc, aDst: SizeInt): Boolean; inline;
    function  RemoveEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  RemoveEdgeI(aSrc, aDst: SizeInt): Boolean;
    procedure SaveToStream(aStream: TStream; aWriteVertex: TOnWriteVertex; aWriteData: TOnWriteData);
    procedure LoadFromStream(aStream: TStream; aReadVertex: TOnReadVertex; aReadData: TOnReadData);
    procedure SaveToFile(const aFileName: string; aWriteVertex: TOnWriteVertex; aWriteData: TOnWriteData);
    procedure LoadFromFile(const aFileName: string; aReadVertex: TOnReadVertex; aReadData: TOnReadData);
    function  Degree(constref aVertex: TVertex): SizeInt; inline;
    function  DegreeI(aVtxIndex: SizeInt): SizeInt;
    function  Isolated(constref aVertex: TVertex): Boolean; inline;
    function  IsolatedI(aVtxIndex: SizeInt): Boolean; inline;
  { checks whether the graph is connected; an empty graph is considered disconnected }
    function  IsConnected: Boolean; inline;
  { if the graph is not empty, then make graph connected, adding, if necessary, new edges
    from the vertex with the index 0; returns count of added edges}
    function  EnsureConnected(aOnAddEdge: TOnAddEdge = nil): SizeInt;
    function  SimplePathExists(constref aSrc, aDst: TVertex): Boolean; inline;
    function  SimplePathExistsI(aSrc, aDst: SizeInt): Boolean;
  { returns graph of connected component that contains aVertex }
    function  SeparateGraph(constref aVertex: TVertex): TGSimpleUGraph; inline;
    function  SeparateGraphI(aVtxIndex: SizeInt): TGSimpleUGraph;
  { returns index of the connected component that contains aVertex }
    function  SeparateIndexOf(constref aVertex: TVertex): SizeInt; inline;
    function  SeparateIndexOfI(aVtxIndex: SizeInt): SizeInt;
  { returns number of Items(population) in the connected component that contains aVertex }
    function  SeparatePop(constref aVertex: TVertex): SizeInt; inline;
    function  SeparatePopI(aVtxIndex: SizeInt): SizeInt;
    function  IsTree: Boolean; inline;
    function  CyclomaticNumber: SizeInt; inline;
  { checks whether the graph is a regular graph (that is, the degree of all its vertices equal);
    an empty graph is considered regular }
    function  IsRegular: Boolean;
  { checks whether exists any cycle in graph;
    if True then aCycle will contain indices of the vertices of the cycle }
    function  ContainsCycle(out aCycle: TIntVector): Boolean;
  { checks whether exists any cycle in graph that contains aVertex;
    if True then aCycle will contain indices of the vertices of the cycle }
    function  ContainsCycle(constref aVertex: TVertex; out aCycle: TIntVector): Boolean; inline;
    function  ContainsCycleI(aVtxIndex: SizeInt; out aCycle: TIntVector): Boolean;
    function  ContainsEulerCycle: Boolean;
  { looking for some Eulerian cycle in the first connected component along the path from the first vertex }
    function  FindEulerCycle: TIntArray;
  { checks whether exists any articulation point that belong to the same connection component as aVertex;
    note: may crash with stack overflow on size ~ 300000*3 because of recursive DFS }
    function  ContainsCutPoint(constref aVertex: TVertex): Boolean; inline;
    function  ContainsCutPointI(aVtxIndex: SizeInt = 0): Boolean;
  { returns the articulation points that belong to the same connection component as aVertex, if any,
    otherwise the empty vector;
    note: crashes with stack overflow on size ~ 300000*3 because of recursive DFS }
    function  FindCutPoints(constref aVertex: TVertex): TIntVector; inline;
    function  FindCutPointsI(aVtxIndex: SizeInt = 0): TIntVector;
  { checks whether exists any bridge in graph;
    note: may crash with stack overflow on size ~ 300000*3 because of recursive DFS }
    function  ContainsBridge: Boolean;
  { returns all bridges in the form of source-destinatin pairs in the result vector, if any,
    otherwise the empty vector;
    note: crashes with stack overflow on size ~ 300000*3 because of recursive DFS}
    function  FindBridges: TIntEdgeVector;
  { checks whether the graph is biconnected; graph with single vertex is considered biconnected }
    function  IsBiconnected: Boolean; inline;
  { returns the vector of the spanning tree, which is constructed starting from aRoot;
    each element contains the index of its parent (or -1 if it is root or not connected),
    i.e. provides a pair of source - destination(Result[index] - source, index - destination) }
    function  DfsSpanningTree(constref aRoot: TVertex): TIntArray; inline;
    function  DfsSpanningTreeI(aRoot: SizeInt = 0): TIntArray;
    function  BfsSpanningTree(constref aRoot: TVertex): TIntArray; inline;
    function  BfsSpanningTreeI(aRoot: SizeInt = 0): TIntArray;
  { returns a graph constructed from the pairs provided by the vector,
    i.e. each element treates as pair of source - destination }
    function  CreateFromVector(constref aVector: TIntArray): TGSimpleUGraph;
    function  DistinctEdges: TDistinctEdges; inline;

    function  Clone: TGSimpleUGraph;
  { count of connected components }
    property  SeparateCount: SizeInt read GetSeparateCount;
  end;

  generic TGWeighedEdgeData<TWeight, TEdgeData> = record
    Weight: TWeight;
    Data: TEdgeData;
    class function Construct(constref w: TWeight; constref d: TEdgeData): TGWeighedEdgeData; static; inline;
  end;

  THandle = LGUtils.THandle;

  { TGWeighedUGraph: simple sparse undirected graph based on adjacency lists;
      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean;
      TWeight must have defined comparision operators and properties MinValue, MaxValue,
      which used as infinity weight values;
      Default(TWeight) used as zero weight value }
  generic TGWeighedUGraph<TVertex, TWeight, TEdgeData, TVertexEqRel> = class
  // must be class(specialize TGSimpleUGraph<TVertex, specialize TGWeighedEdgeData<TWeight, TEdgeData>, TVertexEqRel>)
  // but it is impossible: see #0033788
  public
  type
    TWeighedGraph   = TGWeighedUGraph;
    TWeightArray    = array of TWeight;
    TWeEdgeData     = specialize TGWeighedEdgeData<TWeight, TEdgeData>;
    TGraph          = specialize TGSimpleUGraph<TVertex, TWeEdgeData, TVertexEqRel>;
    TEdge           = TGraph.TEdge;
    TEdges          = TGraph.TEdges;
    TIntEdgeVector  = TGraph.TIntEdgeVector;
    TDistinctEdges  = TGraph.TDistinctEdges;
    TEdgeEnumerator = TGraph.TEdgeEnumerator;
    TAdjVertices    = TGraph.TAdjVertices;
    TIncidentEdges  = TGraph.TIncidentEdges;
    TVertices       = TGraph.TVertices;
    TOnAddEdge      = TGraph.TOnAddEdge;
    TOnReadVertex   = TGraph.TOnReadVertex;
    TOnWriteVertex  = TGraph.TOnWriteVertex;
    TOnReadData     = TGraph.TOnReadData;
    TOnWriteData    = TGraph.TOnWriteData;
    THeuristic      = function(constref aSrc, aDst: TVertex): TWeight;

  protected
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
    end;

    TEdgeArray   = array of TWeightEdge;
    TAdjItem     = TGraph.TAdjItem;
    PAdjItem     = TGraph.PAdjItem;

    TWeightItem = record
      Weight: TWeight;
      Index: SizeInt;
      class operator = (constref L, R: TWeightItem): Boolean; inline;
      class operator <>(constref L, R: TWeightItem): Boolean; inline;
      class operator > (constref L, R: TWeightItem): Boolean; inline;
      class operator < (constref L, R: TWeightItem): Boolean; inline;
      class operator >=(constref L, R: TWeightItem): Boolean; inline;
      class operator <=(constref L, R: TWeightItem): Boolean; inline;
      constructor Create(constref w: TWeight; aIndex: SizeInt);
    end;

    TRankItem = record
      Rank,
      Weight: TWeight;
      Index: SizeInt;
      class operator = (constref L, R: TRankItem): Boolean; inline;
      class operator <>(constref L, R: TRankItem): Boolean; inline;
      class operator > (constref L, R: TRankItem): Boolean; inline;
      class operator < (constref L, R: TRankItem): Boolean; inline;
      class operator >=(constref L, R: TRankItem): Boolean; inline;
      class operator <=(constref L, R: TRankItem): Boolean; inline;
      constructor Create(constref aRank, aWeight: TWeight; aIndex: SizeInt);
    end;

    TEdgeHelper  = specialize TGComparableArrayHelper<TWeightEdge>;
    TPairingHeap = specialize TGLiteComparablePairHeapMin<TWeightItem>;
    TAStarHeap   = specialize TGLiteComparablePairHeapMin<TRankItem>;

  { todo: Filter-Kruskal minimum spanning tree algorithm }
    TFilterKruskal = record
    private
    var
      FEdges: TEdgeArray;
      FTree: TIntArray;
      FDsu: TDisjointSetUnion;
      FTreshold,
      FCount,
      FFound: SizeInt;
      FWeight: TWeight;
      procedure Kruskal(L, R: SizeInt);
      function  Split(L, R: SizeInt): SizeInt;
      procedure FilterKruskal(L, R: SizeInt);
    public
      function  FindMst(aGraph: TWeighedGraph; out aWeight: TWeight): TIntArray;
    end;
  strict private
  class var
    CFInfiniteWeight,
    CFNegInfiniteWeight,
    CFZeroWeight: TWeight;
  protected
  class var
    CFData: TEdgeData;

  var
    FGraph: TGraph;
    function  GetEdgeCount: SizeInt; inline;
    function  GetTitle: string; inline;
    function  GetVertex(aIndex: SizeInt): TVertex; inline;
    procedure SetVertex(aIndex: SizeInt; const aValue: TVertex); inline;
    function  GetVertexCount: SizeInt; inline;
    function  GetSeparateCount: SizeInt; inline;
    procedure SetTitle(const aValue: string); inline;
  { Dijkstra's algorithm: single-source shortest paths problem for non-negative weights  }
    function  DijkstraSssp(aSrc: SizeInt): TWeightArray;
    function  DijkstraSssp(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
  { Dijkstra's pathfinding algorithm }
    function  DijkstraPath(aSrc, aDst: SizeInt): TWeight;
    function  DijkstraPath(aSrc, aDst: SizeInt; out aWeight: TWeight): TIntVector;
  { A* pathfinding algorithm }
    function  AStar(aSrc, aDst: SizeInt; aHeur: THeuristic; out aWeight: TWeight): TIntVector;
  { Bellman-Ford algorithm: single-source shortest paths problem for any weights  }
    function  FordBellman(aSrc: SizeInt; out aWeights: TWeightArray): Boolean;
    function  FordBellman(aSrc: SizeInt; out aPaths: TIntArray; out aWeights: TWeightArray): Boolean;
    function  FilterKruskalMst(out aTotalWeight: TWeight): TIntArray;
    function  KruskalMst(out aTotalWeight: TWeight): TIntArray;
    function  PrimMst(out aTotalWeight: TWeight): TIntArray;
    function  CreateWeightVector: TWeightArray;
    function  CreateEdgeArray: TEdgeArray;
    class function Min(const L, R: TWeight): TWeight; static; inline;
    class constructor Init;
  public
    class property InfiniteWeight: TWeight read CFInfiniteWeight;
    class property NegInfiniteWeight: TWeight read CFNegInfiniteWeight;
    class property ZeroWeight: TWeight read CFZeroWeight;

    constructor Create;
    constructor Create(g: TGraph);
    destructor Destroy; override;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure Clear; inline;
    procedure EnsureCapacity(aValue: SizeInt); inline;
    procedure TrimToFit; inline;
    function  ContainsVertex(constref aVertex: TVertex): Boolean; inline;
    function  ContainsEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  ContainsEdgeI(aSrc, aDst: SizeInt): Boolean;
    function  ContainsEdgeI(aSrc, aDst: SizeInt; out aData: TWeEdgeData): Boolean;
    function  IndexOf(constref aVertex: TVertex): SizeInt; inline;
    function  Adjacent(constref aSrc, aDst: TVertex): Boolean; inline;
    function  AdjacentI(aSrc, aDst: SizeInt): Boolean; inline;
  { enumerates indices of adjacent vertices }
    function  AdjVertices(constref aSrc: TVertex): TAdjVertices; inline;
    function  AdjVerticesI(aSrc: SizeInt): TAdjVertices; inline;
  { enumerates incident edges }
    function  IncidentEdges(constref aSrc: TVertex): TIncidentEdges; inline;
    function  IncidentEdgesI(aSrc: SizeInt): TIncidentEdges; inline;
  { enumerates all vertices }
    function  Vertices: TVertices; inline;
  { enumerates all edges(including the reverse ones) }
    function  Edges: TEdges; inline;
  { enumerates all edges(only once) }
    function  DistinctEdges: TDistinctEdges; inline;
    function  AddVertex(constref aVertex: TVertex; out aIndex: SizeInt): Boolean; inline;
    function  AddVertex(constref aVertex: TVertex): Boolean; inline;
    procedure RemoveVertex(constref aVertex: TVertex); inline;
    procedure RemoveVertexI(aVtxIndex: SizeInt); inline;
    function  AddEdge(constref aSrc, aDst: TVertex; aWeight: TWeight; aData: TEdgeData): Boolean; inline;
    function  AddEdge(constref aSrc, aDst: TVertex; aWeight: TWeight): Boolean; inline;
    function  AddEdge(constref aSrc, aDst: TVertex; aData: TWeEdgeData): Boolean; inline;
    function  AddEdgeI(aSrc, aDst: SizeInt; aWeight: TWeight; aData: TEdgeData): Boolean; inline;
    function  AddEdgeI(aSrc, aDst: SizeInt; aWeight: TWeight): Boolean; inline;
    function  AddEdgeI(aSrc, aDst: SizeInt; aData: TWeEdgeData): Boolean; inline;
    function  RemoveEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  RemoveEdgeI(aSrc, aDst: SizeInt): Boolean; inline;
    function  GetEdgeData(constref aSrc, aDst: TVertex): TWeEdgeData; inline;
    function  GetEdgeDataI(aSrc, aDst: SizeInt): TWeEdgeData; inline;
    procedure SetEdgeData(constref aSrc, aDst: TVertex; constref aValue: TWeEdgeData); inline;
    procedure SetEdgeDataI(aSrc, aDst: SizeInt; constref aValue: TWeEdgeData); inline;
    procedure SaveToStream(aStream: TStream; aWriteVertex: TOnWriteVertex; aWriteData: TOnWriteData); inline;
    procedure LoadFromStream(aStream: TStream; aReadVertex: TOnReadVertex; aReadData: TOnReadData); inline;
    procedure SaveToFile(const aFileName: string; aWriteVertex: TOnWriteVertex; aWriteData: TOnWriteData); inline;
    procedure LoadFromFile(const aFileName: string; aReadVertex: TOnReadVertex; aReadData: TOnReadData); inline;
  { returns count of visited vertices; aOnGray calls after vertex visite, aOnWhite calls after vertex found;
    if aOnGray returns True then traversal stops }
    function  DfsTraversal(constref aRoot: TVertex; aOnGray: TOnIntTest = nil; aOnWhite: TOnIntVisit = nil): SizeInt; inline;
    function  DfsTraversalI(aRoot: SizeInt; aOnGray: TOnIntTest = nil; aOnWhite: TOnIntVisit = nil): SizeInt; inline;
  { returns count of visited vertices; aOnGray calls after vertex visite, aOnWhite calls after vertex found;
    if aOnGray returns True then traversal stops}
    function  BfsTraversal(constref aRoot: TVertex; aOnGray: TOnIntTest = nil; aOnWhite: TOnIntVisit = nil): SizeInt; inline;
    function  BfsTraversalI(aRoot: SizeInt; aOnGray: TOnIntTest = nil; aOnWhite: TOnIntVisit = nil): SizeInt; inline;

    function  SimplePathExists(constref aSrc, aDst: TVertex): Boolean; inline;
    function  SimplePathExistsI(aSrc, aDst: SizeInt): Boolean; inline;
  { test whether the graph is bipartite;
    the graph can be disconnected (in this case it consists of a number of connected
    bipartite components and / or several isolated vertices)}
    function  IsBipartite: Boolean; inline;
  { test whether the graph is bipartite; if returns True then information about the vertex
    belonging to the fractions is returned in v(0 or 1) }
    function  IsBipartite(out v: TShortArray): Boolean; inline;
  { returns the length of the shortest path(in sense 'edges count') between the vertices aSrc and aDst,
    -1 if the path does not exist }
    function  ShortestPathLen(constref aSrc, aDst: TVertex): SizeInt; inline;
    function  ShortestPathLenI(aSrc, aDst: SizeInt): SizeInt; inline;
  { returns a vector containing in the corresponding components the length of shortest path from aRoot,
    (in sense 'edges count')}
    function  ShortestPathsMap(constref aRoot: TVertex): TIntArray; inline;
    function  ShortestPathsMapI(aRoot: SizeInt = 0): TIntArray; inline;
  { returns a vector containing indices of found shortest path(in sense 'edges count'),
    empty if path does not exists }
    function  ShortestPath(constref aSrc, aDst: TVertex): TIntVector; inline;
    function  ShortestPathI(aSrc, aDst: SizeInt): TIntVector; inline;
    function  Degree(constref aVertex: TVertex): SizeInt; inline;
    function  DegreeI(aIndex: SizeInt): SizeInt; inline;
    function  Isolated(constref v: TVertex): Boolean; inline;
    function  IsolatedI(aIndex: SizeInt): Boolean; inline;
  { checks whether the graph is connected; a graph without vertices is considered disconnected }
    function  IsConnected: Boolean; inline;
  { if the graph is not empty, then make graph connected, adding, if necessary, new edges
    from the vertex with the index 0; returns count of added edges }
    function  EnsureConnected(aOnAddEdge: TOnAddEdge = nil): SizeInt; inline;
  { returns index of the connected component that contains v }
    function  SeparateIndex(constref aVertex: TVertex): SizeInt; inline;
    function  SeparateIndexI(aVtxIndex: SizeInt): SizeInt; inline;
  { returns number of vertices(population) in the connected component that contains aVertex }
    function  SeparatePop(constref aVertex: TVertex): SizeInt; inline;
    function  SeparatePopI(aVtxIndex: SizeInt): SizeInt; inline;
  { returns graph of connected component that contains aVertex }
    function  SeparateGraph(constref aVertex: TVertex): TWeighedGraph; inline;
    function  SeparateGraphI(aVtxIndex: SizeInt): TWeighedGraph; inline;
  { checks whether the graph is a regular graph (that is, the degree of all its vertices equal);
    an empty graph is considered regular }
    function  IsRegular: Boolean; inline;
    function  IsTree: Boolean; inline;
    function  ContainsCycle(out aCycle: TIntVector): Boolean; inline;
  { checks whether exists any cycle in graph that contains v;
    if True then aCycle will contain indices of cycle }
    function  ContainsCycle(constref aVertex: TVertex; out aCycle: TIntVector): Boolean; inline;
    function  ContainsCycleI(aVtxIndex: SizeInt; out aCycle: TIntVector): Boolean; inline;
  { checks whether exists any articulation point that belong to the same connection component as aVertex;
    note: may crash with stack overflow on size ~ 300000*3 because of recursive DFS }
    function  ContainsCutPoint(constref aVertex: TVertex): Boolean; inline;
    function  ContainsCutPointI(aVtxIndex: SizeInt = 0): Boolean; inline;
  { returns the articulation points that belong to the same connection component as aVertex, if any,
    otherwise the empty vector;
    note: crashes with stack overflow on size ~ 300000*3 because of recursive DFS}
    function  FindCutPoints(constref aVertex: TVertex): TIntVector;
    function  FindCutPointsI(aVtxIndex: SizeInt = 0): TIntVector;
  { checks whether exists any bridge in graph;
    note: may crash with stack overflow on size ~ 300000*3 because of recursive DFS }
    function  ContainsBridge: Boolean; inline;
  { returns bridges in the form of source-destinatin pairs in the result vector, if any,
    otherwise the empty vector;
    note: crashes with stack overflow on size ~ 300000*3 because of recursive DFS}
    function  FindBridges: TIntEdgeVector; inline;
  { checks whether the graph is biconnected; graph with single vertex is considered biconnected }
    function  IsBiconnected: Boolean; inline;
    function  ContainsEulerCycle: Boolean; inline;
    function  FindEulerCycle: TIntArray; inline;
  { returns the vector of the spanning tree, which is constructed starting from aRoot;
    each element contains the index of its parent (or -1 if it is root or not connected),
    that is, it is a source-destination pair }
    function  DfsSpanningTree(constref aRoot: TVertex): TIntArray; inline;
    function  DfsSpanningTreeI(aRoot: SizeInt = 0): TIntArray; inline;
    function  BfsSpanningTree(constref aRoot: TVertex): TIntArray; inline;
    function  BfsSpanningTreeI(aRoot: SizeInt = 0): TIntArray; inline;
  { returns a graph constructed from the edges provided by the vector,
    i.e. each element treates as pair of source - destination }
    function  CreateFromArray(constref aArray: TIntArray): TWeighedGraph;
  { returns True if exists edge with negative weight }
    function  ContainsNegWeighedEdge: Boolean;
  { finds the paths of minimal weight from a given vertex to the remaining vertices in the same
    connected component(SSSP), the weights of all edges must be nonnegative;
    the result contains in the corresponding component the weight of the path to the vertex or
    InfiniteWeight if the vertex is unreachable; used Dijkstra's algorithm  }
    function  MinPathsMap(constref aSrc: TVertex): TWeightArray; inline;
    function  MinPathsMapI(aSrc: SizeInt): TWeightArray;
  { same as above and in aPathTree returns paths }
    function  MinPathsMap(constref aSrc: TVertex; out aPathTree: TIntArray): TWeightArray; inline;
    function  MinPathsMapI(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray; inline;
  { finds the path of minimal weight from a aSrc to aDst if it exists(pathfinding);
    the weights of all edges must be nonnegative;
    returns path weight or InfiniteWeight if the vertex is unreachable; used Dijkstra's algorithm  }
    function  MinPathWeight(constref aSrc, aDst: TVertex): TWeight; inline;
    function  MinPathWeightI(aSrc, aDst: SizeInt): TWeight;
  { returns the path of minimal weight from a aSrc to aDst, if exists, and it weight in aWeight }
    function  MinPath(constref aSrc, aDst: TVertex; out aWeight: TWeight): TIntVector; inline;
    function  MinPathI(aSrc, aDst: SizeInt; out aWeight: TWeight): TIntVector;
  { finds the path of minimal weight from a aSrc to aDst if it exists(pathfinding);
    the weights of all edges must be nonnegative;
    the result contains shortest path weight or InfiniteWeight if the vertex is unreachable;
    used A* algorithm  }
    function  MinPathAStar(constref aSrc, aDst: TVertex; aHeur: THeuristic; out aWeight: TWeight): TIntVector; inline;
    function  MinPathAStarI(aSrc, aDst: SizeInt; aHeur: THeuristic; out aWeight: TWeight): TIntVector;
  { finds the paths of minimal weight from a given vertex to the remaining vertices in the same
    connected component(SSSP), the weights of the edges can be negative; NO IT IS NOT FOR UNDIRECTED GRAPHS !!!
    returns False and empty aWeights if there is a negative weight cycle, otherwise
    aWeights will contain in the corresponding component the weight of the minimum path to the vertex or
    InfiniteWeight if the vertex is unreachable; used Bellman–Ford algorithm  }
    function  FindMinPathsMap(constref aSrc: TVertex; out aWeights: TWeightArray): Boolean; inline;
    function  FindMinPathsMapI(aSrc: SizeInt; out aWeights: TWeightArray): Boolean;
  { same as above and in aPaths returns paths,
    if there is a negative weight cycle, then aPaths will contain that cycle }
    function  FindMinPathsMap(constref aSrc: TVertex; out aPaths: TIntArray; out aWeights: TWeightArray): Boolean; inline;
    function  FindMinPathsMapI(aSrc: SizeInt; out aPaths: TIntArray; out aWeights: TWeightArray): Boolean;
  { finds a spanning tree of minimal weight, the graph must be connected(Kruskal's algorithm used)}
    function  MinSpanningTreeKrus(out aTotalWeight: TWeight): TIntArray;
  { finds a spanning tree of minimal weight, the graph must be connected(Prim's algorithm used) }
    function  MinSpanningTreePrim(out aTotalWeight: TWeight): TIntArray;

    function  Clone: TWeighedGraph;
    property  Title: string read GetTitle write SetTitle;
    property  VertexCount: SizeInt read GetVertexCount;
    property  EdgeCount: SizeInt read GetEdgeCount;
  { count of connected components }
    property  SeparateCount: SizeInt read GetSeparateCount;
    property  Items[aIndex: SizeInt]: TVertex read GetVertex write SetVertex; default;
  end;

implementation
{$B-}{$COPERATORS ON}
uses
  bufstream;

{ TGSimpleUGraph.TDistinctEdgeEnumerator }

function TGSimpleUGraph.TDistinctEdgeEnumerator.GetCurrent: TEdge;
begin
  Result := TEdge.Create(FCurrIndex, FEnum.Current);
end;

function TGSimpleUGraph.TDistinctEdgeEnumerator.MoveNext: Boolean;
begin
  repeat
    if FEnumDone then
      begin
        if FCurrIndex >= FLastIndex then
          exit(False);
        Inc(FCurrIndex);
        FVisited[FCurrIndex] := True;
        FEnum := FList[FCurrIndex].Item.GetEnumerator;
      end;
    Result := FEnum.MoveNext;
    FEnumDone := not Result;
    if Result then
      Result := not FVisited[FEnum.GetCurrent^.Key];
  until Result;
end;

procedure TGSimpleUGraph.TDistinctEdgeEnumerator.Reset;
begin
  FCurrIndex := -1;
  FEnumDone := True;
  FVisited.ClearBits;
end;

{ TGSimpleUGraph.TDistinctEdges }

function TGSimpleUGraph.TDistinctEdges.GetEnumerator: TDistinctEdgeEnumerator;
begin
  Result.FList := FGraph.FVertexList.FNodeList;
  Result.FLastIndex := Pred(FGraph.FVertexList.Count);
  Result.FVisited.Size := Succ(Result.FLastIndex);
  Result.FCurrIndex := -1;
  Result.FEnumDone := True;
end;

{ TGSimpleUGraph.TCutPointHelper }

procedure TGSimpleUGraph.TCutPointHelper.Init(aGraph: TGSimpleUGraph; aVector: PIntVector);
begin
  Graph := aGraph;
  Counter := Graph.VertexCount;
  Visited.Size := Counter;
  Lowest := CreateIntArray(Counter);
  InOrder := CreateIntArray(Counter);
  Points := aVector;
  Counter := 0;
end;

procedure TGSimpleUGraph.TCutPointHelper.DfsR(Curr: SizeInt; Prev: SizeInt);
var
  Next, ChildCount: SizeInt;
begin
  Visited[Curr] := True;
  InOrder[Curr] := Counter;
  Lowest[Curr] := Counter;
  Inc(Counter);
  ChildCount := 0;
  for Next in Graph.AdjVerticesI(Curr) do
    if Next <> Prev then
      if not Visited[Next] then
        begin
          DfsR(Next, Curr);
          Lowest[Curr] := Math.Min(Lowest[Curr], Lowest[Next]);
          if (Lowest[Next] >= InOrder[Curr]) and (Prev <> -1) then
            Points^.Add(Curr);
          Inc(ChildCount);
        end
      else
        Lowest[Curr] := Math.Min(Lowest[Curr], InOrder[Next]);
  if (Prev = -1) and (ChildCount > 1) then
    Points^.Add(Curr);
end;

function TGSimpleUGraph.TCutPointHelper.DfsRFind(Curr: SizeInt; Prev: SizeInt): Boolean;
var
  Next, ChildCount: SizeInt;
begin
  Visited[Curr] := True;
  InOrder[Curr] := Counter;
  Lowest[Curr] := Counter;
  Inc(Counter);
  ChildCount := 0;
  for Next in Graph.AdjVerticesI(Curr) do
    if Next <> Prev then
      if not Visited[Next] then
        begin
          if DfsRFind(Next, Curr) then
            exit(True);
          Lowest[Curr] := Math.Min(Lowest[Curr], Lowest[Next]);
          if (Lowest[Next] >= InOrder[Curr]) and (Prev <> -1) then
            exit(True);
          Inc(ChildCount);
        end
      else
        Lowest[Curr] := Math.Min(Lowest[Curr], InOrder[Next]);
  if (Prev = -1) and (ChildCount > 1) then
    exit(True);
  Result := False;
end;

procedure TGSimpleUGraph.TCutPointHelper.Search(aGraph: TGSimpleUGraph; aVector: PIntVector; aFrom: SizeInt);
begin
  Init(aGraph, aVector);
  DfsR(aFrom);
end;

function TGSimpleUGraph.TCutPointHelper.ContainsAny(aGraph: TGSimpleUGraph; aFrom: SizeInt): Boolean;
begin
  Init(aGraph, nil);
  Result := DfsRFind(aFrom);
end;

{ TGSimpleUGraph.TBridgeHelper }

procedure TGSimpleUGraph.TBridgeHelper.Init(aGraph: TGSimpleUGraph; aVector: PIntEdgeVector);
begin
  Graph := aGraph;
  Counter := aGraph.VertexCount;
  Visited.Size := Counter;
  Lowest := CreateIntArray(Counter);
  InOrder := CreateIntArray(Counter);
  Bridges := aVector;
  Counter := 0;
end;

procedure TGSimpleUGraph.TBridgeHelper.DfsR(Curr: SizeInt; Prev: SizeInt);
var
  Next: SizeInt;
begin
  Visited[Curr] := True;
  InOrder[Curr] := Counter;
  Lowest[Curr] := Counter;
  Inc(Counter);
  for Next in Graph.AdjVerticesI(Curr) do
    if Next <> Prev then
      if not Visited[Next] then
        begin
          DfsR(Next, Curr);
          Lowest[Curr] := Math.Min(Lowest[Curr], Lowest[Next]);
          if Lowest[Next] > InOrder[Curr] then
            Bridges^.Add(TIntEdge.Create(Curr, Next));
        end
      else
        Lowest[Curr] := Math.Min(Lowest[Curr], InOrder[Next]);
end;

function TGSimpleUGraph.TBridgeHelper.DfsRFind(Curr: SizeInt; Prev: SizeInt): Boolean;
var
  Next: SizeInt;
begin
  Visited[Curr] := True;
  InOrder[Curr] := Counter;
  Lowest[Curr] := Counter;
  Inc(Counter);
  for Next in Graph.AdjVerticesI(Curr) do
    if Next <> Prev then
      if not Visited[Next] then
        begin
          if DfsRFind(Next, Curr) then
            exit(True);
          Lowest[Curr] := Math.Min(Lowest[Curr], Lowest[Next]);
          if Lowest[Next] > InOrder[Curr] then
            exit(True);
        end
      else
        Lowest[Curr] := Math.Min(Lowest[Curr], InOrder[Next]);
  Result := False;
end;

procedure TGSimpleUGraph.TBridgeHelper.Search(aGraph: TGSimpleUGraph; aVector: PIntEdgeVector);
var
  I: SizeInt;
begin
  Init(aGraph, aVector);
  for I := 0 to Pred(Graph.VertexCount) do
    if not Visited[I] then
      DfsR(I);
end;

function TGSimpleUGraph.TBridgeHelper.ContainsAny(aGraph: TGSimpleUGraph): Boolean;
var
  I: SizeInt;
begin
  Init(aGraph, nil);
  for I := 0 to Pred(Graph.VertexCount) do
    if not Visited[I] then
      if DfsRFind(I) then
        exit(True);
  Result := False;
end;

{ TGSimpleUGraph }

procedure TGSimpleUGraph.DoRemoveVertex(aIndex: SizeInt);
var
  CurrEdges: TVertexItem.TAdjItemArray;
  I, J: SizeInt;
begin
  FEdgeCount -= FVertexList.ItemRefs[aIndex]^.Count;
  FVertexList.Delete(aIndex);
  FConnectedValid := False;
  for I := 0 to Pred(FVertexList.Count) do
    begin
      CurrEdges := FVertexList.ItemRefs[I]^.ToArray;
      FVertexList.FNodeList[I].Item.MakeEmpty;
      for J := 0 to System.High(CurrEdges) do
        begin
          if CurrEdges[J].Destination <> aIndex then
            begin
              if CurrEdges[J].Destination > aIndex then
                Dec(CurrEdges[J].Destination);
              FVertexList.ItemRefs[I]^.Add(CurrEdges[J]);
            end;
        end;
    end;
end;

function TGSimpleUGraph.DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
begin
  if aSrc = aDst then
    exit(False);
  Result := FVertexList.ItemRefs[aSrc]^.Add(TAdjItem.Create(aDst, aData));
  if Result then
    begin
      if not FVertexList.ItemRefs[aDst]^.Add(TAdjItem.Create(aSrc, aData)) then
        raise ELGraphError.Create(SEGrapInconsist);
      Inc(FEdgeCount);
      FConnectedValid := False;
    end;
end;

function TGSimpleUGraph.DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
begin
  if aSrc = aDst then
    exit(False);
  Result := FVertexList.ItemRefs[aSrc]^.Remove(aDst);
  if Result then
    begin
      FVertexList.ItemRefs[aDst]^.Remove(aSrc);
      Dec(FEdgeCount);
      FConnectedValid := False;
    end;
end;

function TGSimpleUGraph.GetSeparateGraph(aIndex: SizeInt): TGSimpleUGraph;
var
  Visited: TBitVector;
  Stack: TIntStack;
  Dst, Src: SizeInt;
begin
  Result := TGSimpleUGraph.Create;
  Visited.Size := VertexCount;
  Src := aIndex;
  {%H-}Stack.Push(aIndex);
  repeat
    if not Visited[Src] then
      begin
        Visited[Src] := True;
        for Dst in AdjVerticesI(Src) do
          if not Visited[Dst] then
            begin
              Result.AddEdge(Items[Src], Items[Dst], GetEdgeDataPtr(Src, Dst)^);
              Stack.Push(Dst);
            end;
      end;
  until not Stack.TryPop(Src);
end;

function TGSimpleUGraph.CheckPathExistsI(aSrc, aDst: SizeInt): Boolean;
var
  Visited: TBitVector;
  Stack: TIntStack;
begin
  Visited.Size := VertexCount;
  repeat
    if not Visited[aSrc] then
      begin
        if aSrc = aDst then
        Visited[aSrc] := True;
        for aSrc in AdjVerticesI(aSrc) do
          if not Visited[aSrc] then
            Stack.Push(aSrc);
      end;
  until not Stack.TryPop(aSrc);
  Result := False;
end;

procedure TGSimpleUGraph.CheckSeparateCount;
begin
  FCompCount := FindSeparateCount;
  FConnectedValid := True;
  FConnected := FCompCount = 1;
end;

function TGSimpleUGraph.FindSeparateCount: SizeInt;
var
  Visited: TBitVector;
  Stack: TIntStack;
  I, Curr: SizeInt;
begin
  Result := 0;
  Visited.Size := VertexCount;
  for I := 0 to Pred(VertexCount) do
    if not Visited[I] then
      begin
        Curr := I;
        repeat
          if not Visited[Curr] then
            begin
              Visited[Curr] := True;
              FVertexList.ItemRefs[Curr]^.FCompIndex := Result;
              for Curr in AdjVerticesI(Curr) do
                if not Visited[Curr] then
                  Stack.Push(Curr);
            end;
        until not Stack.TryPop(Curr);
        Inc(Result);
      end;
end;

function TGSimpleUGraph.GetSeparateCount: SizeInt;
begin
  if not ConnectedValid then
    CheckSeparateCount;
  Result := FCompCount;
end;

function TGSimpleUGraph.CountPop(aCompIndex: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I := 0 to Pred(VertexCount) do
    Result += Ord(FVertexList.ItemRefs[I]^.FCompIndex = aCompIndex);
end;

function TGSimpleUGraph.MakeConnected(aOnAddEdge: TOnAddEdge): SizeInt;
var
  Visited: TBitVector;
  Stack: TIntStack;
  I, Curr: SizeInt;
  d: TEdgeData;
begin
  Result := 0;
  Visited.Size := VertexCount;
  for I := 0 to Pred(VertexCount) do
    if not Visited[I] then
      begin
        Curr := I;
        repeat
          if not Visited[Curr] then
            begin
              Visited[Curr] := True;
              FVertexList.ItemRefs[Curr]^.FCompIndex := 0;
              for Curr in AdjVerticesI(Curr) do
                if not Visited[Curr] then
                  Stack.Push(Curr);
            end;
        until not Stack.TryPop(Curr);
        Inc(Result);
        if Result > 1 then
          begin
            if Assigned(aOnAddEdge) then
              begin
                aOnAddEdge(Items[0], Items[Curr], @d);
                AddEdgeI(0, Curr, d);
              end
            else
              AddEdgeI(0, Curr);
          end;
      end;
  FCompCount := 1;
  FConnectedValid := True;
  FConnected := True;
end;

function TGSimpleUGraph.CycleExists(out aCycle: TIntVector): Boolean;
var
  Stack: TIntStack;
  Visited: TBitVector;
  v: TIntArray;
  Curr, Next: SizeInt;
begin
  Visited.Size := VertexCount;
  v := CreateIntArray;
  Curr := 0;
  repeat
    if not Visited[Curr] then
      begin
        Visited[Curr] := True;
        for Next in AdjVerticesI(Curr) do
          if not Visited[Next] then
            begin
              Stack.Push(Next);
              v[Next] := Curr;
            end
          else
            if v[Curr] <> Next then
              begin
                aCycle := CycleChainFromTree(v, Next, Curr);
                exit(True);
              end;
      end;
  until not Stack.TryPop(Curr);
  Result := False;
end;

function TGSimpleUGraph.FindCycle(aIndex: SizeInt; out aCycle: TIntVector): Boolean;
var
  Stack: TIntStack;
  Visited: TBitVector;
  v: TIntArray;
  Curr, Next: SizeInt;
begin
  Visited.Size := VertexCount;
  v := CreateIntArray;
  Curr := 0;
  repeat
    if not Visited[Curr] then
      begin
        Visited[Curr] := True;
        for Next in AdjVerticesI(Curr) do
          if not Visited[Next] then
            begin
              Stack.Push(Next);
              v[Next] := Curr;
            end
          else
            if (v[Curr] <> Next) and (Curr = aIndex) then
              begin
                aCycle := CycleChainFromTree(v, Next, Curr);
                exit(True);
              end;
      end;
  until not Stack.TryPop(Curr);
  Result := False;
end;

procedure TGSimpleUGraph.Clear;
begin
  inherited;
  FCompCount := 0;
  FConnected := False;
  FConnectedValid := False;
end;

function TGSimpleUGraph.AddVertex(constref aVertex: TVertex; out aIndex: SizeInt): Boolean;
begin
  Result := not FVertexList.FindOrAdd(aVertex, aIndex);
  if Result then
    begin
      FVertexList.ItemRefs[aIndex]^.FCompIndex := -1;
      FConnectedValid := False;
    end;
end;

function TGSimpleUGraph.AddVertex(constref aVertex: TVertex): Boolean;
var
  Dummy: SizeInt;
begin
  Result := AddVertex(aVertex, Dummy);
end;

procedure TGSimpleUGraph.RemoveVertex(constref aVertex: TVertex);
begin
  RemoveVertexI(FVertexList.IndexOf(aVertex));
end;

procedure TGSimpleUGraph.RemoveVertexI(aVtxIndex: SizeInt);
begin
  FVertexList.CheckIndexRange(aVtxIndex);
  DoRemoveVertex(aVtxIndex);
end;

function TGSimpleUGraph.AddEdge(constref aSrc, aDst: TVertex; aData: TEdgeData): Boolean;
var
  SrcIdx, DstIdx: SizeInt;
begin
  AddVertex(aSrc, SrcIdx);
  AddVertex(aDst, DstIdx);
  Result := DoAddEdge(SrcIdx, DstIdx, aData);
end;

function TGSimpleUGraph.AddEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := AddEdge(aSrc, aDst, CFData);
end;

function TGSimpleUGraph.AddEdgeI(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
begin
  FVertexList.CheckIndexRange(aSrc);
  FVertexList.CheckIndexRange(aDst);
  Result := DoAddEdge(aSrc, aDst, aData);
end;

function TGSimpleUGraph.AddEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := AddEdgeI(aSrc, aDst, CFData);
end;

function TGSimpleUGraph.RemoveEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := RemoveEdgeI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGSimpleUGraph.RemoveEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  FVertexList.CheckIndexRange(aSrc);
  FVertexList.CheckIndexRange(aDst);
  Result := DoRemoveEdge(aSrc, aDst);
end;

procedure TGSimpleUGraph.SaveToStream(aStream: TStream; aWriteVertex: TOnWriteVertex; aWriteData: TOnWriteData);
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
    Header.Magic := LGRAPH_MAGIC;
    Header.Version := CURRENT_VERSION;
    Header.TitleSize := System.Length(Title);
    Header.VertexCount := VertexCount;
    Header.EdgeCount := EdgeCount;
    wbs.WriteBuffer(Header, SizeOf(Header));
    //write title
    if Header.TitleSize > 0 then
      wbs.WriteBuffer(FTitle[1], Header.TitleSize);
    //write Items, but does not save any info about connected
    //this should allow transfer data between directed/undirected graphs ???
    //or need save edges from dfs ???
    for I := 0 to Pred(Header.VertexCount) do
      aWriteVertex(wbs, FVertexList.ItemRefs[I]^.Vertex);
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

procedure TGSimpleUGraph.LoadFromStream(aStream: TStream; aReadVertex: TOnReadVertex; aReadData: TOnReadData);
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
    if Header.Magic <> LGRAPH_MAGIC then
      raise ELGraphError.Create(SEUnknownGraphStreamFmt);
    if Header.Version > CURRENT_VERSION then
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
        Vertex := aReadVertex(rbs);
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
        Edge.Data := aReadData(rbs);
        AddEdgeI(Edge.Source, Edge.Destination, Edge.Data);
      end;
  finally
    rbs.Free;
  end;
end;

procedure TGSimpleUGraph.SaveToFile(const aFileName: string; aWriteVertex: TOnWriteVertex;
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

procedure TGSimpleUGraph.LoadFromFile(const aFileName: string; aReadVertex: TOnReadVertex; aReadData: TOnReadData);
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

function TGSimpleUGraph.Degree(constref aVertex: TVertex): SizeInt;
begin
  Result := DegreeI(FVertexList.IndexOf(aVertex));
end;

function TGSimpleUGraph.DegreeI(aVtxIndex: SizeInt): SizeInt;
begin
  FVertexList.CheckIndexRange(aVtxIndex);
  Result := FVertexList.ItemRefs[aVtxIndex]^.Count;
end;

function TGSimpleUGraph.Isolated(constref aVertex: TVertex): Boolean;
begin
  Result := Degree(aVertex) = 0;
end;

function TGSimpleUGraph.IsolatedI(aVtxIndex: SizeInt): Boolean;
begin
  Result := DegreeI(aVtxIndex) = 0;
end;

function TGSimpleUGraph.IsConnected: Boolean;
begin
  if not ConnectedValid then
    CheckSeparateCount;
  Result := Connected;
end;

function TGSimpleUGraph.EnsureConnected(aOnAddEdge: TOnAddEdge): SizeInt;
begin
  Result := 0;
  if VertexCount < 2 then
    exit;
  if ConnectedValid and Connected then
    exit;
  Result += MakeConnected(aOnAddEdge);
end;

function TGSimpleUGraph.SimplePathExists(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := SimplePathExistsI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGSimpleUGraph.SimplePathExistsI(aSrc, aDst: SizeInt): Boolean;
begin
  FVertexList.CheckIndexRange(aSrc);
  FVertexList.CheckIndexRange(aDst);
  if aSrc = aDst then
    exit(False);
  //if ConnectedValid then
  //  Result := FVertexList.ItemRefs[aSrc]^.FCompIndex = FVertexList.ItemRefs[aDst]^.FCompIndex
  //else
  //  Result := CheckPathExistsI(aSrc, aDst);
  if not ConnectedValid then
    CheckSeparateCount;
  Result := FVertexList.ItemRefs[aSrc]^.FCompIndex = FVertexList.ItemRefs[aDst]^.FCompIndex;
end;

function TGSimpleUGraph.SeparateGraph(constref aVertex: TVertex): TGSimpleUGraph;
begin
  Result := SeparateGraphI(FVertexList.IndexOf(aVertex));
end;

function TGSimpleUGraph.SeparateGraphI(aVtxIndex: SizeInt): TGSimpleUGraph;
begin
  if SeparateCount > 1 then
    Result := GetSeparateGraph(aVtxIndex)
  else
    Result := Clone;
end;

function TGSimpleUGraph.SeparateIndexOf(constref aVertex: TVertex): SizeInt;
begin
   Result := SeparateIndexOfI(FVertexList.IndexOf(aVertex));
end;

function TGSimpleUGraph.SeparateIndexOfI(aVtxIndex: SizeInt): SizeInt;
begin
  FVertexList.CheckIndexRange(aVtxIndex);
  if SeparateCount > 1 then
    Result := FVertexList.ItemRefs[aVtxIndex]^.FCompIndex
  else
    Result := 0;
end;

function TGSimpleUGraph.SeparatePop(constref aVertex: TVertex): SizeInt;
begin
  Result := SeparatePopI(FVertexList.IndexOf(aVertex));
end;

function TGSimpleUGraph.SeparatePopI(aVtxIndex: SizeInt): SizeInt;
begin
  FVertexList.CheckIndexRange(aVtxIndex);
  if SeparateCount > 1 then
    Result := CountPop(FVertexList.ItemRefs[aVtxIndex]^.FCompIndex)
  else
    Result := VertexCount;
end;

function TGSimpleUGraph.IsTree: Boolean;
begin
  Result := (EdgeCount = Pred(VertexCount)) and IsConnected;
end;

function TGSimpleUGraph.CyclomaticNumber: SizeInt;
begin
  Result := EdgeCount - VertexCount + SeparateCount;
end;

function TGSimpleUGraph.ContainsCycle(out aCycle: TIntVector): Boolean;
begin
  if VertexCount < 3 then
    exit(False);
  if IsTree then
    exit(False);
  Result := CycleExists(aCycle);
end;

function TGSimpleUGraph.ContainsCycle(constref aVertex: TVertex; out aCycle: TIntVector): Boolean;
begin
  Result := ContainsCycleI(IndexOf(aVertex), aCycle);
end;

function TGSimpleUGraph.ContainsCycleI(aVtxIndex: SizeInt; out aCycle: TIntVector): Boolean;
begin
  FVertexList.CheckIndexRange(aVtxIndex);
  Result := FindCycle(aVtxIndex, aCycle);
end;

function TGSimpleUGraph.ContainsEulerCycle: Boolean;
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

function TGSimpleUGraph.FindEulerCycle: TIntArray;
var
  g: TGSimpleUGraph = nil;
  Stack: TIntStack;
  I, s, d, From: SizeInt;
begin
  if not ContainsEulerCycle then
    exit(nil);
  g := Clone;
  try
    I := 1;
    System.SetLength(Result, ARRAY_INITIAL_SIZE);
    s := 0;
    while g.DegreeI(s) = 0 do
      Inc(s);
    From := s;
    Result[0] := From;
    repeat
      repeat
        if not g.FVertexList.ItemRefs[s]^.FindFirst(d) then
          break;
        Stack.Push(s);
        g.RemoveEdgeI(s, d);
        s := d;
      until False;
      if not Stack.TryPop(s) then
        break;
      if System.Length(Result) = I then
        System.SetLength(Result, I shl 1);
      Result[I] := s;
      Inc(I);
    until False;
     System.SetLength(Result, I);
  finally
    g.Free;
  end;
end;

function TGSimpleUGraph.ContainsCutPoint(constref aVertex: TVertex): Boolean;
begin
  Result := ContainsCutPointI(IndexOf(aVertex));
end;

function TGSimpleUGraph.ContainsCutPointI(aVtxIndex: SizeInt): Boolean;
var
  Helper: TCutPointHelper;
begin
  FVertexList.CheckIndexRange(aVtxIndex);
  Result := Helper.ContainsAny(Self, aVtxIndex);
end;

function TGSimpleUGraph.FindCutPoints(constref aVertex: TVertex): TIntVector;
begin
  Result := FindCutPointsI(IndexOf(aVertex));
end;

function TGSimpleUGraph.FindCutPointsI(aVtxIndex: SizeInt): TIntVector;
var
  Helper: TCutPointHelper;
begin
  FVertexList.CheckIndexRange(aVtxIndex);
  Helper.Search(Self, @Result, aVtxIndex);
end;

function TGSimpleUGraph.ContainsBridge: Boolean;
var
  Helper: TBridgeHelper;
begin
  Result := Helper.ContainsAny(Self);
end;

function TGSimpleUGraph.FindBridges: TIntEdgeVector;
var
  Helper: TBridgeHelper;
begin
  Helper.Search(Self, @Result);
end;

function TGSimpleUGraph.IsBiconnected: Boolean;
begin
  if Connected then
    Result := not ContainsCutPointI
  else
    Result := False;
end;

function TGSimpleUGraph.DfsSpanningTree(constref aRoot: TVertex): TIntArray;
begin
  Result := DfsSpanningTreeI(FVertexList.IndexOf(aRoot));
end;

function TGSimpleUGraph.DfsSpanningTreeI(aRoot: SizeInt): TIntArray;
var
  Visited: TBitVector;
  Stack: TIntStack;
  I: SizeInt;
begin
  FVertexList.CheckIndexRange(aRoot);
  Visited.Size := VertexCount;
  Result := CreateIntArray;
  repeat
    if not Visited[aRoot] then
      begin
        Visited[aRoot] := True;
        for I in AdjVerticesI(aRoot) do
          if not Visited[I] then
            begin
              Result[I] := aRoot;
              Stack.Push(I);
            end;
      end;
  until not Stack.TryPop(aRoot);
end;

function TGSimpleUGraph.BfsSpanningTree(constref aRoot: TVertex): TIntArray;
begin
  Result := BfsSpanningTreeI(FVertexList.IndexOf(aRoot));
end;

function TGSimpleUGraph.BfsSpanningTreeI(aRoot: SizeInt): TIntArray;
var
  Visited: TBitVector;
  Queue: TIntQueue;
  I: SizeInt;
begin
  FVertexList.CheckIndexRange(aRoot);
  Visited.Size := VertexCount;
  Result := CreateIntArray;
  repeat
    for I in AdjVerticesI(aRoot) do
      if not Visited[I] then
        begin
          Visited[I] := True;
          Result[I] := aRoot;
          Queue.Enqueue(I);
        end;
  until not Queue.TryDequeue(aRoot);
end;

function TGSimpleUGraph.CreateFromVector(constref aVector: TIntArray): TGSimpleUGraph;
var
  I, Src: SizeInt;
begin
  Result := TGSimpleUGraph.Create;
  for I := 0 to Pred(System.Length(aVector)) do
    begin
      Src := aVector[I];
      if Src <> -1 then
        Result.AddEdge(FVertexList[Src], FVertexList[I], GetEdgeDataPtr(Src, I)^);
    end;
end;

function TGSimpleUGraph.IsRegular: Boolean;
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

function TGSimpleUGraph.DistinctEdges: TDistinctEdges;
begin
  Result.FGraph := Self;
end;

function TGSimpleUGraph.Clone: TGSimpleUGraph;
begin
  Result := TGSimpleUGraph.Create;
  Result.FVertexList := FVertexList;
  Result.FEdgeCount := EdgeCount;
  Result.FTitle := Title;
  Result.FConnected := Connected;
  Result.FConnectedValid := ConnectedValid;
end;

{ TGWeighedEdgeData }

class function TGWeighedEdgeData.Construct(constref w: TWeight; constref d: TEdgeData): TGWeighedEdgeData;
begin
  Result.Weight := w;
  Result.Data := d;
end;

{ TGWeighedUGraph.TWeightEdge }

class operator TGWeighedUGraph.TWeightEdge. = (constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight = R.Weight;
end;

class operator TGWeighedUGraph.TWeightEdge.<>(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight <> R.Weight;
end;

class operator TGWeighedUGraph.TWeightEdge.>(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight > R.Weight;
end;

class operator TGWeighedUGraph.TWeightEdge.<(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight < R.Weight;
end;

class operator TGWeighedUGraph.TWeightEdge.>=(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight >= R.Weight;
end;

class operator TGWeighedUGraph.TWeightEdge.<=(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight <= R.Weight;
end;

constructor TGWeighedUGraph.TWeightEdge.Create(s, d: SizeInt; w: TWeight);
begin
  Source := s;
  Destination := d;
  Weight := w;
end;

{ TGWeighedUGraph.TWeightItem }

class operator TGWeighedUGraph.TWeightItem. = (constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight = R.Weight;
end;

class operator TGWeighedUGraph.TWeightItem.<>(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight <> R.Weight;
end;

class operator TGWeighedUGraph.TWeightItem.>(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight > R.Weight;
end;

class operator TGWeighedUGraph.TWeightItem.<(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight < R.Weight;
end;

class operator TGWeighedUGraph.TWeightItem.>=(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight >= R.Weight;
end;

class operator TGWeighedUGraph.TWeightItem.<=(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight <= R.Weight;
end;

constructor TGWeighedUGraph.TWeightItem.Create(constref w: TWeight; aIndex: SizeInt);
begin
  Weight := w;
  Index := aIndex;
end;

{ TGWeighedUGraph.TWeightItem }

class operator TGWeighedUGraph.TRankItem. = (constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank = R.Rank;
end;

class operator TGWeighedUGraph.TRankItem.<>(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank <> R.Rank;
end;

class operator TGWeighedUGraph.TRankItem.>(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank > R.Rank;
end;

class operator TGWeighedUGraph.TRankItem.<(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank < R.Rank;
end;

class operator TGWeighedUGraph.TRankItem.>=(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank >= R.Rank;
end;

class operator TGWeighedUGraph.TRankItem.<=(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank <= R.Rank;
end;

constructor TGWeighedUGraph.TRankItem.Create(constref aRank, aWeight: TWeight; aIndex: SizeInt);
begin
  Rank := aRank;
  Weight := aWeight;
  Index := aIndex;
end;

{ TGWeighedUGraph.TFilterKruskal }

procedure TGWeighedUGraph.TFilterKruskal.Kruskal(L, R: SizeInt);
var
  s, d: SizeInt;
begin
  TEdgeHelper.IntroSort(FEdges[L..R]);
  while L <= R do
    begin
      s := FEdges[L].Source;
      d := FEdges[L].Destination;
      if FDsu.InDiffSets(s, d)  then
        begin
          FWeight += FEdges[L].Weight;
          FTree[d] := s;
          FDsu.Union(s, d);
          Inc(FFound);
        end;
      Inc(L);
    end;
end;

function TGWeighedUGraph.TFilterKruskal.Split(L, R: SizeInt): SizeInt;
var
  Pivot: TWeight;
  v: TWeightEdge;
begin
  Pivot := FEdges[Succ(L + Random(Pred(R - L)))].Weight; //shouldn't be first or last
  Dec(L);
  Inc(R);
  repeat
    repeat Inc(L) until FEdges[L].Weight >= Pivot;
    repeat Dec(R) until FEdges[R].Weight <= Pivot;
    if L >= R then
      exit(R);
    v := FEdges[L];
    FEdges[L] := FEdges[R];
    FEdges[R] := v;
  until False;
end;

procedure TGWeighedUGraph.TFilterKruskal.FilterKruskal(L, R: SizeInt);
var
  p: SizeInt;
begin
  if R - L > FTreshold then
    begin
      if FFound < FCount then
        begin
          p := Split(L, R);
          FilterKruskal(L, p);
          if FFound < FCount then
            FilterKruskal(Succ(p), R);
        end;
    end
  else
    Kruskal(L, R);
end;

function TGWeighedUGraph.TFilterKruskal.FindMst(aGraph: TWeighedGraph; out aWeight: TWeight): TIntArray;
begin
  FEdges := aGraph.CreateEdgeArray;
  FTree := aGraph.FGraph.CreateIntArray;
  FCount := aGraph.VertexCount;
  FDsu.Size := FCount;
  FTreshold := Pred(FCount); // ??? FCount shr 1
  FWeight := ZeroWeight;
  FFound := 0;
  FilterKruskal(0, System.High(FEdges));
  aWeight := FWeight;
  Result := FTree;
end;

{ TGWeighedUGraph }

function TGWeighedUGraph.GetEdgeCount: SizeInt;
begin
  Result := FGraph.EdgeCount;
end;

function TGWeighedUGraph.GetTitle: string;
begin
  Result := FGraph.Title;
end;

function TGWeighedUGraph.GetVertex(aIndex: SizeInt): TVertex;
begin
  Result := FGraph[aIndex];
end;

procedure TGWeighedUGraph.SetVertex(aIndex: SizeInt; const aValue: TVertex);
begin
  FGraph[aIndex] := aValue;
end;

function TGWeighedUGraph.GetVertexCount: SizeInt;
begin
  Result := FGraph.VertexCount;
end;

function TGWeighedUGraph.GetSeparateCount: SizeInt;
begin
  Result := FGraph.SeparateCount;
end;

procedure TGWeighedUGraph.SetTitle(const aValue: string);
begin
  FGraph.Title := aValue;
end;

function TGWeighedUGraph.DijkstraSssp(aSrc: SizeInt): TWeightArray;
var
  Visited: TBitVector;
  Queue: TPairingHeap;
  Handles: THandleArray;
  Relaxed: TWeight;
  Item: TWeightItem;
  p: PAdjItem;
begin
  FGraph.CheckIndexRange(aSrc);
  Result := CreateWeightVector;
  Handles := FGraph.CreateHandleArray;
  Visited.Size := VertexCount;
  Handles[aSrc] := Queue.Insert(TWeightItem.Create(ZeroWeight, aSrc));
  while Queue.TryDequeue(Item) do
    if not Visited[Item.Index] then
      begin
        Visited[Item.Index] := True;
        Result[Item.Index] := Item.Weight;
        for p in FGraph.AdjVerticesPtr(Item.Index) do
          if Handles[p^.Key] = INVALID_HANDLE then
            Handles[p^.Key] := Queue.Insert(TWeightItem.Create(p^.Data.Weight + Item.Weight, p^.Key))
          else
            if not Visited[p^.Key] then
              begin
                Relaxed := p^.Data.Weight + Item.Weight;
                if Relaxed < Queue.Value(Handles[p^.Key]).Weight then
                  Queue.Update(Handles[p^.Key], TWeightItem.Create(Relaxed, p^.Key));
              end;
      end;
end;

function TGWeighedUGraph.DijkstraSssp(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
var
  Visited: TBitVector;
  Queue: TPairingHeap;
  Handles: THandleArray;
  Relaxed: TWeight;
  Item: TWeightItem;
  p: PAdjItem;
begin
  Result := CreateWeightVector;
  aPathTree := FGraph.CreateIntArray;
  Handles := FGraph.CreateHandleArray;
  Visited.Size := VertexCount;
  Handles[aSrc] := Queue.Insert(TWeightItem.Create(ZeroWeight, aSrc));
  while Queue.TryDequeue(Item) do
    if not Visited[Item.Index] then
      begin
        Visited[Item.Index] := True;
        Result[Item.Index] := Item.Weight;
        for p in FGraph.AdjVerticesPtr(Item.Index) do
          if Handles[p^.Key] = INVALID_HANDLE then
            begin
              Handles[p^.Key] := Queue.Insert(TWeightItem.Create(p^.Data.Weight + Item.Weight, p^.Key));
              aPathTree[p^.Key] := Item.Index;
            end
          else
            if not Visited[p^.Key] then
              begin
                Relaxed := p^.Data.Weight + Item.Weight;
                if Relaxed < Queue.Value(Handles[p^.Key]).Weight then
                  begin
                    Queue.Update(Handles[p^.Key], TWeightItem.Create(Relaxed, p^.Key));
                    aPathTree[p^.Key] := Item.Index;
                  end;
              end;
      end;
end;

function TGWeighedUGraph.DijkstraPath(aSrc, aDst: SizeInt): TWeight;
var
  Visited: TBitVector;
  Queue: TPairingHeap;
  Handles: THandleArray;
  Relaxed: TWeight;
  Item: TWeightItem;
  p: PAdjItem;
begin
  Handles := FGraph.CreateHandleArray;
  Visited.Size := VertexCount;
  Handles[aSrc] := Queue.Insert(TWeightItem.Create(ZeroWeight, aSrc));
  while Queue.TryDequeue(Item) do
    if not Visited[Item.Index] then
      begin
        if Item.Index = aDst then
          exit(Item.Weight);
        Visited[Item.Index] := True;
        for p in FGraph.AdjVerticesPtr(Item.Index) do
          if Handles[p^.Key] = INVALID_HANDLE then
            Handles[p^.Key] := Queue.Insert(TWeightItem.Create(p^.Data.Weight + Item.Weight, p^.Key))
          else
            if not Visited[p^.Key] then
              begin
                Relaxed := p^.Data.Weight + Item.Weight;
                if Relaxed < Queue.Value(Handles[p^.Key]).Weight then
                  Queue.Update(Handles[p^.Key], TWeightItem.Create(Relaxed, p^.Key));
              end
      end;
  Result := InfiniteWeight;
end;

function TGWeighedUGraph.DijkstraPath(aSrc, aDst: SizeInt; out aWeight: TWeight): TIntVector;
var
  Visited: TBitVector;
  Queue: TPairingHeap;
  Handles: THandleArray;
  Tree: TIntArray;
  Relaxed: TWeight;
  Item: TWeightItem;
  p: PAdjItem;
begin
  Handles := FGraph.CreateHandleArray;
  Tree := FGraph.CreateIntArray;
  Visited.Size := VertexCount;
  Handles[aSrc] := Queue.Insert(TWeightItem.Create(ZeroWeight, aSrc));
  while Queue.TryDequeue(Item) do
    if not Visited[Item.Index] then
      begin
        if Item.Index = aDst then
          begin
            aWeight := Item.Weight;
            exit(FGraph.ChainFromTree(Tree, aDst));
          end;
        Visited[Item.Index] := True;
        for p in FGraph.AdjVerticesPtr(Item.Index) do
          begin
            if Handles[p^.Key] = INVALID_HANDLE then
              begin
                Handles[p^.Key] := Queue.Insert(TWeightItem.Create(p^.Data.Weight + Item.Weight, p^.Key));
                Tree[p^.Key] := Item.Index;
              end
            else
              if not Visited[p^.Key] then
                begin
                  Relaxed := p^.Data.Weight + Item.Weight;
                  if Relaxed < Queue.Value(Handles[p^.Key]).Weight then
                    begin
                      Queue.Update(Handles[p^.Key], TWeightItem.Create(Relaxed, p^.Key));
                      Tree[p^.Key] := Item.Index;
                    end;
                end;
          end;
      end;
  aWeight := InfiniteWeight;
end;

function TGWeighedUGraph.AStar(aSrc, aDst: SizeInt; aHeur: THeuristic; out aWeight: TWeight): TIntVector;
var
  Visited: TBitVector;
  Queue: TAStarHeap;
  Handles: THandleArray;
  Tree: TIntArray;
  Relaxed: TWeight;
  Item: TRankItem;
  p: PAdjItem;
begin
  Handles := FGraph.CreateHandleArray;
  Tree := FGraph.CreateIntArray;
  Visited.Size := VertexCount;
  Handles[aSrc] := Queue.Insert(TRankItem.Create(aHeur(FGraph[aSrc], FGraph[aDst]), ZeroWeight, aSrc));
  while Queue.TryDequeue(Item) do
    if not Visited[Item.Index] then
      begin
        if Item.Index = aDst then
          begin
            aWeight := Item.Weight;
            exit(FGraph.ChainFromTree(Tree, aDst));
          end;
        Visited[Item.Index] := True;
        for p in FGraph.AdjVerticesPtr(Item.Index) do
          begin
            if Handles[p^.Key] = INVALID_HANDLE then
              begin
                Relaxed := p^.Data.Weight + Item.Weight;
                Handles[p^.Key] := Queue.Insert(TRankItem.Create(
                  Relaxed + aHeur(FGraph[p^.Key], FGraph[aDst]), Relaxed, p^.Key));
                Tree[p^.Key] := Item.Index;
              end
            else
              if not Visited[p^.Key] then
                begin
                  Relaxed := Item.Weight + p^.Data.Weight;
                  if Relaxed < Queue.Value(Handles[p^.Key]).Weight then
                    begin
                      Queue.Update(Handles[p^.Key], TRankItem.Create(
                        Relaxed + aHeur(FGraph[p^.Key], FGraph[aDst]), Relaxed, p^.Key));
                      Tree[p^.Key] := Item.Index;
                    end;
                end;
          end;
      end;
  aWeight := InfiniteWeight;
end;

function TGWeighedUGraph.FordBellman(aSrc: SizeInt; out aWeights: TWeightArray): Boolean;
var
  Edge: TEdge;
  Enum: TEdgeEnumerator;
  RelaxValue: TWeight;
  I: SizeInt;
  Relaxed: Boolean = False;
begin
  aWeights := CreateWeightVector;
  Enum := Edges.GetEnumerator;
  aWeights[aSrc] := ZeroWeight;
  for I := 1 to VertexCount do
    begin
      Relaxed := False;
      while Enum.MoveNext do
        begin
          Edge := Enum.Current;
          if aWeights[Edge.Source] < InfiniteWeight then
            begin
              RelaxValue := aWeights[Edge.Source] + Edge.Data.Weight;
              if RelaxValue < aWeights[Edge.Destination] then
                begin
                  aWeights[Edge.Destination] := RelaxValue;
                  Relaxed := True;
                end;
            end;
        end;
      if not Relaxed then
        break;
      Enum.Reset;
    end;
  Result := not Relaxed;
  if not Result then
    aWeights := nil;
end;

function TGWeighedUGraph.FordBellman(aSrc: SizeInt; out aPaths: TIntArray; out aWeights: TWeightArray): Boolean;
var
  Edge: TEdge;
  Enum: TEdgeEnumerator;
  v: TIntVector;
  RelaxValue: TWeight;
  I: SizeInt;
  J: SizeInt = -1;
begin
  aWeights := CreateWeightVector;
  aPaths := FGraph.CreateIntArray;
  Enum := Edges.GetEnumerator;
  aWeights[aSrc] := ZeroWeight;
  for I := 1 to VertexCount do
    begin
      J := -1;
      while Enum.MoveNext do
        begin
          Edge := Enum.Current;
          if aWeights[Edge.Source] < InfiniteWeight then
            begin
              RelaxValue := aWeights[Edge.Source] + Edge.Data.Weight;
              if RelaxValue < aWeights[Edge.Destination] then
                begin
                  aWeights[Edge.Destination] := RelaxValue;
                  aPaths[Edge.Destination] := Edge.Source;
                  J := Edge.Destination;
                end;
            end;
        end;
      if J = -1 then
        break;
      Enum.Reset;
    end;

  Result := J = -1;

  if not Result then
    begin
      for I := 1 to VertexCount do
        J := aPaths[J];
      I := J;
      v.Add(J);
      repeat
        I := aPaths[I];
        v.Add(I);
      until I = J;
      aPaths := v.ToArray;
      aWeights := nil;
    end;
end;

function TGWeighedUGraph.FilterKruskalMst(out aTotalWeight: TWeight): TIntArray;
var
  fk: TFilterKruskal;
begin
  Result := fk.FindMst(Self, aTotalWeight);
end;

function TGWeighedUGraph.KruskalMst(out aTotalWeight: TWeight): TIntArray;
var
  e: TEdgeArray;
  I, s, d, VtxCount: SizeInt;
  Dsu: TDisjointSetUnion;
begin
  e := CreateEdgeArray;
  Result := FGraph.CreateIntArray;
  VtxCount := VertexCount;
  TEdgeHelper.Sort(e);
  System.SetLength(Result, VtxCount);
  Dsu.Size := VtxCount;
  aTotalWeight := ZeroWeight;
  for I := 0 to System.High(e) do
    begin
      s := e[I].Source;
      d := e[I].Destination;
      if Dsu.InDiffSets(s, d)  then
        begin
          aTotalWeight += e[I].Weight;
          Result[d] := s;
          Dsu.Union(s, d);
        end;
    end;
end;

function TGWeighedUGraph.PrimMst(out aTotalWeight: TWeight): TIntArray;
var
  Visited: TBitVector;
  Queue: TPairingHeap;
  Handles: THandleArray;
  Curr: SizeInt;
  Item: TWeightItem;
  p: PAdjItem;
begin
  Result := FGraph.CreateIntArray;
  System.SetLength(Result, VertexCount);
  Handles := FGraph.CreateHandleArray;
  Visited.Size := VertexCount;
  Handles[0] := Queue.Insert(TWeightItem.Create(ZeroWeight, 0));
  aTotalWeight := 0;
  while Queue.TryDequeue(Item) do
    if not Visited[Item.Index] then
      begin
        Curr := Item.Index;
        aTotalWeight += Item.Weight;
        Visited[Curr] := True;
        for p in FGraph.AdjVerticesPtr(Curr) do
          begin
            if Handles[p^.Key] = INVALID_HANDLE then
              begin
                Handles[p^.Key] := Queue.Insert(TWeightItem.Create(p^.Data.Weight, p^.Key));
                Result[p^.Key] := Curr;
              end
            else
              if not Visited[p^.Key] and (p^.Data.Weight < Queue.Value(Handles[p^.Key]).Weight) then
                begin
                  Queue.Update(Handles[p^.Key], TWeightItem.Create(p^.Data.Weight, p^.Key));
                  Result[p^.Key] := Curr;
                end;
          end;
      end;
end;

function TGWeighedUGraph.CreateWeightVector: TWeightArray;
var
  I: SizeInt;
begin
  System.SetLength(Result, VertexCount);
  for I := 0 to Pred(VertexCount) do
    Result[I] := InfiniteWeight;
end;

function TGWeighedUGraph.CreateEdgeArray: TEdgeArray;
var
  I: SizeInt = 0;
  e: TEdge;
begin
  System.SetLength(Result, EdgeCount);
  for e in DistinctEdges do
    begin
      Result[I] := TWeightEdge.Create(e.Source, e.Destination, e.Data.Weight);
      Inc(I);
    end;
end;

class function TGWeighedUGraph.Min(const L, R: TWeight): TWeight;
begin
  if L <= R then
    Result := L
  else
    Result := R;
end;

class constructor TGWeighedUGraph.Init;
begin
  CFInfiniteWeight := TWeight.MaxValue;
  CFNegInfiniteWeight := TWeight.MinValue;
  CFZeroWeight := Default(TWeight);
end;

constructor TGWeighedUGraph.Create;
begin
  FGraph := TGraph.Create;
end;

constructor TGWeighedUGraph.Create(g: TGraph);
begin
  FGraph := g;
end;

destructor TGWeighedUGraph.Destroy;
begin
  FGraph.Free;
  inherited;
end;

function TGWeighedUGraph.IsEmpty: Boolean;
begin
  Result := FGraph.IsEmpty;
end;

function TGWeighedUGraph.NonEmpty: Boolean;
begin
  Result := FGraph.NonEmpty;
end;

procedure TGWeighedUGraph.Clear;
begin
  FGraph.Clear;
end;

procedure TGWeighedUGraph.EnsureCapacity(aValue: SizeInt);
begin
  FGraph.EnsureCapacity(aValue);
end;

procedure TGWeighedUGraph.TrimToFit;
begin
  FGraph.TrimToFit;
end;

function TGWeighedUGraph.ContainsVertex(constref aVertex: TVertex): Boolean;
begin
  Result := FGraph.ContainsVertex(aVertex);
end;

function TGWeighedUGraph.ContainsEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := FGraph.ContainsEdge(aSrc, aDst);
end;

function TGWeighedUGraph.ContainsEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := FGraph.ContainsEdgeI(aSrc, aDst);
end;

function TGWeighedUGraph.ContainsEdgeI(aSrc, aDst: SizeInt; out aData: TWeEdgeData): Boolean;
begin
  Result := FGraph.ContainsEdgeI(aSrc, aDst, aData);
end;

function TGWeighedUGraph.IndexOf(constref aVertex: TVertex): SizeInt;
begin
  Result := FGraph.IndexOf(aVertex);
end;

function TGWeighedUGraph.Adjacent(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := FGraph.Adjacent(aSrc, aDst);
end;

function TGWeighedUGraph.AdjacentI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := FGraph.AdjacentI(aSrc, aDst);
end;

function TGWeighedUGraph.AdjVertices(constref aSrc: TVertex): TAdjVertices;
begin
  Result := FGraph.AdjVertices(aSrc);
end;

function TGWeighedUGraph.AdjVerticesI(aSrc: SizeInt): TAdjVertices;
begin
  Result := FGraph.AdjVerticesI(aSrc);
end;

function TGWeighedUGraph.IncidentEdges(constref aSrc: TVertex): TIncidentEdges;
begin
  Result := FGraph.IncidentEdges(aSrc);
end;

function TGWeighedUGraph.IncidentEdgesI(aSrc: SizeInt): TIncidentEdges;
begin
  Result := FGraph.IncidentEdgesI(aSrc);
end;

function TGWeighedUGraph.Vertices: TVertices;
begin
  Result := FGraph.Vertices;
end;

function TGWeighedUGraph.Edges: TEdges;
begin
  Result := FGraph.Edges;
end;

function TGWeighedUGraph.DistinctEdges: TDistinctEdges;
begin
  Result := FGraph.DistinctEdges;
end;

function TGWeighedUGraph.AddVertex(constref aVertex: TVertex; out aIndex: SizeInt): Boolean;
begin
  Result := FGraph.AddVertex(aVertex, aIndex);
end;

function TGWeighedUGraph.AddVertex(constref aVertex: TVertex): Boolean;
begin
  Result := FGraph.AddVertex(aVertex);
end;

procedure TGWeighedUGraph.RemoveVertex(constref aVertex: TVertex);
begin
  FGraph.RemoveVertex(aVertex);
end;

procedure TGWeighedUGraph.RemoveVertexI(aVtxIndex: SizeInt);
begin
  FGraph.RemoveVertexI(aVtxIndex);
end;

function TGWeighedUGraph.AddEdge(constref aSrc, aDst: TVertex; aWeight: TWeight;
  aData: TEdgeData): Boolean;
begin
  Result := FGraph.AddEdge(aSrc, aDst, TWeEdgeData.Construct(aWeight, aData));
end;

function TGWeighedUGraph.AddEdge(constref aSrc, aDst: TVertex; aWeight: TWeight): Boolean;
begin
  Result := FGraph.AddEdge(aSrc, aDst, TWeEdgeData.Construct(aWeight, CFData));
end;

function TGWeighedUGraph.AddEdge(constref aSrc, aDst: TVertex; aData: TWeEdgeData): Boolean;
begin
  Result := FGraph.AddEdge(aSrc, aDst, aData);
end;

function TGWeighedUGraph.AddEdgeI(aSrc, aDst: SizeInt; aWeight: TWeight; aData: TEdgeData): Boolean;
begin
  Result := FGraph.AddEdgeI(aSrc, aDst, TWeEdgeData.Construct(aWeight, aData));
end;

function TGWeighedUGraph.AddEdgeI(aSrc, aDst: SizeInt; aWeight: TWeight): Boolean;
begin
  Result := FGraph.AddEdgeI(aSrc, aDst, TWeEdgeData.Construct(aWeight, CFData));
end;

function TGWeighedUGraph.AddEdgeI(aSrc, aDst: SizeInt; aData: TWeEdgeData): Boolean;
begin
  Result := FGraph.AddEdgeI(aSrc, aDst, aData);
end;

function TGWeighedUGraph.RemoveEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := FGraph.RemoveEdge(aSrc, aDst);
end;

function TGWeighedUGraph.RemoveEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := FGraph.RemoveEdgeI(aSrc, aDst);
end;

function TGWeighedUGraph.GetEdgeData(constref aSrc, aDst: TVertex): TWeEdgeData;
begin
  Result := FGraph.GetEdgeData(aSrc, aDst);
end;

function TGWeighedUGraph.GetEdgeDataI(aSrc, aDst: SizeInt): TWeEdgeData;
begin
  Result := FGraph.GetEdgeDataI(aSrc, aDst);
end;

procedure TGWeighedUGraph.SetEdgeData(constref aSrc, aDst: TVertex; constref aValue: TWeEdgeData);
begin
  FGraph.SetEdgeData(aSrc, aDst, aValue);
end;

procedure TGWeighedUGraph.SetEdgeDataI(aSrc, aDst: SizeInt; constref aValue: TWeEdgeData);
begin
  FGraph.SetEdgeDataI(aSrc, aDst, aValue);
end;

procedure TGWeighedUGraph.SaveToStream(aStream: TStream; aWriteVertex: TOnWriteVertex;
  aWriteData: TOnWriteData);
begin
  FGraph.SaveToStream(aStream, aWriteVertex, aWriteData);
end;

procedure TGWeighedUGraph.LoadFromStream(aStream: TStream; aReadVertex: TOnReadVertex;
  aReadData: TOnReadData);
begin
  FGraph.LoadFromStream(aStream, aReadVertex, aReadData);
end;

procedure TGWeighedUGraph.SaveToFile(const aFileName: string; aWriteVertex: TOnWriteVertex;
  aWriteData: TOnWriteData);
begin
  FGraph.SaveToFile(aFileName, aWriteVertex, aWriteData)
end;

procedure TGWeighedUGraph.LoadFromFile(const aFileName: string; aReadVertex: TOnReadVertex;
  aReadData: TOnReadData);
begin
  FGraph.LoadFromFile(aFileName, aReadVertex, aReadData);
end;

function TGWeighedUGraph.DfsTraversal(constref aRoot: TVertex; aOnGray: TOnIntTest;
  aOnWhite: TOnIntVisit): SizeInt;
begin
  Result := FGraph.DfsTraversal(aRoot, aOnGray, aOnWhite);
end;

function TGWeighedUGraph.DfsTraversalI(aRoot: SizeInt; aOnGray: TOnIntTest;
  aOnWhite: TOnIntVisit): SizeInt;
begin
  Result := FGraph.DfsTraversalI(aRoot, aOnGray, aOnWhite);
end;

function TGWeighedUGraph.BfsTraversal(constref aRoot: TVertex; aOnGray: TOnIntTest;
  aOnWhite: TOnIntVisit): SizeInt;
begin
  Result := FGraph.BfsTraversal(aRoot, aOnGray, aOnWhite);
end;

function TGWeighedUGraph.BfsTraversalI(aRoot: SizeInt; aOnGray: TOnIntTest;
  aOnWhite: TOnIntVisit): SizeInt;
begin
  Result := FGraph.BfsTraversalI(aRoot, aOnGray, aOnWhite);
end;

function TGWeighedUGraph.SimplePathExists(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := FGraph.SimplePathExists(aSrc, aDst);
end;

function TGWeighedUGraph.SimplePathExistsI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := FGraph.SimplePathExistsI(aSrc, aDst);
end;

function TGWeighedUGraph.IsBipartite: Boolean;
begin
  Result := FGraph.IsBipartite;
end;

function TGWeighedUGraph.IsBipartite(out v: TShortArray): Boolean;
begin
  Result := FGraph.IsBipartite(v)
end;

function TGWeighedUGraph.ShortestPathLen(constref aSrc, aDst: TVertex): SizeInt;
begin
  Result := FGraph.ShortestPathLen(aSrc, aDst);
end;

function TGWeighedUGraph.ShortestPathLenI(aSrc, aDst: SizeInt): SizeInt;
begin
  Result := FGraph.ShortestPathLenI(aSrc, aDst);
end;

function TGWeighedUGraph.ShortestPathsMap(constref aRoot: TVertex): TIntArray;
begin
  Result := FGraph.ShortestPathsMap(aRoot);
end;

function TGWeighedUGraph.ShortestPathsMapI(aRoot: SizeInt): TIntArray;
begin
  Result := FGraph.ShortestPathsMapI(aRoot);
end;

function TGWeighedUGraph.ShortestPath(constref aSrc, aDst: TVertex): TIntVector;
begin
  Result := FGraph.ShortestPath(aSrc, aDst);
end;

function TGWeighedUGraph.ShortestPathI(aSrc, aDst: SizeInt): TIntVector;
begin
  Result := FGraph.ShortestPathI(aSrc, aDst);
end;

function TGWeighedUGraph.Degree(constref aVertex: TVertex): SizeInt;
begin
  Result := FGraph.Degree(aVertex);
end;

function TGWeighedUGraph.DegreeI(aIndex: SizeInt): SizeInt;
begin
  Result := FGraph.DegreeI(aIndex);
end;

function TGWeighedUGraph.Isolated(constref v: TVertex): Boolean;
begin
  Result := FGraph.Isolated(v);
end;

function TGWeighedUGraph.IsolatedI(aIndex: SizeInt): Boolean;
begin
  Result := FGraph.IsolatedI(aIndex);
end;

function TGWeighedUGraph.IsConnected: Boolean;
begin
  Result := FGraph.IsConnected;
end;

function TGWeighedUGraph.EnsureConnected(aOnAddEdge: TOnAddEdge): SizeInt;
begin
  Result := FGraph.EnsureConnected(aOnAddEdge);
end;

function TGWeighedUGraph.SeparateIndex(constref aVertex: TVertex): SizeInt;
begin
  Result := FGraph.SeparateIndexOf(aVertex);
end;

function TGWeighedUGraph.SeparateIndexI(aVtxIndex: SizeInt): SizeInt;
begin
  Result := FGraph.SeparateIndexOfI(aVtxIndex);
end;

function TGWeighedUGraph.SeparatePop(constref aVertex: TVertex): SizeInt;
begin
  Result := FGraph.SeparatePop(aVertex);
end;

function TGWeighedUGraph.SeparatePopI(aVtxIndex: SizeInt): SizeInt;
begin
  Result := FGraph.SeparatePopI(aVtxIndex);
end;

function TGWeighedUGraph.SeparateGraph(constref aVertex: TVertex): TWeighedGraph;
begin
  Result := SeparateGraphI(IndexOf(aVertex));
end;

function TGWeighedUGraph.SeparateGraphI(aVtxIndex: SizeInt): TWeighedGraph;
begin
  Result := TWeighedGraph.Create(FGraph.SeparateGraphI(aVtxIndex));
end;

function TGWeighedUGraph.IsRegular: Boolean;
begin
  Result := FGraph.IsRegular;
end;

function TGWeighedUGraph.IsTree: Boolean;
begin
  Result := FGraph.IsTree;
end;

function TGWeighedUGraph.ContainsCycle(out aCycle: TIntVector): Boolean;
begin
  Result := FGraph.ContainsCycle(aCycle);
end;

function TGWeighedUGraph.ContainsCycle(constref aVertex: TVertex; out aCycle: TIntVector): Boolean;
begin
  Result := FGraph.ContainsCycle(aVertex, aCycle);
end;

function TGWeighedUGraph.ContainsCycleI(aVtxIndex: SizeInt; out aCycle: TIntVector): Boolean;
begin
  Result := FGraph.ContainsCycleI(aVtxIndex, aCycle);
end;

function TGWeighedUGraph.ContainsCutPoint(constref aVertex: TVertex): Boolean;
begin
  Result := FGraph.ContainsCutPoint(aVertex);
end;

function TGWeighedUGraph.ContainsCutPointI(aVtxIndex: SizeInt): Boolean;
begin
  Result := FGraph.ContainsCutPointI(aVtxIndex);
end;

function TGWeighedUGraph.FindCutPoints(constref aVertex: TVertex): TIntVector;
begin
  Result := FGraph.FindCutPoints(aVertex);
end;

function TGWeighedUGraph.FindCutPointsI(aVtxIndex: SizeInt): TIntVector;
begin
  Result := FGraph.FindCutPointsI(aVtxIndex);
end;

function TGWeighedUGraph.ContainsBridge: Boolean;
begin
  Result := FGraph.ContainsBridge;
end;

function TGWeighedUGraph.FindBridges: TIntEdgeVector;
begin
  Result := FGraph.FindBridges;
end;

function TGWeighedUGraph.IsBiconnected: Boolean;
begin
  Result := FGraph.IsBiconnected;
end;

function TGWeighedUGraph.ContainsEulerCycle: Boolean;
begin
  Result := FGraph.ContainsEulerCycle;
end;

function TGWeighedUGraph.FindEulerCycle: TIntArray;
begin
  Result := FGraph.FindEulerCycle;
end;

function TGWeighedUGraph.DfsSpanningTree(constref aRoot: TVertex): TIntArray;
begin
  Result := FGraph.DfsSpanningTree(aRoot);
end;

function TGWeighedUGraph.DfsSpanningTreeI(aRoot: SizeInt): TIntArray;
begin
  Result := FGraph.DfsSpanningTreeI(aRoot);
end;

function TGWeighedUGraph.BfsSpanningTree(constref aRoot: TVertex): TIntArray;
begin
  Result := FGraph.BfsSpanningTree(aRoot);
end;

function TGWeighedUGraph.BfsSpanningTreeI(aRoot: SizeInt): TIntArray;
begin
  Result := FGraph.BfsSpanningTreeI(aRoot);
end;

function TGWeighedUGraph.CreateFromArray(constref aArray: TIntArray): TWeighedGraph;
begin
  Result := TGWeighedUGraph.Create(FGraph.CreateFromVector(aArray));
end;

function TGWeighedUGraph.ContainsNegWeighedEdge: Boolean;
var
  e: TEdge;
begin
  for e in DistinctEdges do
    if e.Data.Weight < ZeroWeight then
      exit(True);
  Result := False;
end;

function TGWeighedUGraph.MinPathsMap(constref aSrc: TVertex): TWeightArray;
begin
  Result := MinPathsMapI(FGraph.IndexOf(aSrc));
end;

function TGWeighedUGraph.MinPathsMapI(aSrc: SizeInt): TWeightArray;
begin
  FGraph.CheckIndexRange(aSrc);
  Result := DijkstraSssp(aSrc);
end;

function TGWeighedUGraph.MinPathsMap(constref aSrc: TVertex; out aPathTree: TIntArray): TWeightArray;
begin
  Result := MinPathsMapI(FGraph.IndexOf(aSrc), aPathTree);
end;

function TGWeighedUGraph.MinPathsMapI(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
begin
  FGraph.CheckIndexRange(aSrc);
  Result := DijkstraSssp(aSrc, aPathTree);
end;

function TGWeighedUGraph.MinPathWeight(constref aSrc, aDst: TVertex): TWeight;
begin
  Result := MinPathWeightI(FGraph.IndexOf(aSrc), FGraph.IndexOf(aDst));
end;

function TGWeighedUGraph.MinPathWeightI(aSrc, aDst: SizeInt): TWeight;
begin
  FGraph.CheckIndexRange(aSrc);
  FGraph.CheckIndexRange(aDst);
  Result := DijkstraPath(aSrc, aDst);
end;

function TGWeighedUGraph.MinPath(constref aSrc, aDst: TVertex; out aWeight: TWeight): TIntVector;
begin
  Result := MinPathI(FGraph.IndexOf(aSrc), FGraph.IndexOf(aDst), aWeight);
end;

function TGWeighedUGraph.MinPathI(aSrc, aDst: SizeInt; out aWeight: TWeight): TIntVector;
begin
  FGraph.CheckIndexRange(aSrc);
  FGraph.CheckIndexRange(aDst);
  Result := DijkstraPath(aSrc, aDst, aWeight);
end;

function TGWeighedUGraph.MinPathAStar(constref aSrc, aDst: TVertex; aHeur: THeuristic;
  out aWeight: TWeight): TIntVector;
begin
  Result := MinPathAStarI(FGraph.IndexOf(aSrc), FGraph.IndexOf(aSrc), aHeur, aWeight);
end;

function TGWeighedUGraph.MinPathAStarI(aSrc, aDst: SizeInt; aHeur: THeuristic; out aWeight: TWeight): TIntVector;
begin
  FGraph.CheckIndexRange(aSrc);
  FGraph.CheckIndexRange(aDst);
  if aHeur <> nil then
    Result := AStar(aSrc, aDst, aHeur, aWeight)
  else
    Result := DijkstraPath(aSrc, aDst, aWeight);
end;

function TGWeighedUGraph.FindMinPathsMap(constref aSrc: TVertex; out aWeights: TWeightArray): Boolean;
begin
  Result := FindMinPathsMapI(FGraph.IndexOf(aSrc), aWeights);
end;

function TGWeighedUGraph.FindMinPathsMapI(aSrc: SizeInt; out aWeights: TWeightArray): Boolean;
begin
  FGraph.CheckIndexRange(aSrc);
  Result := FordBellman(aSrc, aWeights);
end;

function TGWeighedUGraph.FindMinPathsMap(constref aSrc: TVertex; out aPaths: TIntArray;
  out aWeights: TWeightArray): Boolean;
begin
  Result := FindMinPathsMapI(FGraph.IndexOf(aSrc), aPaths, aWeights);
end;

function TGWeighedUGraph.FindMinPathsMapI(aSrc: SizeInt; out aPaths: TIntArray;
  out aWeights: TWeightArray): Boolean;
begin
  FGraph.CheckIndexRange(aSrc);
  Result := FordBellman(aSrc, aPaths, aWeights);
end;

function TGWeighedUGraph.MinSpanningTreeKrus(out aTotalWeight: TWeight): TIntArray;
begin
  if IsConnected then
    Result := KruskalMst(aTotalWeight)
    //Result := FilterKruskalMst(aTotalWeight)
  else
    raise ELGraphError.Create(SEGraphIsNotConnected);
end;

function TGWeighedUGraph.MinSpanningTreePrim(out aTotalWeight: TWeight): TIntArray;
begin
  if IsConnected then   //todo: is it required ???
    Result := PrimMst(aTotalWeight)
  else
    raise ELGraphError.Create(SEGraphIsNotConnected);
end;

function TGWeighedUGraph.Clone: TWeighedGraph;
begin
  Result := TWeighedGraph.Create(FGraph.Clone);
end;

end.

