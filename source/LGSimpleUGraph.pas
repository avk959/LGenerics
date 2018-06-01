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
unit LGSimpleUGraph;

{$mode objfpc}{$H+}
{$INLINE ON}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils,
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
      Low, Ord: TIntArray;
      Counter: SizeInt;
      Points: ^TIntVector;
    public
      constructor Create(aGraph: TGSimpleUGraph; aVector: PIntVector);
      procedure Dfs(Curr: SizeInt; Prev: SizeInt = -1);
    end;

    procedure DoRemoveVertex(aIndex: SizeInt);
    function  DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
    function  GetSeparateGraph(aIndex: SizeInt): TGSimpleUGraph;
  public
    procedure Clear; override;
  { returns True and vertex index, if it was added, False otherwise }
    function  AddVertex(constref v: TVertex; out aIndex: SizeInt): Boolean;
    function  RemoveVertex(constref v: TVertex): Boolean; inline;
    function  RemoveVertexI(aIndex: SizeInt): Boolean;
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
    function  Degree(constref v: TVertex): SizeInt; inline;
    function  DegreeI(aIndex: SizeInt): SizeInt;
    function  Isolated(constref v: TVertex): Boolean; inline;
    function  IsolatedI(aIndex: SizeInt): Boolean; inline;
  { checks whether the graph is connected; a graph without Items is considered disconnected }
    function  IsConnected: Boolean; inline;
  { if the graph is not empty, then make graph connected, adding, if necessary, new edges
    from the vertex with the index 0}
    function  MakeConnected(aOnAddEdge: TOnAddEdge = nil): SizeInt;
  { returns graph of connected component that contains v }
    function  SeparateGraph(constref v: TVertex): TGSimpleUGraph; inline;
    function  SeparateGraphI(aVtxIndex: SizeInt): TGSimpleUGraph;

  { returns index of the connected component that contains v }
    function  SeparateIndexOf(constref v: TVertex): SizeInt; inline;
    function  SeparateIndexOfI(aVtxIndex: SizeInt): SizeInt; inline;
  { returns number of Items(population) in the connected component that contains v }
    function  SeparatePop(constref v: TVertex): SizeInt; inline;
    function  SeparatePopI(aVtxIndex: SizeInt): SizeInt;
    function  IsTree: Boolean; inline;
    function  CyclomaticNumber: SizeInt; inline;
  { checks whether the graph is a regular graph (that is, the degree of all its
     Items equal); an empty graph is considered regular }
    function  IsRegular: Boolean;
  { checks whether the exists any cycle in graph; if True then aCycle will contain indices of cycle }
    function  CycleExists(out aCycle: TIntVector): Boolean;
  { checks whether the exists any cycle in graph that contains v;
    if True then aCycle will contain indices of cycle }
    function  CycleExists(constref v: TVertex; out aCycle: TIntVector): Boolean; inline;
    function  CycleExistsI(aVtxIndex: SizeInt; out aCycle: TIntVector): Boolean;
    function  EulerCycleExists: Boolean;
  { looking for some Eulerian cycle in the first connected component along the path from the first vertex }
    function  FindEulerCycle: TIntArray;
  { returns indices of the articulation points in the result vector, if any, otherwise the empty vector;
    note: crashes with stack overflow on size ~ 400000*3 because of recursive DFS}
    function  FindCutPoints: TIntVector;
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
      TWeight must have defined comparision operators and property MaxValue,
      which used as infinity weight value;
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
    TDistinctEdges  = TGraph.TDistinctEdges;
    TEdgeEnumerator = TGraph.TEdgeEnumerator;
    TAdjVertices    = TGraph.TAdjVertices;
    TIncidentEdges  = TGraph.TIncidentEdges;
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
      class function Construct(s, d: SizeInt; w: TWeight): TWeightEdge; static; inline;
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
      class function Construct(constref w: TWeight; aIndex: SizeInt): TWeightItem; static; inline;
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
      class function Construct(constref aRank, aWeight: TWeight; aIndex: SizeInt): TRankItem; static; inline;
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
    CFInfiniteWeight: TWeight;
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
    function  DijkstraPath(aSrc, aDst: SizeInt; out aPath: TIntVector): TWeight;
  { A* pathfinding algorithm }
    function  AStar(aSrc, aDst: SizeInt; aHeur: THeuristic; out aPath: TIntVector): TWeight;
  { Bellman-Ford algorithm: single-source shortest paths problem for any weights  }
    function  FordBellman(aSrc: SizeInt; out aWeights: TWeightArray): Boolean;
    function  FordBellman(aSrc: SizeInt; out aPathTree: TIntArray; out aWeights: TWeightArray): Boolean;

    function  FilterKruskalMst(out aTotalWeight: TWeight): TIntArray;
    function  KruskalMst(out aTotalWeight: TWeight): TIntArray;
    function  PrimMst(out aTotalWeight: TWeight): TIntArray;
    function  CreateWeightVector: TWeightArray;
    function  CreateEdgeArray: TEdgeArray;
    class function Min(const L, R: TWeight): TWeight; static; inline;
    class constructor Init;
  public
    class property InfiniteWeight: TWeight read CFInfiniteWeight;
    class property ZeroWeight: TWeight read CFZeroWeight;

    constructor Create;
    constructor Create(g: TGraph);
    destructor Destroy; override;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure Clear; inline;
    procedure EnsureCapacity(aValue: SizeInt); inline;
    procedure TrimToFit; inline;
    function  ContainsVertex(constref v: TVertex): Boolean; inline;
    function  ContainsEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  ContainsEdgeI(aSrc, aDst: SizeInt): Boolean;
    function  ContainsEdgeI(aSrc, aDst: SizeInt; out aData: TWeEdgeData): Boolean;
    function  IndexOf(constref v: TVertex): SizeInt; inline;
    function  Adjacent(constref aSrc, aDst: TVertex): Boolean; inline;
    function  AdjacentI(aSrc, aDst: SizeInt): Boolean; inline;
  { enumerates indices of adjacent vertices }
    function  AdjVertices(constref aSrc: TVertex): TAdjVertices; inline;
    function  AdjVerticesI(aSrc: SizeInt): TAdjVertices; inline;
  { enumerates incident edges }
    function  IncidentEdges(constref aSrc: TVertex): TIncidentEdges; inline;
    function  IncidentEdgesI(aSrc: SizeInt): TIncidentEdges; inline;
  { enumerates all edges(including the reverse ones) }
    function  Edges: TEdges; inline;
  { enumerates all edges(only once) }
    function  DistinctEdges: TDistinctEdges; inline;
    function  AddVertex(constref v: TVertex; out aIndex: SizeInt): Boolean; inline;
    function  RemoveVertex(constref v: TVertex): Boolean; inline;
    function  RemoveVertexI(aIndex: SizeInt): Boolean; inline;
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
    function  FindMinPathLen(constref aSrc, aDst: TVertex): SizeInt; inline;
    function  FindMinPathLenI(aSrc, aDst: SizeInt): SizeInt; inline;
  { returns a vector containing in the corresponding components the shortest paths from aRoot,
    (in sense 'edges count')}
    function  FindMinPathLenMap(constref aRoot: TVertex): TIntArray; inline;
    function  FindMinPathLenMapI(aRoot: SizeInt = 0): TIntArray; inline;
  { returns a vector containing indices of found shortest path(in sense 'edges count'),
    empty if path does not exists }
    function  FindMinPath(constref aSrc, aDst: TVertex): TIntVector; inline;
    function  FindMinPathI(aSrc, aDst: SizeInt): TIntVector; inline;
    function  Degree(constref v: TVertex): SizeInt; inline;
    function  DegreeI(aIndex: SizeInt): SizeInt; inline;
    function  Isolated(constref v: TVertex): Boolean; inline;
    function  IsolatedI(aIndex: SizeInt): Boolean; inline;
  { checks whether the graph is connected; a graph without vertices is considered disconnected }
    function  IsConnected: Boolean; inline;
  { if the graph is not empty, then make graph connected, adding, if necessary, new edges
    from the vertex with the index 0}
    function  MakeConnected(aOnAddEdge: TOnAddEdge = nil): SizeInt; inline;
  { returns index of the connected component that contains v }
    function  SeparateIndex(constref v: TVertex): SizeInt; inline;
    function  SeparateIndexI(aVertex: SizeInt): SizeInt; inline;
  { returns number of vertices(population) in the connected component that contains aRoot }
    function  SeparatePop(constref v: TVertex): SizeInt; inline;
    function  SeparatePopI(aVertex: SizeInt): SizeInt; inline;
  { returns graph of connected component that contains v }
    function  SeparateGraph(constref v: TVertex): TWeighedGraph; inline;
    function  SeparateGraphI(aVtxIndex: SizeInt): TWeighedGraph; inline;
  { checks whether the graph is a regular graph (that is, the degree of all its
     vertices coincide); an empty graph is considered regular }
    function  IsRegular: Boolean; inline;
    function  IsTree: Boolean; inline;
    function  CycleExists(out aCycle: TIntVector): Boolean; inline;
  { checks whether exists any cycle in graph that contains v;
    if True then aCycle will contain indices of cycle }
    function  CycleExists(constref v: TVertex; out aCycle: TIntVector): Boolean; inline;
    function  CycleExistsI(aVtxIndex: SizeInt; out aCycle: TIntVector): Boolean; inline;
  { returns indices of the articulation points in the result vector, if any, otherwise the empty vector}
    function FindCutPoints: TIntVector; inline;
    function  EulerCycleExists: Boolean; inline;
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
    function  NegWeighedEdgeExists: Boolean;
  { finds the shortest paths from a given vertex to the remaining vertices in same connected component(SSSP),
    the weights of all edges must be nonnegative;
    the result contains in the corresponding component the weight of the minimum path to the vertex or
    InfiniteWeight if the vertex is unreachable; used Dijkstra's algorithm  }
    function  FindMinPaths(constref aSrc: TVertex): TWeightArray; inline;
    function  FindMinPathsI(aSrc: SizeInt): TWeightArray;
  { same as above and in aPathTree returns paths }
    function  FindMinPaths(constref aSrc: TVertex; out aPathTree: TIntArray): TWeightArray; inline;
    function  FindMinPathsI(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray; inline;
  { finds the shortest path weight from a aSrc to aDst if it exists(pathfinding);
    the weights of all edges must be nonnegative;
    the result contains shortest path weight or InfiniteWeight if the vertex is unreachable;
    used Dijkstra's algorithm  }
    function  FindMinPathWeight(constref aSrc, aDst: TVertex): TWeight; inline;
    function  FindMinPathWeightI(aSrc, aDst: SizeInt): TWeight;
  { same as above and in aPath returns path }
    function  FindMinPathWeight(constref aSrc, aDst: TVertex; out aPath: TIntVector): TWeight; inline;
    function  FindMinPathWeightI(aSrc, aDst: SizeInt; out aPath: TIntVector): TWeight;
  { finds the shortest path weight from a aSrc to aDst if it exists(pathfinding);
    the weights of all edges must be nonnegative;
    the result contains shortest path weight or InfiniteWeight if the vertex is unreachable;
    used A* algorithm  }
    function  FindMinPathAStar(constref aSrc, aDst: TVertex; aHeur: THeuristic; out aPath: TIntVector): TWeight; inline;
    function  FindMinPathAStarI(aSrc, aDst: SizeInt; aHeur: THeuristic; out aPath: TIntVector): TWeight;
  { finds the shortest paths from a given vertex to the remaining vertices in same connected component(SSSP);
    the weights of edges may be negative;
    returns False and empty aWeights if there is a negative weight cycle, otherwise
    aWeights will contain in the corresponding component the weight of the minimum path to the vertex or
    InfiniteWeight if the vertex is unreachable; used Bellman–Ford algorithm  }
    function  FindMinPathsFordB(constref aSrc: TVertex; out aWeights: TWeightArray): Boolean; inline;
    function  FindMinPathsFordBI(aSrc: SizeInt; out aWeights: TWeightArray): Boolean;
  { same as above and in aPaths returns paths,
    if there is a negative weight cycle, then aPaths will contain one vertex index that exactly
    lies on the cycle }
    function  FindMinPathsFordB(constref aSrc: TVertex; out aPaths: TIntArray; out aWeights: TWeightArray): Boolean; inline;
    function  FindMinPathsFordBI(aSrc: SizeInt; out aPaths: TIntArray; out aWeights: TWeightArray): Boolean;
  { finds a spanning tree of minimal weight, the graph must be connected(Kruskal's algorithm used)}
    function  FindMinSpanningTreeKrus(out aTotalWeight: TWeight): TIntArray;
  { finds a spanning tree of minimal weight, the graph must be connected(Prim's algorithm used) }
    function  FindMinSpanningTreePrim(out aTotalWeight: TWeight): TIntArray;

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
  Result{%H-}.Init(FCurrIndex, FEnum.Current);
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

constructor TGSimpleUGraph.TCutPointHelper.Create(aGraph: TGSimpleUGraph; aVector: PIntVector);
var
  I: SizeInt;
begin
  Graph := aGraph;
  Counter := 0;
  Visited.Size := aGraph.VertexCount;
  System.SetLength(Low, aGraph.VertexCount);
  for I := 0 to System.High(Low) do
    Low[I] := aGraph.VertexCount;
  System.SetLength(Ord, aGraph.VertexCount);
  for I := 0 to System.High(Ord) do
    Ord[I] := aGraph.VertexCount;
  Points := aVector;
end;

procedure TGSimpleUGraph.TCutPointHelper.Dfs(Curr: SizeInt; Prev: SizeInt);
var
  Next, ChildCount: SizeInt;
begin
  Visited[Curr] := True;
  Ord[Curr] := Counter;
  Low[Curr] := Counter;
  Inc(Counter);
  ChildCount := 0;
  for Next in Graph.AdjVerticesI(Curr) do
    if Next <> Prev then
      if not Visited[Next] then
        begin
          Dfs(Next, Curr);
          if Low[Next] < Low[Curr] then
            Low[Curr] := Low[Next];
          if (Low[Next] >= Ord[Curr]) and (Prev <> -1) then
            Points^.Add(Curr);
          Inc(ChildCount);
        end
      else
        if Ord[Next] < Low[Curr] then
          Low[Curr] := Ord[Next];
  if (Prev = -1) and (ChildCount > 1) then
    Points^.Add(Curr);
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
var
  p: ^TAdjItem;
begin
  if aSrc = aDst then
    exit(False);
  Result := not FVertexList.ItemRefs[aSrc]^.FindOrAdd(aDst, p);
  if Result then
    begin
      p^.Destination := aDst;
      p^.Data := aData;
        FVertexList.ItemRefs[aDst]^.FindOrAdd(aSrc, p);
        p^.Destination := aSrc;
        p^.Data := aData;
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

procedure TGSimpleUGraph.Clear;
begin
  inherited;
  FCompCount := 0;
  FConnected := False;
  FConnectedValid := False;
end;

function TGSimpleUGraph.AddVertex(constref v: TVertex; out aIndex: SizeInt): Boolean;
begin
  Result := not FVertexList.FindOrAdd(v, aIndex);
  if Result then
    begin
      FVertexList.ItemRefs[aIndex]^.CompIdx := -1;
      FConnectedValid := False;
    end;
end;

function TGSimpleUGraph.RemoveVertex(constref v: TVertex): Boolean;
begin
  Result := RemoveVertexI(FVertexList.IndexOf(v));
end;

function TGSimpleUGraph.RemoveVertexI(aIndex: SizeInt): Boolean;
begin
  Result := (aIndex >= 0) and (aIndex < FVertexList.Count);
  if Result then
    DoRemoveVertex(aIndex);
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
  if (aSrc < 0) or (aSrc >= FVertexList.Count) then
    exit(False);
  if (aDst < 0) or (aDst >= FVertexList.Count) then
    exit(False);
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
  if (aSrc < 0) or (aSrc >= FVertexList.Count) then
    exit(False);
  if (aDst < 0) or (aDst >= FVertexList.Count) then
    exit(False);
  Result := DoRemoveEdge(aSrc, aDst);
end;

procedure TGSimpleUGraph.SaveToStream(aStream: TStream; aWriteVertex: TOnWriteVertex; aWriteData: TOnWriteData);
var
  h: TStreamHeader;
  I: SizeInt;
  e: TEdge;
  bs: TWriteBufStream;
begin
  if not (Assigned(aWriteVertex) and Assigned(aWriteData)) then
    raise ELGraphError.Create(SEWriteCallbackMissed);
  bs := TWriteBufStream.Create(aStream);
  try
    //write header
    h.Magic := LGRAPH_MAGIC;
    h.Version := CURRENT_VERSION;
    h.TitleSize := System.Length(Title);
    h.VertexCount := VertexCount;
    h.EdgeCount := EdgeCount;
    bs.WriteBuffer(h, SizeOf(h));
    //write title
    bs.WriteBuffer(FTitle[1], h.TitleSize);
    //write Items, but does not save any info about connected
    //this should allow transfer data between directed/undirected graphs ???
    //or need save edges from dfs ???
    for I := 0 to Pred(h.VertexCount) do
      aWriteVertex(bs, FVertexList.ItemRefs[I]^.Vertex);
    //write edges
    for e in DistinctEdges do
      begin
        bs.WriteBuffer(e.Source, SizeOf(e.Source));
        bs.WriteBuffer(e.Destination, SizeOf(e.Destination));
        aWriteData(bs, e.Data);
      end;
  finally
    bs.Free;
  end;
end;

procedure TGSimpleUGraph.LoadFromStream(aStream: TStream; aReadVertex: TOnReadVertex; aReadData: TOnReadData);
var
  h: TStreamHeader;
  I, vInd: SizeInt;
  e: TEdge;
  v: TVertex;
  bs: TReadBufStream;
begin
  if not (Assigned(aReadVertex) and Assigned(aReadData)) then
    raise ELGraphError.Create(SEReadCallbackMissed);
  bs := TReadBufStream.Create(aStream);
  try
    //read header
    bs.ReadBuffer(h, SizeOf(h));
    if h.Magic <> LGRAPH_MAGIC then
      raise ELGraphError.Create(SEUnknownGraphStreamFmt);
    if h.Version > CURRENT_VERSION then
      raise ELGraphError.Create(SEUnsuppGraphFmtVersion);
    Clear;
    //read title
    System.SetLength(FTitle, h.TitleSize);
    bs.ReadBuffer(FTitle[1], h.TitleSize);
    //read Items
    for I := 0 to Pred(h.VertexCount) do
      begin
        v := aReadVertex(bs);
        if not AddVertex(v, vInd) then
          raise ELGraphError.Create(SEGraphStreamCorrupt);
        if vInd <> I then
          raise ELGraphError.Create(SEGraphStreamReadIntern);
      end;
    //read edges
    for I := 0 to Pred(h.EdgeCount) do
      begin
        bs.ReadBuffer(e.Source, SizeOf(e.Source));
        bs.ReadBuffer(e.Destination, SizeOf(e.Destination));
        e.Data := aReadData(bs);
        AddEdgeI(e.Source, e.Destination, e.Data);
      end;
  finally
    bs.Free;
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

function TGSimpleUGraph.Degree(constref v: TVertex): SizeInt;
begin
  Result := DegreeI(FVertexList.IndexOf(v));
end;

function TGSimpleUGraph.DegreeI(aIndex: SizeInt): SizeInt;
begin
  FVertexList.CheckIndexRange(aIndex);
  Result := FVertexList.ItemRefs[aIndex]^.Count;
end;

function TGSimpleUGraph.Isolated(constref v: TVertex): Boolean;
begin
  Result := Degree(v) = 0;
end;

function TGSimpleUGraph.IsolatedI(aIndex: SizeInt): Boolean;
begin
  Result := DegreeI(aIndex) = 0;
end;

function TGSimpleUGraph.IsConnected: Boolean;
begin
  if not ConnectedValid then
    FConnected := CheckConnected;
  Result := Connected;
end;

function TGSimpleUGraph.MakeConnected(aOnAddEdge: TOnAddEdge): SizeInt;
var
  Visited: TBitVector;
  Stack: TIntStack;
  I, Curr: SizeInt;
  d: TEdgeData;
begin
  Result := 0;
  if VertexCount < 2 then
    exit;
  if ConnectedValid and Connected then
    exit;
  Visited.Size := VertexCount;
  for I := 0 to Pred(VertexCount) do
    if not Visited[I] then
      begin
        Curr := I;
        repeat
          if not Visited[Curr] then
            begin
              Visited[Curr] := True;
              FVertexList.ItemRefs[Curr]^.CompIdx := 0;
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

function TGSimpleUGraph.SeparateGraph(constref v: TVertex): TGSimpleUGraph;
begin
  Result := SeparateGraphI(FVertexList.IndexOf(v));
end;

function TGSimpleUGraph.SeparateGraphI(aVtxIndex: SizeInt): TGSimpleUGraph;
begin
  if SeparateCount > 1 then
    Result := GetSeparateGraph(aVtxIndex)
  else
    Result := Clone;
end;

function TGSimpleUGraph.SeparateIndexOf(constref v: TVertex): SizeInt;
begin
   Result := SeparateIndexOfI(FVertexList.IndexOf(v));
end;

function TGSimpleUGraph.SeparateIndexOfI(aVtxIndex: SizeInt): SizeInt;
begin
  FVertexList.CheckIndexRange(aVtxIndex);
  if SeparateCount > 1 then
    Result := FVertexList.ItemRefs[aVtxIndex]^.CompIdx
  else
    Result := 0;
end;

function TGSimpleUGraph.SeparatePop(constref v: TVertex): SizeInt;
begin
  Result := SeparatePopI(FVertexList.IndexOf(v));
end;

function TGSimpleUGraph.SeparatePopI(aVtxIndex: SizeInt): SizeInt;
begin
  FVertexList.CheckIndexRange(aVtxIndex);
  if SeparateCount > 1 then
    Result := CountPop(FVertexList.ItemRefs[aVtxIndex]^.CompIdx)
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

function TGSimpleUGraph.CycleExists(out aCycle: TIntVector): Boolean;
var
  Stack: TIntStack;
  Visited: TBitVector;
  v: TIntArray;
  Curr, Next: SizeInt;
begin
  if IsEmpty then
    exit(False);
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

function TGSimpleUGraph.CycleExists(constref v: TVertex; out aCycle: TIntVector): Boolean;
begin
  Result := CycleExistsI(IndexOf(v), aCycle);
end;

function TGSimpleUGraph.CycleExistsI(aVtxIndex: SizeInt; out aCycle: TIntVector): Boolean;
var
  Stack: TIntStack;
  Visited: TBitVector;
  v: TIntArray;
  Curr, Next: SizeInt;
begin
  FVertexList.CheckIndexRange(aVtxIndex);
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
            if (v[Curr] <> Next) and (Curr = aVtxIndex) then
              begin
                aCycle := CycleChainFromTree(v, Next, Curr);
                exit(True);
              end;
      end;
  until not Stack.TryPop(Curr);
  Result := False;
end;

function TGSimpleUGraph.EulerCycleExists: Boolean;
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
  if not EulerCycleExists then
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

function TGSimpleUGraph.FindCutPoints: TIntVector;
var
  d: TCutPointHelper;
begin
  if IsEmpty then
    exit;
  d := TCutPointHelper.Create(Self, @Result);

  d.Dfs(0);
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

class function TGWeighedUGraph.TWeightEdge.Construct(s, d: SizeInt; w: TWeight): TWeightEdge;
begin
  Result.Source := s;
  Result.Destination := d;
  Result.Weight := w;
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

class function TGWeighedUGraph.TWeightItem.Construct(constref w: TWeight; aIndex: SizeInt): TWeightItem;
begin
  Result.Weight := w;
  Result.Index := aIndex;
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

class function TGWeighedUGraph.TRankItem.Construct(constref aRank, aWeight: TWeight; aIndex: SizeInt): TRankItem;
begin
  Result.Rank := aRank;
  Result.Weight := aWeight;
  Result.Index := aIndex;
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
  Handles[aSrc] := Queue.Insert(TWeightItem.Construct(ZeroWeight, aSrc));
  while Queue.TryDequeue(Item) do
    if not Visited[Item.Index] then
      begin
        Visited[Item.Index] := True;
        Result[Item.Index] := Item.Weight;
        for p in FGraph.AdjVerticesPtr(Item.Index) do
          if Handles[p^.Key] = INVALID_HANDLE then
            Handles[p^.Key] := Queue.Insert(TWeightItem.Construct(p^.Data.Weight + Item.Weight, p^.Key))
          else
            if not Visited[p^.Key] then
              begin
                Relaxed := p^.Data.Weight + Item.Weight;
                if Relaxed < Queue.Value(Handles[p^.Key]).Weight then
                  Queue.Update(Handles[p^.Key], TWeightItem.Construct(Relaxed, p^.Key));
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
  Handles[aSrc] := Queue.Insert(TWeightItem.Construct(ZeroWeight, aSrc));
  while Queue.TryDequeue(Item) do
    if not Visited[Item.Index] then
      begin
        Visited[Item.Index] := True;
        Result[Item.Index] := Item.Weight;
        for p in FGraph.AdjVerticesPtr(Item.Index) do
          if Handles[p^.Key] = INVALID_HANDLE then
            begin
              Handles[p^.Key] := Queue.Insert(TWeightItem.Construct(p^.Data.Weight + Item.Weight, p^.Key));
              aPathTree[p^.Key] := Item.Index;
            end
          else
            if not Visited[p^.Key] then
              begin
                Relaxed := p^.Data.Weight + Item.Weight;
                if Relaxed < Queue.Value(Handles[p^.Key]).Weight then
                  begin
                    Queue.Update(Handles[p^.Key], TWeightItem.Construct(Relaxed, p^.Key));
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
  Handles[aSrc] := Queue.Insert(TWeightItem.Construct(ZeroWeight, aSrc));
  while Queue.TryDequeue(Item) do
    if not Visited[Item.Index] then
      begin
        if Item.Index = aDst then
          exit(Item.Weight);
        Visited[Item.Index] := True;
        for p in FGraph.AdjVerticesPtr(Item.Index) do
          if Handles[p^.Key] = INVALID_HANDLE then
            Handles[p^.Key] := Queue.Insert(TWeightItem.Construct(p^.Data.Weight + Item.Weight, p^.Key))
          else
            if not Visited[p^.Key] then
              begin
                Relaxed := p^.Data.Weight + Item.Weight;
                if Relaxed < Queue.Value(Handles[p^.Key]).Weight then
                  Queue.Update(Handles[p^.Key], TWeightItem.Construct(Relaxed, p^.Key));
              end
      end;
  Result := InfiniteWeight;
end;

function TGWeighedUGraph.DijkstraPath(aSrc, aDst: SizeInt; out aPath: TIntVector): TWeight;
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
  Handles[aSrc] := Queue.Insert(TWeightItem.Construct(ZeroWeight, aSrc));
  while Queue.TryDequeue(Item) do
    if not Visited[Item.Index] then
      begin
        if Item.Index = aDst then
          begin
            aPath := FGraph.ChainFromTree(Tree, aDst);
            exit(Item.Weight);
          end;
        Visited[Item.Index] := True;
        for p in FGraph.AdjVerticesPtr(Item.Index) do
          begin
            if Handles[p^.Key] = INVALID_HANDLE then
              begin
                Handles[p^.Key] := Queue.Insert(TWeightItem.Construct(p^.Data.Weight + Item.Weight, p^.Key));
                Tree[p^.Key] := Item.Index;
              end
            else
              if not Visited[p^.Key] then
                begin
                  Relaxed := p^.Data.Weight + Item.Weight;
                  if Relaxed < Queue.Value(Handles[p^.Key]).Weight then
                    begin
                      Queue.Update(Handles[p^.Key], TWeightItem.Construct(Relaxed, p^.Key));
                      Tree[p^.Key] := Item.Index;
                    end;
                end;
          end;
      end;
  Result := InfiniteWeight;
end;

function TGWeighedUGraph.AStar(aSrc, aDst: SizeInt; aHeur: THeuristic; out aPath: TIntVector): TWeight;
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
  Handles[aSrc] := Queue.Insert(TRankItem.Construct(aHeur(FGraph[aSrc], FGraph[aDst]), ZeroWeight, aSrc));
  while Queue.TryDequeue(Item) do
    if not Visited[Item.Index] then
      begin
        if Item.Index = aDst then
          begin
            aPath := FGraph.ChainFromTree(Tree, aDst);
            exit(Item.Weight);
          end;
        Visited[Item.Index] := True;
        for p in FGraph.AdjVerticesPtr(Item.Index) do
          begin
            if Handles[p^.Key] = INVALID_HANDLE then
              begin
                Relaxed := p^.Data.Weight + Item.Weight;
                Handles[p^.Key] := Queue.Insert(TRankItem.Construct(
                  Relaxed + aHeur(FGraph[p^.Key], FGraph[aDst]), Relaxed, p^.Key));
                Tree[p^.Key] := Item.Index;
              end
            else
              if not Visited[p^.Key] then
                begin
                  Relaxed := Item.Weight + p^.Data.Weight;
                  if Relaxed < Queue.Value(Handles[p^.Key]).Weight then
                    begin
                      Queue.Update(Handles[p^.Key], TRankItem.Construct(
                        Relaxed + aHeur(FGraph[p^.Key], FGraph[aDst]), Relaxed, p^.Key));
                      Tree[p^.Key] := Item.Index;
                    end;
                end;
          end;
      end;
  Result := InfiniteWeight;
end;

function TGWeighedUGraph.FordBellman(aSrc: SizeInt; out aWeights: TWeightArray): Boolean;
var
  e: TEdge;
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
          e := Enum.Current;
          if aWeights[e.Source] < InfiniteWeight then
            begin
              RelaxValue := aWeights[e.Source] + e.Data.Weight;
              if RelaxValue < aWeights[e.Destination] then
                begin
                  aWeights[e.Destination] := RelaxValue;
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

function TGWeighedUGraph.FordBellman(aSrc: SizeInt; out aPathTree: TIntArray; out aWeights: TWeightArray): Boolean;
var
  e: TEdge;
  Enum: TEdgeEnumerator;
  RelaxValue: TWeight;
  I, J: SizeInt;
  Relaxed: Boolean = False;
begin
  aWeights := CreateWeightVector;
  aPathTree := FGraph.CreateIntArray;
  Enum := Edges.GetEnumerator;
  aWeights[aSrc] := ZeroWeight;
  for I := 1 to VertexCount do
    begin
      Relaxed := False;
      while Enum.MoveNext do
        begin
          e := Enum.Current;
          if aWeights[e.Source] < InfiniteWeight then
            begin
              RelaxValue := aWeights[e.Source] + e.Data.Weight;
              if RelaxValue < aWeights[e.Destination] then
                begin
                  aWeights[e.Destination] := RelaxValue;
                  J := e.Destination;
                  aPathTree[J] := e.Source;
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
    begin
      aWeights := nil;
      System.SetLength(aPathTree, 1);
      aPathTree[0] := J;
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
  Handles[0] := Queue.Insert(TWeightItem.Construct(ZeroWeight, 0));
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
                Handles[p^.Key] := Queue.Insert(TWeightItem.Construct(p^.Data.Weight, p^.Key));
                Result[p^.Key] := Curr;
              end
            else
              if not Visited[p^.Key] and (p^.Data.Weight < Queue.Value(Handles[p^.Key]).Weight) then
                begin
                  Queue.Update(Handles[p^.Key], TWeightItem.Construct(p^.Data.Weight, p^.Key));
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
      Result[I].Source := e.Source;
      Result[I].Destination := e.Destination;
      Result[I].Weight := e.Data.Weight;
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

function TGWeighedUGraph.ContainsVertex(constref v: TVertex): Boolean;
begin
  Result := FGraph.ContainsVertex(v);
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

function TGWeighedUGraph.IndexOf(constref v: TVertex): SizeInt;
begin
  Result := FGraph.IndexOf(v);
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

function TGWeighedUGraph.Edges: TEdges;
begin
  Result := FGraph.Edges;
end;

function TGWeighedUGraph.DistinctEdges: TDistinctEdges;
begin
  Result := FGraph.DistinctEdges;
end;

function TGWeighedUGraph.AddVertex(constref v: TVertex; out aIndex: SizeInt): Boolean;
begin
  Result := FGraph.AddVertex(v, aIndex);
end;

function TGWeighedUGraph.RemoveVertex(constref v: TVertex): Boolean;
begin
  Result := FGraph.RemoveVertex(v);
end;

function TGWeighedUGraph.RemoveVertexI(aIndex: SizeInt): Boolean;
begin
  Result := FGraph.RemoveVertexI(aIndex);
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

function TGWeighedUGraph.FindMinPathLen(constref aSrc, aDst: TVertex): SizeInt;
begin
  Result := FGraph.FindMinPathLen(aSrc, aDst);
end;

function TGWeighedUGraph.FindMinPathLenI(aSrc, aDst: SizeInt): SizeInt;
begin
  Result := FGraph.FindMinPathLenI(aSrc, aDst);
end;

function TGWeighedUGraph.FindMinPathLenMap(constref aRoot: TVertex): TIntArray;
begin
  Result := FGraph.FindMinPathLenMap(aRoot);
end;

function TGWeighedUGraph.FindMinPathLenMapI(aRoot: SizeInt): TIntArray;
begin
  Result := FGraph.FindMinPathLenMapI(aRoot);
end;

function TGWeighedUGraph.FindMinPath(constref aSrc, aDst: TVertex): TIntVector;
begin
  Result := FGraph.FindMinPath(aSrc, aDst);
end;

function TGWeighedUGraph.FindMinPathI(aSrc, aDst: SizeInt): TIntVector;
begin
  Result := FGraph.FindMinPathI(aSrc, aDst);
end;

function TGWeighedUGraph.Degree(constref v: TVertex): SizeInt;
begin
  Result := FGraph.Degree(v);
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

function TGWeighedUGraph.MakeConnected(aOnAddEdge: TOnAddEdge): SizeInt;
begin
  Result := FGraph.MakeConnected(aOnAddEdge);
end;

function TGWeighedUGraph.SeparateIndex(constref v: TVertex): SizeInt;
begin
  Result := FGraph.SeparateIndexOf(v);
end;

function TGWeighedUGraph.SeparateIndexI(aVertex: SizeInt): SizeInt;
begin
  Result := FGraph.SeparateIndexOfI(aVertex);
end;

function TGWeighedUGraph.SeparatePop(constref v: TVertex): SizeInt;
begin
  Result := FGraph.SeparatePop(v);
end;

function TGWeighedUGraph.SeparatePopI(aVertex: SizeInt): SizeInt;
begin
  Result := FGraph.SeparatePopI(aVertex);
end;

function TGWeighedUGraph.SeparateGraph(constref v: TVertex): TWeighedGraph;
begin
  Result := SeparateGraphI(IndexOf(v));
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

function TGWeighedUGraph.CycleExists(out aCycle: TIntVector): Boolean;
begin
  Result := FGraph.CycleExists(aCycle);
end;

function TGWeighedUGraph.CycleExists(constref v: TVertex; out aCycle: TIntVector): Boolean;
begin
  Result := FGraph.CycleExists(v, aCycle);
end;

function TGWeighedUGraph.CycleExistsI(aVtxIndex: SizeInt; out aCycle: TIntVector): Boolean;
begin
  Result := FGraph.CycleExistsI(aVtxIndex, aCycle);
end;

function TGWeighedUGraph.FindCutPoints: TIntVector;
begin
  Result := FGraph.FindCutPoints;
end;

function TGWeighedUGraph.EulerCycleExists: Boolean;
begin
  Result := FGraph.EulerCycleExists;
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

function TGWeighedUGraph.NegWeighedEdgeExists: Boolean;
var
  e: TEdge;
begin
  for e in DistinctEdges do
    if e.Data.Weight < 0 then
      exit(True);
  Result := False;
end;

function TGWeighedUGraph.FindMinPaths(constref aSrc: TVertex): TWeightArray;
begin
  Result := FindMinPathsI(FGraph.IndexOf(aSrc));
end;

function TGWeighedUGraph.FindMinPathsI(aSrc: SizeInt): TWeightArray;
begin
  FGraph.CheckIndexRange(aSrc);
  Result := DijkstraSssp(aSrc);
end;

function TGWeighedUGraph.FindMinPaths(constref aSrc: TVertex; out aPathTree: TIntArray): TWeightArray;
begin
  Result := FindMinPathsI(FGraph.IndexOf(aSrc), aPathTree);
end;

function TGWeighedUGraph.FindMinPathsI(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
begin
  FGraph.CheckIndexRange(aSrc);
  Result := DijkstraSssp(aSrc, aPathTree);
end;

function TGWeighedUGraph.FindMinPathWeight(constref aSrc, aDst: TVertex): TWeight;
begin
  Result := FindMinPathWeightI(FGraph.IndexOf(aSrc), FGraph.IndexOf(aDst));
end;

function TGWeighedUGraph.FindMinPathWeightI(aSrc, aDst: SizeInt): TWeight;
begin
  FGraph.CheckIndexRange(aSrc);
  FGraph.CheckIndexRange(aDst);
  Result := DijkstraPath(aSrc, aDst);
end;

function TGWeighedUGraph.FindMinPathWeight(constref aSrc, aDst: TVertex; out aPath: TIntVector): TWeight;
begin
  Result := FindMinPathWeightI(FGraph.IndexOf(aSrc), FGraph.IndexOf(aDst), aPath);
end;

function TGWeighedUGraph.FindMinPathWeightI(aSrc, aDst: SizeInt; out aPath: TIntVector): TWeight;
begin
  FGraph.CheckIndexRange(aSrc);
  FGraph.CheckIndexRange(aDst);
  Result := DijkstraPath(aSrc, aDst, aPath);
end;

function TGWeighedUGraph.FindMinPathAStar(constref aSrc, aDst: TVertex; aHeur: THeuristic; out aPath: TIntVector
  ): TWeight;
begin
  Result := FindMinPathAStarI(FGraph.IndexOf(aSrc), FGraph.IndexOf(aSrc), aHeur, aPath);
end;

function TGWeighedUGraph.FindMinPathAStarI(aSrc, aDst: SizeInt; aHeur: THeuristic; out aPath: TIntVector): TWeight;
begin
  FGraph.CheckIndexRange(aSrc);
  FGraph.CheckIndexRange(aDst);
  Result := AStar(aSrc, aDst, aHeur, aPath);
end;

function TGWeighedUGraph.FindMinPathsFordB(constref aSrc: TVertex; out aWeights: TWeightArray): Boolean;
begin
  Result := FindMinPathsFordBI(FGraph.IndexOf(aSrc), aWeights);
end;

function TGWeighedUGraph.FindMinPathsFordBI(aSrc: SizeInt; out aWeights: TWeightArray): Boolean;
begin
  FGraph.CheckIndexRange(aSrc);
  Result := FordBellman(aSrc, aWeights);
end;

function TGWeighedUGraph.FindMinPathsFordB(constref aSrc: TVertex; out aPaths: TIntArray;
  out aWeights: TWeightArray): Boolean;
begin
  Result := FindMinPathsFordBI(FGraph.IndexOf(aSrc), aPaths, aWeights);
end;

function TGWeighedUGraph.FindMinPathsFordBI(aSrc: SizeInt; out aPaths: TIntArray;
  out aWeights: TWeightArray): Boolean;
begin
  FGraph.CheckIndexRange(aSrc);
  Result := FordBellman(aSrc, aPaths, aWeights);
end;

function TGWeighedUGraph.FindMinSpanningTreeKrus(out aTotalWeight: TWeight): TIntArray;
begin
  if IsConnected then
    Result := KruskalMst(aTotalWeight)
    //Result := FilterKruskalMst(aTotalWeight)
  else
    raise ELGraphError.Create(SEGraphIsNotConnected);
end;

function TGWeighedUGraph.FindMinSpanningTreePrim(out aTotalWeight: TWeight): TIntArray;
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

