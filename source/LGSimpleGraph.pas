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
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils,
  LGUtils,
  {%H-}LGHelpers,
  LGArrayHelpers,
  LGVector,
  LGPriorityQueue,
  LGHashTable,
  LGHash,
  LGraphUtils,
  LGStrConst;

type

  TIntPair = record
  private
    FLess,
    FGreater: SizeInt;
    function GetKey: TIntPair; inline;
  public
    class function HashCode(const aValue: TIntPair): SizeInt; static; inline;
    class function Equal(const L, R: TIntPair): Boolean; static; inline;
    constructor Create(L, R: SizeInt);
    property Left: SizeInt read FLess;
    property Right: SizeInt read FGreater;
    property Key: TIntPair read GetKey;
  end;

  PIntPair = ^TIntPair;

  TIntPairSet = record
  private
  type
    TTable = specialize TGLiteHashTableLP<TIntPair, TIntPair, TIntPair>;
  var
    FTable: TTable;
    function GetCount: SizeInt; inline;
  public
    function Contains(L, R: SizeInt): Boolean; inline;
    function Add(L, R: SizeInt): Boolean;
    function Remove(L, R: SizeInt): Boolean; inline;
    property Count: SizeInt read GetCount;
  end;


  { TGSimpleGraph: simple sparse undirected graph based on adjacency lists;
      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGSimpleGraph<TVertex, TEdgeData, TVertexEqRel> = class(specialize TGCustomGraph
    <TVertex, TEdgeData, TVertexEqRel>)
  protected
  type
    TDistinctEdgeEnumerator = record
    private
      FVisited: TBitVector;
      FList: TNodeList;
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

    TCutPointHelper = record
    private
      Graph: TGSimpleGraph;
      Lowest,
      InOrder: TIntArray;
      Counter,
      Total: SizeInt;
      Points: PIntVector;
      NewEdges: PIntEdgeVector;
      procedure Init(aGraph: TGSimpleGraph; aVector: PIntVector);
      procedure InitRemove(aGraph: TGSimpleGraph; aNewEdges: PIntEdgeVector);
      procedure Dfs(Curr: SizeInt; Prev: SizeInt = -1);
      function  DfsFind(Curr: SizeInt; Prev: SizeInt = -1): Boolean;
      procedure DfsRemove(Curr: SizeInt; Prev: SizeInt = -1);
    public
      procedure Search(aGraph: TGSimpleGraph; aVector: PIntVector; aFrom: SizeInt);
      function  ContainsAny(aGraph: TGSimpleGraph; aFrom: SizeInt): Boolean;
      procedure Remove(aGraph: TGSimpleGraph; aNewEdges: PIntEdgeVector);
    end;

    TBridgeHelper = record
      Graph: TGSimpleGraph;
      Lowest,
      InOrder: TIntArray;
      Counter,
      Total: SizeInt;
      Bridges: PIntEdgeVector;
      procedure Init(aGraph: TGSimpleGraph; aVector: PIntEdgeVector);
      procedure Dfs(Curr: SizeInt; Prev: SizeInt = -1);
      function  DfsFind(Curr: SizeInt; Prev: SizeInt = -1): Boolean;
    public
      procedure Search(aGraph: TGSimpleGraph; aVector: PIntEdgeVector);
      function  ContainsAny(aGraph: TGSimpleGraph): Boolean;
    end;

  var
    FCompCount: SizeInt;
    FConnected,
    FConnectedValid: Boolean;
    procedure DoRemoveVertex(aIndex: SizeInt);
    function  DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
    function  GetSeparateGraph(aIndex: SizeInt): TGSimpleGraph;
    function  CheckPathExists(aSrc, aDst: SizeInt): Boolean;
    function  FindSeparateCount: SizeInt;
    function  GetSeparateCount: SizeInt;
    function  CountPop(aCompIndex: SizeInt): SizeInt;
    function  MakeConnected(aOnAddEdge: TOnAddEdge): SizeInt;
    function  CycleExists(aRoot: SizeInt; out aCycle: TIntArray): Boolean;
    procedure SearchForCutPoints(aRoot: SizeInt; var aPoints: TIntVector);
    function  CutPointExists(aRoot: SizeInt): Boolean;
    procedure SearchForBiconnect(aRoot: SizeInt; var aEdges: TIntEdgeVector);
    procedure SearchForBicomponent(aRoot: SizeInt; var aComp: TEdgeArrayVector);
    function  BridgeExists: Boolean;
    procedure SearchForBridges(var aBridges: TIntEdgeVector);
    procedure SearchForFundamentalsCycles(aRoot: SizeInt; out aCycles: TIntArrayVector);
    procedure SearchForFundamentalsCyclesLen(aRoot: SizeInt; out aCycleLens: TIntVector);
    function  FindFundamentalCyclesLen(out aCycleLens: TIntVector): Boolean;
    function  CmpIntArrayLen(constref L, R: TIntArray): SizeInt;
    property  InnerConnected: Boolean read FConnected;
    property  ConnectedValid: Boolean read FConnectedValid;
  public
    class function MayBeEqual(L, R: TGSimpleGraph): Boolean;
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
    function  Connected: Boolean;
  { if the graph is not empty, then make graph connected, adding, if necessary, new edges
    from the vertex with the index 0; returns count of added edges }
    function  EnsureConnected(aOnAddEdge: TOnAddEdge): SizeInt;
  { checks whether the aDst reachable from the aSrc; each vertex reachable from itself  }
    function  PathExists(constref aSrc, aDst: TVertex): Boolean; inline;
    function  PathExistsI(aSrc, aDst: SizeInt): Boolean;
  { returns graph of connected component that contains aVertex }
    function  SeparateGraph(constref aVertex: TVertex): TGSimpleGraph; inline;
    function  SeparateGraphI(aVtxIndex: SizeInt): TGSimpleGraph;
  { returns index of the connected component that contains aVertex }
    function  SeparateIndexOf(constref aVertex: TVertex): SizeInt; inline;
    function  SeparateIndexI(aVtxIndex: SizeInt): SizeInt;
  { returns number of Items(population) in the InnerConnected component that contains aVertex }
    function  SeparatePop(constref aVertex: TVertex): SizeInt; inline;
    function  SeparatePopI(aVtxIndex: SizeInt): SizeInt;
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
  { returns a graph constructed from the pairs provided by the array,
    i.e. each element treates as pair of source - destination }
    function  CreateFromArray(constref aValue: TIntArray): TGSimpleGraph;
  { returns a graph constructed from the edges indices provided by the array }
    function  CreateFromEdgeArray(constref aEdges: TIntEdgeArray): TGSimpleGraph;
    function  DistinctEdges: TDistinctEdges; inline;
  { returns copy of the source graph }
    function  Clone: TGSimpleGraph;
  { returns complement of the source graph;
    warning: if the source graph is sparse then complement is dense }
    function  Complement(aOnAddEdge: TOnAddEdge): TGSimpleGraph;
  { count of connected components }
    property  SeparateCount: SizeInt read GetSeparateCount;
  end;

  { TGOutline: simple outline;
      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGOutline<TVertex, TVertexEqRel> = class(specialize TGSimpleGraph<TVertex, TEmptyRec, TVertexEqRel>)
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
    function  Complement: TGOutline;
  end;

  TIntOutline = class(specialize TGOutline<SizeInt, SizeInt>)
  protected
    procedure WriteVertex(aStream: TStream; constref aValue: SizeInt);
    procedure ReadVertex(aStream: TStream; out aValue: SizeInt);
  public
    procedure SaveToStream(aStream: TStream);
    procedure LoadFromStream(aStream: TStream);
    procedure SaveToFile(const aFileName: string);
    procedure LoadFromFile(const aFileName: string);
  end;

  { TStrOutline
    note: SaveToStream limitation for max string length = High(SmallInt) }
  TStrOutline = class(specialize TGOutline<string, string>)
  protected
    procedure WriteVertex(aStream: TStream; constref aValue: string);
    procedure ReadVertex(aStream: TStream; out aValue: string);
  public
    procedure SaveToStream(aStream: TStream);
    procedure LoadFromStream(aStream: TStream);
    procedure SaveToFile(const aFileName: string);
    procedure LoadFromFile(const aFileName: string);
  end;

  THandle = LGUtils.THandle;

  { TGWeighedGraph: simple sparse undirected weighed graph based on adjacency lists;

      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean;

      TEdgeData MUST have field/property/function Weight: TWeight;

      TWeight must have defined comparision operators and properties MinValue, MaxValue,
      which used as infinity weight values;
      Default(TWeight) used as zero weight value }
  generic TGWeighedGraph<TVertex, TWeight, TEdgeData, TVertexEqRel> = class(
     specialize TGSimpleGraph<TVertex, TEdgeData, TVertexEqRel>)
  public
  type
    TWeightArray = array of TWeight;
    THeuristic   = function(constref aSrc, aDst: TVertex): TWeight;

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
      function  FindMst(aGraph: TGWeighedGraph; out aWeight: TWeight): TIntArray;
    end;
  strict private
  class var
    CFInfiniteWeight,
    CFNegInfiniteWeight,
    CFZeroWeight: TWeight;

  protected
  { Dijkstra's algorithm: single-source shortest paths problem for non-negative weights  }
    function  DijkstraSssp(aSrc: SizeInt): TWeightArray;
    function  DijkstraSssp(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
  { Dijkstra's pathfinding algorithm }
    function  DijkstraPath(aSrc, aDst: SizeInt): TWeight;
    function  DijkstraPath(aSrc, aDst: SizeInt; out aWeight: TWeight): TIntArray;
  { A* pathfinding algorithm }
    function  AStar(aSrc, aDst: SizeInt; aHeur: THeuristic; out aWeight: TWeight): TIntArray;
  { Bellman-Ford algorithm: single-source shortest paths problem for any weights  }
    function  FordBellman(aSrc: SizeInt; out aWeights: TWeightArray): Boolean;
    function  FordBellman(aSrc: SizeInt; out aPaths: TIntArray; out aWeights: TWeightArray): Boolean;
    function  FilterKruskalMst(out aTotalWeight: TWeight): TIntArray;
    function  KruskalMst(out aTotalWeight: TWeight): TIntArray;
    function  PrimMst(out aTotalWeight: TWeight): TIntArray;
    function  CreateWeighArray: TWeightArray;
    function  CreateEdgeArray: TEdgeArray;
    class function Min(const L, R: TWeight): TWeight; static; inline;
    class constructor Init;
  public
    class property InfiniteWeight: TWeight read CFInfiniteWeight;
    class property NegInfiniteWeight: TWeight read CFNegInfiniteWeight;
    class property ZeroWeight: TWeight read CFZeroWeight;

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
    function  MinPathsMapI(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
  { finds the path of minimal weight from a aSrc to aDst if it exists(pathfinding);
    the weights of all edges must be nonnegative;
    returns path weight or InfiniteWeight if the vertex is unreachable; used Dijkstra's algorithm  }
    function  MinPathWeight(constref aSrc, aDst: TVertex): TWeight; inline;
    function  MinPathWeightI(aSrc, aDst: SizeInt): TWeight;
  { returns the path of minimal weight from a aSrc to aDst, if exists, and it weight in aWeight }
    function  MinPath(constref aSrc, aDst: TVertex; out aWeight: TWeight): TIntArray; inline;
    function  MinPathI(aSrc, aDst: SizeInt; out aWeight: TWeight): TIntArray;
  { finds the path of minimal weight from a aSrc to aDst if it exists(pathfinding);
    the weights of all edges must be nonnegative;
    the result contains shortest path weight or InfiniteWeight if the vertex is unreachable;
    used A* algorithm  }
    function  MinPathAStar(constref aSrc, aDst: TVertex; aHeur: THeuristic; out aWeight: TWeight): TIntArray; inline;
    function  MinPathAStarI(aSrc, aDst: SizeInt; aHeur: THeuristic; out aWeight: TWeight): TIntArray;
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
  end;

  TRealPointEdge = record
    Weight: ValReal;
    constructor Create(const aWeight: ValReal);
  end;

  { TPointsOutline }
  TPointsOutline = class(specialize TGWeighedGraph<TPoint, ValReal, TRealPointEdge, TPoint>)
  protected
    procedure OnAddEdge(constref aSrc, aDst: TPoint; aData: Pointer);
    procedure WritePoint(aStream: TStream; constref aValue: TPoint);
    procedure WriteWeight(aStream: TStream; constref aValue: TRealPointEdge);
    procedure ReadPoint(aStream: TStream; out aValue: TPoint);
    procedure ReadWeight(aStream: TStream; out aValue: TRealPointEdge);
  public
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
    function  Clone: TPointsOutline;
    function  Complement: TPointsOutline;
  end;

implementation
{$B-}{$COPERATORS ON}
uses
  bufstream;

{ TIntPairSet }

function TIntPairSet.GetCount: SizeInt;
begin
  Result := FTable.Count;
end;

function TIntPairSet.Contains(L, R: SizeInt): Boolean;
var
  Dummy: SizeInt;
begin
  Result := FTable.Find(TIntPair.Create(L, R), Dummy) <> nil;
end;

function TIntPairSet.Add(L, R: SizeInt): Boolean;
var
  Dummy: SizeInt;
  p: PIntPair;
  v: TIntPair;
begin
  v := TIntPair.Create(L, R);
  Result := not FTable.FindOrAdd(v, p, Dummy);
  if Result then
    p^ := v;
end;

function TIntPairSet.Remove(L, R: SizeInt): Boolean;
begin
  Result := FTable.Remove(TIntPair.Create(L, R));
end;

{ TIntPair }

function TIntPair.GetKey: TIntPair;
begin
  Result := Self;
end;

class function TIntPair.HashCode(const aValue: TIntPair): SizeInt;
begin
{$IF DEFINED (CPU64)}
    Result := TxxHash32LE.HashBuf(@aValue, SizeOf(aValue));
{$ELSEIF DEFINED (CPU32)}
   Result := TxxHash32LE.HashQWord(QWord(aValue));
{$ELSE }
   Result := TxxHash32LE.HashDWord(DWord(aValue));
{$ENDIF}
end;

class function TIntPair.Equal(const L, R: TIntPair): Boolean;
begin
  Result := (L.Left = R.Left) and (L.Right = R.Right);
end;

constructor TIntPair.Create(L, R: SizeInt);
begin
  if L <= R then
    begin
      FLess := L;
      FGreater := R;
    end
  else
    begin
      FLess := R;
      FGreater := L;
    end;
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
        FVisited[FCurrIndex] := True;
        FEnum := FList[FCurrIndex].Item.GetEnumerator;
      end;
    Result := FEnum.MoveNext;
    FEnumDone := not Result;
    if Result then
      Result := not FVisited[FEnum.GetCurrent^.Key];
  until Result;
end;

procedure TGSimpleGraph.TDistinctEdgeEnumerator.Reset;
begin
  FCurrIndex := -1;
  FEnumDone := True;
  FVisited.ClearBits;
end;

{ TGSimpleGraph.TDistinctEdges }

function TGSimpleGraph.TDistinctEdges.GetEnumerator: TDistinctEdgeEnumerator;
begin
  Result.FList := FGraph.FNodeList;
  Result.FLastIndex := Pred(FGraph.VertexCount);
  Result.FVisited.Size := Succ(Result.FLastIndex);
  Result.FCurrIndex := -1;
  Result.FEnumDone := True;
end;

{ TGSimpleGraph.TCutPointHelper }

procedure TGSimpleGraph.TCutPointHelper.Init(aGraph: TGSimpleGraph; aVector: PIntVector);
begin
  Graph := aGraph;
  Total := Graph.VertexCount;
  Lowest := Graph.CreateIntArray(Total);
  InOrder := Graph.CreateIntArray(Total);
  Points := aVector;
  Counter := 0;
end;

procedure TGSimpleGraph.TCutPointHelper.InitRemove(aGraph: TGSimpleGraph; aNewEdges: PIntEdgeVector);
begin
  Graph := aGraph;
  Total := Graph.VertexCount;
  Lowest := Graph.CreateIntArray(Total);
  InOrder := Graph.CreateIntArray(Total);
  NewEdges := aNewEdges;
  Counter := 0;
end;

procedure TGSimpleGraph.TCutPointHelper.Dfs(Curr: SizeInt; Prev: SizeInt);
var
  Next, ChildCount: SizeInt;
begin
  InOrder[Curr] := Counter;
  Lowest[Curr] := Counter;
  Inc(Counter);
  ChildCount := 0;
  for Next in Graph.AdjVerticesI(Curr) do
    if Next <> Prev then
      if InOrder[Next] = Total then
        begin
          Dfs(Next, Curr);
          if Lowest[Curr] > Lowest[Next] then
            Lowest[Curr] := Lowest[Next];
          if (Lowest[Next] >= InOrder[Curr]) and (Prev <> -1) then
            Points^.Add(Curr);
          Inc(ChildCount);
        end
      else
        if Lowest[Curr] > InOrder[Next] then
          Lowest[Curr] := InOrder[Next];
  if (Prev = -1) and (ChildCount > 1) then
    Points^.Add(Curr);
end;

function TGSimpleGraph.TCutPointHelper.DfsFind(Curr: SizeInt; Prev: SizeInt): Boolean;
var
  Next, ChildCount: SizeInt;
begin
  InOrder[Curr] := Counter;
  Lowest[Curr] := Counter;
  Inc(Counter);
  ChildCount := 0;
  for Next in Graph.AdjVerticesI(Curr) do
    if Next <> Prev then
      if InOrder[Next] = Total then
        begin
          if DfsFind(Next, Curr) then
            exit(True);
          if Lowest[Curr] > Lowest[Next] then
            Lowest[Curr] := Lowest[Next];
          if (Lowest[Next] >= InOrder[Curr]) and (Prev <> -1) then
            exit(True);
          Inc(ChildCount);
        end
      else
        if Lowest[Curr] > InOrder[Next] then
          Lowest[Curr] := InOrder[Next];
  if (Prev = -1) and (ChildCount > 1) then
    exit(True);
  Result := False;
end;

procedure TGSimpleGraph.TCutPointHelper.DfsRemove(Curr: SizeInt; Prev: SizeInt);
var
  Next, Across: SizeInt;
begin
  InOrder[Curr] := Counter;
  Lowest[Curr] := Counter;
  Inc(Counter);
  Across := -1;
  for Next in Graph.AdjVerticesI(Curr) do
    if Next <> Prev then
      if InOrder[Next] = Total then
        begin
          if Across = -1 then
            Across := Next;
          DfsRemove(Next, Curr);
          if Lowest[Curr] > Lowest[Next] then
            Lowest[Curr] := Lowest[Next];
          if Lowest[Next] >= InOrder[Curr] then
            begin
              if Next = Across then
                begin
                  if Prev <> -1 then
                    NewEdges^.Add(TIntEdge.Create(Prev, Next));
                end
              else
                NewEdges^.Add(TIntEdge.Create(Across, Next));
            end;
        end
      else
        if Lowest[Curr] > InOrder[Next] then
          Lowest[Curr] := InOrder[Next];
end;

procedure TGSimpleGraph.TCutPointHelper.Search(aGraph: TGSimpleGraph; aVector: PIntVector; aFrom: SizeInt);
begin
  Init(aGraph, aVector);
  Dfs(aFrom);
end;

function TGSimpleGraph.TCutPointHelper.ContainsAny(aGraph: TGSimpleGraph; aFrom: SizeInt): Boolean;
begin
  InitRemove(aGraph, nil);
  Result := DfsFind(aFrom);
end;

procedure TGSimpleGraph.TCutPointHelper.Remove(aGraph: TGSimpleGraph; aNewEdges: PIntEdgeVector);
var
  I: SizeInt;
begin
  InitRemove(aGraph, aNewEdges);
  for I := 0 to Pred(Total) do
    if InOrder[I] = Total then
      DfsRemove(I);
end;

{ TGSimpleGraph.TBridgeHelper }

procedure TGSimpleGraph.TBridgeHelper.Init(aGraph: TGSimpleGraph; aVector: PIntEdgeVector);
begin
  Graph := aGraph;
  Total := aGraph.VertexCount;
  Lowest := Graph.CreateIntArray(Total);
  InOrder := Graph.CreateIntArray(Total);
  Bridges := aVector;
  Counter := 0;
end;

procedure TGSimpleGraph.TBridgeHelper.Dfs(Curr: SizeInt; Prev: SizeInt);
var
  Next: SizeInt;
begin
  InOrder[Curr] := Counter;
  Lowest[Curr] := Counter;
  Inc(Counter);
  for Next in Graph.AdjVerticesI(Curr) do
    if Next <> Prev then
      if InOrder[Next] = Total then
        begin
          Dfs(Next, Curr);
          if Lowest[Curr] > Lowest[Next] then
            Lowest[Curr] := Lowest[Next];
          if Lowest[Next] > InOrder[Curr] then
            Bridges^.Add(TIntEdge.Create(Curr, Next));
        end
      else
        if Lowest[Curr] > InOrder[Next] then
          Lowest[Curr] := InOrder[Next];
end;

function TGSimpleGraph.TBridgeHelper.DfsFind(Curr: SizeInt; Prev: SizeInt): Boolean;
var
  Next: SizeInt;
begin
  InOrder[Curr] := Counter;
  Lowest[Curr] := Counter;
  Inc(Counter);
  for Next in Graph.AdjVerticesI(Curr) do
    if Next <> Prev then
      if InOrder[Next] = Total then
        begin
          if DfsFind(Next, Curr) then
            exit(True);
          if Lowest[Curr] > Lowest[Next] then
            Lowest[Curr] := Lowest[Next];
          if Lowest[Next] > InOrder[Curr] then
            exit(True);
        end
      else
        if Lowest[Curr] > InOrder[Next] then
          Lowest[Curr] := InOrder[Next];
  Result := False;
end;

procedure TGSimpleGraph.TBridgeHelper.Search(aGraph: TGSimpleGraph; aVector: PIntEdgeVector);
var
  I: SizeInt;
begin
  Init(aGraph, aVector);
  for I := 0 to Pred(Total) do
    if InOrder[I] = Total then
      Dfs(I);
end;

function TGSimpleGraph.TBridgeHelper.ContainsAny(aGraph: TGSimpleGraph): Boolean;
var
  I: SizeInt;
begin
  Init(aGraph, nil);
  for I := 0 to Pred(Total) do
    if InOrder[I] = Total then
      if DfsFind(I) then
        exit(True);
  Result := False;
end;

{ TGSimpleGraph }

procedure TGSimpleGraph.DoRemoveVertex(aIndex: SizeInt);
var
  CurrEdges: TAdjList.TAdjItemArray;
  I, J: SizeInt;
begin
  FEdgeCount -= AdjList[aIndex]^.Count;
  Delete(aIndex);
  FConnectedValid := False;
  for I := 0 to Pred(VertexCount) do
    begin
      CurrEdges := AdjList[I]^.ToArray;
      AdjList[I]^.MakeEmpty;
      for J := 0 to System.High(CurrEdges) do
        begin
          if CurrEdges[J].Destination <> aIndex then
            begin
              if CurrEdges[J].Destination > aIndex then
                Dec(CurrEdges[J].Destination);
              AdjList[I]^.Add(CurrEdges[J]);
            end;
        end;
    end;
end;

function TGSimpleGraph.DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
begin
  if aSrc = aDst then
    exit(False);
  Result := AdjList[aSrc]^.Add(TAdjItem.Create(aDst, aData));
  if Result then
    begin
      if not AdjList[aDst]^.Add(TAdjItem.Create(aSrc, aData)) then
        raise ELGraphError.Create(SEGrapInconsist);
      Inc(FEdgeCount);
      FConnectedValid := False;
    end;
end;

function TGSimpleGraph.DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
begin
  if aSrc = aDst then
    exit(False);
  Result := AdjList[aSrc]^.Remove(aDst);
  if Result then
    begin
      AdjList[aDst]^.Remove(aSrc);
      Dec(FEdgeCount);
      FConnectedValid := False;
    end;
end;

function TGSimpleGraph.GetSeparateGraph(aIndex: SizeInt): TGSimpleGraph;
var
  Visited: TBitVector;
  Stack: TIntStack;
  Dst, Src: SizeInt;
begin
  Result := TGSimpleGraph.Create;
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

function TGSimpleGraph.CheckPathExists(aSrc, aDst: SizeInt): Boolean;
var
  Visited: TBitVector;
  Stack: TIntStack;
begin
  Visited.Size := VertexCount;
  repeat
    if aSrc = aDst then
      exit(True);
    Visited[aSrc] := True;
    for aSrc in AdjVerticesI(aSrc) do
      if not Visited[aSrc] then
        Stack.Push(aSrc);
  until not Stack.TryPop(aSrc);
  Result := False;
end;

function TGSimpleGraph.FindSeparateCount: SizeInt;
var
  Visited: TBitVector;
  Queue: TIntQueue;
  I, Curr, Next: SizeInt;
begin
  Result := 0;
  Visited.Size := VertexCount;
  {%H-}Queue.EnsureCapacity(VertexCount);
  for I := 0 to Pred(VertexCount) do
    if not Visited[I] then
      begin
        Curr := I;
        repeat
          AdjList[Curr]^.FCompIndex := Result;
          for Next in AdjVerticesI(Curr) do
            if not Visited[Next] then
              begin
                Visited[Next] := True;
                Queue.Enqueue(Next);
              end;
        until not Queue.TryDequeue(Curr);
        Inc(Result);
      end;
end;

function TGSimpleGraph.GetSeparateCount: SizeInt;
begin
  if not ConnectedValid then
    begin
      FCompCount := FindSeparateCount;
      FConnectedValid := True;
      FConnected := FCompCount = 1;
    end;
  Result := FCompCount;
end;

function TGSimpleGraph.CountPop(aCompIndex: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I := 0 to Pred(VertexCount) do
    Result += Ord(AdjList[I]^.FCompIndex = aCompIndex);
end;

function TGSimpleGraph.MakeConnected(aOnAddEdge: TOnAddEdge): SizeInt;
var
  Visited: TBitVector;
  Queue: TIntQueue;
  I, Curr: SizeInt;
  d: TEdgeData;
begin
  Result := 0;
  Visited.Size := VertexCount;
  d := DefaultEdgeData;
  for I := 0 to Pred(VertexCount) do
    if not Visited[I] then
      begin
        Curr := I;
        repeat
          AdjList[Curr]^.FCompIndex := 0;
          for Curr in AdjVerticesI(Curr) do
            if not Visited[Curr] then
              begin
                Visited[Curr] := True;
                Queue.Enqueue(Curr);
              end;
        until not Queue.TryDequeue(Curr);
        Inc(Result);
        if Result > 1 then
          begin
            if Assigned(aOnAddEdge) then
              aOnAddEdge(AdjList[0]^.Vertex, AdjList[Curr]^.Vertex, @d);
            AddEdgeI(0, Curr, d);
          end;
      end;
  FCompCount := 1;
  FConnectedValid := True;
  FConnected := True;
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

procedure TGSimpleGraph.SearchForCutPoints(aRoot: SizeInt; var aPoints: TIntVector);
var
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  Lowest, InOrder, Parents: TIntArray;
  Counter, Curr, Next, ChildCount: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  Lowest := CreateIntArray;
  InOrder := CreateIntArray;
  Parents := CreateIntArray;
  InOrder[aRoot] := 0;
  Lowest[aRoot] := 0;
  {%H-}Stack.Push(aRoot);
  Counter := 1;
  ChildCount := 0;
  while Stack.TryPeek(Curr) do
    if AdjEnums[{%H-}Curr].MoveNext then
      begin
        Next := AdjEnums[Curr].Current;
        if Next <> Parents[Curr] then
          if InOrder[Next] = -1 then
            begin
              Parents[Next] := Curr;
              InOrder[Next] := Counter;
              Lowest[Next] := Counter;
              Inc(Counter);
              Inc(ChildCount, Ord(Curr = aRoot));
              Stack.Push(Next);
            end
          else
            if Lowest[Curr] > InOrder[Next] then
              Lowest[Curr] := InOrder[Next];
      end
    else
      begin
        Stack.Pop;
        Next := Curr;
        Curr := Parents[Curr];
        if Lowest[Curr] > Lowest[Next] then
          Lowest[Curr] := Lowest[Next];
        if (Lowest[Next] >= InOrder[Curr]) and (Curr <> aRoot) then
          aPoints.Add(Curr);
      end;
  if ChildCount > 1 then
    aPoints.Add(aRoot);
end;

function TGSimpleGraph.CutPointExists(aRoot: SizeInt): Boolean;
var
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  Lowest, InOrder, Parents: TIntArray;
  Counter, Curr, Next, ChildCount: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  Lowest := CreateIntArray;
  InOrder := CreateIntArray;
  Parents := CreateIntArray;
  InOrder[aRoot] := 0;
  Lowest[aRoot] := 0;
  {%H-}Stack.Push(aRoot);
  Counter := 1;
  ChildCount := 0;
  while Stack.TryPeek(Curr) do
    if AdjEnums[{%H-}Curr].MoveNext then
      begin
        Next := AdjEnums[Curr].Current;
        if Next <> Parents[Curr] then
          if InOrder[Next] = -1 then
            begin
              Parents[Next] := Curr;
              InOrder[Next] := Counter;
              Lowest[Next] := Counter;
              Inc(Counter);
              Inc(ChildCount, Ord(Curr = aRoot));
              Stack.Push(Next);
            end
          else
            if Lowest[Curr] > InOrder[Next] then
              Lowest[Curr] := InOrder[Next];
      end
    else
      begin
        Stack.Pop;
        Next := Curr;
        Curr := Parents[Curr];
        if Lowest[Curr] > Lowest[Next] then
          Lowest[Curr] := Lowest[Next];
        if (Lowest[Next] >= InOrder[Curr]) and (Curr <> aRoot) then
          exit(True);
      end;
  Result := ChildCount > 1;
end;

procedure TGSimpleGraph.SearchForBiconnect(aRoot: SizeInt; var aEdges: TIntEdgeVector);
var
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  Lowest, InOrder, Parents, Across: TIntArray;
  Counter, Curr, Next: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  Lowest := CreateIntArray;
  InOrder := CreateIntArray;
  Parents := CreateIntArray;
  Across := CreateIntArray;
  InOrder[aRoot] := 0;
  Lowest[aRoot] := 0;
  {%H-}Stack.Push(aRoot);
  Counter := 1;
  while Stack.TryPeek(Curr) do
    if AdjEnums[{%H-}Curr].MoveNext then
      begin
        Next := AdjEnums[Curr].Current;
        if Next <> Parents[Curr] then
          if InOrder[Next] = -1 then
            begin
              if Across[Curr] = -1 then
                Across[Curr] := Next;
              Parents[Next] := Curr;
              InOrder[Next] := Counter;
              Lowest[Next] := Counter;
              Inc(Counter);
              Stack.Push(Next);
            end
          else
            if Lowest[Curr] > InOrder[Next] then
              Lowest[Curr] := InOrder[Next];
      end
    else
      begin
        Stack.Pop;
        Next := Curr;
        Curr := Parents[Curr];
        if Lowest[Curr] > Lowest[Next] then
          Lowest[Curr] := Lowest[Next];
        if Lowest[Next] >= InOrder[Curr] then
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
  Lowest, InOrder, Parents: TIntArray;
  Counter, Curr, Next, ChildCount, I: SizeInt;
  e: TIntEdge;
begin
  AdjEnums := CreateAdjEnumArray;
  Lowest := CreateIntArray;
  InOrder := CreateIntArray;
  Parents := CreateIntArray;
  InOrder[aRoot] := 0;
  Lowest[aRoot] := 0;
  {%H-}Stack.Push(aRoot);
  Counter := 1;
  ChildCount := 0;
  while Stack.TryPeek(Curr) do
    if AdjEnums[{%H-}Curr].MoveNext then
      begin
        Next := AdjEnums[Curr].Current;
        if Next <> Parents[Curr] then
          if InOrder[Next] = -1 then
            begin
              Parents[Next] := Curr;
              InOrder[Next] := Counter;
              Lowest[Next] := Counter;
              Inc(Counter);
              Inc(ChildCount, Ord(Curr = aRoot));
              Stack.Push(Next);
              EdgeStack.Add(TIntEdge.Create(Curr, Next));
            end
          else
            if Lowest[Curr] > InOrder[Next] then
              begin
                Lowest[Curr] := InOrder[Next];
                EdgeStack.Add(TIntEdge.Create(Curr, Next));
              end;
      end
    else
      begin
        Stack.Pop;
        Next := Curr;
        Curr := Parents[Curr];
        if Lowest[Curr] > Lowest[Next] then
          Lowest[Curr] := Lowest[Next];
        if (Lowest[Next] >= InOrder[Curr]) and (Curr <> aRoot) then
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
  Lowest, InOrder, Parents: TIntArray;
  Counter, Curr, Next, I: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  Lowest := CreateIntArray;
  InOrder := CreateIntArray;
  Parents := CreateIntArray;
  Counter := 0;
  for I := 0 to Pred(VertexCount) do
    if InOrder[I] = -1 then
      begin
        InOrder[I] := Counter;
        Lowest[I] := Counter;
        {%H-}Stack.Push(I);
        while Stack.TryPeek(Curr) do
          if AdjEnums[{%H-}Curr].MoveNext then
            begin
              Next := AdjEnums[Curr].Current;
              if Next <> Parents[Curr] then
                if InOrder[Next] = -1 then
                  begin
                    Parents[Next] := Curr;
                    InOrder[Next] := Counter;
                    Lowest[Next] := Counter;
                    Inc(Counter);
                    Stack.Push(Next);
                  end
                else
                  if Lowest[Curr] > InOrder[Next] then
                    Lowest[Curr] := InOrder[Next];
            end
          else
            begin
              Stack.Pop;
              Next := Curr;
              Curr := Parents[Curr];
              if Lowest[Curr] > Lowest[Next] then
                Lowest[Curr] := Lowest[Next];
              if Lowest[Next] > InOrder[Curr] then
                exit(True);
            end;
      end;
  Result := False;
end;

procedure TGSimpleGraph.SearchForBridges(var aBridges: TIntEdgeVector);
var
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  Lowest, InOrder, Parents: TIntArray;
  Counter, Curr, Next, I: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  Lowest := CreateIntArray;
  InOrder := CreateIntArray;
  Parents := CreateIntArray;
  Counter := 0;
  for I := 0 to Pred(VertexCount) do
    if InOrder[I] = -1 then
      begin
        InOrder[I] := Counter;
        Lowest[I] := Counter;
        {%H-}Stack.Push(I);
        while Stack.TryPeek(Curr) do
          if AdjEnums[{%H-}Curr].MoveNext then
            begin
              Next := AdjEnums[Curr].Current;
              if Next <> Parents[Curr] then
                if InOrder[Next] = -1 then
                  begin
                    Parents[Next] := Curr;
                    InOrder[Next] := Counter;
                    Lowest[Next] := Counter;
                    Inc(Counter);
                    Stack.Push(Next);
                  end
                else
                  if Lowest[Curr] > InOrder[Next] then
                    Lowest[Curr] := InOrder[Next];
            end
          else
            begin
              Stack.Pop;
              Next := Curr;
              Curr := Parents[Curr];
              if Lowest[Curr] > Lowest[Next] then
                Lowest[Curr] := Lowest[Next];
              if Lowest[Next] > InOrder[Curr] then
                aBridges.Add(TIntEdge.Create(Curr, Next));
            end;
      end;
end;

procedure TGSimpleGraph.SearchForFundamentalsCycles(aRoot: SizeInt; out aCycles: TIntArrayVector);
var
  Stack: TIntStack;
  Visited: TBitVector;
  AdjEnums: TAdjEnumArray;
  Parents: TIntArray;
  EdgeSet: TIntPairSet;
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
          if (Parents[aRoot] <> Next) and EdgeSet.Add(aRoot, Next) then
            aCycles.Add(TreeToCycle(Parents, Next, aRoot));
      end
    else
      Stack.Pop;
end;

procedure TGSimpleGraph.SearchForFundamentalsCyclesLen(aRoot: SizeInt; out aCycleLens: TIntVector);
var
  Stack: TIntStack;
  Visited: TBitVector;
  AdjEnums: TAdjEnumArray;
  Parents: TIntArray;
  EdgeSet: TIntPairSet;
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
          if (Parents[aRoot] <> Next) and EdgeSet.Add(aRoot, Next) then
            aCycleLens.Add(TreeToCycleLen(Parents, Next, aRoot));
      end
    else
      Stack.Pop;
end;

function TGSimpleGraph.FindFundamentalCyclesLen(out aCycleLens: TIntVector): Boolean;
begin
  if not Connected then
    exit(False);
  if IsTree then
    exit(False);
  SearchForFundamentalsCyclesLen(0, aCycleLens);
  if aCycleLens.Count <> CyclomaticNumber then
    raise ELGraphError.Create(SEGrapInconsist);
  TIntVectorHelper.Sort(aCycleLens);
  Result := True;
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
  if L.VertexCount <> R.VertexCount then
    exit(False);
  if L.EdgeCount <> R.EdgeCount then
    exit(False);
  if not L.FindFundamentalCyclesLen(fcL) then
    Result := not R.FindFundamentalCyclesLen(fcR)
  else
    begin
      if not R.FindFundamentalCyclesLen(fcR) then
        exit(False);
      if fcL.Count <> fcR.Count then
        exit(False);
      for I := 0 to Pred(fcL.Count) do
        if fcL[I] <> fcR[I] then
          exit(False);
      Result := True;
    end
end;

procedure TGSimpleGraph.Clear;
begin
  inherited;
  FCompCount := 0;
  FConnected := False;
  FConnectedValid := False;
end;

function TGSimpleGraph.AddVertex(constref aVertex: TVertex; out aIndex: SizeInt): Boolean;
begin
  Result := not FindOrAdd(aVertex, aIndex);
  if Result then
    begin
      AdjList[aIndex]^.FCompIndex := -1;
      FConnectedValid := False;
    end;
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

procedure TGSimpleGraph.RemoveVertexI(aVtxIndex: SizeInt);
begin
  CheckIndexRange(aVtxIndex);
  DoRemoveVertex(aVtxIndex);
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
      aWriteVertex(wbs, AdjList[I]^.Vertex);
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

function TGSimpleGraph.DegreeI(aVtxIndex: SizeInt): SizeInt;
begin
  CheckIndexRange(aVtxIndex);
  Result := AdjList[aVtxIndex]^.Count;
end;

function TGSimpleGraph.Isolated(constref aVertex: TVertex): Boolean;
begin
  Result := Degree(aVertex) = 0;
end;

function TGSimpleGraph.IsolatedI(aVtxIndex: SizeInt): Boolean;
begin
  Result := DegreeI(aVtxIndex) = 0;
end;

function TGSimpleGraph.Connected: Boolean;
begin
  Result := SeparateCount = 1;
end;

function TGSimpleGraph.EnsureConnected(aOnAddEdge: TOnAddEdge): SizeInt;
begin
  Result := 0;
  if VertexCount < 2 then
    exit;
  if ConnectedValid and Connected then
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
  if ConnectedValid then
    exit(AdjList[aSrc]^.FCompIndex = AdjList[aDst]^.FCompIndex);
  Result := CheckPathExists(aSrc, aDst);
end;

function TGSimpleGraph.SeparateGraph(constref aVertex: TVertex): TGSimpleGraph;
begin
  Result := SeparateGraphI(IndexOf(aVertex));
end;

function TGSimpleGraph.SeparateGraphI(aVtxIndex: SizeInt): TGSimpleGraph;
begin
  if SeparateCount > 1 then
    Result := GetSeparateGraph(aVtxIndex)
  else
    Result := Clone;
end;

function TGSimpleGraph.SeparateIndexOf(constref aVertex: TVertex): SizeInt;
begin
   Result := SeparateIndexI(IndexOf(aVertex));
end;

function TGSimpleGraph.SeparateIndexI(aVtxIndex: SizeInt): SizeInt;
begin
  CheckIndexRange(aVtxIndex);
  if SeparateCount > 1 then
    Result := AdjList[aVtxIndex]^.FCompIndex
  else
    Result := 0;
end;

function TGSimpleGraph.SeparatePop(constref aVertex: TVertex): SizeInt;
begin
  Result := SeparatePopI(IndexOf(aVertex));
end;

function TGSimpleGraph.SeparatePopI(aVtxIndex: SizeInt): SizeInt;
begin
  CheckIndexRange(aVtxIndex);
  if SeparateCount > 1 then
    Result := CountPop(AdjList[aVtxIndex]^.FCompIndex)
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
  g: TGSimpleGraph = nil;
  Stack: TIntStack;
  s, d: SizeInt;
begin
  if not ContainsEulerianCircuit then
    exit(False);
  g := Clone;
  try
    s := 0;
    while g.DegreeI(s) = 0 do
      Inc(s);
    aCircuit.Add(s);
    repeat
      repeat
        if not g.AdjList[s]^.FindFirst(d) then
          break;
        Stack.Push(s);
        g.RemoveEdgeI(s, d);
        s := d;
      until False;
      if not Stack.TryPop(s) then
        break;
      aCircuit.Add(s);
    until False;
  finally
    g.Free;
  end;
  Result := aCircuit.Count > 0;
  if Result then
    TIntVectorHelper.Reverse(aCircuit);
end;

function TGSimpleGraph.FindFundamentalCycles(out aCycles: TIntArrayVector): Boolean;
begin
  if not Connected then
    exit(False);
  if IsTree then
    exit(False);
  SearchForFundamentalsCycles(0, aCycles);
  if aCycles.Count <> CyclomaticNumber then
    raise ELGraphError.Create(SEGrapInconsist);
  TIntArrayVectorHelper.Sort(aCycles, @CmpIntArrayLen);
  Result := True;
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
        aOnAddEdge(AdjList[e.Source]^.Vertex, AdjList[e.Destination]^.Vertex, @d);
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
  if InnerConnected then
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

function TGSimpleGraph.CreateFromArray(constref aValue: TIntArray): TGSimpleGraph;
var
  I, Src: SizeInt;
begin
  Result := TGSimpleGraph.Create;
  for I := 0 to Pred(System.Length(aValue)) do
    begin
      Src := aValue[I];
      if Src <> -1 then
        Result.AddEdge(Items[Src], Items[I], GetEdgeDataPtr(Src, I)^);
    end;
end;

function TGSimpleGraph.CreateFromEdgeArray(constref aEdges: TIntEdgeArray): TGSimpleGraph;
var
  e: TIntEdge;
begin
  Result := TGSimpleGraph.Create;
  for e in aEdges do
    Result.AddEdge(Items[e.Source], Items[e.Destination], GetEdgeDataPtr(e.Source, e.Destination)^);
end;

function TGSimpleGraph.DistinctEdges: TDistinctEdges;
begin
  Result.FGraph := Self;
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

function TGSimpleGraph.Complement(aOnAddEdge: TOnAddEdge): TGSimpleGraph;
var
  v: TVertex;
  I, J: SizeInt;
  d: TEdgeData;
begin
  Result := TGSimpleGraph.Create(VertexCount);
  for v in Vertices do
    Result.AddVertex(v);
  d := DefaultEdgeData;
  for I := 0 to Pred(VertexCount) do
    for J := Succ(I) to Pred(VertexCount) do
      if not AdjacentI(I, J) then
        begin
          if Assigned(aOnAddEdge) then
            aOnAddEdge(AdjList[I]^.Vertex, AdjList[J]^.Vertex, @d);
          Result.AddEdgeI(I, J, d);
        end;
end;

{ TGOutline }

procedure TGOutline.WriteData(aStream: TStream; constref aValue: TEmptyRec);
begin
  //do nothing
end;

procedure TGOutline.ReadData(aStream: TStream; out aValue: TEmptyRec);
begin
  //do nothing
end;

function TGOutline.AddEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := inherited AddEdge(aSrc, aDst);
end;

function TGOutline.AddEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := inherited AddEdgeI(aSrc, aDst);
end;

function TGOutline.EnsureConnected: SizeInt;
begin
  Result := inherited EnsureConnected(nil);
end;

function TGOutline.RemoveCutPoints(constref aRoot: TVertex): SizeInt;
begin
  Result := inherited RemoveCutPoints(aRoot, nil);
end;

function TGOutline.RemoveCutPointsI(aRoot: SizeInt): SizeInt;
begin
  Result := inherited RemoveCutPointsI(aRoot, nil);
end;

function TGOutline.EnsureBiconnected: SizeInt;
begin
  Result := inherited EnsureBiconnected(nil);
end;

procedure TGOutline.SaveToStream(aStream: TStream; aOnWriteVertex: TOnWriteVertex);
begin
  inherited SaveToStream(aStream, aOnWriteVertex, @WriteData);
end;

procedure TGOutline.LoadFromStream(aStream: TStream; aOnReadVertex: TOnReadVertex);
begin
  inherited LoadFromStream(aStream, aOnReadVertex, @ReadData);
end;

procedure TGOutline.SaveToFile(const aFileName: string; aOnWriteVertex: TOnWriteVertex);
begin
  inherited SaveToFile(aFileName, aOnWriteVertex, @WriteData);
end;

procedure TGOutline.LoadFromFile(const aFileName: string; aOnReadVertex: TOnReadVertex);
begin
  inherited LoadFromFile(aFileName, aOnReadVertex, @ReadData);
end;

function TGOutline.Complement: TGOutline;
begin
  Result := TGOutline(inherited Complement(nil));
end;

{ TIntOutline }

procedure TIntOutline.WriteVertex(aStream: TStream; constref aValue: SizeInt);
begin
  aStream.WriteBuffer(aValue, SizeOf(aValue));
end;

procedure TIntOutline.ReadVertex(aStream: TStream; out aValue: SizeInt);
begin
  aStream.ReadBuffer(aValue{%H-}, SizeOf(aValue));
end;

procedure TIntOutline.SaveToStream(aStream: TStream);
begin
  inherited SaveToStream(aStream, @WriteVertex);
end;

procedure TIntOutline.LoadFromStream(aStream: TStream);
begin
  inherited LoadFromStream(aStream, @ReadVertex);
end;

procedure TIntOutline.SaveToFile(const aFileName: string);
begin
  inherited SaveToFile(aFileName, @WriteVertex);
end;

procedure TIntOutline.LoadFromFile(const aFileName: string);
begin
  inherited LoadFromFile(aFileName, @ReadVertex);
end;

{ TStrOutline }

procedure TStrOutline.WriteVertex(aStream: TStream; constref aValue: string);
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

procedure TStrOutline.ReadVertex(aStream: TStream; out aValue: string);
var
  Len: SmallInt;
begin
  aStream.ReadBuffer(Len, SizeOf(Len));
  System.SetLength(aValue, Len);
  if Len > 0 then
    aStream.ReadBuffer(aValue[1], Len);
end;

procedure TStrOutline.SaveToStream(aStream: TStream);
begin
  inherited SaveToStream(aStream, @WriteVertex);
end;

procedure TStrOutline.LoadFromStream(aStream: TStream);
begin
  inherited LoadFromStream(aStream, @ReadVertex);
end;

procedure TStrOutline.SaveToFile(const aFileName: string);
begin
  inherited SaveToFile(aFileName, @WriteVertex);
end;

procedure TStrOutline.LoadFromFile(const aFileName: string);
begin
  inherited LoadFromFile(aFileName, @ReadVertex);
end;

{ TGWeighedGraph.TWeightEdge }

class operator TGWeighedGraph.TWeightEdge. = (constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight = R.Weight;
end;

class operator TGWeighedGraph.TWeightEdge.<>(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight <> R.Weight;
end;

class operator TGWeighedGraph.TWeightEdge.>(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight > R.Weight;
end;

class operator TGWeighedGraph.TWeightEdge.<(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight < R.Weight;
end;

class operator TGWeighedGraph.TWeightEdge.>=(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight >= R.Weight;
end;

class operator TGWeighedGraph.TWeightEdge.<=(constref L, R: TWeightEdge): Boolean;
begin
  Result := L.Weight <= R.Weight;
end;

constructor TGWeighedGraph.TWeightEdge.Create(s, d: SizeInt; w: TWeight);
begin
  Source := s;
  Destination := d;
  Weight := w;
end;

{ TGWeighedGraph.TWeightItem }

class operator TGWeighedGraph.TWeightItem. = (constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight = R.Weight;
end;

class operator TGWeighedGraph.TWeightItem.<>(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight <> R.Weight;
end;

class operator TGWeighedGraph.TWeightItem.>(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight > R.Weight;
end;

class operator TGWeighedGraph.TWeightItem.<(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight < R.Weight;
end;

class operator TGWeighedGraph.TWeightItem.>=(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight >= R.Weight;
end;

class operator TGWeighedGraph.TWeightItem.<=(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight <= R.Weight;
end;

constructor TGWeighedGraph.TWeightItem.Create(constref w: TWeight; aIndex: SizeInt);
begin
  Weight := w;
  Index := aIndex;
end;

{ TGWeighedGraph.TWeightItem }

class operator TGWeighedGraph.TRankItem. = (constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank = R.Rank;
end;

class operator TGWeighedGraph.TRankItem.<>(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank <> R.Rank;
end;

class operator TGWeighedGraph.TRankItem.>(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank > R.Rank;
end;

class operator TGWeighedGraph.TRankItem.<(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank < R.Rank;
end;

class operator TGWeighedGraph.TRankItem.>=(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank >= R.Rank;
end;

class operator TGWeighedGraph.TRankItem.<=(constref L, R: TRankItem): Boolean;
begin
  Result := L.Rank <= R.Rank;
end;

constructor TGWeighedGraph.TRankItem.Create(constref aRank, aWeight: TWeight; aIndex: SizeInt);
begin
  Rank := aRank;
  Weight := aWeight;
  Index := aIndex;
end;

{ TGWeighedGraph.TFilterKruskal }

procedure TGWeighedGraph.TFilterKruskal.Kruskal(L, R: SizeInt);
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

function TGWeighedGraph.TFilterKruskal.Split(L, R: SizeInt): SizeInt;
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

procedure TGWeighedGraph.TFilterKruskal.FilterKruskal(L, R: SizeInt);
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

function TGWeighedGraph.TFilterKruskal.FindMst(aGraph: TGWeighedGraph; out aWeight: TWeight): TIntArray;
begin
  FEdges := aGraph.CreateEdgeArray;
  FTree := aGraph.CreateIntArray;
  FCount := aGraph.VertexCount;
  FDsu.Size := FCount;
  FTreshold := Pred(FCount); // ??? FCount shr 1
  FWeight := ZeroWeight;
  FFound := 0;
  FilterKruskal(0, System.High(FEdges));
  aWeight := FWeight;
  Result := FTree;
end;

{ TGWeighedGraph }

function TGWeighedGraph.DijkstraSssp(aSrc: SizeInt): TWeightArray;
var
  Visited: TBitVector;
  Queue: TPairingHeap;
  Handles: THandleArray;
  Relaxed: TWeight;
  Item: TWeightItem;
  p: PAdjItem;
begin
  CheckIndexRange(aSrc);
  Result := CreateWeighArray;
  Handles := CreateHandleArray;
  Visited.Size := VertexCount;
  Handles[aSrc] := Queue.Insert(TWeightItem.Create(ZeroWeight, aSrc));
  while Queue.TryDequeue(Item) do
    begin
      Visited[Item.Index] := True;
      Result[Item.Index] := Item.Weight;
      for p in AdjVerticesPtr(Item.Index) do
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

function TGWeighedGraph.DijkstraSssp(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
var
  Visited: TBitVector;
  Queue: TPairingHeap;
  Handles: THandleArray;
  Relaxed: TWeight;
  Item: TWeightItem;
  p: PAdjItem;
begin
  Result := CreateWeighArray;
  aPathTree := CreateIntArray;
  Handles := CreateHandleArray;
  Visited.Size := VertexCount;
  Handles[aSrc] := Queue.Insert(TWeightItem.Create(ZeroWeight, aSrc));
  while Queue.TryDequeue(Item) do
    begin
      Visited[Item.Index] := True;
      Result[Item.Index] := Item.Weight;
      for p in AdjVerticesPtr(Item.Index) do
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

function TGWeighedGraph.DijkstraPath(aSrc, aDst: SizeInt): TWeight;
var
  Visited: TBitVector;
  Queue: TPairingHeap;
  Handles: THandleArray;
  Relaxed: TWeight;
  Item: TWeightItem;
  p: PAdjItem;
begin
  Handles := CreateHandleArray;
  Visited.Size := VertexCount;
  Handles[aSrc] := Queue.Insert(TWeightItem.Create(ZeroWeight, aSrc));
  while Queue.TryDequeue(Item) do
    begin
      if Item.Index = aDst then
        exit(Item.Weight);
      Visited[Item.Index] := True;
      for p in AdjVerticesPtr(Item.Index) do
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

function TGWeighedGraph.DijkstraPath(aSrc, aDst: SizeInt; out aWeight: TWeight): TIntArray;
var
  Visited: TBitVector;
  Queue: TPairingHeap;
  Handles: THandleArray;
  Tree: TIntArray;
  Relaxed: TWeight;
  Item: TWeightItem;
  p: PAdjItem;
begin
  Handles := CreateHandleArray;
  Tree := CreateIntArray;
  Visited.Size := VertexCount;
  Handles[aSrc] := Queue.Insert(TWeightItem.Create(ZeroWeight, aSrc));
  while Queue.TryDequeue(Item) do
    begin
      if Item.Index = aDst then
        begin
          aWeight := Item.Weight;
          exit(TreeToChain(Tree, aDst));
        end;
      Visited[Item.Index] := True;
      for p in AdjVerticesPtr(Item.Index) do
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

function TGWeighedGraph.AStar(aSrc, aDst: SizeInt; aHeur: THeuristic; out aWeight: TWeight): TIntArray;
var
  Visited: TBitVector;
  Queue: TAStarHeap;
  Handles: THandleArray;
  Tree: TIntArray;
  Relaxed: TWeight;
  Item: TRankItem;
  p: PAdjItem;
begin
  Handles := CreateHandleArray;
  Tree := CreateIntArray;
  Visited.Size := VertexCount;
  Handles[aSrc] := Queue.Insert(TRankItem.Create(aHeur(Items[aSrc], Items[aDst]), ZeroWeight, aSrc));
  while Queue.TryDequeue(Item) do
    begin
      if Item.Index = aDst then
        begin
          aWeight := Item.Weight;
          exit(TreeToChain(Tree, aDst));
        end;
      Visited[Item.Index] := True;
      for p in AdjVerticesPtr(Item.Index) do
        begin
          if Handles[p^.Key] = INVALID_HANDLE then
            begin
              Relaxed := p^.Data.Weight + Item.Weight;
              Handles[p^.Key] := Queue.Insert(TRankItem.Create(
                Relaxed + aHeur(Items[p^.Key], Items[aDst]), Relaxed, p^.Key));
              Tree[p^.Key] := Item.Index;
            end
          else
            if not Visited[p^.Key] then
              begin
                Relaxed := Item.Weight + p^.Data.Weight;
                if Relaxed < Queue.Value(Handles[p^.Key]).Weight then
                  begin
                    Queue.Update(Handles[p^.Key], TRankItem.Create(
                      Relaxed + aHeur(Items[p^.Key], Items[aDst]), Relaxed, p^.Key));
                    Tree[p^.Key] := Item.Index;
                  end;
              end;
        end;
    end;
  aWeight := InfiniteWeight;
end;

function TGWeighedGraph.FordBellman(aSrc: SizeInt; out aWeights: TWeightArray): Boolean;
var
  Edge: TEdge;
  Enum: TEdgeEnumerator;
  RelaxValue: TWeight;
  I: SizeInt;
  Relaxed: Boolean = False;
begin
  aWeights := CreateWeighArray;
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

function TGWeighedGraph.FordBellman(aSrc: SizeInt; out aPaths: TIntArray; out aWeights: TWeightArray): Boolean;
var
  Edge: TEdge;
  Enum: TEdgeEnumerator;
  v: TIntVector;
  RelaxValue: TWeight;
  I: SizeInt;
  J: SizeInt = -1;
begin
  aWeights := CreateWeighArray;
  aPaths := CreateIntArray;
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

function TGWeighedGraph.FilterKruskalMst(out aTotalWeight: TWeight): TIntArray;
var
  fk: TFilterKruskal;
begin
  Result := fk.FindMst(Self, aTotalWeight);
end;

function TGWeighedGraph.KruskalMst(out aTotalWeight: TWeight): TIntArray;
var
  e: TEdgeArray;
  I, s, d, VtxCount: SizeInt;
  Dsu: TDisjointSetUnion;
begin
  e := CreateEdgeArray;
  Result := CreateIntArray;
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

function TGWeighedGraph.PrimMst(out aTotalWeight: TWeight): TIntArray;
var
  Visited: TBitVector;
  Queue: TPairingHeap;
  Handles: THandleArray;
  Curr: SizeInt;
  Item: TWeightItem;
  p: PAdjItem;
begin
  Result := CreateIntArray;
  System.SetLength(Result, VertexCount);
  Handles := CreateHandleArray;
  Visited.Size := VertexCount;
  Handles[0] := Queue.Insert(TWeightItem.Create(ZeroWeight, 0));
  aTotalWeight := 0;
  while Queue.TryDequeue(Item) do
    begin
      Curr := Item.Index;
      aTotalWeight += Item.Weight;
      Visited[Curr] := True;
      for p in AdjVerticesPtr(Curr) do
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

function TGWeighedGraph.CreateWeighArray: TWeightArray;
var
  I: SizeInt;
begin
  System.SetLength(Result, VertexCount);
  for I := 0 to Pred(VertexCount) do
    Result[I] := InfiniteWeight;
end;

function TGWeighedGraph.CreateEdgeArray: TEdgeArray;
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

class function TGWeighedGraph.Min(const L, R: TWeight): TWeight;
begin
  if L <= R then
    Result := L
  else
    Result := R;
end;

class constructor TGWeighedGraph.Init;
begin
  CFInfiniteWeight := TWeight.MaxValue;
  CFNegInfiniteWeight := TWeight.MinValue;
  CFZeroWeight := Default(TWeight);
end;

function TGWeighedGraph.ContainsNegWeighedEdge: Boolean;
var
  e: TEdge;
begin
  for e in DistinctEdges do
    if e.Data.Weight < ZeroWeight then
      exit(True);
  Result := False;
end;

function TGWeighedGraph.MinPathsMap(constref aSrc: TVertex): TWeightArray;
begin
  Result := MinPathsMapI(IndexOf(aSrc));
end;

function TGWeighedGraph.MinPathsMapI(aSrc: SizeInt): TWeightArray;
begin
  CheckIndexRange(aSrc);
  Result := DijkstraSssp(aSrc);
end;

function TGWeighedGraph.MinPathsMap(constref aSrc: TVertex; out aPathTree: TIntArray): TWeightArray;
begin
  Result := MinPathsMapI(IndexOf(aSrc), aPathTree);
end;

function TGWeighedGraph.MinPathsMapI(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
begin
  CheckIndexRange(aSrc);
  Result := DijkstraSssp(aSrc, aPathTree);
end;

function TGWeighedGraph.MinPathWeight(constref aSrc, aDst: TVertex): TWeight;
begin
  Result := MinPathWeightI(IndexOf(aSrc), IndexOf(aDst));
end;

function TGWeighedGraph.MinPathWeightI(aSrc, aDst: SizeInt): TWeight;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  Result := DijkstraPath(aSrc, aDst);
end;

function TGWeighedGraph.MinPath(constref aSrc, aDst: TVertex; out aWeight: TWeight): TIntArray;
begin
  Result := MinPathI(IndexOf(aSrc), IndexOf(aDst), aWeight);
end;

function TGWeighedGraph.MinPathI(aSrc, aDst: SizeInt; out aWeight: TWeight): TIntArray;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  Result := DijkstraPath(aSrc, aDst, aWeight);
end;

function TGWeighedGraph.MinPathAStar(constref aSrc, aDst: TVertex; aHeur: THeuristic; out aWeight: TWeight
  ): TIntArray;
begin
  Result := MinPathAStarI(IndexOf(aSrc), IndexOf(aSrc), aHeur, aWeight);
end;

function TGWeighedGraph.MinPathAStarI(aSrc, aDst: SizeInt; aHeur: THeuristic; out aWeight: TWeight): TIntArray;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  if aHeur <> nil then
    Result := AStar(aSrc, aDst, aHeur, aWeight)
  else
    Result := DijkstraPath(aSrc, aDst, aWeight);
end;

function TGWeighedGraph.FindMinPathsMap(constref aSrc: TVertex; out aWeights: TWeightArray): Boolean;
begin
  Result := FindMinPathsMapI(IndexOf(aSrc), aWeights);
end;

function TGWeighedGraph.FindMinPathsMapI(aSrc: SizeInt; out aWeights: TWeightArray): Boolean;
begin
  CheckIndexRange(aSrc);
  Result := FordBellman(aSrc, aWeights);
end;

function TGWeighedGraph.FindMinPathsMap(constref aSrc: TVertex; out aPaths: TIntArray;
  out aWeights: TWeightArray): Boolean;
begin
  Result := FindMinPathsMapI(IndexOf(aSrc), aPaths, aWeights);
end;

function TGWeighedGraph.FindMinPathsMapI(aSrc: SizeInt; out aPaths: TIntArray;
  out aWeights: TWeightArray): Boolean;
begin
  CheckIndexRange(aSrc);
  Result := FordBellman(aSrc, aPaths, aWeights);
end;

function TGWeighedGraph.MinSpanningTreeKrus(out aTotalWeight: TWeight): TIntArray;
begin
  if Connected then
    Result := KruskalMst(aTotalWeight)
    //Result := FilterKruskalMst(aTotalWeight)
  else
    raise ELGraphError.Create(SEGraphIsNotConnected);
end;

function TGWeighedGraph.MinSpanningTreePrim(out aTotalWeight: TWeight): TIntArray;
begin
  if Connected then   //todo: is it required ???
    Result := PrimMst(aTotalWeight)
  else
    raise ELGraphError.Create(SEGraphIsNotConnected);
end;

{ TRealPointEdge }

constructor TRealPointEdge.Create(const aWeight: ValReal);
begin
  Weight := aWeight;
end;

{ TPointsOutline }

procedure TPointsOutline.OnAddEdge(constref aSrc, aDst: TPoint; aData: Pointer);
begin
  PEdgeData(aData)^.Weight := aSrc.Distance(aDst);
end;

procedure TPointsOutline.WritePoint(aStream: TStream; constref aValue: TPoint);
begin
  aStream.WriteBuffer(aValue, SizeOf(aValue));
end;

procedure TPointsOutline.WriteWeight(aStream: TStream; constref aValue: TRealPointEdge);
begin
  aStream.WriteBuffer(aValue, SizeOf(aValue));
end;

procedure TPointsOutline.ReadPoint(aStream: TStream; out aValue: TPoint);
begin
  aStream.ReadBuffer(aValue{%H-}, SizeOf(aValue));
end;

procedure TPointsOutline.ReadWeight(aStream: TStream; out aValue: TRealPointEdge);
begin
  aStream.ReadBuffer(aValue{%H-}, SizeOf(aValue));
end;

function TPointsOutline.AddEdge(constref aSrc, aDst: TPoint): Boolean;
begin
  Result := inherited AddEdge(aSrc, aDst, TRealPointEdge.Create(aSrc.Distance(aDst)));
end;

function TPointsOutline.AddEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := inherited AddEdgeI(aSrc, aDst, TRealPointEdge.Create(Items[aSrc].Distance(Items[aDst])));
end;

function TPointsOutline.EnsureConnected: SizeInt;
begin
  Result := inherited EnsureConnected(@OnAddEdge);
end;

function TPointsOutline.RemoveCutPoints(constref aRoot: TPoint): SizeInt;
begin
  Result := inherited RemoveCutPoints(aRoot, @OnAddEdge);
end;

function TPointsOutline.RemoveCutPointsI(aRoot: SizeInt): SizeInt;
begin
  Result := inherited RemoveCutPointsI(aRoot, @OnAddEdge);
end;

function TPointsOutline.EnsureBiconnected: SizeInt;
begin
  Result := inherited EnsureBiconnected(@OnAddEdge);
end;

procedure TPointsOutline.SaveToStream(aStream: TStream);
begin
  inherited SaveToStream(aStream, @WritePoint, @WriteWeight);
end;

procedure TPointsOutline.LoadFromStream(aStream: TStream);
begin
  inherited LoadFromStream(aStream, @ReadPoint, @ReadWeight);
end;

procedure TPointsOutline.SaveToFile(const aFileName: string);
begin
   inherited SaveToFile(aFileName, @WritePoint, @WriteWeight);
end;

procedure TPointsOutline.LoadFromFile(const aFileName: string);
begin
  inherited LoadFromFile(aFileName, @ReadPoint, @ReadWeight);
end;

function TPointsOutline.Clone: TPointsOutline;
begin
  Result := TPointsOutline(inherited Clone);
end;

function TPointsOutline.Complement: TPointsOutline;
begin
  Result := TPointsOutline(inherited Complement(@OnAddEdge));
end;

end.

