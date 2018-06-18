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
  LGPriorityQueue,
  LGraphUtils,
  LGStrConst;

type
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

  var
    FCompCount: SizeInt;
    FConnected,
    FConnectedValid: Boolean;
    function  GetConnected: Boolean; inline;
    procedure DoRemoveVertex(aIndex: SizeInt);
    function  DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
    function  GetSeparateGraph(aIndex: SizeInt): TGSimpleGraph;
    function  FindSeparateCount: SizeInt;
    function  GetSeparateCount: SizeInt;
    function  CountPop(aCompIndex: SizeInt): SizeInt;
    function  GetEccentricity(aIndex: SizeInt): SizeInt;
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
  public
    class function MayBeEqual(L, R: TGSimpleGraph): Boolean;
    procedure Clear; override;
  { returns True and vertex index, if it was added, False otherwise }
    function  AddVertex(constref aVertex: TVertex; out aIndex: SizeInt): Boolean;
    function  AddVertex(constref aVertex: TVertex): Boolean; inline;
    procedure RemoveVertex(constref aVertex: TVertex); inline;
    procedure RemoveVertexI(aIndex: SizeInt);
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
    function  DegreeI(aIndex: SizeInt): SizeInt;
    function  Isolated(constref aVertex: TVertex): Boolean; inline;
    function  IsolatedI(aIndex: SizeInt): Boolean; inline;
    function  DistinctEdges: TDistinctEdges; inline;
  { returns the eccentricity of the aVertex within its connected component }
    function  Eccentricity(constref aVertex: TVertex): SizeInt; inline;
    function  EccentricityI(aIndex: SizeInt): SizeInt;
  { returns local clustering coefficient of the aVertex - how close its neighbours are to being a clique }
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
  { returns complement of the source graph;
    warning: if the source graph is sparse then complement is dense }
    function  Complement(aOnAddEdge: TOnAddEdge): TGSimpleGraph;
  { checks whether the cached info about connected is up-to-date }
    property  ConnectedValid: Boolean read FConnectedValid;
  { checks whether the graph is connected; an empty graph is considered disconnected }
    property  Connected: Boolean read GetConnected;
  { count of connected components }
    property  SeparateCount: SizeInt read GetSeparateCount;
  end;

  { TGChart: simple outline;
      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGChart<TVertex, TVertexEqRel> = class(specialize TGSimpleGraph<TVertex, TEmptyRec, TVertexEqRel>)
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
    function  Complement: TGChart;
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
    function  Complement: TIntChart;
  end;

  { TStrChart
    note: SaveToStream limitation for max string length = High(SmallInt) }
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
    function  Complement: TStrChart;
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
    TWeightEdge  = specialize TGWeighedEdge<TWeight>;
    TWeightItem  = specialize TGWeighedItem<TWeight>;
    TRankItem    = specialize TGRankWeighedItem<TWeight>;
    TEdgeHelper  = specialize TGComparableArrayHelper<TWeightEdge>;
    TPairingHeap = specialize TGLiteComparablePairHeapMin<TWeightItem>;
    TAStarHeap   = specialize TGLiteComparablePairHeapMin<TRankItem>;
    TEdgeArray   = array of TWeightEdge;

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
    function  AStar(aSrc, aDst: SizeInt; out aWeight: TWeight; aHeur: THeuristic): TIntArray;
    function  KruskalMst(out aTotalWeight: TWeight): TIntArray;
    function  PrimMst(out aTotalWeight: TWeight): TIntArray;
    function  CreateWeightArray: TWeightArray;
    function  CreateEdgeArray: TEdgeArray;
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
    function  MinPathAStar(constref aSrc, aDst: TVertex; out aWeight: TWeight; aHeur: THeuristic): TIntArray; inline;
    function  MinPathAStarI(aSrc, aDst: SizeInt; out aWeight: TWeight; aHeur: THeuristic): TIntArray;
  { finds a spanning tree of minimal weight, the graph must be connected(Kruskal's algorithm used)}
    function  MinSpanningTreeKrus(out aTotalWeight: TWeight): TIntArray;
  { finds a spanning tree of minimal weight, the graph must be connected(Prim's algorithm used) }
    function  MinSpanningTreePrim(out aTotalWeight: TWeight): TIntArray;
    function  SeparateGraph(constref aVertex: TVertex): TGWeighedGraph;
    function  SeparateGraphI(aIndex: SizeInt): TGWeighedGraph;
    function  SubgraphFromVertexList(constref aList: TIntArray): TGWeighedGraph;
    function  SubgraphFromPairs(constref aPairs: TIntArray): TGWeighedGraph;
    function  SubgraphFromEdges(constref aEdges: TIntEdgeArray): TGWeighedGraph;
    function  Clone: TGWeighedGraph;
    function  Complement(aOnAddEdge: TOnAddEdge): TGWeighedGraph;
  end;

  TRealPointEdge = record
    Weight: ValReal;
    constructor Create(const aWeight: ValReal);
  end;

  { TPointsChart }

  TPointsChart = class(specialize TGWeighedGraph<TPoint, ValReal, TRealPointEdge, TPoint>)
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
    function  Complement: TPointsChart;
    function  MinPathAStar(constref aSrc, aDst: TPoint; out aWeight: ValReal; aHeur: THeuristic = nil): TIntArray; inline;
    function  MinPathAStarI(aSrc, aDst: SizeInt; out aWeight: ValReal; aHeur: THeuristic = nil): TIntArray;
  end;

implementation
{$B-}{$COPERATORS ON}
uses
  bufstream;

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
        FEnum := FList[FCurrIndex].AdjList.GetEnumerator;
      end;
    Result := FEnum.MoveNext;
    FEnumDone := not Result;
    if Result then
      Result := not FVisited[FEnum.GetCurrent^.Destination];
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

{ TGSimpleGraph }

function TGSimpleGraph.GetConnected: Boolean;
begin
  Result := GetSeparateCount = 1;
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
      if not FNodeList[aDst].AdjList.Add(TAdjItem.Create(aSrc, aData)) then
        raise ELGraphError.Create(SEGrapInconsist);
      Inc(FEdgeCount);
      FConnectedValid := False;
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

function TGSimpleGraph.GetSeparateGraph(aIndex: SizeInt): TGSimpleGraph;
var
  cIdx, I: SizeInt;
  p: PAdjItem;
begin
  Result := TGSimpleGraph.Create;
  cIdx := FNodeList[aIndex].Tag;
  for I := 0 to Pred(VertexCount) do
    if FNodeList[I].Tag = cIdx then
      for p in FNodeList[I].AdjList do
        Result.AddEdge(Items[I], Items[p^.Destination], p^.Data);
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
          FNodeList[Curr].Tag := Result;
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
    Result += Ord(FNodeList[I].Tag = aCompIndex);
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
          FNodeList[Curr].Tag := 0;
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
              aOnAddEdge(FNodeList[0].Vertex, FNodeList[Curr].Vertex, @d);
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
  LowPt, InOrder, Parents: TIntArray;
  Counter, Curr, Next, ChildCount: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  LowPt := CreateIntArray;
  InOrder := CreateIntArray;
  Parents := CreateIntArray;
  InOrder[aRoot] := 0;
  LowPt[aRoot] := 0;
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
              LowPt[Next] := Counter;
              Inc(Counter);
              Inc(ChildCount, Ord(Curr = aRoot));
              Stack.Push(Next);
            end
          else
            if LowPt[Curr] > InOrder[Next] then
              LowPt[Curr] := InOrder[Next];
      end
    else
      begin
        Stack.Pop;
        Next := Curr;
        Curr := Parents[Curr];
        if LowPt[Curr] > LowPt[Next] then
          LowPt[Curr] := LowPt[Next];
        if (LowPt[Next] >= InOrder[Curr]) and (Curr <> aRoot) then
          aPoints.Add(Curr);
      end;
  if ChildCount > 1 then
    aPoints.Add(aRoot);
end;

function TGSimpleGraph.CutPointExists(aRoot: SizeInt): Boolean;
var
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  LowPt, InOrder, Parents: TIntArray;
  Counter, Curr, Next, ChildCount: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  LowPt := CreateIntArray;
  InOrder := CreateIntArray;
  Parents := CreateIntArray;
  InOrder[aRoot] := 0;
  LowPt[aRoot] := 0;
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
              LowPt[Next] := Counter;
              Inc(Counter);
              Inc(ChildCount, Ord(Curr = aRoot));
              Stack.Push(Next);
            end
          else
            if LowPt[Curr] > InOrder[Next] then
              LowPt[Curr] := InOrder[Next];
      end
    else
      begin
        Stack.Pop;
        Next := Curr;
        Curr := Parents[Curr];
        if LowPt[Curr] > LowPt[Next] then
          LowPt[Curr] := LowPt[Next];
        if (LowPt[Next] >= InOrder[Curr]) and (Curr <> aRoot) then
          exit(True);
      end;
  Result := ChildCount > 1;
end;

procedure TGSimpleGraph.SearchForBiconnect(aRoot: SizeInt; var aEdges: TIntEdgeVector);
var
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  LowPt, InOrder, Parents, Across: TIntArray;
  Counter, Curr, Next: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  LowPt := CreateIntArray;
  InOrder := CreateIntArray;
  Parents := CreateIntArray;
  Across := CreateIntArray;
  InOrder[aRoot] := 0;
  LowPt[aRoot] := 0;
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
              LowPt[Next] := Counter;
              Inc(Counter);
              Stack.Push(Next);
            end
          else
            if LowPt[Curr] > InOrder[Next] then
              LowPt[Curr] := InOrder[Next];
      end
    else
      begin
        Stack.Pop;
        Next := Curr;
        Curr := Parents[Curr];
        if LowPt[Curr] > LowPt[Next] then
          LowPt[Curr] := LowPt[Next];
        if LowPt[Next] >= InOrder[Curr] then
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
  LowPt, InOrder, Parents: TIntArray;
  Counter, Curr, Next, ChildCount, I: SizeInt;
  e: TIntEdge;
begin
  AdjEnums := CreateAdjEnumArray;
  LowPt := CreateIntArray;
  InOrder := CreateIntArray;
  Parents := CreateIntArray;
  InOrder[aRoot] := 0;
  LowPt[aRoot] := 0;
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
              LowPt[Next] := Counter;
              Inc(Counter);
              Inc(ChildCount, Ord(Curr = aRoot));
              Stack.Push(Next);
              EdgeStack.Add(TIntEdge.Create(Curr, Next));
            end
          else
            if LowPt[Curr] > InOrder[Next] then
              begin
                LowPt[Curr] := InOrder[Next];
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
        if (LowPt[Next] >= InOrder[Curr]) and (Curr <> aRoot) then
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
  LowPt, InOrder, Parents: TIntArray;
  Counter, Curr, Next, I: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  LowPt := CreateIntArray;
  InOrder := CreateIntArray;
  Parents := CreateIntArray;
  Counter := 0;
  for I := 0 to Pred(VertexCount) do
    if InOrder[I] = -1 then
      begin
        InOrder[I] := Counter;
        LowPt[I] := Counter;
        Inc(Counter);
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
                    LowPt[Next] := Counter;
                    Inc(Counter);
                    Stack.Push(Next);
                  end
                else
                  if LowPt[Curr] > InOrder[Next] then
                    LowPt[Curr] := InOrder[Next];
            end
          else
            begin
              Stack.Pop;
              Next := Curr;
              Curr := Parents[Curr];
              if LowPt[Curr] > LowPt[Next] then
                LowPt[Curr] := LowPt[Next];
              if LowPt[Next] > InOrder[Curr] then
                exit(True);
            end;
      end;
  Result := False;
end;

procedure TGSimpleGraph.SearchForBridges(var aBridges: TIntEdgeVector);
var
  Stack: TIntStack;
  AdjEnums: TAdjEnumArray;
  LowPt, InOrder, Parents: TIntArray;
  Counter, Curr, Next, I: SizeInt;
begin
  AdjEnums := CreateAdjEnumArray;
  LowPt := CreateIntArray;
  InOrder := CreateIntArray;
  Parents := CreateIntArray;
  Counter := 0;
  for I := 0 to Pred(VertexCount) do
    if InOrder[I] = -1 then
      begin
        InOrder[I] := Counter;
        LowPt[I] := Counter;
        Inc(Counter);
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
                    LowPt[Next] := Counter;
                    Inc(Counter);
                    Stack.Push(Next);
                  end
                else
                  if LowPt[Curr] > InOrder[Next] then
                    LowPt[Curr] := InOrder[Next];
            end
          else
            begin
              Stack.Pop;
              Next := Curr;
              Curr := Parents[Curr];
              if LowPt[Curr] > LowPt[Next] then
                LowPt[Curr] := LowPt[Next];
              if LowPt[Next] > InOrder[Curr] then
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
      FNodeList[aIndex].Tag := -1;
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
      if (I <> J) and AdjacentI(I, J) then
        Inc(Counter);
  Result := ValReal(Counter) / ValReal(d * Pred(d));
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
  //if ConnectedValid then
  //  exit(FNodeList[aSrc].Tag = FNodeList[aDst].Tag);
  //Result := CheckPathExists(aSrc, aDst);
  if SeparateCount > 1 then
    Result := FNodeList[aSrc].Tag = FNodeList[aDst].Tag
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
  if SeparateCount > 1 then
    Result := FNodeList[aIndex].Tag
  else
    Result := 0;
end;

function TGSimpleGraph.SeparatePop(constref aVertex: TVertex): SizeInt;
begin
  Result := SeparatePopI(IndexOf(aVertex));
end;

function TGSimpleGraph.SeparatePopI(aIndex: SizeInt): SizeInt;
begin
  CheckIndexRange(aIndex);
  if SeparateCount > 1 then
    Result := CountPop(FNodeList[aIndex].Tag)
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
        if not g.FNodeList[s].AdjList.FindFirst(d) then
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
  vSet: TIntSet;
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
            aOnAddEdge(FNodeList[I].Vertex, FNodeList[J].Vertex, @d);
          Result.AddEdgeI(I, J, d);
        end;
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

function TGChart.Complement: TGChart;
begin
  Result := inherited Complement(nil) as TGChart;
end;

{ TIntChart }

procedure TIntChart.WriteVertex(aStream: TStream; constref aValue: SizeInt);
begin
  aStream.WriteBuffer(aValue, SizeOf(aValue));
end;

procedure TIntChart.ReadVertex(aStream: TStream; out aValue: SizeInt);
begin
  aStream.ReadBuffer(aValue{%H-}, SizeOf(aValue));
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

function TIntChart.Complement: TIntChart;
begin
  Result := inherited Complement as TIntChart;
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

function TStrChart.Complement: TStrChart;
begin
  Result := inherited Complement as TStrChart;
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
  Result := CreateWeightArray;
  Handles := CreateHandleArray;
  Visited.Size := VertexCount;
  Handles[aSrc] := Queue.Insert(TWeightItem.Create(ZeroWeight, aSrc));
  while Queue.TryDequeue(Item) do
    begin
      Visited[Item.Index] := True;
      Result[Item.Index] := Item.Weight;
      for p in FNodeList[Item.Index].AdjList do
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
  Result := CreateWeightArray;
  aPathTree := CreateIntArray;
  Handles := CreateHandleArray;
  Visited.Size := VertexCount;
  Handles[aSrc] := Queue.Insert(TWeightItem.Create(ZeroWeight, aSrc));
  while Queue.TryDequeue(Item) do
    begin
      Visited[Item.Index] := True;
      Result[Item.Index] := Item.Weight;
      for p in FNodeList[Item.Index].AdjList do
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
      for p in FNodeList[Item.Index].AdjList do
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
{%H-}begin
  Handles := CreateHandleArray;
  Tree := CreateIntArray;
  Visited.Size := VertexCount;
  Handles[aSrc] := Queue.Insert(TWeightItem.Create(ZeroWeight, aSrc));
  while {%H-}Queue.TryDequeue(Item) do
    begin
      if Item.Index = aDst then
        begin
          aWeight := Item.Weight;
          exit(TreeToChain(Tree, aDst));
        end;
      Visited[Item.Index] := True;
      for p in FNodeList[Item.Index].AdjList do
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

function TGWeighedGraph.AStar(aSrc, aDst: SizeInt; out aWeight: TWeight; aHeur: THeuristic): TIntArray;
var
  Visited: TBitVector;
  Queue: TAStarHeap;
  Handles: THandleArray;
  Tree: TIntArray;
  Relaxed: TWeight;
  Item: TRankItem;
  p: PAdjItem;
{%H-}begin
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
      for p in FNodeList[Item.Index].AdjList do
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

function TGWeighedGraph.KruskalMst(out aTotalWeight: TWeight): TIntArray;
var
  e: TEdgeArray;
  I, s, d, Total: SizeInt;
  Dsu: TDisjointSetUnion;
begin
  e := CreateEdgeArray;
  Result := CreateIntArray;
  Total := VertexCount;
  TEdgeHelper.Sort(e);
  System.SetLength(Result, Total);
  Dsu.Size := Total;
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
      for p in FNodeList[Curr].AdjList do
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

function TGWeighedGraph.CreateWeightArray: TWeightArray;
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

function TGWeighedGraph.MinPathAStar(constref aSrc, aDst: TVertex; out aWeight: TWeight;
  aHeur: THeuristic): TIntArray;
begin
  Result := MinPathAStarI(IndexOf(aSrc), IndexOf(aSrc), aWeight, aHeur);
end;

function TGWeighedGraph.MinPathAStarI(aSrc, aDst: SizeInt; out aWeight: TWeight; aHeur: THeuristic): TIntArray;
begin
  CheckIndexRange(aSrc);
  CheckIndexRange(aDst);
  if aHeur <> nil then
    Result := AStar(aSrc, aDst, aWeight, aHeur)
  else
    Result := DijkstraPath(aSrc, aDst, aWeight);
end;

function TGWeighedGraph.MinSpanningTreeKrus(out aTotalWeight: TWeight): TIntArray;
begin
  if Connected then
    Result := KruskalMst(aTotalWeight)
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

function TGWeighedGraph.SeparateGraph(constref aVertex: TVertex): TGWeighedGraph;
begin
  Result := inherited SeparateGraph(aVertex) as TGWeighedGraph;
end;

function TGWeighedGraph.SeparateGraphI(aIndex: SizeInt): TGWeighedGraph;
begin
  Result := inherited SeparateGraphI(aIndex) as TGWeighedGraph;
end;

function TGWeighedGraph.SubgraphFromVertexList(constref aList: TIntArray): TGWeighedGraph;
begin
  Result := inherited SubgraphFromVertexList(aList) as TGWeighedGraph;
end;

function TGWeighedGraph.SubgraphFromPairs(constref aPairs: TIntArray): TGWeighedGraph;
begin
  Result := inherited SubgraphFromPairs(aPairs) as TGWeighedGraph;
end;

function TGWeighedGraph.SubgraphFromEdges(constref aEdges: TIntEdgeArray): TGWeighedGraph;
begin
  Result := inherited SubgraphFromEdges(aEdges) as TGWeighedGraph;
end;

function TGWeighedGraph.Clone: TGWeighedGraph;
begin
  Result := inherited Clone as TGWeighedGraph;
end;

function TGWeighedGraph.Complement(aOnAddEdge: TOnAddEdge): TGWeighedGraph;
begin
  Result := inherited Complement(aOnAddEdge) as TGWeighedGraph;
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

function TPointsChart.Complement: TPointsChart;
begin
  Result := TPointsChart(inherited Complement(@OnAddEdge));
end;

function TPointsChart.MinPathAStar(constref aSrc, aDst: TPoint; out aWeight: ValReal; aHeur: THeuristic): TIntArray;
begin
  Result := MinPathAStarI(IndexOf(aSrc), IndexOf(aDst), aWeight, aHeur);
end;

function TPointsChart.MinPathAStarI(aSrc, aDst: SizeInt; out aWeight: ValReal; aHeur: THeuristic): TIntArray;
begin
  if aHeur = nil then
    Result := inherited MinPathAStarI(aSrc, aDst, aWeight, @Distance)
  else
    Result := inherited MinPathAStarI(aSrc, aDst, aWeight, aHeur);
end;

end.

