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

  { TGSimpleUGraph is simple sparse undirected graph based on adjacency lists;
      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGSimpleUGraph<TVertex, TEdgeData, TVertexEqRel> = class(specialize TGCustomSimpleGraph
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

  var
    FCompoCount: SizeInt;
    FConnected,
    FConnectedValid: Boolean;
    procedure DoRemoveVertex(aIndex: SizeInt);
    function  DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
    function  CheckConnected: Boolean; inline;
    function  FindSeparateCount: SizeInt;
    function  GetSeparateCount: SizeInt;
    function  CountPop(aCompIndex: SizeInt): SizeInt;
    function  GetSeparateGraph(aIndex: SizeInt): TGSimpleUGraph;
    property  Connected: Boolean read FConnected;
    property  ConnectedValid: Boolean read FConnectedValid;
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
    function  EulerCycleExists: Boolean;
    function  FindEulerCycle: TIntArray;
  { checks whether the graph is connected; a graph without vertices is considered disconnected }
    function  IsConnected: Boolean; inline;
  { if the graph is not empty, then make graph connected, adding, if necessary, new edges
    from the vertex with the index 0}
    function  MakeConnected(aOnAddEdge: TOnAddEdge = nil): SizeInt;
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
  { returns index of the connected component that contains v }
    function  SeparateIndexOf(constref v: TVertex): SizeInt; inline;
    function  SeparateIndexOfI(aVtxIndex: SizeInt): SizeInt; inline;
  { returns number of vertices(population) in the connected component that contains v }
    function  SeparatePop(constref v: TVertex): SizeInt; inline;
    function  SeparatePopI(aVtxIndex: SizeInt): SizeInt;
  { returns graph of connected component that contains v }
    function  SeparateGraph(constref v: TVertex): TGSimpleUGraph; inline;
    function  SeparateGraphI(aVtxIndex: SizeInt): TGSimpleUGraph;
  { checks whether the graph is a regular graph (that is, the degree of all its
     vertices coincide); an empty graph is considered regular }
    function  IsRegular: Boolean;
    function  IsTree: Boolean; inline;
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

  { TGSimpleWeighedUGraph simple sparse undirected graph based on adjacency lists;
      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean;
      TWeight must have defined comparision operators and MaxValue }
  generic TGSimpleWeighedUGraph<TVertex, TWeight, TEdgeData, TVertexEqRel> = class
  public
  type
    TWeighedGraph  = TGSimpleWeighedUGraph;
    TWeightArray   = array of TWeight;
    TWeEdgeData    = specialize TGWeighedEdgeData<TWeight, TEdgeData>;
    TGraph         = specialize TGSimpleUGraph<TVertex, TWeEdgeData, TVertexEqRel>;
    TEdge          = TGraph.TEdge;
    TEdges         = TGraph.TEdges;
    TDistinctEdges = TGraph.TDistinctEdges;
    TAdjVertices   = TGraph.TAdjVertices;
    TIncidentEdges = TGraph.TIncidentEdges;
    TOnAddEdge     = TGraph.TOnAddEdge;
    TOnReadVertex  = TGraph.TOnReadVertex;
    TOnWriteVertex = TGraph.TOnWriteVertex;
    TOnReadData    = TGraph.TOnReadData;
    TOnWriteData   = TGraph.TOnWriteData;

  protected
  type
    TWeightEdge = record
      Source,
      Destination: SizeInt;
      Weight:  TWeight;
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
      class function Construct(w: TWeight; aIndex: SizeInt): TWeightItem; static; inline;
    end;

    TEdgeHelper  = specialize TGDelegatedArrayHelper<TWeightEdge>;
    TPairingHeap = specialize TGLiteComparablePairHeapMin<TWeightItem>;

  { Filter-Kruskal minimum spanning tree algorithm}
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
      function FindMst(aGraph: TWeighedGraph; out aWeight: TWeight): TIntArray;
    end;

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
    function  DijkstraMap(aSrc: SizeInt): TWeightArray;
    function  DijkstraMap(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
    function  DijkstraPath(aSrc, aDst: SizeInt): TWeight;
    function  DijkstraPath(aSrc, aDst: SizeInt; out aPath: TIntArray): TWeight;
    function  FilterKruskalMst(out aTotalWeight: TWeight): TIntArray;
    function  KruskalMst(out aTotalWeight: TWeight): TIntArray;
    function  PrimMst(out aTotalWeight: TWeight): TIntArray;
    function  CreateWeightVector: TWeightArray;
    function  CreateEdgeVector: TEdgeArray;
    function  EdgeCompare(constref L, R: TWeightEdge): SizeInt; inline;
    class function Min(const L, R: TWeight): TWeight; static; inline;
  public
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
  { returns the length of the shortest path between the vertices aSrc and aDst,
    -1 if the path does not exist }
    function  FindMinPathLen(constref aSrc, aDst: TVertex): SizeInt; inline;
    function  FindMinPathLenI(aSrc, aDst: SizeInt): SizeInt; inline;
  { returns a vector containing in the corresponding components the shortest paths from aRoot }
    function  FindMinPathLenMap(constref aRoot: TVertex): TIntArray; inline;
    function  FindMinPathLenMapI(aRoot: SizeInt = 0): TIntArray; inline;
  { returns a vector containing indices of found shortest path(empty if path does not exists) }
    function  FindMinPath(constref aSrc, aDst: TVertex): TIntArray; inline;
    function  FindMinPathI(aSrc, aDst: SizeInt): TIntArray; inline;
    function  Degree(constref v: TVertex): SizeInt; inline;
    function  DegreeI(aIndex: SizeInt): SizeInt; inline;
    function  Isolated(constref v: TVertex): Boolean; inline;
    function  IsolatedI(aIndex: SizeInt): Boolean; inline;
    function  EulerCycleExists: Boolean; inline;
    function  FindEulerCycle: TIntArray; inline;
  { checks whether the graph is connected; a graph without vertices is considered disconnected }
    function  IsConnected: Boolean; inline;
  { if the graph is not empty, then make graph connected, adding, if necessary, new edges
    from the vertex with the index 0}
    function  MakeConnected(aOnAddEdge: TOnAddEdge = nil): SizeInt; inline;
  { returns the vector of the spanning tree, which is constructed starting from aRoot;
    each element contains the index of its parent (or -1 if it is root or not connected),
    that is, it is a source-destination pair }
    function  DfsSpanningTree(constref aRoot: TVertex): TIntArray; inline;
    function  DfsSpanningTreeI(aRoot: SizeInt = 0): TIntArray; inline;
    function  BfsSpanningTree(constref aRoot: TVertex): TIntArray; inline;
    function  BfsSpanningTreeI(aRoot: SizeInt = 0): TIntArray; inline;
  { returns a graph constructed from the pairs provided by the vector,
    i.e. each element treates as pair of source - destination }
    function  CreateFromVector(constref aVector: TIntArray): TWeighedGraph;
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
  { returns True if exists edge with negative weight }
    function  NegWeighedEdgeExists: Boolean;
  { finds the shortest paths from a given vertex to the remaining vertices in same connected component,
    the weights of all edges must be nonnegative;
    the result contains in the corresponding component the weight of the minimum path to the vertex or
    TWeight.MaxValue if the vertex is unreachable; used Dijkstra's algorithm  }
    function  FindMinPaths(constref aSrc: TVertex): TWeightArray; inline;
    function  FindMinPathsI(aSrc: SizeInt): TWeightArray; inline;
  { same as above and in aPathTree returns paths }
    function  FindMinPaths(constref aSrc: TVertex; out aPathTree: TIntArray): TWeightArray; inline;
    function  FindMinPathsI(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray; inline;
  { finds the shortest path weight from a aSrc to aDst if it exists; the weights of all edges
    must be nonnegative; the result contains shortest path weight or TWeight.MaxValue if the vertex
    is unreachable; used Dijkstra's algorithm  }
    function  FindMinPathWeight(constref aSrc, aDst: TVertex): TWeight; inline;
    function  FindMinPathWeightI(aSrc, aDst: SizeInt): TWeight; inline;
  { same as above and in aPath returns path }
    function  FindMinPathWeight(constref aSrc, aDst: TVertex; out aPath: TIntArray): TWeight; inline;
    function  FindMinPathWeightI(aSrc, aDst: SizeInt; out aPath: TIntArray): TWeight; inline;
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
    property  Vertices[aIndex: SizeInt]: TVertex read GetVertex write SetVertex; default;
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

function TGSimpleUGraph.CheckConnected: Boolean;
begin
  Result := SeparateCount = 1;
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
        {%H-}Stack.Push(Curr);
        repeat
          if not Visited[Curr] then
            begin
              Visited[Curr] := True;
              FVertexList.ItemRefs[Curr]^.Tag := Result;
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
    begin
      FCompoCount := FindSeparateCount;
      FConnectedValid := True;
      FConnected := FCompoCount = 1;
    end;
  Result := FCompoCount;
end;

function TGSimpleUGraph.CountPop(aCompIndex: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I := 0 to Pred(VertexCount) do
    Result += Ord(FVertexList.ItemRefs[I]^.Tag = aCompIndex);
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
              Result.AddEdge(Vertices[Src], Vertices[Dst], GetEdgeDataPtr(Src, Dst)^);
              Stack.Push(Dst);
            end;
      end;
  until not Stack.TryPop(Src);
end;

procedure TGSimpleUGraph.Clear;
begin
  inherited;
  FCompoCount := 0;
  FConnected := False;
  FConnectedValid := False;
end;

function TGSimpleUGraph.AddVertex(constref v: TVertex; out aIndex: SizeInt): Boolean;
begin
  Result := not FVertexList.FindOrAdd(v, aIndex);
  if Result then
    begin
      FVertexList.ItemRefs[aIndex]^.Tag := -1;
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
    //write vertices, but does not save any info about connected
    //this should allow transfer data between directed/undirected graphs
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
    //read vertices
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
        {%H-}Stack.Push(Curr);
        repeat
          if not Visited[Curr] then
            begin
              Visited[Curr] := True;
              FVertexList.ItemRefs[Curr]^.Tag := 0;
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
                aOnAddEdge(0, Curr, @d);
                AddEdgeI(0, Curr, d);
              end
            else
              AddEdgeI(0, Curr);
          end;
      end;
  FCompoCount := 1;
  FConnectedValid := True;
  FConnected := True;
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
  Result := CreateIntVector;
  {%H-}Stack.Push(aRoot);
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
  Result := CreateIntVector;
  Queue.Enqueue(aRoot);
  while Queue{%H-}.TryDequeue(aRoot) do
    if not Visited[aRoot] then
      begin
        Visited[aRoot] := True;
        for I in AdjVerticesI(aRoot) do
          if not Visited[I] then
            begin
              Result[I] := aRoot;
              Queue.Enqueue(I);
            end;
      end;
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

function TGSimpleUGraph.SeparateIndexOf(constref v: TVertex): SizeInt;
begin
   Result := SeparateIndexOfI(FVertexList.IndexOf(v));
end;

function TGSimpleUGraph.SeparateIndexOfI(aVtxIndex: SizeInt): SizeInt;
begin
  FVertexList.CheckIndexRange(aVtxIndex);
  if SeparateCount > 1 then
    Result := FVertexList.ItemRefs[aVtxIndex]^.Tag
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
    Result := CountPop(FVertexList.ItemRefs[aVtxIndex]^.Tag)
  else
    Result := VertexCount;
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

function TGSimpleUGraph.IsTree: Boolean;
begin
  Result := (EdgeCount = Pred(VertexCount)) and IsConnected;
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

{ TGSimpleWeighedUGraph.TWeightItem }

class operator TGSimpleWeighedUGraph.TWeightItem. = (constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight = R.Weight;
end;

class operator TGSimpleWeighedUGraph.TWeightItem.<>(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight <> R.Weight;
end;

class operator TGSimpleWeighedUGraph.TWeightItem.>(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight > R.Weight;
end;

class operator TGSimpleWeighedUGraph.TWeightItem.<(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight < R.Weight;
end;

class operator TGSimpleWeighedUGraph.TWeightItem.>=(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight >= R.Weight;
end;

class operator TGSimpleWeighedUGraph.TWeightItem.<=(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight <= R.Weight;
end;

class function TGSimpleWeighedUGraph.TWeightItem.Construct(w: TWeight; aIndex: SizeInt): TWeightItem;
begin
  Result.Weight := w;
  Result.Index := aIndex;
end;

{ TGSimpleWeighedUGraph.TFilterKruskal }

procedure TGSimpleWeighedUGraph.TFilterKruskal.Kruskal(L, R: SizeInt);
var
  s, d: SizeInt;
begin
  TEdgeHelper.IntroSort(FEdges[L..R], @EdgeCompare);
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

function TGSimpleWeighedUGraph.TFilterKruskal.Split(L, R: SizeInt): SizeInt;
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

procedure TGSimpleWeighedUGraph.TFilterKruskal.FilterKruskal(L, R: SizeInt);
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

function TGSimpleWeighedUGraph.TFilterKruskal.FindMst(aGraph: TWeighedGraph; out aWeight: TWeight): TIntArray;
begin
  FEdges := aGraph.CreateEdgeVector;
  FTree := aGraph.FGraph.CreateIntVector;
  FCount := aGraph.VertexCount;
  FDsu.Size := FCount;
  FTreshold := Succ(FCount); // ??? FCount shr 1
  FWeight := 0;
  FFound := 0;
  FilterKruskal(0, System.High(FEdges));
  aWeight := FWeight;
  Result := FTree;
end;

{ TGSimpleWeighedUGraph }

function TGSimpleWeighedUGraph.GetEdgeCount: SizeInt;
begin
  Result := FGraph.EdgeCount;
end;

function TGSimpleWeighedUGraph.GetTitle: string;
begin
  Result := FGraph.Title;
end;

function TGSimpleWeighedUGraph.GetVertex(aIndex: SizeInt): TVertex;
begin
  Result := FGraph[aIndex];
end;

procedure TGSimpleWeighedUGraph.SetVertex(aIndex: SizeInt; const aValue: TVertex);
begin
  FGraph[aIndex] := aValue;
end;

function TGSimpleWeighedUGraph.GetVertexCount: SizeInt;
begin
  Result := FGraph.VertexCount;
end;

function TGSimpleWeighedUGraph.GetSeparateCount: SizeInt;
begin
  Result := FGraph.SeparateCount;
end;

procedure TGSimpleWeighedUGraph.SetTitle(const aValue: string);
begin
  FGraph.Title := aValue;
end;

function TGSimpleWeighedUGraph.DijkstraMap(aSrc: SizeInt): TWeightArray;
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
  Handles := FGraph.CreateHandleVector;
  Visited.Size := VertexCount;
  Handles[aSrc] := Queue.Insert(TWeightItem.Construct(Default(TWeight), aSrc));
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

function TGSimpleWeighedUGraph.DijkstraMap(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
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
  aPathTree := FGraph.CreateIntVector;
  Handles := FGraph.CreateHandleVector;
  Visited.Size := VertexCount;
  Handles[aSrc] := Queue.Insert(TWeightItem.Construct(Default(TWeight), aSrc));
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

function TGSimpleWeighedUGraph.DijkstraPath(aSrc, aDst: SizeInt): TWeight;
var
  Visited: TBitVector;
  Queue: TPairingHeap;
  Handles: THandleArray;
  Relaxed: TWeight;
  Item: TWeightItem;
  p: PAdjItem;
begin
  Handles := FGraph.CreateHandleVector;
  Visited.Size := VertexCount;
  Handles[aSrc] := Queue.Insert(TWeightItem.Construct(Default(TWeight), aSrc));
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
  Result := TWeight.MaxValue;
end;

function TGSimpleWeighedUGraph.DijkstraPath(aSrc, aDst: SizeInt; out aPath: TIntArray): TWeight;
var
  Visited: TBitVector;
  Queue: TPairingHeap;
  Handles: THandleArray;
  Tree: TIntArray;
  Relaxed: TWeight;
  Item: TWeightItem;
  p: PAdjItem;
begin
  FGraph.CheckIndexRange(aSrc);
  FGraph.CheckIndexRange(aDst);
  Handles := FGraph.CreateHandleVector;
  Tree := FGraph.CreateIntVector;
  Visited.Size := VertexCount;
  Handles[aSrc] := Queue.Insert(TWeightItem.Construct(Default(TWeight), aSrc));
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
            if Handles[p^.Key] <> INVALID_HANDLE then
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
  Result := TWeight.MaxValue;
  aPath := nil;
end;

function TGSimpleWeighedUGraph.FilterKruskalMst(out aTotalWeight: TWeight): TIntArray;
var
  FilterKruskal: TFilterKruskal;
begin
  Result := FilterKruskal.FindMst(Self, aTotalWeight);
end;

function TGSimpleWeighedUGraph.KruskalMst(out aTotalWeight: TWeight): TIntArray;
var
  e: TEdgeArray;
  I, s, d, VtxCount: SizeInt;
  Dsu: TDisjointSetUnion;
begin
  e := CreateEdgeVector;
  Result := FGraph.CreateIntVector;
  VtxCount := VertexCount;
  TEdgeHelper.Sort(e, @EdgeCompare);
  System.SetLength(Result, VtxCount);
  Dsu.Size := VtxCount;
  aTotalWeight := 0;
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

function TGSimpleWeighedUGraph.PrimMst(out aTotalWeight: TWeight): TIntArray;
var
  Visited: TBitVector;
  Queue: TPairingHeap;
  Handles: THandleArray;
  Curr: SizeInt;
  Item: TWeightItem;
  p: PAdjItem;
begin
  Result := FGraph.CreateIntVector;
  System.SetLength(Result, VertexCount);
  Handles := FGraph.CreateHandleVector;
  Visited.Size := VertexCount;
  Handles[0] := Queue.Insert(TWeightItem.Construct(Default(TWeight), 0));
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

function TGSimpleWeighedUGraph.CreateWeightVector: TWeightArray;
var
  I: SizeInt;
begin
  System.SetLength(Result, VertexCount);
  for I := 0 to Pred(VertexCount) do
    Result[I] := TWeight.MaxValue;
end;

function TGSimpleWeighedUGraph.CreateEdgeVector: TEdgeArray;
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

function TGSimpleWeighedUGraph.EdgeCompare(constref L, R: TWeightEdge): SizeInt;
begin
  if L.Weight > R.Weight then
    Result := 1
  else
    if R.Weight > L.Weight then
      Result := -1
    else
      Result := 0;
end;

class function TGSimpleWeighedUGraph.Min(const L, R: TWeight): TWeight;
begin
  if L <= R then
    Result := L
  else
    Result := R;
end;

constructor TGSimpleWeighedUGraph.Create;
begin
  FGraph := TGraph.Create;
end;

constructor TGSimpleWeighedUGraph.Create(g: TGraph);
begin
  FGraph := g;
end;

destructor TGSimpleWeighedUGraph.Destroy;
begin
  FGraph.Free;
  inherited;
end;

function TGSimpleWeighedUGraph.IsEmpty: Boolean;
begin
  Result := FGraph.IsEmpty;
end;

function TGSimpleWeighedUGraph.NonEmpty: Boolean;
begin
  Result := FGraph.NonEmpty;
end;

procedure TGSimpleWeighedUGraph.Clear;
begin
  FGraph.Clear;
end;

procedure TGSimpleWeighedUGraph.EnsureCapacity(aValue: SizeInt);
begin
  FGraph.EnsureCapacity(aValue);
end;

procedure TGSimpleWeighedUGraph.TrimToFit;
begin
  FGraph.TrimToFit;
end;

function TGSimpleWeighedUGraph.ContainsVertex(constref v: TVertex): Boolean;
begin
  Result := FGraph.ContainsVertex(v);
end;

function TGSimpleWeighedUGraph.ContainsEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := FGraph.ContainsEdge(aSrc, aDst);
end;

function TGSimpleWeighedUGraph.ContainsEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := FGraph.ContainsEdgeI(aSrc, aDst);
end;

function TGSimpleWeighedUGraph.ContainsEdgeI(aSrc, aDst: SizeInt; out aData: TWeEdgeData): Boolean;
begin
  Result := FGraph.ContainsEdgeI(aSrc, aDst, aData);
end;

function TGSimpleWeighedUGraph.IndexOf(constref v: TVertex): SizeInt;
begin
  Result := FGraph.IndexOf(v);
end;

function TGSimpleWeighedUGraph.Adjacent(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := FGraph.Adjacent(aSrc, aDst);
end;

function TGSimpleWeighedUGraph.AdjacentI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := FGraph.AdjacentI(aSrc, aDst);
end;

function TGSimpleWeighedUGraph.AdjVertices(constref aSrc: TVertex): TAdjVertices;
begin
  Result := FGraph.AdjVertices(aSrc);
end;

function TGSimpleWeighedUGraph.AdjVerticesI(aSrc: SizeInt): TAdjVertices;
begin
  Result := FGraph.AdjVerticesI(aSrc);
end;

function TGSimpleWeighedUGraph.IncidentEdges(constref aSrc: TVertex): TIncidentEdges;
begin
  Result := FGraph.IncidentEdges(aSrc);
end;

function TGSimpleWeighedUGraph.IncidentEdgesI(aSrc: SizeInt): TIncidentEdges;
begin
  Result := FGraph.IncidentEdgesI(aSrc);
end;

function TGSimpleWeighedUGraph.Edges: TEdges;
begin
  Result := FGraph.Edges;
end;

function TGSimpleWeighedUGraph.DistinctEdges: TDistinctEdges;
begin
  Result := FGraph.DistinctEdges;
end;

function TGSimpleWeighedUGraph.AddVertex(constref v: TVertex; out aIndex: SizeInt): Boolean;
begin
  Result := FGraph.AddVertex(v, aIndex);
end;

function TGSimpleWeighedUGraph.RemoveVertex(constref v: TVertex): Boolean;
begin
  Result := FGraph.RemoveVertex(v);
end;

function TGSimpleWeighedUGraph.RemoveVertexI(aIndex: SizeInt): Boolean;
begin
  Result := FGraph.RemoveVertex(aIndex);
end;

function TGSimpleWeighedUGraph.AddEdge(constref aSrc, aDst: TVertex; aWeight: TWeight;
  aData: TEdgeData): Boolean;
begin
  Result := FGraph.AddEdge(aSrc, aDst, TWeEdgeData.Construct(aWeight, aData));
end;

function TGSimpleWeighedUGraph.AddEdge(constref aSrc, aDst: TVertex; aWeight: TWeight): Boolean;
begin
  Result := FGraph.AddEdge(aSrc, aDst, TWeEdgeData.Construct(aWeight, CFData));
end;

function TGSimpleWeighedUGraph.AddEdge(constref aSrc, aDst: TVertex; aData: TWeEdgeData): Boolean;
begin
  Result := FGraph.AddEdge(aSrc, aDst, aData);
end;

function TGSimpleWeighedUGraph.AddEdgeI(aSrc, aDst: SizeInt; aWeight: TWeight; aData: TEdgeData): Boolean;
begin
  Result := FGraph.AddEdgeI(aSrc, aDst, TWeEdgeData.Construct(aWeight, aData));
end;

function TGSimpleWeighedUGraph.AddEdgeI(aSrc, aDst: SizeInt; aWeight: TWeight): Boolean;
begin
  Result := FGraph.AddEdgeI(aSrc, aDst, TWeEdgeData.Construct(aWeight, CFData));
end;

function TGSimpleWeighedUGraph.AddEdgeI(aSrc, aDst: SizeInt; aData: TWeEdgeData): Boolean;
begin
  Result := FGraph.AddEdgeI(aSrc, aDst, aData);
end;

function TGSimpleWeighedUGraph.RemoveEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := FGraph.RemoveEdge(aSrc, aDst);
end;

function TGSimpleWeighedUGraph.RemoveEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := FGraph.RemoveEdgeI(aSrc, aDst);
end;

function TGSimpleWeighedUGraph.GetEdgeData(constref aSrc, aDst: TVertex): TWeEdgeData;
begin
  Result := FGraph.GetEdgeData(aSrc, aDst);
end;

function TGSimpleWeighedUGraph.GetEdgeDataI(aSrc, aDst: SizeInt): TWeEdgeData;
begin
  Result := FGraph.GetEdgeDataI(aSrc, aDst);
end;

procedure TGSimpleWeighedUGraph.SetEdgeData(constref aSrc, aDst: TVertex; constref aValue: TWeEdgeData);
begin
  FGraph.SetEdgeData(aSrc, aDst, aValue);
end;

procedure TGSimpleWeighedUGraph.SetEdgeDataI(aSrc, aDst: SizeInt; constref aValue: TWeEdgeData);
begin
  FGraph.SetEdgeDataI(aSrc, aDst, aValue);
end;

procedure TGSimpleWeighedUGraph.SaveToStream(aStream: TStream; aWriteVertex: TOnWriteVertex;
  aWriteData: TOnWriteData);
begin
  FGraph.SaveToStream(aStream, aWriteVertex, aWriteData);
end;

procedure TGSimpleWeighedUGraph.LoadFromStream(aStream: TStream; aReadVertex: TOnReadVertex;
  aReadData: TOnReadData);
begin
  FGraph.LoadFromStream(aStream, aReadVertex, aReadData);
end;

procedure TGSimpleWeighedUGraph.SaveToFile(const aFileName: string; aWriteVertex: TOnWriteVertex;
  aWriteData: TOnWriteData);
begin
  FGraph.SaveToFile(aFileName, aWriteVertex, aWriteData)
end;

procedure TGSimpleWeighedUGraph.LoadFromFile(const aFileName: string; aReadVertex: TOnReadVertex;
  aReadData: TOnReadData);
begin
  FGraph.LoadFromFile(aFileName, aReadVertex, aReadData);
end;

function TGSimpleWeighedUGraph.DfsTraversal(constref aRoot: TVertex; aOnGray: TOnIntTest;
  aOnWhite: TOnIntVisit): SizeInt;
begin
  Result := FGraph.DfsTraversal(aRoot, aOnGray, aOnWhite);
end;

function TGSimpleWeighedUGraph.DfsTraversalI(aRoot: SizeInt; aOnGray: TOnIntTest;
  aOnWhite: TOnIntVisit): SizeInt;
begin
  Result := FGraph.DfsTraversalI(aRoot, aOnGray, aOnWhite);
end;

function TGSimpleWeighedUGraph.BfsTraversal(constref aRoot: TVertex; aOnGray: TOnIntTest;
  aOnWhite: TOnIntVisit): SizeInt;
begin
  Result := FGraph.BfsTraversal(aRoot, aOnGray, aOnWhite);
end;

function TGSimpleWeighedUGraph.BfsTraversalI(aRoot: SizeInt; aOnGray: TOnIntTest;
  aOnWhite: TOnIntVisit): SizeInt;
begin
  Result := FGraph.BfsTraversalI(aRoot, aOnGray, aOnWhite);
end;

function TGSimpleWeighedUGraph.SimplePathExists(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := FGraph.SimplePathExists(aSrc, aDst);
end;

function TGSimpleWeighedUGraph.SimplePathExistsI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := FGraph.SimplePathExistsI(aSrc, aDst);
end;

function TGSimpleWeighedUGraph.IsBipartite: Boolean;
begin
  Result := FGraph.IsBipartite;
end;

function TGSimpleWeighedUGraph.IsBipartite(out v: TShortArray): Boolean;
begin
  Result := FGraph.IsBipartite(v)
end;

function TGSimpleWeighedUGraph.FindMinPathLen(constref aSrc, aDst: TVertex): SizeInt;
begin
  Result := FGraph.FindMinPathLen(aSrc, aDst);
end;

function TGSimpleWeighedUGraph.FindMinPathLenI(aSrc, aDst: SizeInt): SizeInt;
begin
  Result := FGraph.FindMinPathLenI(aSrc, aDst);
end;

function TGSimpleWeighedUGraph.FindMinPathLenMap(constref aRoot: TVertex): TIntArray;
begin
  Result := FGraph.FindMinPathLenMap(aRoot);
end;

function TGSimpleWeighedUGraph.FindMinPathLenMapI(aRoot: SizeInt): TIntArray;
begin
  Result := FGraph.FindMinPathLenMapI(aRoot);
end;

function TGSimpleWeighedUGraph.FindMinPath(constref aSrc, aDst: TVertex): TIntArray;
begin
  Result := FGraph.FindMinPath(aSrc, aDst);
end;

function TGSimpleWeighedUGraph.FindMinPathI(aSrc, aDst: SizeInt): TIntArray;
begin
  Result := FGraph.FindMinPathI(aSrc, aDst);
end;

function TGSimpleWeighedUGraph.Degree(constref v: TVertex): SizeInt;
begin
  Result := FGraph.Degree(v);
end;

function TGSimpleWeighedUGraph.DegreeI(aIndex: SizeInt): SizeInt;
begin
  Result := FGraph.DegreeI(aIndex);
end;

function TGSimpleWeighedUGraph.Isolated(constref v: TVertex): Boolean;
begin
  Result := FGraph.Isolated(v);
end;

function TGSimpleWeighedUGraph.IsolatedI(aIndex: SizeInt): Boolean;
begin
  Result := FGraph.IsolatedI(aIndex);
end;

function TGSimpleWeighedUGraph.EulerCycleExists: Boolean;
begin
  Result := FGraph.EulerCycleExists;
end;

function TGSimpleWeighedUGraph.FindEulerCycle: TIntArray;
begin
  Result := FGraph.FindEulerCycle;
end;

function TGSimpleWeighedUGraph.IsConnected: Boolean;
begin
  Result := FGraph.IsConnected;
end;

function TGSimpleWeighedUGraph.MakeConnected(aOnAddEdge: TOnAddEdge): SizeInt;
begin
  Result := FGraph.MakeConnected(aOnAddEdge);
end;

function TGSimpleWeighedUGraph.DfsSpanningTree(constref aRoot: TVertex): TIntArray;
begin
  Result := FGraph.DfsSpanningTree(aRoot);
end;

function TGSimpleWeighedUGraph.DfsSpanningTreeI(aRoot: SizeInt): TIntArray;
begin
  Result := FGraph.DfsSpanningTreeI(aRoot);
end;

function TGSimpleWeighedUGraph.BfsSpanningTree(constref aRoot: TVertex): TIntArray;
begin
  Result := FGraph.BfsSpanningTree(aRoot);
end;

function TGSimpleWeighedUGraph.BfsSpanningTreeI(aRoot: SizeInt): TIntArray;
begin
  Result := FGraph.BfsSpanningTreeI(aRoot);
end;

function TGSimpleWeighedUGraph.CreateFromVector(constref aVector: TIntArray): TWeighedGraph;
begin
  Result := TGSimpleWeighedUGraph.Create(FGraph.CreateFromVector(aVector));
end;

function TGSimpleWeighedUGraph.SeparateIndex(constref v: TVertex): SizeInt;
begin
  Result := FGraph.SeparateIndexOf(v);
end;

function TGSimpleWeighedUGraph.SeparateIndexI(aVertex: SizeInt): SizeInt;
begin
  Result := FGraph.SeparateIndexOfI(aVertex);
end;

function TGSimpleWeighedUGraph.SeparatePop(constref v: TVertex): SizeInt;
begin
  Result := FGraph.SeparatePop(v);
end;

function TGSimpleWeighedUGraph.SeparatePopI(aVertex: SizeInt): SizeInt;
begin
  Result := FGraph.SeparatePopI(aVertex);
end;

function TGSimpleWeighedUGraph.SeparateGraph(constref v: TVertex): TWeighedGraph;
begin
  Result := SeparateGraphI(IndexOf(v));
end;

function TGSimpleWeighedUGraph.SeparateGraphI(aVtxIndex: SizeInt): TWeighedGraph;
begin
  Result := TWeighedGraph.Create(FGraph.SeparateGraphI(aVtxIndex));
end;

function TGSimpleWeighedUGraph.IsRegular: Boolean;
begin
  Result := FGraph.IsRegular;
end;

function TGSimpleWeighedUGraph.IsTree: Boolean;
begin
  Result := FGraph.IsTree;
end;

function TGSimpleWeighedUGraph.NegWeighedEdgeExists: Boolean;
var
  e: TEdge;
begin
  for e in DistinctEdges do
    if e.Data.Weight < 0 then
      exit(True);
  Result := False;
end;

function TGSimpleWeighedUGraph.FindMinPaths(constref aSrc: TVertex): TWeightArray;
begin
  Result := FindMinPathsI(FGraph.IndexOf(aSrc));
end;

function TGSimpleWeighedUGraph.FindMinPathsI(aSrc: SizeInt): TWeightArray;
begin
  Result := DijkstraMap(aSrc);
end;

function TGSimpleWeighedUGraph.FindMinPaths(constref aSrc: TVertex; out aPathTree: TIntArray): TWeightArray;
begin
  Result := FindMinPathsI(FGraph.IndexOf(aSrc), aPathTree);
end;

function TGSimpleWeighedUGraph.FindMinPathsI(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
begin
  Result := DijkstraMap(aSrc, aPathTree);
end;

function TGSimpleWeighedUGraph.FindMinPathWeight(constref aSrc, aDst: TVertex): TWeight;
begin
  Result := FindMinPathWeightI(FGraph.IndexOf(aSrc), FGraph.IndexOf(aDst));
end;

function TGSimpleWeighedUGraph.FindMinPathWeightI(aSrc, aDst: SizeInt): TWeight;
begin
  Result := DijkstraPath(aSrc, aDst);
end;

function TGSimpleWeighedUGraph.FindMinPathWeight(constref aSrc, aDst: TVertex; out aPath: TIntArray): TWeight;
begin
  Result := FindMinPathWeightI(FGraph.IndexOf(aSrc), FGraph.IndexOf(aDst), aPath);
end;

function TGSimpleWeighedUGraph.FindMinPathWeightI(aSrc, aDst: SizeInt; out aPath: TIntArray): TWeight;
begin
  Result := DijkstraPath(aSrc, aDst, aPath);
end;

function TGSimpleWeighedUGraph.FindMinSpanningTreeKrus(out aTotalWeight: TWeight): TIntArray;
begin
  if IsConnected then
    //Result := KruskalMst(aTotalWeight)
    Result := FilterKruskalMst(aTotalWeight)
  else
    raise ELGraphError.Create(SEGraphIsNotConnected);
end;

function TGSimpleWeighedUGraph.FindMinSpanningTreePrim(out aTotalWeight: TWeight): TIntArray;
begin
  if IsConnected then
    Result := PrimMst(aTotalWeight)
  else
    raise ELGraphError.Create(SEGraphIsNotConnected);
end;

function TGSimpleWeighedUGraph.Clone: TWeighedGraph;
begin
  Result := TWeighedGraph.Create(FGraph.Clone);
end;

end.

