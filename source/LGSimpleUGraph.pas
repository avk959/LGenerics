{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generic simple undirected graph implementation.                         *
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

  { TGSimpleSparseUGraph is simple sparse undirected graph based on adjacency lists;
      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGSimpleSparseUGraph<TVertex, TEdgeData, TVertexEqRel> = class(
    specialize TGCustomSimpleSparseGraph<TVertex, TEdgeData, TVertexEqRel>)
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
        FGraph: TGSimpleSparseUGraph;
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
    property  Connected: Boolean read FConnected;
    property  ConnectedValid: Boolean read FConnectedValid;
  public
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
    function  CreateFromVector(constref aVector: TIntArray): TGSimpleSparseUGraph;
  { returns index of the connected component that contains v }
    function  SeparateIndex(constref v: TVertex): SizeInt; inline;
    function  SeparateIndexI(aVtxIndex: SizeInt): SizeInt; inline;
  { returns number of vertices(population) in the connected component that contains v }
    function  SeparatePop(constref v: TVertex): SizeInt; inline;
    function  SeparatePopI(aVtxIndex: SizeInt): SizeInt;
  { returns list of vertices of connected component that contains v }
    function  SeparateList(constref v: TVertex): TIntArray; inline;
    function  SeparateListI(aVtxIndex: SizeInt): TIntArray;
  { checks whether the graph is a regular graph (that is, the degree of all its
     vertices coincide); an empty graph is considered regular }
    function  IsRegular: Boolean;
    function  IsTree: Boolean; inline;
    function  DistinctEdges: TDistinctEdges; inline;

    function  Clone: TGSimpleSparseUGraph;
  { count of connected components }
    property  SeparateCount: SizeInt read GetSeparateCount;
  end;

  generic TGWeighedEdgeData<TWeight, TEdgeData> = record
    Weight: TWeight;
    Data: TEdgeData;
    class function Construct(constref w: TWeight; constref d: TEdgeData): TGWeighedEdgeData; static; inline;
  end;

  THandle = LGUtils.THandle;

  { TGSimpleSparseWeighedUGraph simple sparse undirected graph based on adjacency lists;
      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean;
      TWeight must have defined comparision operators and MaxValue }
  generic TGSimpleSparseWeighedUGraph<TVertex, TWeight, TEdgeData, TVertexEqRel> = class
  public
  type
    TWeighedGraph  = TGSimpleSparseWeighedUGraph;
    TWeightArray   = array of TWeight;
    TWeEdgeData    = specialize TGWeighedEdgeData<TWeight, TEdgeData>;
    TGraph         = specialize TGSimpleSparseUGraph<TVertex, TWeEdgeData, TVertexEqRel>;
    TEdge          = TGraph.TEdge;
    TEdges         = TGraph.TEdges;
    TDistinctEdges = TGraph.TDistinctEdges;
    TAdjVertices   = TGraph.TAdjVertices;
    TIncidentEdges = TGraph.TIncidentEdges;
    TOnAddEdge     = TGraph.TOnAddEdge;

  protected
  type
    TEdgeArray   = array of TEdge;
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

    TEdgeHelper  = specialize TGDelegatedArrayHelper<TEdge>;

    TPriorityQueue = specialize TGLiteComparablePairHeapMin<TWeightItem>;

    //TKruskalMST = record
    //private
    //const
    //  TRESHOLD = 128;
    //var
    //  FDsu: TDisjointSetUnion;
    //  FEdges: TEdgeArray;
    //  FTotalWeight: TWeight;
    //  FDone: Boolean;
    //  procedure Kruskal();
    //public
    //  function Execute(aGraph: TWeighedGraph; out aTotalWeight: TWeight): TIntArray;
    //end;

  class var
    CFData: TEdgeData;

  var
    FGraph: TGraph;
    function  GetEdgeCount: SizeInt; inline;
    function  GetTitle: string; inline;
    function  GetVertex(aIndex: SizeInt): TVertex; inline;
    function  GetVertexCount: SizeInt; inline;
    function  GetSeparateCount: SizeInt; inline;
    procedure SetTitle(const aValue: string); inline;
  { Dijkstra's algorithm: single-source shortest paths problem for non-negative weights  }
    function  DijkstraMap(aSrc: SizeInt): TWeightArray;
    function  DijkstraMap(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
    function  DijkstraPath(aSrc, aDst: SizeInt): TWeight;
    function  DijkstraPath(aSrc, aDst: SizeInt; out aPath: TIntArray): TWeight;
    function  KruskalMst(out aTotalWeight: TWeight): TEdgeArray;
    function  PrimMst(out aTotalWeight: TWeight): TIntArray;
    function  CreateEdgeArray: TEdgeArray;
    function  EdgeCompare(constref L, R: TEdge): SizeInt; inline;
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
    function  AddEdgeI(aSrc, aDst: SizeInt; aWeight: TWeight; aData: TEdgeData): Boolean; inline;
    function  AddEdgeI(aSrc, aDst: SizeInt; aWeight: TWeight): Boolean; inline;
    function  RemoveEdge(constref aSrc, aDst: TVertex): Boolean; inline;
    function  RemoveEdgeI(aSrc, aDst: SizeInt): Boolean; inline;
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
    function  FindMinPathLenVector(constref aRoot: TVertex): TIntArray; inline;
    function  FindMinPathLenVectorI(aRoot: SizeInt = 0): TIntArray; inline;
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
  { returns list of vertices of connected component that contains v }
    function  SeparateList(constref v: TVertex): TIntArray; inline;
    function  SeparateListI(aVtxIndex: SizeInt): TIntArray; inline;
  { checks whether the graph is a regular graph (that is, the degree of all its
     vertices coincide); an empty graph is considered regular }
    function  IsRegular: Boolean; inline;
    function  IsTree: Boolean; inline;
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
  { returns a graph constructed from the edges provided by the array }
    function  CreateFromEdgeArray(constref aEdgeArray: TEdgeArray): TWeighedGraph;
  { finds a spanning tree of minimal weight, the graph must be connected(Kruskal's algorithm used)}
    function  FindMinSpanningTreeKrus(out aTotalWeight: TWeight): TEdgeArray;
  { finds a spanning tree of minimal weight, the graph must be connected(Prim's algorithm used) }
    function  FindMinSpanningTreePrim(out aTotalWeight: TWeight): TIntArray;

    function  Clone: TWeighedGraph;
    property  Title: string read GetTitle write SetTitle;
    property  VertexCount: SizeInt read GetVertexCount;
    property  EdgeCount: SizeInt read GetEdgeCount;
  { count of connected components }
    property  SeparateCount: SizeInt read GetSeparateCount;
    property  Vertices[aIndex: SizeInt]: TVertex read GetVertex; default;
  end;

implementation
{$B-}{$COPERATORS ON}

{ TGSimpleSparseUGraph.TDistinctEdgeEnumerator }

function TGSimpleSparseUGraph.TDistinctEdgeEnumerator.GetCurrent: TEdge;
begin
  Result.Init(FCurrIndex, FEnum.Current);
end;

function TGSimpleSparseUGraph.TDistinctEdgeEnumerator.MoveNext: Boolean;
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

procedure TGSimpleSparseUGraph.TDistinctEdgeEnumerator.Reset;
begin
  FCurrIndex := -1;
  FEnumDone := True;
  FVisited.Reset;
end;

{ TGSimpleSparseUGraph.TDistinctEdges }

function TGSimpleSparseUGraph.TDistinctEdges.GetEnumerator: TDistinctEdgeEnumerator;
begin
  Result.FList := FGraph.FVertexList.FNodeList;
  Result.FLastIndex := Pred(FGraph.FVertexList.Count);
  Result.FVisited.Size := Succ(Result.FLastIndex);
  Result.FCurrIndex := -1;
  Result.FEnumDone := True;
end;

{ TGSimpleSparseUGraph }

procedure TGSimpleSparseUGraph.DoRemoveVertex(aIndex: SizeInt);
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

function TGSimpleSparseUGraph.DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
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

function TGSimpleSparseUGraph.DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
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

function TGSimpleSparseUGraph.CheckConnected: Boolean;
begin
  Result := SeparateCount = 1;
end;

function TGSimpleSparseUGraph.FindSeparateCount: SizeInt;
var
  Visited: TBitVector;
  Stack: TIntStack;
  I, Curr: SizeInt;
begin
  Result := 0;
  if IsEmpty then
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
              FVertexList.ItemRefs[Curr]^.Tag := Result;
              for Curr in AdjVerticesI(Curr) do
                if not Visited[Curr] then
                  Stack.Push(Curr);
            end;
        until not Stack.TryPop(Curr);
        Inc(Result);
      end;
end;

function TGSimpleSparseUGraph.GetSeparateCount: SizeInt;
begin
  if not ConnectedValid then
    begin
      FCompoCount := FindSeparateCount;
      FConnectedValid := True;
      FConnected := FCompoCount = 1;
    end;
  Result := FCompoCount;
end;

function TGSimpleSparseUGraph.CountPop(aCompIndex: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  Result := 0;
  for I := 0 to Pred(VertexCount) do
    Result += Ord(FVertexList.ItemRefs[I]^.Tag = aCompIndex);
end;

function TGSimpleSparseUGraph.AddVertex(constref v: TVertex; out aIndex: SizeInt): Boolean;
begin
  Result := not FVertexList.FindOrAdd(v, aIndex);
  if Result then
    begin
      FVertexList.ItemRefs[aIndex]^.Tag := -1;
      FConnectedValid := False;
    end;
end;

function TGSimpleSparseUGraph.RemoveVertex(constref v: TVertex): Boolean;
begin
  Result := RemoveVertexI(FVertexList.IndexOf(v));
end;

function TGSimpleSparseUGraph.RemoveVertexI(aIndex: SizeInt): Boolean;
begin
  Result := (aIndex >= 0) and (aIndex < FVertexList.Count);
  if Result then
    DoRemoveVertex(aIndex);
end;

function TGSimpleSparseUGraph.AddEdge(constref aSrc, aDst: TVertex; aData: TEdgeData): Boolean;
var
  SrcIdx, DstIdx: SizeInt;
begin
  AddVertex(aSrc, SrcIdx);
  AddVertex(aDst, DstIdx);
  Result := DoAddEdge(SrcIdx, DstIdx, aData);
end;

function TGSimpleSparseUGraph.AddEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := AddEdge(aSrc, aDst, CFData);
end;

function TGSimpleSparseUGraph.AddEdgeI(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
begin
  if (aSrc < 0) or (aSrc >= FVertexList.Count) then
    exit(False);
  if (aDst < 0) or (aDst >= FVertexList.Count) then
    exit(False);
  Result := DoAddEdge(aSrc, aDst, aData);
end;

function TGSimpleSparseUGraph.AddEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := AddEdgeI(aSrc, aDst, CFData);
end;

function TGSimpleSparseUGraph.RemoveEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := RemoveEdgeI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGSimpleSparseUGraph.RemoveEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  if (aSrc < 0) or (aSrc >= FVertexList.Count) then
    exit(False);
  if (aDst < 0) or (aDst >= FVertexList.Count) then
    exit(False);
  Result := DoRemoveEdge(aSrc, aDst);
end;

function TGSimpleSparseUGraph.Degree(constref v: TVertex): SizeInt;
begin
  Result := DegreeI(FVertexList.IndexOf(v));
end;

function TGSimpleSparseUGraph.DegreeI(aIndex: SizeInt): SizeInt;
begin
  FVertexList.CheckIndexRange(aIndex);
  Result := FVertexList.ItemRefs[aIndex]^.Count;
end;

function TGSimpleSparseUGraph.Isolated(constref v: TVertex): Boolean;
begin
  Result := Degree(v) = 0;
end;

function TGSimpleSparseUGraph.IsolatedI(aIndex: SizeInt): Boolean;
begin
  Result := DegreeI(aIndex) = 0;
end;

function TGSimpleSparseUGraph.EulerCycleExists: Boolean;
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

function TGSimpleSparseUGraph.FindEulerCycle: TIntArray;
var
  g: TGSimpleSparseUGraph = nil;
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

function TGSimpleSparseUGraph.IsConnected: Boolean;
begin
  if not ConnectedValid then
    FConnected := CheckConnected;
  Result := Connected;
end;

function TGSimpleSparseUGraph.MakeConnected(aOnAddEdge: TOnAddEdge): SizeInt;
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

function TGSimpleSparseUGraph.DfsSpanningTree(constref aRoot: TVertex): TIntArray;
begin
  Result := DfsSpanningTreeI(FVertexList.IndexOf(aRoot));
end;

function TGSimpleSparseUGraph.DfsSpanningTreeI(aRoot: SizeInt): TIntArray;
var
  Visited: TBitVector;
  Stack: TIntStack;
  I: SizeInt;
begin
  FVertexList.CheckIndexRange(aRoot);
  Visited.Size := VertexCount;
  Result := CreateIndexVector;
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

function TGSimpleSparseUGraph.BfsSpanningTree(constref aRoot: TVertex): TIntArray;
begin
  Result := BfsSpanningTreeI(FVertexList.IndexOf(aRoot));
end;

function TGSimpleSparseUGraph.BfsSpanningTreeI(aRoot: SizeInt): TIntArray;
var
  Visited: TBitVector;
  Queue: TIntQueue;
  I: SizeInt;
begin
  FVertexList.CheckIndexRange(aRoot);
  Visited.Size := VertexCount;
  Result := CreateIndexVector;
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

function TGSimpleSparseUGraph.CreateFromVector(constref aVector: TIntArray): TGSimpleSparseUGraph;
var
  I, Src: SizeInt;
begin
  Result := TGSimpleSparseUGraph.Create;
  for I := 0 to Pred(System.Length(aVector)) do
    begin
      Src := aVector[I];
      if Src <> -1 then
        Result.AddEdge(FVertexList[Src], FVertexList[I], GetEdgeData(Src, I)^);
    end;
end;

function TGSimpleSparseUGraph.SeparateIndex(constref v: TVertex): SizeInt;
begin
   Result := SeparateIndexI(FVertexList.IndexOf(v));
end;

function TGSimpleSparseUGraph.SeparateIndexI(aVtxIndex: SizeInt): SizeInt;
begin
  FVertexList.CheckIndexRange(aVtxIndex);
  if SeparateCount > 1 then
    Result := FVertexList.ItemRefs[aVtxIndex]^.Tag
  else
    Result := 0;
end;

function TGSimpleSparseUGraph.SeparatePop(constref v: TVertex): SizeInt;
begin
  Result := SeparatePopI(FVertexList.IndexOf(v));
end;

function TGSimpleSparseUGraph.SeparatePopI(aVtxIndex: SizeInt): SizeInt;
begin
  FVertexList.CheckIndexRange(aVtxIndex);
  if SeparateCount > 1 then
    Result := CountPop(FVertexList.ItemRefs[aVtxIndex]^.Tag)
  else
    Result := VertexCount;
end;

function TGSimpleSparseUGraph.SeparateList(constref v: TVertex): TIntArray;
begin
  Result := SeparateListI(FVertexList.IndexOf(v));
end;

function TGSimpleSparseUGraph.SeparateListI(aVtxIndex: SizeInt): TIntArray;
var
  I, J, sIdx: SizeInt;
begin
  FVertexList.CheckIndexRange(aVtxIndex);
  System.SetLength(Result, SeparatePopI(aVtxIndex));
  sIdx := SeparateIndexI(aVtxIndex);
  J := 0;
  for I := 0 to Pred(VertexCount) do
    if FVertexList.ItemRefs[I]^.Tag = sIdx then
      begin
        Result[J] := I;
        Inc(J)
      end;
end;

function TGSimpleSparseUGraph.IsRegular: Boolean;
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

function TGSimpleSparseUGraph.IsTree: Boolean;
begin
  Result := (EdgeCount = Pred(VertexCount)) and IsConnected;
end;

function TGSimpleSparseUGraph.DistinctEdges: TDistinctEdges;
begin
  Result.FGraph := Self;
end;

function TGSimpleSparseUGraph.Clone: TGSimpleSparseUGraph;
begin
  Result := TGSimpleSparseUGraph.Create;
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

{ TGSimpleSparseWeighedUGraph.TWeightItem }

class operator TGSimpleSparseWeighedUGraph.TWeightItem. = (constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight = R.Weight;
end;

class operator TGSimpleSparseWeighedUGraph.TWeightItem.<>(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight <> R.Weight;
end;

class operator TGSimpleSparseWeighedUGraph.TWeightItem.>(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight > R.Weight;
end;

class operator TGSimpleSparseWeighedUGraph.TWeightItem.<(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight < R.Weight;
end;

class operator TGSimpleSparseWeighedUGraph.TWeightItem.>=(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight >= R.Weight;
end;

class operator TGSimpleSparseWeighedUGraph.TWeightItem.<=(constref L, R: TWeightItem): Boolean;
begin
  Result := L.Weight <= R.Weight;
end;

class function TGSimpleSparseWeighedUGraph.TWeightItem.Construct(w: TWeight; aIndex: SizeInt): TWeightItem;
begin
  Result.Weight := w;
  Result.Index := aIndex;
end;

{ TGSimpleSparseWeighedUGraph }

function TGSimpleSparseWeighedUGraph.GetEdgeCount: SizeInt;
begin
  Result := FGraph.EdgeCount;
end;

function TGSimpleSparseWeighedUGraph.GetTitle: string;
begin
  Result := FGraph.Title;
end;

function TGSimpleSparseWeighedUGraph.GetVertex(aIndex: SizeInt): TVertex;
begin
  Result := FGraph[aIndex];
end;

function TGSimpleSparseWeighedUGraph.GetVertexCount: SizeInt;
begin
  Result := FGraph.VertexCount;
end;

function TGSimpleSparseWeighedUGraph.GetSeparateCount: SizeInt;
begin
  Result := FGraph.SeparateCount;
end;

procedure TGSimpleSparseWeighedUGraph.SetTitle(const aValue: string);
begin
  FGraph.Title := aValue;
end;

function TGSimpleSparseWeighedUGraph.DijkstraMap(aSrc: SizeInt): TWeightArray;
var
  Visited: TBitVector;
  Queue: TPriorityQueue;
  Handles: THandleArray;
  NewWeight: TWeight;
  Item: TWeightItem;
  I: SizeInt;
  p: PAdjItem;
begin
  FGraph.CheckIndexRange(aSrc);
  System.SetLength(Result, VertexCount);
  System.SetLength(Handles, VertexCount);
  Visited.Size := VertexCount;
  Queue.EnsureCapacity(VertexCount);
  for I := 0 to Pred(VertexCount) do
    Handles[I] := Queue.Insert(TWeightItem.Construct(TWeight.MaxValue, I));
  Queue.Update(Handles[aSrc], TWeightItem.Construct(Default(TWeight), aSrc));
  while Queue.TryDequeue(Item) do
    if not Visited[Item.Index] then
      begin
        Visited[Item.Index] := True;
        Result[Item.Index] := Item.Weight;
        for p in FGraph.AdjVerticesPtr(Item.Index) do
          if not Visited[p^.Key] then
            if p^.Data.Weight >= 0 then
              begin
                NewWeight := p^.Data.Weight + Item.Weight;
                if NewWeight < Queue.Value(Handles[p^.Key]).Weight then
                  Queue.Update(Handles[p^.Key], TWeightItem.Construct(NewWeight, p^.Key));
              end
            else
              raise ELGGraphError.Create(SENegValuesNotAllowed);
      end;
end;

function TGSimpleSparseWeighedUGraph.DijkstraMap(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
var
  Visited: TBitVector;
  Queue: TPriorityQueue;
  Handles: THandleArray;
  NewWeight: TWeight;
  Item: TWeightItem;
  I: SizeInt;
  p: PAdjItem;
begin
  FGraph.CheckIndexRange(aSrc);
  System.SetLength(Result, VertexCount);
  System.SetLength(Handles, VertexCount);
  aPathTree := FGraph.CreateIndexVector;
  Visited.Size := VertexCount;
  Queue.EnsureCapacity(VertexCount);
  for I := 0 to Pred(VertexCount) do
    Handles[I] := Queue.Insert(TWeightItem.Construct(TWeight.MaxValue, I));
  Queue.Update(Handles[aSrc], TWeightItem.Construct(Default(TWeight), aSrc));
  while Queue.TryDequeue(Item) do
    if not Visited[Item.Index] then
      begin
        Visited[Item.Index] := True;
        Result[Item.Index] := Item.Weight;
        for p in FGraph.AdjVerticesPtr(Item.Index) do
          if not Visited[p^.Key] then
            if p^.Data.Weight >= 0 then
              begin
                NewWeight := p^.Data.Weight + Item.Weight;
                if NewWeight < Queue.Value(Handles[p^.Key]).Weight then
                  begin
                    aPathTree[p^.Key] := Item.Index;
                    Queue.Update(Handles[p^.Key], TWeightItem.Construct(NewWeight, p^.Key));
                  end;
              end
            else
              raise ELGGraphError.Create(SENegValuesNotAllowed);
      end;
end;

function TGSimpleSparseWeighedUGraph.DijkstraPath(aSrc, aDst: SizeInt): TWeight;
var
  Visited: TBitVector;
  Queue: TPriorityQueue;
  Handles: THandleArray;
  NewWeight: TWeight;
  Item: TWeightItem;
  I: SizeInt;
  p: PAdjItem;
begin
  FGraph.CheckIndexRange(aSrc);
  FGraph.CheckIndexRange(aDst);
  System.SetLength(Handles, VertexCount);
  Queue.EnsureCapacity(VertexCount);
  Visited.Size := VertexCount;
  for I := 0 to Pred(VertexCount) do
    Handles[I] := Queue.Insert(TWeightItem.Construct(TWeight.MaxValue, I));
  Queue.Update(Handles[aSrc], TWeightItem.Construct(Default(TWeight), aSrc));
  while Queue.TryDequeue(Item) do
    if not Visited[Item.Index] then
      begin
        if Item.Index = aDst then
          exit(Item.Weight);
        Visited[Item.Index] := True;
        for p in FGraph.AdjVerticesPtr(Item.Index) do
          if not Visited[p^.Key] then
            if p^.Data.Weight >= 0 then
              begin
                NewWeight := p^.Data.Weight + Item.Weight;
                if NewWeight < Queue.Value(Handles[p^.Key]).Weight then
                  Queue.Update(Handles[p^.Key], TWeightItem.Construct(NewWeight, p^.Key));
              end
            else
              raise ELGGraphError.Create(SENegValuesNotAllowed);
      end;
  Result := TWeight.MaxValue;
end;

function TGSimpleSparseWeighedUGraph.DijkstraPath(aSrc, aDst: SizeInt; out aPath: TIntArray): TWeight;
var
  Visited: TBitVector;
  Queue: TPriorityQueue;
  Handles: THandleArray;
  Tree: TIntArray;
  NewWeight: TWeight;
  Item: TWeightItem;
  I: SizeInt;
  p: PAdjItem;
begin
  FGraph.CheckIndexRange(aSrc);
  FGraph.CheckIndexRange(aDst);
  System.SetLength(Handles, VertexCount);
  Tree := FGraph.CreateIndexVector;
  Queue.EnsureCapacity(VertexCount);
  Visited.Size := VertexCount;
  for I := 0 to Pred(VertexCount) do
    Handles[I] := Queue.Insert(TWeightItem.Construct(TWeight.MaxValue, I));
  Queue.Update(Handles[aSrc], TWeightItem.Construct(Default(TWeight), aSrc));
  Result := TWeight.MaxValue;
  aPath := nil;
  while Queue.TryDequeue(Item) do
    if not Visited[Item.Index] then
      begin
        if Item.Index = aDst then
          begin
            Result := Item.Weight;
            aPath := FGraph.PathFromTree(Tree, aDst);
            break;
          end;
        Visited[Item.Index] := True;
        for p in FGraph.AdjVerticesPtr(Item.Index) do
          if not Visited[p^.Key] then
            if p^.Data.Weight >= 0 then
              begin
                NewWeight := p^.Data.Weight + Item.Weight;
                if NewWeight < Queue.Value(Handles[p^.Key]).Weight then
                  begin
                    Queue.Update(Handles[p^.Key], TWeightItem.Construct(NewWeight, p^.Key));
                    Tree[p^.Key] := Item.Index;
                  end;
              end
            else
              raise ELGGraphError.Create(SENegValuesNotAllowed);
      end;
end;

function TGSimpleSparseWeighedUGraph.KruskalMst(out aTotalWeight: TWeight): TEdgeArray;
var
  e: TEdgeArray;
  ResIdx, EdgeIdx, s, d, VtxCount: SizeInt;
  Dsu: TDisjointSetUnion;
begin
  e := CreateEdgeArray;
  VtxCount := VertexCount;
  TEdgeHelper.Sort(e, @EdgeCompare);
  System.SetLength(Result, VtxCount);
  Dsu.Size := VtxCount;
  aTotalWeight := 0;
  ResIdx := 0;
  for EdgeIdx := 0 to System.High(e) do
    begin
      s := e[EdgeIdx].Source;
      d := e[EdgeIdx].Destination;
      if not Dsu.InSameSet(s, d)  then
        begin
          aTotalWeight += e[EdgeIdx].Data.Weight;
          Result[ResIdx] := e[EdgeIdx];
          Inc(ResIdx);
          Dsu.Union(s, d);
        end;
    end;
end;

function TGSimpleSparseWeighedUGraph.PrimMst(out aTotalWeight: TWeight): TIntArray;
var
  Visited: TBitVector;
  Queue: TPriorityQueue;
  Handles: THandleArray;
  Curr: SizeInt;
  Item: TWeightItem;
  p: PAdjItem;
begin
  Result := FGraph.CreateIndexVector;
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
              Handles[p^.Key] := Queue.Insert(TWeightItem.Construct(TWeight.MaxValue, p^.Key));
            if not Visited[p^.Key] and (p^.Data.Weight < Queue.Value(Handles[p^.Key]).Weight) then
              begin
                  Queue.Update(Handles[p^.Key], TWeightItem.Construct(p^.Data.Weight, p^.Key));
                  Result[p^.Key] := Curr;
              end;
          end;
      end;
end;

function TGSimpleSparseWeighedUGraph.CreateEdgeArray: TEdgeArray;
var
  I: SizeInt = 0;
  e: TEdge;
begin
  System.SetLength(Result, EdgeCount);
  for e in DistinctEdges do
    begin
      Result[I] := e;
      Inc(I);
    end;
end;

function TGSimpleSparseWeighedUGraph.EdgeCompare(constref L, R: TEdge): SizeInt;
begin
  if L.Data.Weight > R.Data.Weight then
    Result := 1
  else
    if R.Data.Weight > L.Data.Weight then
      Result := -1
    else
      Result := 0;
end;

class function TGSimpleSparseWeighedUGraph.Min(const L, R: TWeight): TWeight;
begin
  if L <= R then
    Result := L
  else
    Result := R;
end;

constructor TGSimpleSparseWeighedUGraph.Create;
begin
  FGraph := TGraph.Create;
end;

constructor TGSimpleSparseWeighedUGraph.Create(g: TGraph);
begin
  FGraph := g;
end;

destructor TGSimpleSparseWeighedUGraph.Destroy;
begin
  FGraph.Free;
  inherited;
end;

function TGSimpleSparseWeighedUGraph.IsEmpty: Boolean;
begin
  Result := FGraph.IsEmpty;
end;

function TGSimpleSparseWeighedUGraph.NonEmpty: Boolean;
begin
  Result := FGraph.NonEmpty;
end;

procedure TGSimpleSparseWeighedUGraph.Clear;
begin
  FGraph.Clear;
end;

procedure TGSimpleSparseWeighedUGraph.EnsureCapacity(aValue: SizeInt);
begin
  FGraph.EnsureCapacity(aValue);
end;

procedure TGSimpleSparseWeighedUGraph.TrimToFit;
begin
  FGraph.TrimToFit;
end;

function TGSimpleSparseWeighedUGraph.ContainsVertex(constref v: TVertex): Boolean;
begin
  Result := FGraph.ContainsVertex(v);
end;

function TGSimpleSparseWeighedUGraph.ContainsEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := FGraph.ContainsEdge(aSrc, aDst);
end;

function TGSimpleSparseWeighedUGraph.ContainsEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := FGraph.ContainsEdgeI(aSrc, aDst);
end;

function TGSimpleSparseWeighedUGraph.ContainsEdgeI(aSrc, aDst: SizeInt; out aData: TWeEdgeData): Boolean;
begin
  Result := FGraph.ContainsEdgeI(aSrc, aDst, aData);
end;

function TGSimpleSparseWeighedUGraph.IndexOf(constref v: TVertex): SizeInt;
begin
  Result := FGraph.IndexOf(v);
end;

function TGSimpleSparseWeighedUGraph.Adjacent(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := FGraph.Adjacent(aSrc, aDst);
end;

function TGSimpleSparseWeighedUGraph.AdjacentI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := FGraph.AdjacentI(aSrc, aDst);
end;

function TGSimpleSparseWeighedUGraph.AdjVertices(constref aSrc: TVertex): TAdjVertices;
begin
  Result := FGraph.AdjVertices(aSrc);
end;

function TGSimpleSparseWeighedUGraph.AdjVerticesI(aSrc: SizeInt): TAdjVertices;
begin
  Result := FGraph.AdjVerticesI(aSrc);
end;

function TGSimpleSparseWeighedUGraph.IncidentEdges(constref aSrc: TVertex): TIncidentEdges;
begin
  Result := FGraph.IncidentEdges(aSrc);
end;

function TGSimpleSparseWeighedUGraph.IncidentEdgesI(aSrc: SizeInt): TIncidentEdges;
begin
  Result := FGraph.IncidentEdgesI(aSrc);
end;

function TGSimpleSparseWeighedUGraph.Edges: TEdges;
begin
  Result := FGraph.Edges;
end;

function TGSimpleSparseWeighedUGraph.DistinctEdges: TDistinctEdges;
begin
  Result := FGraph.DistinctEdges;
end;

function TGSimpleSparseWeighedUGraph.AddVertex(constref v: TVertex; out aIndex: SizeInt): Boolean;
begin
  Result := FGraph.AddVertex(v, aIndex);
end;

function TGSimpleSparseWeighedUGraph.RemoveVertex(constref v: TVertex): Boolean;
begin
  Result := FGraph.RemoveVertex(v);
end;

function TGSimpleSparseWeighedUGraph.RemoveVertexI(aIndex: SizeInt): Boolean;
begin
  Result := FGraph.RemoveVertex(aIndex);
end;

function TGSimpleSparseWeighedUGraph.AddEdge(constref aSrc, aDst: TVertex; aWeight: TWeight;
  aData: TEdgeData): Boolean;
begin
  Result := FGraph.AddEdge(aSrc, aDst, TWeEdgeData.Construct(aWeight, aData));
end;

function TGSimpleSparseWeighedUGraph.AddEdge(constref aSrc, aDst: TVertex; aWeight: TWeight): Boolean;
begin
  Result := FGraph.AddEdge(aSrc, aDst, TWeEdgeData.Construct(aWeight, CFData));
end;

function TGSimpleSparseWeighedUGraph.AddEdgeI(aSrc, aDst: SizeInt; aWeight: TWeight; aData: TEdgeData): Boolean;
begin
  Result := FGraph.AddEdgeI(aSrc, aDst, TWeEdgeData.Construct(aWeight, aData));
end;

function TGSimpleSparseWeighedUGraph.AddEdgeI(aSrc, aDst: SizeInt; aWeight: TWeight): Boolean;
begin
  Result := FGraph.AddEdgeI(aSrc, aDst, TWeEdgeData.Construct(aWeight, CFData));
end;

function TGSimpleSparseWeighedUGraph.RemoveEdge(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := FGraph.RemoveEdge(aSrc, aDst);
end;

function TGSimpleSparseWeighedUGraph.RemoveEdgeI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := FGraph.RemoveEdgeI(aSrc, aDst);
end;

function TGSimpleSparseWeighedUGraph.DfsTraversal(constref aRoot: TVertex; aOnGray: TOnIntTest;
  aOnWhite: TOnIntVisit): SizeInt;
begin
  Result := FGraph.DfsTraversal(aRoot, aOnGray, aOnWhite);
end;

function TGSimpleSparseWeighedUGraph.DfsTraversalI(aRoot: SizeInt; aOnGray: TOnIntTest;
  aOnWhite: TOnIntVisit): SizeInt;
begin
  Result := FGraph.DfsTraversalI(aRoot, aOnGray, aOnWhite);
end;

function TGSimpleSparseWeighedUGraph.BfsTraversal(constref aRoot: TVertex; aOnGray: TOnIntTest;
  aOnWhite: TOnIntVisit): SizeInt;
begin
  Result := FGraph.BfsTraversal(aRoot, aOnGray, aOnWhite);
end;

function TGSimpleSparseWeighedUGraph.BfsTraversalI(aRoot: SizeInt; aOnGray: TOnIntTest;
  aOnWhite: TOnIntVisit): SizeInt;
begin
  Result := FGraph.BfsTraversalI(aRoot, aOnGray, aOnWhite);
end;

function TGSimpleSparseWeighedUGraph.SimplePathExists(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := FGraph.SimplePathExists(aSrc, aDst);
end;

function TGSimpleSparseWeighedUGraph.SimplePathExistsI(aSrc, aDst: SizeInt): Boolean;
begin
  Result := FGraph.SimplePathExistsI(aSrc, aDst);
end;

function TGSimpleSparseWeighedUGraph.IsBipartite: Boolean;
begin
  Result := FGraph.IsBipartite;
end;

function TGSimpleSparseWeighedUGraph.IsBipartite(out v: TShortArray): Boolean;
begin
  Result := FGraph.IsBipartite(v)
end;

function TGSimpleSparseWeighedUGraph.FindMinPathLen(constref aSrc, aDst: TVertex): SizeInt;
begin
  Result := FGraph.FindMinPathLen(aSrc, aDst);
end;

function TGSimpleSparseWeighedUGraph.FindMinPathLenI(aSrc, aDst: SizeInt): SizeInt;
begin
  Result := FGraph.FindMinPathLenI(aSrc, aDst);
end;

function TGSimpleSparseWeighedUGraph.FindMinPathLenVector(constref aRoot: TVertex): TIntArray;
begin
  Result := FGraph.FindMinPathLenVector(aRoot);
end;

function TGSimpleSparseWeighedUGraph.FindMinPathLenVectorI(aRoot: SizeInt): TIntArray;
begin
  Result := FGraph.FindMinPathLenVectorI(aRoot);
end;

function TGSimpleSparseWeighedUGraph.FindMinPath(constref aSrc, aDst: TVertex): TIntArray;
begin
  Result := FGraph.FindMinPath(aSrc, aDst);
end;

function TGSimpleSparseWeighedUGraph.FindMinPathI(aSrc, aDst: SizeInt): TIntArray;
begin
  Result := FGraph.FindMinPathI(aSrc, aDst);
end;

function TGSimpleSparseWeighedUGraph.Degree(constref v: TVertex): SizeInt;
begin
  Result := FGraph.Degree(v);
end;

function TGSimpleSparseWeighedUGraph.DegreeI(aIndex: SizeInt): SizeInt;
begin
  Result := FGraph.DegreeI(aIndex);
end;

function TGSimpleSparseWeighedUGraph.Isolated(constref v: TVertex): Boolean;
begin
  Result := FGraph.Isolated(v);
end;

function TGSimpleSparseWeighedUGraph.IsolatedI(aIndex: SizeInt): Boolean;
begin
  Result := FGraph.IsolatedI(aIndex);
end;

function TGSimpleSparseWeighedUGraph.EulerCycleExists: Boolean;
begin
  Result := FGraph.EulerCycleExists;
end;

function TGSimpleSparseWeighedUGraph.FindEulerCycle: TIntArray;
begin
  Result := FGraph.FindEulerCycle;
end;

function TGSimpleSparseWeighedUGraph.IsConnected: Boolean;
begin
  Result := FGraph.IsConnected;
end;

function TGSimpleSparseWeighedUGraph.MakeConnected(aOnAddEdge: TOnAddEdge): SizeInt;
begin
  Result := FGraph.MakeConnected(aOnAddEdge);
end;

function TGSimpleSparseWeighedUGraph.DfsSpanningTree(constref aRoot: TVertex): TIntArray;
begin
  Result := FGraph.DfsSpanningTree(aRoot);
end;

function TGSimpleSparseWeighedUGraph.DfsSpanningTreeI(aRoot: SizeInt): TIntArray;
begin
  Result := FGraph.DfsSpanningTreeI(aRoot);
end;

function TGSimpleSparseWeighedUGraph.BfsSpanningTree(constref aRoot: TVertex): TIntArray;
begin
  Result := FGraph.BfsSpanningTree(aRoot);
end;

function TGSimpleSparseWeighedUGraph.BfsSpanningTreeI(aRoot: SizeInt): TIntArray;
begin
  Result := FGraph.BfsSpanningTreeI(aRoot);
end;

function TGSimpleSparseWeighedUGraph.CreateFromVector(constref aVector: TIntArray): TWeighedGraph;
begin
  Result := TGSimpleSparseWeighedUGraph.Create(FGraph.CreateFromVector(aVector));
end;

function TGSimpleSparseWeighedUGraph.SeparateIndex(constref v: TVertex): SizeInt;
begin
  Result := FGraph.SeparateIndex(v);
end;

function TGSimpleSparseWeighedUGraph.SeparateIndexI(aVertex: SizeInt): SizeInt;
begin
  Result := FGraph.SeparateIndexI(aVertex);
end;

function TGSimpleSparseWeighedUGraph.SeparatePop(constref v: TVertex): SizeInt;
begin
  Result := FGraph.SeparatePop(v);
end;

function TGSimpleSparseWeighedUGraph.SeparatePopI(aVertex: SizeInt): SizeInt;
begin
  Result := FGraph.SeparatePopI(aVertex);
end;

function TGSimpleSparseWeighedUGraph.SeparateList(constref v: TVertex): TIntArray;
begin
  Result := FGraph.SeparateList(v);
end;

function TGSimpleSparseWeighedUGraph.SeparateListI(aVtxIndex: SizeInt): TIntArray;
begin
  Result := FGraph.SeparateListI(aVtxIndex);
end;

function TGSimpleSparseWeighedUGraph.IsRegular: Boolean;
begin
  Result := FGraph.IsRegular;
end;

function TGSimpleSparseWeighedUGraph.IsTree: Boolean;
begin
  Result := FGraph.IsTree;
end;

function TGSimpleSparseWeighedUGraph.FindMinPaths(constref aSrc: TVertex): TWeightArray;
begin
  Result := FindMinPathsI(FGraph.IndexOf(aSrc));
end;

function TGSimpleSparseWeighedUGraph.FindMinPathsI(aSrc: SizeInt): TWeightArray;
begin
  Result := DijkstraMap(aSrc);
end;

function TGSimpleSparseWeighedUGraph.FindMinPaths(constref aSrc: TVertex; out aPathTree: TIntArray): TWeightArray;
begin
  Result := FindMinPathsI(FGraph.IndexOf(aSrc), aPathTree);
end;

function TGSimpleSparseWeighedUGraph.FindMinPathsI(aSrc: SizeInt; out aPathTree: TIntArray): TWeightArray;
begin
  Result := DijkstraMap(aSrc, aPathTree);
end;

function TGSimpleSparseWeighedUGraph.FindMinPathWeight(constref aSrc, aDst: TVertex): TWeight;
begin
  Result := FindMinPathWeightI(FGraph.IndexOf(aSrc), FGraph.IndexOf(aDst));
end;

function TGSimpleSparseWeighedUGraph.FindMinPathWeightI(aSrc, aDst: SizeInt): TWeight;
begin
  Result := DijkstraPath(aSrc, aDst);
end;

function TGSimpleSparseWeighedUGraph.FindMinPathWeight(constref aSrc, aDst: TVertex; out aPath: TIntArray): TWeight;
begin
  Result := FindMinPathWeightI(FGraph.IndexOf(aSrc), FGraph.IndexOf(aDst), aPath);
end;

function TGSimpleSparseWeighedUGraph.FindMinPathWeightI(aSrc, aDst: SizeInt; out aPath: TIntArray): TWeight;
begin
  Result := DijkstraPath(aSrc, aDst, aPath);
end;

function TGSimpleSparseWeighedUGraph.CreateFromEdgeArray(constref aEdgeArray: TEdgeArray): TWeighedGraph;
var
  e: TEdge;
begin
  Result := TWeighedGraph.Create;
  for e in aEdgeArray do
    Result.AddEdge(Vertices[e.Source], Vertices[e.Destination], e.Data.Weight, e.Data.Data);
end;

function TGSimpleSparseWeighedUGraph.FindMinSpanningTreeKrus(out aTotalWeight: TWeight): TEdgeArray;
begin
  if IsConnected then
    Result := KruskalMst(aTotalWeight)
  else
    raise ELGGraphError.Create(SEGraphIsNotConnected);
end;

function TGSimpleSparseWeighedUGraph.FindMinSpanningTreePrim(out aTotalWeight: TWeight): TIntArray;
begin
  if IsConnected then
    Result := PrimMst(aTotalWeight)
  else
    raise ELGGraphError.Create(SEGraphIsNotConnected);
end;

function TGSimpleSparseWeighedUGraph.Clone: TWeighedGraph;
begin
  Result := TWeighedGraph.Create(FGraph.Clone);
end;

end.

