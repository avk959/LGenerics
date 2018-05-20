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
  LGraphType;

type

  { TGSimpleSparseUGraph is simple sparse undirected graph based on adjacency lists;
      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGSimpleSparseUGraph<TVertex, TEdgeData, TVertexEqRel> = class(
    specialize TGCustomSimpleSparseGraph<TVertex, TEdgeData, TVertexEqRel>)
  protected
    FConnected,
    FConnectedValid: Boolean;
    procedure DoRemoveVertex(aIndex: SizeInt);
    function  DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
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
    function  IsConnected: Boolean;
  { if the graph is not empty, then make graph connected, adding, if necessary, new edges
    from the vertex with the index 0}
    function  MakeConnected(aOnAddEdge: TOnAddEdge = nil): SizeInt;
  { returns the vector of the spanning tree, which is constructed starting from aRoot;
    each element contains the index of its parent (or -1 if it is root or not connected) }
    function  DfsSpanningTree(constref aRoot: TVertex): TIntArray; inline;
    function  DfsSpanningTreeI(aRoot: SizeInt = 0): TIntArray;
    function  BfsSpanningTree(constref aRoot: TVertex): TIntArray; inline;
    function  BfsSpanningTreeI(aRoot: SizeInt = 0): TIntArray;
  { returns the spanning tree, which is constructed starting from aRoot }
    function  CreateDfsSpanningTree(constref aRoot: TVertex): TGSimpleSparseUGraph; inline;
    function  CreateDfsSpanningTreeI(aRoot: SizeInt = 0): TGSimpleSparseUGraph;
    function  CreateBfsSpanningTree(constref aRoot: TVertex): TGSimpleSparseUGraph; inline;
    function  CreateBfsSpanningTreeI(aRoot: SizeInt = 0): TGSimpleSparseUGraph;
  { returns count of connected components }
    function  ComponentCount: SizeInt;
  { returns number of vertices(population) in the connected component that contains aRoot }
    function  ComponentPop(constref aRoot: TVertex): SizeInt; inline;
    function  ComponentPopI(aRoot: SizeInt): SizeInt; inline;
  { returns a vector whose elements contain the component index of the corresponding vertex }
    function  ComponentVector: TIntArray;
  { returns a vector whose elements contain the component index of the corresponding vertex;
    aCompCount returns count of connected components }
    function  ComponentVector(out aCompCount: SizeInt): TIntArray;
  { checks whether the graph is a regular graph (that is, the degree of all its
     vertices coincide); an empty graph is considered regular }
    function  IsRegular: Boolean;
    function  IsTree: Boolean; inline;
    //edge-connected

    function  Clone: TGSimpleSparseUGraph;
  end;

  generic TGWeighedEdgeData<TWeight, TEdgeData> = record
    Weight: TWeight;
    Data: TEdgeData;
  end;

  { TGSimpleSparseWeighedUGraph simple sparse undirected graph based on adjacency lists;
      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean;
      TWeight must have defined comparision operators }
  generic TGSimpleSparseWeighedUGraph<TVertex, TWeight, TEdgeData, TVertexEqRel> = class
  public
  type
    TWeightArray = array of TWeight;

  protected
  type
    THandleArray = array of LGUtils.THandle;
    TData        = specialize TGWeighedEdgeData<TWeight, TEdgeData>;
    TGraph       = specialize TGSimpleSparseUGraph<TVertex, TData, TVertexEqRel>;
    TAdjItem     = TGraph.TAdjItem;
    PAdjItem     = TGraph.PAdjItem;

    TWeightItem  = record
      Weight: TWeight;
      Index: SizeInt;
      class operator = (constref L, R: TWeightItem): Boolean; inline;
      class operator <>(constref L, R: TWeightItem): Boolean; inline;
      class operator > (constref L, R: TWeightItem): Boolean; inline;
      class operator < (constref L, R: TWeightItem): Boolean; inline;
      class operator >=(constref L, R: TWeightItem): Boolean; inline;
      class operator <=(constref L, R: TWeightItem): Boolean; inline;
      class function Construct(w: TWeight; i: SizeInt): TWeightItem; static; inline;
    end;

    TPriorityQueue = specialize TGLiteComparablePairHeapMin<TWeightItem>;
    //TPriorityQueue = specialize TGLiteComparableBinHeapMin<TWeightItem>;

  class var
    CFData: TEdgeData;

  var
    FGraph: TGraph;
    function  GetEdgeCount: SizeInt; inline;
    function  GetTitle: string; inline;
    function  GetVertex(aIndex: SizeInt): TVertex; inline;
    function  GetVertexCount: SizeInt; inline;
    procedure SetTitle(const aValue: string); inline;
    class function Min(const L, R: TWeight): TWeight; static; inline;
  public
  type
    TEdge = TGraph.TEdge;

    constructor Create;
    destructor Destroy; override;
    function  AddVertex(constref v: TVertex; out aIndex: SizeInt): Boolean; inline;
    function  RemoveVertex(constref v: TVertex): Boolean; inline;
    function  RemoveVertexI(aIndex: SizeInt): Boolean; inline;
    function  AddEdge(constref aSrc, aDst: TVertex; aWeight: TWeight; aData: TEdgeData): Boolean;
    function  AddEdge(constref aSrc, aDst: TVertex; aWeight: TWeight): Boolean; inline;
    function  AddEdgeI(aSrc, aDst: SizeInt; aWeight: TWeight; aData: TEdgeData): Boolean;
    function  AddEdgeI(aSrc, aDst: SizeInt; aWeight: TWeight): Boolean; inline;
  { single-source shortest paths problem }
    function  Dijkstra(constref aRoot: TVertex): TWeightArray; inline;
    function  DijkstraI(aRoot: SizeInt): TWeightArray;

    property  Title: string read GetTitle write SetTitle;
    property  VertexCount: SizeInt read GetVertexCount;
    property  EdgeCount: SizeInt read GetEdgeCount;
    property  Vertices[aIndex: SizeInt]: TVertex read GetVertex; default;
  end;

implementation
{$B-}{$COPERATORS ON}

{ TGSimpleSparseUGraph }

procedure TGSimpleSparseUGraph.DoRemoveVertex(aIndex: SizeInt);
var
  CurrEdges: TVertexItem.TAdjItemArray;
  I, J: SizeInt;
begin
  FEdgeCount -= FVertexList.ItemRefs[aIndex]^.Count;
  FConnectedValid := False;
  FVertexList.Delete(aIndex);
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

function TGSimpleSparseUGraph.AddVertex(constref v: TVertex; out aIndex: SizeInt): Boolean;
begin
  Result := not FVertexList.FindOrAdd(v, aIndex);
  if Result then
    begin
      FVertexList.ItemRefs[aIndex]^.Tag := 0;
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
    begin
      if VertexCount > 1 then
        begin
          if EdgeCount >= Pred(VertexCount) then
            FConnected := DfsTraversalI(0) = VertexCount
          else
            FConnected := False;
        end
      else
        Result := VertexCount = 1;
      FConnectedValid := True;
    end;
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
  Parent: SizeInt;
begin
  FVertexList.CheckIndexRange(aRoot);
  Visited.Size := VertexCount;
  Result := CreateIndexVector;
  {%H-}Stack.Push(aRoot);
  repeat
    if not Visited[aRoot] then
      begin
        Parent := aRoot;
        Visited[aRoot] := True;
        for aRoot in AdjVerticesI(aRoot) do
          if not Visited[aRoot] then
            begin
              Result[aRoot] := Parent;
              Stack.Push(aRoot);
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
  Parent: SizeInt;
begin
  FVertexList.CheckIndexRange(aRoot);
  Visited.Size := VertexCount;
  Result := CreateIndexVector;
  Queue.Enqueue(aRoot);
  while Queue{%H-}.TryDequeue(aRoot) do
    if not Visited[aRoot] then
      begin
        Parent := aRoot;
        Visited[aRoot] := True;
        for aRoot in AdjVerticesI(aRoot) do
          if not Visited[aRoot] then
            begin
              Result[aRoot] := Parent;
              Queue.Enqueue(aRoot);
            end;
      end;
end;

function TGSimpleSparseUGraph.CreateDfsSpanningTree(constref aRoot: TVertex): TGSimpleSparseUGraph;
begin
  Result := CreateDfsSpanningTreeI(FVertexList.IndexOf(aRoot));
end;

function TGSimpleSparseUGraph.CreateDfsSpanningTreeI(aRoot: SizeInt): TGSimpleSparseUGraph;
var
  Tree: TIntArray;
  I, Src: SizeInt;
begin
  Tree := DfsSpanningTreeI(aRoot);
  Result := TGSimpleSparseUGraph.Create;
  for I := 0 to Pred(System.Length(Tree)) do
    if Tree[I] <> -1 then
      begin
        Src := Tree[I];
        Result.AddEdge(FVertexList[Src], FVertexList[I], GetEdgeData(Src, I)^);
      end;
end;

function TGSimpleSparseUGraph.CreateBfsSpanningTree(constref aRoot: TVertex): TGSimpleSparseUGraph;
begin
  Result := CreateBfsSpanningTreeI(FVertexList.IndexOf(aRoot));
end;

function TGSimpleSparseUGraph.CreateBfsSpanningTreeI(aRoot: SizeInt): TGSimpleSparseUGraph;
var
  Tree: TIntArray;
  I, Src: SizeInt;
begin
  Tree := BfsSpanningTreeI(aRoot);
  Result := TGSimpleSparseUGraph.Create;
  for I := 0 to Pred(System.Length(Tree)) do
    if Tree[I] <> -1 then
      begin
        Src := Tree[I];
        Result.AddEdge(FVertexList[Src], FVertexList[I], GetEdgeData(Src, I)^);
      end;
end;

function TGSimpleSparseUGraph.ComponentCount: SizeInt;
var
  Visited: TBitVector;
  Stack: TIntStack;
  I, Curr: SizeInt;
begin
  Visited.Size := VertexCount;
  Result := 0;
  for I := 0 to Pred(VertexCount) do
    if not Visited[I] then
      begin
        Curr := I;
        {%H-}Stack.Push(Curr);
        repeat
          if not Visited[Curr] then
            begin
              Visited[Curr] := True;
              for Curr in AdjVerticesI(Curr) do
                if not Visited[Curr] then
                  Stack.Push(Curr);
            end;
        until not Stack.TryPop(Curr);
        Inc(Result);
      end;
end;

function TGSimpleSparseUGraph.ComponentPop(constref aRoot: TVertex): SizeInt;
begin
  Result := DfsTraversalI(FVertexList.IndexOf(aRoot));
end;

function TGSimpleSparseUGraph.ComponentPopI(aRoot: SizeInt): SizeInt;
begin
  Result := DfsTraversalI(aRoot);
end;

function TGSimpleSparseUGraph.ComponentVector: TIntArray;
var
  Visited: TBitVector;
  Stack: TIntStack;
  I, Curr, Group: SizeInt;
begin
  System.SetLength(Result, VertexCount);
  Visited.Size := VertexCount;
  Group := 0;
  for I := 0 to Pred(VertexCount) do
    if not Visited[I] then
      begin
        Curr := I;
        {%H-}Stack.Push(I);
        repeat
          if not Visited[Curr] then
            begin
              Visited[Curr] := True;
              Result[Curr] := Group;
              for Curr in AdjVerticesI(Curr) do
                if not Visited[Curr] then
                  Stack.Push(Curr);
            end;
        until not Stack.TryPop(Curr);
        Inc(Group);
      end;
end;

function TGSimpleSparseUGraph.ComponentVector(out aCompCount: SizeInt): TIntArray;
var
  Visited: TBitVector;
  Stack: TIntStack;
  I, Curr: SizeInt;
begin
  System.SetLength(Result, VertexCount);
  Visited.Size := VertexCount;
  aCompCount := 0;
  for I := 0 to Pred(VertexCount) do
    if not Visited[I] then
      begin
        Curr := I;
        {%H-}Stack.Push(I);
        repeat
          if not Visited[Curr] then
            begin
              Visited[Curr] := True;
              Result[Curr] := aCompCount;
              for Curr in AdjVerticesI(Curr) do
                if not Visited[Curr] then
                  Stack.Push(Curr);
            end;
        until not Stack.TryPop(Curr);
        Inc(aCompCount);
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

function TGSimpleSparseUGraph.Clone: TGSimpleSparseUGraph;
begin
  Result := TGSimpleSparseUGraph.Create;
  Result.FVertexList := FVertexList;
  Result.FEdgeCount := EdgeCount;
  Result.FTitle := Title;
  Result.FConnected := Connected;
  Result.FConnectedValid := ConnectedValid;
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

class function TGSimpleSparseWeighedUGraph.TWeightItem.Construct(w: TWeight; i: SizeInt): TWeightItem;
begin
  Result.Weight := w;
  Result.Index := I;
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

procedure TGSimpleSparseWeighedUGraph.SetTitle(const aValue: string);
begin
  FGraph.Title := aValue;
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

destructor TGSimpleSparseWeighedUGraph.Destroy;
begin
  FGraph.Free;
  inherited;
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
var
  d: TData;
begin
  d.Weight := aWeight;
  d.Data := aData;
  Result := FGraph.AddEdge(aSrc, aDst, d);
end;

function TGSimpleSparseWeighedUGraph.AddEdge(constref aSrc, aDst: TVertex; aWeight: TWeight): Boolean;
var
  d: TData;
begin
  d.Weight := aWeight;
  d.Data := CFData;
  Result := FGraph.AddEdge(aSrc, aDst, d);
end;

function TGSimpleSparseWeighedUGraph.AddEdgeI(aSrc, aDst: SizeInt; aWeight: TWeight; aData: TEdgeData): Boolean;
var
  d: TData;
begin
  d.Weight := aWeight;
  d.Data := aData;
  Result := FGraph.AddEdgeI(aSrc, aDst, d);
end;

function TGSimpleSparseWeighedUGraph.AddEdgeI(aSrc, aDst: SizeInt; aWeight: TWeight): Boolean;
var
  d: TData;
begin
  d.Weight := aWeight;
  d.Data := CFData;
  Result := FGraph.AddEdgeI(aSrc, aDst, d);
end;

function TGSimpleSparseWeighedUGraph.Dijkstra(constref aRoot: TVertex): TWeightArray;
begin
  Result := DijkstraI(FGraph.IndexOf(aRoot));
end;

function TGSimpleSparseWeighedUGraph.DijkstraI(aRoot: SizeInt): TWeightArray;
var
  Visited: TBitVector;
  Queue: TPriorityQueue;
  CurrWeight, MinWeight: TWeight;
  I: SizeInt;
  Item: TWeightItem;
  p: PAdjItem;
begin
  FGraph.CheckIndexRange(aRoot);
  System.SetLength(Result, VertexCount);
  for I := 0 to Pred(VertexCount) do
    Result[I] := TWeight.MaxValue;
  Result[aRoot] := Default(TWeight);
  if VertexCount < 2 then
    exit;
  Visited.Size := VertexCount;
  Queue.Enqueue(TWeightItem.Construct(Result[aRoot], aRoot));
  while Queue{%H-}.TryDequeue(Item) do
    begin
      if not Visited[Item.Index] then
        begin
          I := Item.Index;
          CurrWeight := Item.Weight;
          Visited[I] := True;
          Result[I] := CurrWeight;
          for p in FGraph.AdjVerticesPtr(I) do
            begin
              MinWeight := Min(Result[p^.Key], p^.Data.Weight + CurrWeight);
              Queue.Enqueue(TWeightItem.Construct(MinWeight, p^.Key));
            end;
        end;
    end;
end;

end.

