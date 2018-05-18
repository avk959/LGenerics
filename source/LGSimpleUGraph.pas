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
  LGraphUtils,
  LGraphType;

type

  { TGSimpleSparseUGraph simple sparse undirected graph based on adjacency lists;
      functor TVertexEqRel must provide:
        class function HashCode([const[ref]] aValue: TVertex): SizeInt;
        class function Equal([const[ref]] L, R: TVertex): Boolean; }
  generic TGSimpleSparseUGraph<TVertex, TEdgeData, TVertexEqRel> = class(
    specialize TGCustomSimpleSparseGraph<TVertex, TEdgeData, TVertexEqRel>)
  protected
  class var
    CFData: TEdgeData;
    procedure DoRemoveVertex(aIndex: SizeInt);
    function  DoAddEdge(aSrc, aDst: SizeInt; aData: TEdgeData): Boolean;
    function  DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
  public
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
    function  EulerPathExists(constref aSrc, aDst: TVertex): Boolean; inline;
    function  EulerPathExistsI(aSrc, aDst: SizeInt): Boolean;
  { checks whether the graph is connected; a graph without vertices is considered disconnected }
    function  Connected: Boolean;
  { returns the vector of the spanning tree, which is constructed starting from aRoot;
    each element contains the index of its parent (or -1 if it is root or not connected) }
    function  SpanningTree(constref aRoot: TVertex): TIntArray; inline;
    function  SpanningTreeI(aRoot: SizeInt = 0): TIntArray;
  { returns the spanning tree, which is constructed starting from aRoot }
    function  CreateSpanningTree(constref aRoot: TVertex): TGSimpleSparseUGraph; inline;
    function  CreateSpanningTreeI(aRoot: SizeInt = 0): TGSimpleSparseUGraph;
  { returns count of connected components }
    function  ComponentCount: SizeInt;
  { returns number of vertices in the connected component that contains aRoot }
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
  { returns the length of the shortest path between the vertices aSrc and aDst,
    -1 if the path does not exist }
    function  FindMinPath(constref aSrc, aDst: TVertex): SizeInt; inline;
    function  FindMinPathI(aSrc, aDst: SizeInt): SizeInt;
  { returns a vector containing in the corresponding components the shortest distances from aRoot }
    function  FindMinPathVector(constref aRoot: TVertex): TIntArray; inline;
    function  FindMinPathVectorI(aRoot: SizeInt = 0): TIntArray;

    function  Clone: TGSimpleSparseUGraph;
  end;

implementation
{$B-}{$COPERATORS ON}

{ TGSimpleSparseUGraph }

procedure TGSimpleSparseUGraph.DoRemoveVertex(aIndex: SizeInt);
var
  I, J: SizeInt;
  pV: ^TVertexItem;
  CurrEdges: TVertexItem.TAdjItemArray;
begin
  FEdgeCount -= FVertexList.FNodeList[aIndex].Item.Count;
  FVertexList.Delete(aIndex);
  for I := 0 to Pred(FVertexList.Count) do
    begin
      pV := @FVertexList.FNodeList[I].Item;
      CurrEdges := pV^.ToArray;
      pV^.ClearItems;
      for J := 0 to System.High(CurrEdges) do
        begin
          if CurrEdges[J].Destination <> aIndex then
            begin
              if CurrEdges[J].Destination > aIndex then
                Dec(CurrEdges[J].Destination);
              pV^.Add(CurrEdges[J]);
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
  Result := not FVertexList.FNodeList[aSrc].Item.FindOrAdd(aDst, p);
  if Result then
    begin
      p^.Destination := aDst;
      p^.Data := aData;
      FVertexList.FNodeList[aDst].Item.FindOrAdd(aSrc, p);
      p^.Destination := aSrc;
      p^.Data := aData;
      Inc(FEdgeCount);
    end;
end;

function TGSimpleSparseUGraph.DoRemoveEdge(aSrc, aDst: SizeInt): Boolean;
begin
  if aSrc = aDst then
    exit(False);
  Result := FVertexList.FNodeList[aSrc].Item.Remove(aDst);
  if Result then
    begin
      FVertexList.FNodeList[aSrc].Item.Remove(aSrc);
      Dec(FEdgeCount);
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
  Result := FVertexList.FNodeList[aIndex].Item.Count;
end;

function TGSimpleSparseUGraph.Isolated(constref v: TVertex): Boolean;
begin
  Result := Degree(v) = 0;
end;

function TGSimpleSparseUGraph.IsolatedI(aIndex: SizeInt): Boolean;
begin
  Result := DegreeI(aIndex) = 0;
end;

function TGSimpleSparseUGraph.EulerPathExists(constref aSrc, aDst: TVertex): Boolean;
begin
  Result := EulerPathExistsI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aDst));
end;

function TGSimpleSparseUGraph.EulerPathExistsI(aSrc, aDst: SizeInt): Boolean;
var
  I: SizeInt;
begin
  if Odd(DegreeI(aSrc) + DegreeI(aDst)) then
    exit(False);
  for I := 0 to Pred(VertexCount) do
    if (I <> aSrc) and (I <> aDst) then
      if Odd(DegreeI(I)) then
        exit(False);
  Result := True;
end;

function TGSimpleSparseUGraph.Connected: Boolean;
begin
  if VertexCount > 1 then
    begin
      if EdgeCount >= Pred(VertexCount) then
        Result := DSFTraversalI(0) = VertexCount
      else
        Result := False;
    end
  else
    Result := VertexCount = 1;
end;

function TGSimpleSparseUGraph.SpanningTree(constref aRoot: TVertex): TIntArray;
begin
  Result := SpanningTreeI(FVertexList.IndexOf(aRoot));
end;

function TGSimpleSparseUGraph.SpanningTreeI(aRoot: SizeInt): TIntArray;
var
  Visited: TBitVector;
  Stack: TIntStack;
  Parent: SizeInt;
begin
  if IsEmpty then
    exit(nil);
  Visited.Size := VertexCount;
  System.SetLength(Result, VertexCount);
  System.FillChar(Result[0], System.Length(Result) * SizeOf(SizeInt), $ff);
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

function TGSimpleSparseUGraph.CreateSpanningTree(constref aRoot: TVertex): TGSimpleSparseUGraph;
begin
  Result := CreateSpanningTreeI(FVertexList.IndexOf(aRoot));
end;

function TGSimpleSparseUGraph.CreateSpanningTreeI(aRoot: SizeInt): TGSimpleSparseUGraph;
var
  Tree: TIntArray;
  I, Src: SizeInt;
begin
  Result := TGSimpleSparseUGraph.Create;
  Tree := SpanningTreeI(aRoot);
  for I := 0 to Pred(Length(Tree)) do
    if Tree[I] <> -1 then
      begin
        Src := Tree[I];
        Result.AddEdge(FVertexList[Src], FVertexList[I], GetEdgeData(Src, I));
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
  Result := DSFTraversalI(FVertexList.IndexOf(aRoot));
end;

function TGSimpleSparseUGraph.ComponentPopI(aRoot: SizeInt): SizeInt;
begin
  Result := DSFTraversalI(aRoot);
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
  Result := (EdgeCount = Pred(VertexCount)) and Connected;
end;

function TGSimpleSparseUGraph.FindMinPath(constref aSrc, aDst: TVertex): SizeInt;
begin
  Result := FindMinPathI(FVertexList.IndexOf(aSrc), FVertexList.IndexOf(aSrc));
end;

function TGSimpleSparseUGraph.FindMinPathI(aSrc, aDst: SizeInt): SizeInt;
var
  Queue: TIntQueue;
  v: TIntArray;
  d: SizeInt;
begin
  FVertexList.CheckIndexRange(aSrc);
  FVertexList.CheckIndexRange(aDst);
  System.SetLength(v, VertexCount);
  System.FillChar(v[0], System.Length(v) * SizeOf(SizeInt), $ff);
  v[aSrc] := 0;
  Queue.Enqueue(aSrc);
  while Queue.TryDequeue(aSrc) do
    for d in AdjVerticesI(aSrc) do
      if v[d] = -1 then
        begin
          Queue.Enqueue(d);
          v[d] := Succ(v[aSrc]);
        end;
  Result := v[aDst];
end;

function TGSimpleSparseUGraph.FindMinPathVector(constref aRoot: TVertex): TIntArray;
begin
  Result := FindMinPathVectorI(FVertexList.IndexOf(aRoot));
end;

function TGSimpleSparseUGraph.FindMinPathVectorI(aRoot: SizeInt): TIntArray;
var
  Queue: TIntQueue;
  d: SizeInt;
begin
  FVertexList.CheckIndexRange(aRoot);
  System.SetLength(Result, VertexCount);
  System.FillChar(Result[0], System.Length(Result) * SizeOf(SizeInt), $ff);
  Result[aRoot] := 0;
  Queue.Enqueue(aRoot);
  while Queue.TryDequeue(aRoot) do
    for d in AdjVerticesI(aRoot) do
      if Result[d] = -1 then
        begin
          Queue.Enqueue(d);
          Result[d] := Succ(Result[aRoot]);
        end;
end;

function TGSimpleSparseUGraph.Clone: TGSimpleSparseUGraph;
begin
  Result := TGSimpleSparseUGraph.Create;
  Result.FVertexList := FVertexList;
  Result.FEdgeCount := EdgeCount;
  Result.FTitle := Title;
end;

end.

